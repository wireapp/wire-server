{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.Sem.GundeckAccess.Http (gundeckAccessToHttp) where

import qualified Bilge as RPC
import Bilge.IO
import Bilge.RPC
import Bilge.Request
import Brig.RPC
import Brig.Sem.Common
import Brig.Sem.GundeckAccess
import Brig.Types.User.Event
import Control.Lens ((.~), (?~))
import Control.Monad.Catch
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Id
import Data.Json.Util ((#))
import Data.List.NonEmpty
import Data.List.Split (chunksOf)
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Gundeck.Types.Push.V2
import Imports hiding (toList)
import Network.HTTP.Types.Method
import Polysemy
import System.Logger.Class as Log hiding (name, (.=))
import Wire.API.Connection
import Wire.API.Properties
import Wire.API.User
import Wire.API.User.Client

gundeckAccessToHttp ::
  forall m r a.
  ( MonadIO m,
    MonadLogger m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    Member (Embed m) r
  ) =>
  RPC.Request ->
  Sem (GundeckAccess ': r) a ->
  Sem r a
gundeckAccessToHttp g =
  interpret $
    embed @m . \case
      PushEvents events users orig route mConn -> do
        push g events users orig route mConn

-- | Push events to other users.
push ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadMask m,
    MonadCatch m,
    MonadHttp m,
    HasRequestId m
  ) =>
  -- | The request to the Gundeck component
  RPC.Request ->
  -- | The events to push.
  NonEmpty Event ->
  -- | The users to push to.
  NonEmpty UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  m ()
push g (toList -> events) usrs orig route conn =
  case mapMaybe toPushData events of
    [] -> pure ()
    x : xs -> rawPush g (x :| xs) usrs orig route conn
  where
    toPushData :: Event -> Maybe (Builder, (A.Object, Maybe ApsData))
    toPushData e = case toPushFormat e of
      Just o -> Just (Log.bytes e, (o, toApsData e))
      Nothing -> Nothing

-- | Push encoded events to other users. Useful if you want to push
-- something that's not defined in Brig.
rawPush ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadMask m,
    MonadCatch m,
    MonadHttp m,
    HasRequestId m
  ) =>
  -- | The request to the Gundeck component
  RPC.Request ->
  -- | The events to push.
  NonEmpty (Builder, (A.Object, Maybe ApsData)) ->
  -- | The users to push to.
  NonEmpty UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  m ()
-- TODO: if we decide to have service whitelist events in Brig instead of
-- Galley, let's merge 'push' and 'rawPush' back. See Note [whitelist events].
rawPush g (toList -> events) usrs orig route conn = do
  for_ events $ \e -> debug $ remote "gundeck" . msg (fst e)
  let gReq rs =
        method POST
          . path "/i/push/v2"
          . zUser orig -- FUTUREWORK: Remove, because gundeck handler ignores this.
          . json (fmap (mkPush rs . snd) events)
          . expect2xx
  forM_ recipients $ \rcps -> makeReq "gundeck" g POST (gReq rcps)
  where
    recipients :: [Range 1 1024 (Set.Set Recipient)]
    recipients =
      fmap (unsafeRange . Set.fromList) $
        chunksOf 512 $
          fmap (`recipient` route) $
            toList usrs
    mkPush :: Range 1 1024 (Set.Set Recipient) -> (A.Object, Maybe ApsData) -> Push
    mkPush rcps (o, aps) =
      newPush
        (Just orig)
        rcps
        (singletonPayload o)
        & pushOriginConnection .~ conn
        & pushNativeAps .~ aps

toPushFormat :: Event -> Maybe A.Object
toPushFormat (UserEvent (UserCreated u)) =
  Just $
    A.fromList
      [ "type" .= ("user.new" :: Text),
        "user" .= SelfProfile (u {userIdentity = Nothing})
      ]
toPushFormat (UserEvent (UserActivated u)) =
  Just $
    A.fromList
      [ "type" .= ("user.activate" :: Text),
        "user" .= SelfProfile u
      ]
toPushFormat (UserEvent (UserUpdated (UserUpdatedData i n pic acc ass hdl loc mb ssoId ssoIdDel))) =
  Just $
    A.fromList
      [ "type" .= ("user.update" :: Text),
        "user"
          .= object
            ( "id" .= i
                # "name" .= n
                # "picture" .= pic -- DEPRECATED
                # "accent_id" .= acc
                # "assets" .= ass
                # "handle" .= hdl
                # "locale" .= loc
                # "managed_by" .= mb
                # "sso_id" .= ssoId
                # "sso_id_deleted" .= ssoIdDel
                # []
            )
      ]
toPushFormat (UserEvent (UserIdentityUpdated UserIdentityUpdatedData {..})) =
  Just $
    A.fromList
      [ "type" .= ("user.update" :: Text),
        "user"
          .= object
            ( "id" .= eiuId
                # "email" .= eiuEmail
                # "phone" .= eiuPhone
                # []
            )
      ]
toPushFormat (UserEvent (UserIdentityRemoved (UserIdentityRemovedData i e p))) =
  Just $
    A.fromList
      [ "type" .= ("user.identity-remove" :: Text),
        "user"
          .= object
            ( "id" .= i
                # "email" .= e
                # "phone" .= p
                # []
            )
      ]
toPushFormat (ConnectionEvent (ConnectionUpdated uc _ name)) =
  Just $
    A.fromList $
      "type" .= ("user.connection" :: Text)
        # "connection" .= uc
        # "user" .= case name of
          Just n -> Just $ object ["name" .= n]
          Nothing -> Nothing
        # []
toPushFormat (UserEvent (UserSuspended i)) =
  Just $
    A.fromList
      [ "type" .= ("user.suspend" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserResumed i)) =
  Just $
    A.fromList
      [ "type" .= ("user.resume" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserDeleted qid)) =
  Just $
    A.fromList
      [ "type" .= ("user.delete" :: Text),
        "id" .= qUnqualified qid,
        "qualified_id" .= qid
      ]
toPushFormat (UserEvent (UserLegalHoldDisabled i)) =
  Just $
    A.fromList
      [ "type" .= ("user.legalhold-disable" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserLegalHoldEnabled i)) =
  Just $
    A.fromList
      [ "type" .= ("user.legalhold-enable" :: Text),
        "id" .= i
      ]
toPushFormat (PropertyEvent (PropertySet _ k v)) =
  Just $
    A.fromList
      [ "type" .= ("user.properties-set" :: Text),
        "key" .= k,
        "value" .= propertyValue v
      ]
toPushFormat (PropertyEvent (PropertyDeleted _ k)) =
  Just $
    A.fromList
      [ "type" .= ("user.properties-delete" :: Text),
        "key" .= k
      ]
toPushFormat (PropertyEvent (PropertiesCleared _)) =
  Just $
    A.fromList
      [ "type" .= ("user.properties-clear" :: Text)
      ]
toPushFormat (ClientEvent (ClientAdded _ c)) =
  Just $
    A.fromList
      [ "type" .= ("user.client-add" :: Text),
        "client" .= c
      ]
toPushFormat (ClientEvent (ClientRemoved _ c)) =
  Just $
    A.fromList
      [ "type" .= ("user.client-remove" :: Text),
        "client" .= IdObject (clientId c)
      ]
toPushFormat (UserEvent (LegalHoldClientRequested payload)) =
  let LegalHoldClientRequestedData targetUser lastPrekey' clientId = payload
   in Just $
        A.fromList
          [ "type" .= ("user.legalhold-request" :: Text),
            "id" .= targetUser,
            "last_prekey" .= lastPrekey',
            "client" .= IdObject clientId
          ]

toApsData :: Event -> Maybe ApsData
toApsData (ConnectionEvent (ConnectionUpdated uc _ name)) =
  case (ucStatus uc, name) of
    (MissingLegalholdConsent, _) -> Nothing
    (Pending, n) -> apsConnRequest <$> n
    (Accepted, n) -> apsConnAccept <$> n
    (Blocked, _) -> Nothing
    (Ignored, _) -> Nothing
    (Sent, _) -> Nothing
    (Cancelled, _) -> Nothing
  where
    apsConnRequest n =
      apsData (ApsLocKey "push.notification.connection.request") [fromName n]
        & apsSound ?~ ApsSound "new_message_apns.caf"
    apsConnAccept n =
      apsData (ApsLocKey "push.notification.connection.accepted") [fromName n]
        & apsSound ?~ ApsSound "new_message_apns.caf"
toApsData _ = Nothing
