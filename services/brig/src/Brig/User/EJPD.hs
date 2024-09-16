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

-- | Identify users for law enforcement.  (Wire has legal requirements to cooperate with the
-- authorities.  The wire backend operations team uses this to answer identification requests
-- manually.)
module Brig.User.EJPD (ejpdRequest) where

import Bilge.Request
import Bilge.Response
import Brig.API.Handler
import Brig.API.User (lookupHandle)
import Brig.App
import Brig.Data.Connection qualified as Conn
import Brig.Data.User (lookupUser)
import Control.Error hiding (bool)
import Control.Lens (view, (^.))
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Imports hiding (head)
import Network.HTTP.Types.Method
import Polysemy
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Connection
import Wire.API.Push.Token qualified as PushTok
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Team.Member qualified as Team
import Wire.API.User
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Rpc
import Wire.UserStore (UserStore)

-- FUTUREWORK(mangoiv): this uses 'UserStore' and should hence go to 'UserSubSystem'
ejpdRequest ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member NotificationSubsystem r,
    Member UserStore r,
    Member Rpc r
  ) =>
  Maybe Bool ->
  EJPDRequestBody ->
  (Handler r) EJPDResponseBody
ejpdRequest (fromMaybe False -> includeContacts) (EJPDRequestBody handles) = do
  ExceptT $ Right . EJPDResponseBody . catMaybes <$> forM handles responseItemForHandle
  where
    -- find uid given handle
    responseItemForHandle :: Handle -> AppT r (Maybe EJPDResponseItemRoot)
    responseItemForHandle hdl = do
      mbUid <- liftSem $ lookupHandle hdl
      mbUsr <- maybe (pure Nothing) (wrapClient . lookupUser NoPendingInvitations) mbUid
      maybe (pure Nothing) (fmap Just . responseItemForExistingUser includeContacts) mbUsr

    -- construct response item given uid
    responseItemForExistingUser :: Bool -> User -> (AppT r) EJPDResponseItemRoot
    responseItemForExistingUser reallyIncludeContacts target = do
      let uid = userId target
      luid <- qualifyLocal uid

      ptoks <-
        PushTok.tokenText . view PushTok.token <$$> liftSem (getPushTokens uid)

      mbContacts <-
        if reallyIncludeContacts
          then do
            contacts <-
              wrapClient $ -- FUTUREWORK: use polysemy effect, not wrapClient
                Conn.lookupContactListWithRelation uid

            localContacts <-
              catMaybes <$> do
                forM contacts $ \(uid', relationDropHistory -> rel) -> do
                  mbUsr <- wrapClient $ lookupUser NoPendingInvitations uid' -- FUTUREWORK: use polysemy effect, not wrapClient
                  maybe (pure Nothing) (fmap (Just . EJPDContactFound rel . toEJPDResponseItemLeaf) . responseItemForExistingUser False) mbUsr

            pure . Just . Set.fromList $ localContacts
          else do
            pure Nothing

      mbTeamContacts <-
        case (reallyIncludeContacts, userTeam target) of
          (True, Just tid) -> do
            memberList <- liftSem $ GalleyAPIAccess.getTeamMembers tid
            let members = (view Team.userId <$> (memberList ^. Team.teamMembers)) \\ [uid]

            contactsFull <-
              forM members $ \uid' -> do
                mbUsr <- wrapClient $ lookupUser NoPendingInvitations uid'
                maybe (pure Nothing) (fmap Just . responseItemForExistingUser False) mbUsr

            let listType = Team.toNewListType (memberList ^. Team.teamMemberListType)

            pure . Just $
              EJPDTeamContacts
                (Set.fromList $ toEJPDResponseItemLeaf <$> catMaybes contactsFull)
                listType
          _ -> do
            pure Nothing

      mbConversations <-
        if reallyIncludeContacts
          then liftSem $ Just . Set.fromList <$> GalleyAPIAccess.getEJPDConvInfo uid
          else pure Nothing

      mbAssets <- do
        urls <- forM (userAssets target) $ \(asset :: Asset) -> do
          cgh <- asks (view cargoholdEndpoint)
          let key = toByteString' $ assetKey asset
          resp <- liftSem $ rpcWithRetries "cargohold" cgh (method GET . paths ["/i/assets", key])
          pure $
            case (statusCode resp, responseJsonEither resp) of
              (200, Right (A.String loc)) -> loc
              _ ->
                T.pack $
                  "could not fetch asset: "
                    <> show key
                    <> ", error: "
                    <> show (statusCode resp, responseBody resp)
        pure $ case urls of
          [] -> Nothing
          something -> Just (Set.fromList something)

      pure $
        EJPDResponseItemRoot
          { ejpdResponseRootUserId = tUntagged luid,
            ejpdResponseRootTeamId = userTeam target,
            ejpdResponseRootName = userDisplayName target,
            ejpdResponseRootHandle = userHandle target,
            ejpdResponseRootEmail = userEmail target,
            ejpdResponseRootPhone = Nothing,
            ejpdResponseRootPushTokens = Set.fromList ptoks,
            ejpdResponseRootContacts = mbContacts,
            ejpdResponseRootTeamContacts = mbTeamContacts,
            ejpdResponseRootConversations = mbConversations,
            ejpdResponseRootAssets = mbAssets
          }
