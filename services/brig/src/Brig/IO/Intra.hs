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

-- FUTUREWORK: Move to Brig.User.RPC or similar.
module Brig.IO.Intra
  ( -- * Pushing & Journaling Events
    onUserEvent,
    onConnectionEvent,
    onPropertyEvent,
    onClientEvent,

    -- * Conversations
    createConnectConv,
    acceptConnectConv,
    blockConv,
    unblockConv,
    upsertOne2OneConversation,

    -- * Clients
    rmClient,
    lookupPushToken,

    -- * Account Deletion
    rmUser,

    -- * Legalhold
    guardLegalhold,

    -- * Low Level API for Notifications
    notify,
  )
where

import Bilge hiding (head, options, requestId)
import Bilge.RPC
import Bilge.Retry
import Brig.API.Error (internalServerError)
import Brig.API.Types
import Brig.API.Util
import Brig.App
import Brig.Data.Connection (lookupContactList)
import qualified Brig.Data.Connection as Data
import Brig.Federation.Client (notifyUserDeleted)
import qualified Brig.IO.Journal as Journal
import Brig.RPC
import Brig.Types.User.Event
import Brig.User.Search.Index (MonadIndexIO)
import qualified Brig.User.Search.Index as Search
import Cassandra (MonadClient)
import Conduit (runConduit, (.|))
import Control.Error (ExceptT)
import Control.Error.Util
import Control.Lens (view, (.~), (?~), (^.))
import Control.Monad.Catch
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Retry
import Data.Aeson hiding (json)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as C
import Data.Domain
import Data.Either.Combinators (whenLeft)
import Data.Id
import Data.Json.Util ((#))
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import Data.Proxy
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import GHC.TypeLits
import Galley.Types.Conversations.Intra (UpsertOne2OneConversationRequest, UpsertOne2OneConversationResponse)
import Galley.Types.Teams.Intra (GuardLegalholdPolicyConflicts (GuardLegalholdPolicyConflicts))
import Gundeck.Types.Push.V2
import qualified Gundeck.Types.Push.V2 as Push
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class as Log hiding (name, (.=))
import qualified System.Logger.Extended as ExLog
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Event.Conversation (Connect (Connect))
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Error
import Wire.API.Properties
import Wire.API.Team.LegalHold (LegalholdProtectee)
import qualified Wire.API.Team.Member as Team
import Wire.API.User
import Wire.API.User.Client

-----------------------------------------------------------------------------
-- Event Handlers

onUserEvent ::
  ( MonadLogger m,
    MonadIndexIO m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m,
    MonadClient m,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  UserId ->
  Maybe ConnId ->
  UserEvent ->
  m ()
onUserEvent orig conn e =
  updateSearchIndex orig e
    *> dispatchNotifications orig conn e
    *> journalEvent orig e

onConnectionEvent ::
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID, if any.
  Maybe ConnId ->
  -- | The event.
  ConnectionEvent ->
  (AppT r) ()
onConnectionEvent orig conn evt = do
  let from = ucFrom (ucConn evt)
  wrapHttp $
    notify
      (singleton $ ConnectionEvent evt)
      orig
      Push.RouteAny
      conn
      (pure $ list1 from [])

onPropertyEvent ::
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  ConnId ->
  PropertyEvent ->
  (AppT r) ()
onPropertyEvent orig conn e =
  wrapHttp $
    notify
      (singleton $ PropertyEvent e)
      orig
      Push.RouteDirect
      (Just conn)
      (pure $ list1 orig [])

onClientEvent ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  m ()
onClientEvent orig conn e = do
  let events = singleton (ClientEvent e)
  let rcps = list1 orig []
  -- Synchronous push for better delivery guarantees of these
  -- events and to make sure new clients have a first notification
  -- in the stream.
  push events rcps orig Push.RouteAny conn

updateSearchIndex ::
  ( MonadClient m,
    MonadCatch m,
    MonadLogger m,
    MonadIndexIO m
  ) =>
  UserId ->
  UserEvent ->
  m ()
updateSearchIndex orig e = case e of
  -- no-ops
  UserCreated {} -> pure ()
  UserIdentityUpdated UserIdentityUpdatedData {..} -> do
    when (isJust eiuEmail) $ Search.reindex orig
  UserIdentityRemoved {} -> pure ()
  UserLegalHoldDisabled {} -> pure ()
  UserLegalHoldEnabled {} -> pure ()
  LegalHoldClientRequested {} -> pure ()
  UserSuspended {} -> Search.reindex orig
  UserResumed {} -> Search.reindex orig
  UserActivated {} -> Search.reindex orig
  UserDeleted {} -> Search.reindex orig
  UserUpdated UserUpdatedData {..} -> do
    let interesting =
          or
            [ isJust eupName,
              isJust eupAccentId,
              isJust eupHandle,
              isJust eupManagedBy,
              isJust eupSSOId || eupSSOIdRemoved
            ]
    when interesting $ Search.reindex orig

journalEvent :: (MonadReader Env m, MonadIO m) => UserId -> UserEvent -> m ()
journalEvent orig e = case e of
  UserActivated acc ->
    Journal.userActivate acc
  UserUpdated UserUpdatedData {eupName = Just name} ->
    Journal.userUpdate orig Nothing Nothing (Just name)
  UserUpdated UserUpdatedData {eupLocale = Just loc} ->
    Journal.userUpdate orig Nothing (Just loc) Nothing
  UserIdentityUpdated (UserIdentityUpdatedData _ (Just em) _) ->
    Journal.userUpdate orig (Just em) Nothing Nothing
  UserIdentityRemoved (UserIdentityRemovedData _ (Just em) _) ->
    Journal.userEmailRemove orig em
  UserDeleted {} ->
    Journal.userDelete orig
  _ ->
    pure ()

-------------------------------------------------------------------------------
-- Low-Level Event Notification

-- | Notify the origin user's contact list (first-level contacts),
-- as well as his other clients about a change to his user account
-- or profile.
dispatchNotifications ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m,
    MonadClient m,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  UserId ->
  Maybe ConnId ->
  UserEvent ->
  m ()
dispatchNotifications orig conn e = case e of
  UserCreated {} -> pure ()
  UserSuspended {} -> pure ()
  UserResumed {} -> pure ()
  LegalHoldClientRequested {} -> notifyContacts event orig Push.RouteAny conn
  UserLegalHoldDisabled {} -> notifyContacts event orig Push.RouteAny conn
  UserLegalHoldEnabled {} -> notifyContacts event orig Push.RouteAny conn
  UserUpdated UserUpdatedData {..}
    -- This relies on the fact that we never change the locale AND something else.
    | isJust eupLocale -> notifySelf event orig Push.RouteDirect conn
    | otherwise -> notifyContacts event orig Push.RouteDirect conn
  UserActivated {} -> notifySelf event orig Push.RouteAny conn
  UserIdentityUpdated {} -> notifySelf event orig Push.RouteDirect conn
  UserIdentityRemoved {} -> notifySelf event orig Push.RouteDirect conn
  UserDeleted {} -> do
    -- n.b. Synchronously fetch the contact list on the current thread.
    -- If done asynchronously, the connections may already have been deleted.
    notifyUserDeletionLocals orig conn event
    notifyUserDeletionRemotes orig
  where
    event = singleton $ UserEvent e

notifyUserDeletionLocals ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m,
    MonadClient m
  ) =>
  UserId ->
  Maybe ConnId ->
  List1 Event ->
  m ()
notifyUserDeletionLocals deleted conn event = do
  recipients <- list1 deleted <$> lookupContactList deleted
  notify event deleted Push.RouteDirect conn (pure recipients)

notifyUserDeletionRemotes ::
  forall m.
  ( MonadReader Env m,
    MonadClient m,
    MonadLogger m,
    CallsFed 'Brig "on-user-deleted-connections"
  ) =>
  UserId ->
  m ()
notifyUserDeletionRemotes deleted = do
  runConduit $
    Data.lookupRemoteConnectedUsersC deleted (fromInteger (natVal (Proxy @UserDeletedNotificationMaxConnections)))
      .| C.mapM_ fanoutNotifications
  where
    fanoutNotifications :: [Remote UserId] -> m ()
    fanoutNotifications = mapM_ notifyBackend . bucketRemote

    notifyBackend :: Remote [UserId] -> m ()
    notifyBackend uids = do
      case tUnqualified (checked <$> uids) of
        Nothing ->
          -- The user IDs cannot be more than 1000, so we can assume the range
          -- check will only fail because there are 0 User Ids.
          pure ()
        Just rangedUids -> do
          luidDeleted <- qualifyLocal deleted
          eitherFErr <- runExceptT (notifyUserDeleted luidDeleted (qualifyAs uids rangedUids))
          whenLeft eitherFErr $
            logFederationError (tDomain uids)

    logFederationError :: Domain -> FederationError -> m ()
    logFederationError domain fErr =
      Log.err $
        Log.msg ("Federation error while notifying remote backends of a user deletion." :: ByteString)
          . Log.field "user_id" (show deleted)
          . Log.field "domain" (domainText domain)
          . Log.field "error" (show fErr)

-- | Push events to other users.
push ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  -- | The events to push.
  List1 Event ->
  -- | The users to push to.
  List1 UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Push.Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  m ()
push (toList -> events) usrs orig route conn =
  case mapMaybe toPushData events of
    [] -> pure ()
    x : xs -> rawPush (list1 x xs) usrs orig route conn
  where
    toPushData :: Event -> Maybe (Builder, (Object, Maybe ApsData))
    toPushData e = case toPushFormat e of
      Just o -> Just (Log.bytes e, (o, toApsData e))
      Nothing -> Nothing

-- | Push encoded events to other users. Useful if you want to push
-- something that's not defined in Brig.
rawPush ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  -- | The events to push.
  List1 (Builder, (Object, Maybe ApsData)) ->
  -- | The users to push to.
  List1 UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Push.Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  m ()
-- TODO: if we decide to have service whitelist events in Brig instead of
-- Galley, let's merge 'push' and 'rawPush' back. See Note [whitelist events].
rawPush (toList -> events) usrs orig route conn = do
  for_ events $ \e -> debug $ remote "gundeck" . msg (fst e)
  g <- view gundeck
  forM_ recipients $ \rcps ->
    void . recovering x3 rpcHandlers $
      const $
        rpc'
          "gundeck"
          g
          ( method POST
              . path "/i/push/v2"
              . zUser orig -- FUTUREWORK: Remove, because gundeck handler ignores this.
              . json (map (mkPush rcps . snd) events)
              . expect2xx
          )
  where
    recipients :: [Range 1 1024 (Set.Set Recipient)]
    recipients =
      map (unsafeRange . Set.fromList) $
        chunksOf 512 $
          map (`recipient` route) $
            toList usrs
    mkPush :: Range 1 1024 (Set.Set Recipient) -> (Object, Maybe ApsData) -> Push
    mkPush rcps (o, aps) =
      newPush
        (Just orig)
        rcps
        (singletonPayload o)
        & pushOriginConnection .~ conn
        & pushNativeAps .~ aps

-- | (Asynchronously) notifies other users of events.
notify ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m
  ) =>
  List1 Event ->
  -- | Origin user, TODO: Delete
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  -- | Users to notify.
  m (List1 UserId) ->
  m ()
notify events orig route conn recipients = fork (Just orig) $ do
  rs <- recipients
  push events rs orig route conn

fork ::
  (MonadIO m, MonadUnliftIO m, MonadReader Env m) =>
  Maybe UserId ->
  m a ->
  m ()
fork u ma = do
  g <- view applog
  r <- view requestId
  let logErr e = ExLog.err g $ request r ~~ user u ~~ msg (show e)
  withRunInIO $ \lower ->
    void . liftIO . forkIO $
      either logErr (const $ pure ())
        =<< runExceptT (syncIO $ lower ma)
  where
    request = field "request" . unRequestId
    user = maybe id (field "user" . toByteString)

notifySelf ::
  ( MonadIO m,
    Log.MonadLogger m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m
  ) =>
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  m ()
notifySelf events orig route conn =
  notify events orig route conn (pure (singleton orig))

notifyContacts ::
  forall m.
  ( MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m,
    MonadClient m,
    MonadUnliftIO m
  ) =>
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  m ()
notifyContacts events orig route conn = do
  notify events orig route conn $
    list1 orig <$> liftA2 (++) contacts teamContacts
  where
    contacts :: m [UserId]
    contacts = lookupContactList orig

    teamContacts :: m [UserId]
    teamContacts = screenMemberList <$> getTeamContacts orig
    -- If we have a truncated team, we just ignore it all together to avoid very large fanouts
    --
    screenMemberList :: Maybe Team.TeamMemberList -> [UserId]
    screenMemberList (Just mems)
      | mems ^. Team.teamMemberListType == Team.ListComplete =
          view Team.userId <$> mems ^. Team.teamMembers
    screenMemberList _ = []

-- Event Serialisation:

toPushFormat :: Event -> Maybe Object
toPushFormat (UserEvent (UserCreated u)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.new" :: Text),
        "user" .= SelfProfile (u {userIdentity = Nothing})
      ]
toPushFormat (UserEvent (UserActivated u)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.activate" :: Text),
        "user" .= SelfProfile u
      ]
toPushFormat (UserEvent (UserUpdated (UserUpdatedData i n pic acc ass hdl loc mb ssoId ssoIdDel))) =
  Just $
    KeyMap.fromList
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
    KeyMap.fromList
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
    KeyMap.fromList
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
    KeyMap.fromList $
      "type" .= ("user.connection" :: Text)
        # "connection" .= uc
        # "user" .= case name of
          Just n -> Just $ object ["name" .= n]
          Nothing -> Nothing
        # []
toPushFormat (UserEvent (UserSuspended i)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.suspend" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserResumed i)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.resume" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserDeleted qid)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.delete" :: Text),
        "id" .= qUnqualified qid,
        "qualified_id" .= qid
      ]
toPushFormat (UserEvent (UserLegalHoldDisabled i)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.legalhold-disable" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserLegalHoldEnabled i)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.legalhold-enable" :: Text),
        "id" .= i
      ]
toPushFormat (PropertyEvent (PropertySet _ k v)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.properties-set" :: Text),
        "key" .= k,
        "value" .= propertyValue v
      ]
toPushFormat (PropertyEvent (PropertyDeleted _ k)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.properties-delete" :: Text),
        "key" .= k
      ]
toPushFormat (PropertyEvent (PropertiesCleared _)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.properties-clear" :: Text)
      ]
toPushFormat (ClientEvent (ClientAdded _ c)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.client-add" :: Text),
        "client" .= c
      ]
toPushFormat (ClientEvent (ClientRemoved _ clientId)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.client-remove" :: Text),
        "client" .= IdObject clientId
      ]
toPushFormat (UserEvent (LegalHoldClientRequested payload)) =
  let LegalHoldClientRequestedData targetUser lastPrekey' clientId = payload
   in Just $
        KeyMap.fromList
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

-------------------------------------------------------------------------------
-- Conversation Management

-- | Calls 'Galley.API.Create.createConnectConversation'.
createLocalConnectConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Local UserId ->
  Maybe Text ->
  Maybe ConnId ->
  m ConvId
createLocalConnectConv from to cname conn = do
  debug $
    logConnection (tUnqualified from) (tUntagged to)
      . remote "galley"
      . msg (val "Creating connect conversation")
  let req =
        path "/i/conversations/connect"
          . zUser (tUnqualified from)
          . maybe id (header "Z-Connection" . fromConnId) conn
          . contentJson
          . lbytes (encode $ Connect (tUntagged to) Nothing cname Nothing)
          . expect2xx
  r <- galleyRequest POST req
  maybe (error "invalid conv id") pure $
    fromByteString $
      getHeader' "Location" r

createConnectConv ::
  Qualified UserId ->
  Qualified UserId ->
  Maybe Text ->
  Maybe ConnId ->
  (AppT r) (Qualified ConvId)
createConnectConv from to cname conn = do
  lfrom <- ensureLocal from
  lto <- ensureLocal to
  tUntagged . qualifyAs lfrom
    <$> wrapHttp (createLocalConnectConv lfrom lto cname conn)

-- | Calls 'Galley.API.acceptConvH'.
acceptLocalConnectConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  m Conversation
acceptLocalConnectConv from conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Accepting connect conversation")
  galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "accept", "v2"]
        . zUser (tUnqualified from)
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

acceptConnectConv :: Local UserId -> Maybe ConnId -> Qualified ConvId -> AppT r Conversation
acceptConnectConv from conn =
  foldQualified
    from
    (wrapHttp . acceptLocalConnectConv from conn . tUnqualified)
    (const (throwM federationNotImplemented))

-- | Calls 'Galley.API.blockConvH'.
blockLocalConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  m ()
blockLocalConv lusr conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Blocking conversation")
  void $ galleyRequest PUT req
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "block"]
        . zUser (tUnqualified lusr)
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

blockConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  m ()
blockConv lusr conn =
  foldQualified
    lusr
    (blockLocalConv lusr conn . tUnqualified)
    (const (throwM federationNotImplemented))

-- | Calls 'Galley.API.unblockConvH'.
unblockLocalConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  m Conversation
unblockLocalConv lusr conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Unblocking conversation")
  galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "unblock"]
        . zUser (tUnqualified lusr)
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

unblockConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  m Conversation
unblockConv luid conn =
  foldQualified
    luid
    (unblockLocalConv luid conn . tUnqualified)
    (const (throwM federationNotImplemented))

upsertOne2OneConversation ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  UpsertOne2OneConversationRequest ->
  m UpsertOne2OneConversationResponse
upsertOne2OneConversation urequest = do
  response <- galleyRequest POST req
  case Bilge.statusCode response of
    200 -> decodeBody "galley" response
    _ -> throwM internalServerError
  where
    req =
      paths ["i", "conversations", "one2one", "upsert"]
        . header "Content-Type" "application/json"
        . lbytes (encode urequest)

-------------------------------------------------------------------------------
-- User management

-- | Calls 'Galley.API.rmUserH', as well as gundeck and cargohold.
rmUser ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  [Asset] ->
  m ()
rmUser usr asts = do
  debug $
    remote "gundeck"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  void $ gundeckRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  debug $
    remote "galley"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  void $ galleyRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  debug $
    remote "cargohold"
      . field "user" (toByteString usr)
      . msg (val "remove profile assets")
  -- Note that we _may_ not get a 2xx response code from cargohold (e.g., client has
  -- deleted the asset "directly" with cargohold; on our side, we just do our best to
  -- delete it in case it is still there
  forM_ asts $ \ast ->
    cargoholdRequest DELETE (paths ["assets/v3", toByteString' $ assetKey ast] . zUser usr)

-------------------------------------------------------------------------------
-- Client management

-- | Calls 'Galley.API.rmClientH', as well as gundeck.
rmClient ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  ClientId ->
  m ()
rmClient u c = do
  let cid = toByteString' c
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "remove client")
  let p = paths ["i", "clients", cid]
  void $ galleyRequest DELETE (p . zUser u . expect expected)
  -- for_ clabel rmClientCookie
  debug $
    remote "gundeck"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "unregister push client")
  g <- view gundeck
  void . recovering x3 rpcHandlers $
    const $
      rpc'
        "gundeck"
        g
        ( method DELETE
            . paths ["i", "clients", cid]
            . zUser u
            . expect expected
        )
  where
    expected = [status200, status204, status404]

lookupPushToken ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  UserId ->
  m [Push.PushToken]
lookupPushToken uid = do
  g <- view gundeck
  rsp <-
    rpc'
      "gundeck"
      (g :: Request)
      ( method GET
          . paths ["i", "push-tokens", toByteString' uid]
          . zUser uid
          . expect2xx
      )
  responseJsonMaybe rsp & maybe (pure []) (pure . pushTokens)

-------------------------------------------------------------------------------
-- Team Management

-- | Only works on 'BindingTeam's! The list of members returned is potentially truncated.
--
-- Calls 'Galley.API.getBindingTeamMembersH'.
getTeamContacts ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  m (Maybe Team.TeamMemberList)
getTeamContacts u = do
  debug $ remote "galley" . msg (val "Get team contacts")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team", "members"]
        . expect [status200, status404]

guardLegalhold ::
  LegalholdProtectee ->
  UserClients ->
  ExceptT ClientError (AppT r) ()
guardLegalhold protectee userClients = do
  res <- lift . wrapHttp $ galleyRequest PUT req
  case Bilge.statusCode res of
    200 -> pure ()
    403 -> throwE ClientMissingLegalholdConsent
    404 -> pure () -- allow for galley not to be ready, so the set of valid deployment orders is non-empty.
    _ -> throwM internalServerError
  where
    req =
      paths ["i", "guard-legalhold-policy-conflicts"]
        . header "Content-Type" "application/json"
        . lbytes (encode $ GuardLegalholdPolicyConflicts protectee userClients)
