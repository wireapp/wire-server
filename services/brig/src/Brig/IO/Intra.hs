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
    createSelfConv,
    createConnectConv,
    acceptConnectConv,
    blockConv,
    unblockConv,
    getConv,
    upsertOne2OneConversation,

    -- * Clients
    Brig.IO.Intra.newClient,
    rmClient,
    lookupPushToken,

    -- * Account Deletion
    rmUser,

    -- * Teams
    addTeamMember,
    checkUserCanJoinTeam,
    createTeam,
    getTeamMember,
    getTeamMembers,
    memberIsTeamOwner,
    getTeam,
    getTeamConv,
    getTeamName,
    getTeamId,
    getTeamContacts,
    getTeamLegalHoldStatus,
    changeTeamStatus,
    getTeamSearchVisibility,

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
import Brig.App
import Brig.Data.Connection (lookupContactList)
import qualified Brig.Data.Connection as Data
import Brig.Federation.Client (notifyUserDeleted)
import qualified Brig.IO.Journal as Journal
import Brig.RPC
import Brig.Types
import Brig.Types.User.Event
import qualified Brig.User.Search.Index as Search
import Conduit (runConduit, (.|))
import Control.Error (ExceptT)
import Control.Lens (view, (.~), (?~), (^.))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Retry
import Data.Aeson hiding (json)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import Data.Coerce (coerce)
import qualified Data.Conduit.List as C
import qualified Data.Currency as Currency
import Data.Domain
import Data.Either.Combinators (whenLeft)
import Data.Id
import Data.Json.Util (UTCTimeMillis, (#))
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import Data.Proxy
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import GHC.TypeLits
import Galley.Types (Connect (..), Conversation)
import Galley.Types.Conversations.Intra (UpsertOne2OneConversationRequest, UpsertOne2OneConversationResponse)
import qualified Galley.Types.Teams as Team
import Galley.Types.Teams.Intra (GuardLegalholdPolicyConflicts (GuardLegalholdPolicyConflicts))
import qualified Galley.Types.Teams.Intra as Team
import qualified Galley.Types.Teams.SearchVisibility as Team
import Gundeck.Types.Push.V2
import qualified Gundeck.Types.Push.V2 as Push
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import System.Logger.Class as Log hiding (name, (.=))
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Error
import Wire.API.Message (UserClients)
import Wire.API.Team.Feature (IncludeLockStatus (..), TeamFeatureName (..), TeamFeatureStatus)
import Wire.API.Team.LegalHold (LegalholdProtectee)
import qualified Wire.API.Team.Member as Member

-----------------------------------------------------------------------------
-- Event Handlers

onUserEvent :: UserId -> Maybe ConnId -> UserEvent -> (AppIO r) ()
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
  (AppIO r) ()
onConnectionEvent orig conn evt = do
  let from = ucFrom (ucConn evt)
  notify
    (singleton $ ConnectionEvent evt)
    orig
    Push.RouteAny
    conn
    (return $ list1 from [])

onPropertyEvent ::
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  ConnId ->
  PropertyEvent ->
  (AppIO r) ()
onPropertyEvent orig conn e =
  notify
    (singleton $ PropertyEvent e)
    orig
    Push.RouteDirect
    (Just conn)
    (return $ list1 orig [])

onClientEvent ::
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  (AppIO r) ()
onClientEvent orig conn e = do
  let events = singleton (ClientEvent e)
  let rcps = list1 orig []
  -- Synchronous push for better delivery guarantees of these
  -- events and to make sure new clients have a first notification
  -- in the stream.
  push events rcps orig Push.RouteAny conn

updateSearchIndex :: UserId -> UserEvent -> (AppIO r) ()
updateSearchIndex orig e = case e of
  -- no-ops
  UserCreated {} -> return ()
  UserIdentityUpdated UserIdentityUpdatedData {..} -> do
    when (isJust eiuEmail) $ wrapClient $ Search.reindex orig
  UserIdentityRemoved {} -> return ()
  UserLegalHoldDisabled {} -> return ()
  UserLegalHoldEnabled {} -> return ()
  LegalHoldClientRequested {} -> return ()
  UserSuspended {} -> wrapClient $ Search.reindex orig
  UserResumed {} -> wrapClient $ Search.reindex orig
  UserActivated {} -> wrapClient $ Search.reindex orig
  UserDeleted {} -> wrapClient $ Search.reindex orig
  UserUpdated UserUpdatedData {..} -> do
    let interesting =
          or
            [ isJust eupName,
              isJust eupAccentId,
              isJust eupHandle,
              isJust eupManagedBy,
              isJust eupSSOId || eupSSOIdRemoved
            ]
    when interesting $ wrapClient $ Search.reindex orig

journalEvent :: UserId -> UserEvent -> (AppIO r) ()
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
    return ()

-------------------------------------------------------------------------------
-- Low-Level Event Notification

-- | Notify the origin user's contact list (first-level contacts),
-- as well as his other clients about a change to his user account
-- or profile.
dispatchNotifications :: UserId -> Maybe ConnId -> UserEvent -> (AppIO r) ()
dispatchNotifications orig conn e = case e of
  UserCreated {} -> return ()
  UserSuspended {} -> return ()
  UserResumed {} -> return ()
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

notifyUserDeletionLocals :: UserId -> Maybe ConnId -> List1 Event -> (AppIO r) ()
notifyUserDeletionLocals deleted conn event = do
  recipients <- list1 deleted <$> lookupContactList deleted
  notify event deleted Push.RouteDirect conn (pure recipients)

notifyUserDeletionRemotes :: UserId -> (AppIO r) ()
notifyUserDeletionRemotes deleted = do
  runConduit $
    Data.lookupRemoteConnectedUsersC deleted (fromInteger (natVal (Proxy @UserDeletedNotificationMaxConnections)))
      .| C.mapM_ fanoutNotifications
  where
    fanoutNotifications :: [Remote UserId] -> (AppIO r) ()
    fanoutNotifications = mapM_ notifyBackend . bucketRemote

    notifyBackend :: Remote [UserId] -> (AppIO r) ()
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

    logFederationError :: Domain -> FederationError -> AppT r IO ()
    logFederationError domain fErr =
      Log.err $
        Log.msg ("Federation error while notifying remote backends of a user deletion." :: ByteString)
          . Log.field "user_id" (show deleted)
          . Log.field "domain" (domainText domain)
          . Log.field "error" (show fErr)

-- | Push events to other users.
push ::
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
  (AppIO r) ()
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
  (AppIO r) ()
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
  List1 Event ->
  -- | Origin user, TODO: Delete
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  -- | Users to notify.
  IO (List1 UserId) ->
  (AppIO r) ()
notify events orig route conn recipients = forkAppIO (Just orig) $ do
  rs <- liftIO recipients
  push events rs orig route conn

notifySelf ::
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  (AppIO r) ()
notifySelf events orig route conn =
  notify events orig route conn (pure (singleton orig))

notifyContacts ::
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  (AppIO r) ()
notifyContacts events orig route conn = do
  env <- ask
  notify events orig route conn $
    runAppT env $
      list1 orig <$> liftA2 (++) contacts teamContacts
  where
    contacts :: (AppIO r) [UserId]
    contacts = lookupContactList orig
    teamContacts :: (AppIO r) [UserId]
    teamContacts = screenMemberList =<< getTeamContacts orig
    -- If we have a truncated team, we just ignore it all together to avoid very large fanouts
    screenMemberList :: Maybe Team.TeamMemberList -> (AppIO r) [UserId]
    screenMemberList (Just mems)
      | mems ^. Team.teamMemberListType == Team.ListComplete =
        return $ fmap (view Team.userId) (mems ^. Team.teamMembers)
    screenMemberList _ = return []

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
        "value" .= v
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
toPushFormat (ClientEvent (ClientRemoved _ c)) =
  Just $
    KeyMap.fromList
      [ "type" .= ("user.client-remove" :: Text),
        "client" .= IdObject (clientId c)
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

-- | Calls 'Galley.API.createSelfConversationH'.
createSelfConv :: UserId -> (AppIO r) ()
createSelfConv u = do
  debug $
    remote "galley"
      . msg (val "Creating self conversation")
  void $ galleyRequest POST req
  where
    req =
      path "/conversations/self"
        . zUser u
        . expect2xx

-- | Calls 'Galley.API.Create.createConnectConversation'.
createLocalConnectConv ::
  Local UserId ->
  Local UserId ->
  Maybe Text ->
  Maybe ConnId ->
  (AppIO r) ConvId
createLocalConnectConv from to cname conn = do
  debug $
    logConnection (tUnqualified from) (qUntagged to)
      . remote "galley"
      . msg (val "Creating connect conversation")
  let req =
        path "/i/conversations/connect"
          . zUser (tUnqualified from)
          . maybe id (header "Z-Connection" . fromConnId) conn
          . contentJson
          . lbytes (encode $ Connect (qUntagged to) Nothing cname Nothing)
          . expect2xx
  r <- galleyRequest POST req
  maybe (error "invalid conv id") return $
    fromByteString $
      getHeader' "Location" r

createConnectConv ::
  Qualified UserId ->
  Qualified UserId ->
  Maybe Text ->
  Maybe ConnId ->
  (AppIO r) (Qualified ConvId)
createConnectConv from to cname conn = do
  lfrom <- ensureLocal from
  lto <- ensureLocal to
  qUntagged . qualifyAs lfrom
    <$> createLocalConnectConv lfrom lto cname conn
  where
    ensureLocal :: Qualified a -> (AppIO r) (Local a)
    ensureLocal x = do
      loc <- qualifyLocal ()
      foldQualified loc pure (\_ -> throwM federationNotImplemented) x

-- | Calls 'Galley.API.acceptConvH'.
acceptLocalConnectConv :: Local UserId -> Maybe ConnId -> ConvId -> (AppIO r) Conversation
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

acceptConnectConv :: Local UserId -> Maybe ConnId -> Qualified ConvId -> (AppIO r) Conversation
acceptConnectConv from conn =
  foldQualified
    from
    (acceptLocalConnectConv from conn . tUnqualified)
    (const (throwM federationNotImplemented))

-- | Calls 'Galley.API.blockConvH'.
blockLocalConv :: Local UserId -> Maybe ConnId -> ConvId -> (AppIO r) ()
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

blockConv :: Local UserId -> Maybe ConnId -> Qualified ConvId -> (AppIO r) ()
blockConv lusr conn =
  foldQualified
    lusr
    (blockLocalConv lusr conn . tUnqualified)
    (const (throwM federationNotImplemented))

-- | Calls 'Galley.API.unblockConvH'.
unblockLocalConv :: Local UserId -> Maybe ConnId -> ConvId -> (AppIO r) Conversation
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

unblockConv :: Local UserId -> Maybe ConnId -> Qualified ConvId -> (AppIO r) Conversation
unblockConv luid conn =
  foldQualified
    luid
    (unblockLocalConv luid conn . tUnqualified)
    (const (throwM federationNotImplemented))

-- | Calls 'Galley.API.getConversationH'.
getConv :: UserId -> ConvId -> (AppIO r) (Maybe Conversation)
getConv usr cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

upsertOne2OneConversation :: UpsertOne2OneConversationRequest -> (AppIO r) UpsertOne2OneConversationResponse
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

-- | Calls 'Galley.API.getTeamConversationH'.
getTeamConv :: UserId -> TeamId -> ConvId -> (AppIO r) (Maybe Team.TeamConversation)
getTeamConv usr tid cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting team conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["teams", toByteString' tid, "conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-------------------------------------------------------------------------------
-- User management

-- | Calls 'Galley.API.rmUserH', as well as gundeck and cargohold.
rmUser :: UserId -> [Asset] -> (AppIO r) ()
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

-- | Calls 'Galley.API.addClientH'.
newClient :: UserId -> ClientId -> (AppIO r) ()
newClient u c = do
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (toByteString c)
      . msg (val "new client")
  let p = paths ["i", "clients", toByteString' c]
  void $ galleyRequest POST (p . zUser u . expect2xx)

-- | Calls 'Galley.API.rmClientH', as well as gundeck.
rmClient :: UserId -> ClientId -> (AppIO r) ()
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

lookupPushToken :: UserId -> (AppIO r) [Push.PushToken]
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

-- | Calls 'Galley.API.canUserJoinTeamH'.
checkUserCanJoinTeam :: TeamId -> (AppIO r) (Maybe Wai.Error)
checkUserCanJoinTeam tid = do
  debug $
    remote "galley"
      . msg (val "Check if can add member to team")
  rs <- galleyRequest GET req
  return $ case Bilge.statusCode rs of
    200 -> Nothing
    _ -> case decodeBody "galley" rs of
      Just (e :: Wai.Error) -> return e
      Nothing -> error ("Invalid response from galley: " <> show rs)
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", "check"]
        . header "Content-Type" "application/json"

-- | Calls 'Galley.API.uncheckedAddTeamMemberH'.
addTeamMember :: UserId -> TeamId -> (Maybe (UserId, UTCTimeMillis), Team.Role) -> (AppIO r) Bool
addTeamMember u tid (minvmeta, role) = do
  debug $
    remote "galley"
      . msg (val "Adding member to team")
  rs <- galleyRequest POST req
  return $ case Bilge.statusCode rs of
    200 -> True
    _ -> False
  where
    prm = Team.rolePermissions role
    bdy = Member.mkNewTeamMember u prm minvmeta
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . header "Content-Type" "application/json"
        . zUser u
        . expect [status200, status403]
        . lbytes (encode bdy)

-- | Calls 'Galley.API.createBindingTeamH'.
createTeam :: UserId -> Team.BindingNewTeam -> TeamId -> (AppIO r) CreateUserTeam
createTeam u t@(Team.BindingNewTeam bt) teamid = do
  debug $
    remote "galley"
      . msg (val "Creating Team")
  r <- galleyRequest PUT $ req teamid
  tid <-
    maybe (error "invalid team id") return $
      fromByteString $
        getHeader' "Location" r
  return (CreateUserTeam tid $ fromRange (bt ^. Team.newTeamName))
  where
    req tid =
      paths ["i", "teams", toByteString' tid]
        . header "Content-Type" "application/json"
        . zUser u
        . expect2xx
        . lbytes (encode t)

-- | Calls 'Galley.API.uncheckedGetTeamMemberH'.
getTeamMember :: UserId -> TeamId -> (AppIO r) (Maybe Team.TeamMember)
getTeamMember u tid = do
  debug $
    remote "galley"
      . msg (val "Get team member")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", toByteString' u]
        . zUser u
        . expect [status200, status404]

-- | Calls 'Galley.API.uncheckedGetTeamMembersH'.
--
-- | TODO: is now truncated.  this is (only) used for team suspension / unsuspension, which
-- means that only the first 2000 members of a team (according to some arbitrary order) will
-- be suspended, and the rest will remain active.
getTeamMembers :: TeamId -> (AppIO r) Team.TeamMemberList
getTeamMembers tid = do
  debug $ remote "galley" . msg (val "Get team members")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . expect2xx

memberIsTeamOwner :: TeamId -> UserId -> (AppIO r) Bool
memberIsTeamOwner tid uid = do
  r <-
    galleyRequest GET $
      paths ["i", "teams", toByteString' tid, "is-team-owner", toByteString' uid]
  pure $ responseStatus r /= status403

-- | Only works on 'BindingTeam's! The list of members returned is potentially truncated.
--
-- Calls 'Galley.API.getBindingTeamMembersH'.
getTeamContacts :: UserId -> (AppIO r) (Maybe Team.TeamMemberList)
getTeamContacts u = do
  debug $ remote "galley" . msg (val "Get team contacts")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team", "members"]
        . expect [status200, status404]

-- | Calls 'Galley.API.getBindingTeamIdH'.
getTeamId :: UserId -> (AppIO r) (Maybe TeamId)
getTeamId u = do
  debug $ remote "galley" . msg (val "Get team from user")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team"]
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamInternalH'.
getTeam :: TeamId -> (AppIO r) Team.TeamData
getTeam tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid]
        . expect2xx

-- | Calls 'Galley.API.getTeamInternalH'.
getTeamName :: TeamId -> (AppIO r) Team.TeamName
getTeamName tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "name"]
        . expect2xx

-- | Calls 'Galley.API.getTeamFeatureStatusH'.
getTeamLegalHoldStatus :: TeamId -> (AppIO r) (TeamFeatureStatus 'WithoutLockStatus 'TeamFeatureLegalHold)
getTeamLegalHoldStatus tid = do
  debug $ remote "galley" . msg (val "Get legalhold settings")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", toByteString' TeamFeatureLegalHold]
        . expect2xx

-- | Calls 'Galley.API.getSearchVisibilityInternalH'.
getTeamSearchVisibility :: TeamId -> (AppIO r) Team.TeamSearchVisibility
getTeamSearchVisibility tid =
  coerce @Team.TeamSearchVisibilityView @Team.TeamSearchVisibility <$> do
    debug $ remote "galley" . msg (val "Get search visibility settings")
    galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "search-visibility"]
        . expect2xx

-- | Calls 'Galley.API.updateTeamStatusH'.
changeTeamStatus :: TeamId -> Team.TeamStatus -> Maybe Currency.Alpha -> (AppIO r) ()
changeTeamStatus tid s cur = do
  debug $ remote "galley" . msg (val "Change Team status")
  void $ galleyRequest PUT req
  where
    req =
      paths ["i", "teams", toByteString' tid, "status"]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Team.TeamStatusUpdate s cur)

guardLegalhold :: LegalholdProtectee -> UserClients -> ExceptT ClientError (AppIO r) ()
guardLegalhold protectee userClients = do
  res <- lift $ galleyRequest PUT req
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
