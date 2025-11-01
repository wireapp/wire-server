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
  ( -- * Events
    onConnectionEvent,
    onPropertyEvent,
    onClientEvent,

    -- * Conversations
    createConnectConv,
    acceptConnectConv,
    blockConv,
    upsertOne2OneConversation,

    -- * Clients
    rmClient,

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
import Brig.API.Error (internalServerError)
import Brig.API.Types
import Brig.App
import Brig.IO.Logging
import Brig.RPC
import Control.Error (ExceptT)
import Control.Lens ((^?))
import Control.Monad.Catch
import Control.Monad.Trans.Except (throwE)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Default
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty (..))
import Data.Qualified
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Polysemy
import Polysemy.TinyLog (TinyLog)
import System.Logger.Message hiding ((.=))
import Wire.API.Connection
import Wire.API.Conversation hiding (Member)
import Wire.API.Event.Conversation (Connect (Connect))
import Wire.API.Federation.Error
import Wire.API.Push.V2 qualified as V2
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.Internal.Galley.TeamsIntra (GuardLegalholdPolicyConflicts (GuardLegalholdPolicyConflicts))
import Wire.API.Team.LegalHold (LegalholdProtectee)
import Wire.API.User
import Wire.API.User.Client
import Wire.API.UserEvent
import Wire.Events.Interpreter (notify, toApsData)
import Wire.NotificationSubsystem
import Wire.Rpc
import Wire.Sem.Logger qualified as Log

-----------------------------------------------------------------------------
-- Event Handlers

onConnectionEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID, if any.
  Maybe ConnId ->
  -- | The event.
  ConnectionEvent ->
  Sem r ()
onConnectionEvent orig conn evt = do
  let from = ucFrom (ucConn evt)
  notify
    (ConnectionEvent evt)
    orig
    V2.RouteAny
    conn
    (pure $ from :| [])

onPropertyEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  ConnId ->
  PropertyEvent ->
  Sem r ()
onPropertyEvent orig conn e =
  notify
    (PropertyEvent e)
    orig
    V2.RouteDirect
    (Just conn)
    (pure $ orig :| [])

onClientEvent ::
  (Member NotificationSubsystem r) =>
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  Sem r ()
onClientEvent orig conn e = do
  let event = ClientEvent e
  let rcpt = Recipient orig V2.RecipientClientsAll
  pushNotifications
    [ def
        { origin = Just orig,
          json = toJSONObject event,
          recipients = [rcpt],
          conn,
          apsData = toApsData event
        }
    ]

-------------------------------------------------------------------------------
-- Conversation Management

-- | Calls 'Galley.API.Create.createConnectConversation'.
createLocalConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Local UserId ->
  Maybe Text ->
  Maybe ConnId ->
  Sem r ConvId
createLocalConnectConv from to cname conn = do
  Log.debug $
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
  r <- embed $ galleyRequest POST req
  maybe (error "invalid conv id") pure $
    fromByteString $
      getHeader' "Location" r

createConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Qualified UserId ->
  Maybe Text ->
  Maybe ConnId ->
  (AppT r) (Qualified ConvId)
createConnectConv from to cname conn = do
  lfrom <- ensureLocal from
  lto <- ensureLocal to
  tUntagged . qualifyAs lfrom
    <$> liftSem (createLocalConnectConv lfrom lto cname conn)

-- | Calls 'Galley.API.acceptConvH'.
acceptLocalConnectConv ::
  (Member (Embed HttpClientIO) r, Member TinyLog r) =>
  Local UserId ->
  Maybe ConnId ->
  ConvId ->
  Sem r OwnConversation
acceptLocalConnectConv from conn cnv = do
  Log.debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Accepting connect conversation")
  embed $ galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "accept", "v2"]
        . zUser (tUnqualified from)
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

acceptConnectConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  AppT r OwnConversation
acceptConnectConv from conn =
  foldQualified
    from
    (liftSem . acceptLocalConnectConv from conn . tUnqualified)
    (const (throwM federationNotImplemented))

blockConv ::
  ( Member (Embed HttpClientIO) r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r ()
blockConv lusr qcnv = do
  Log.debug $
    remote "galley"
      . field "conv" (toByteString . qUnqualified $ qcnv)
      . field "domain" (toByteString . qDomain $ qcnv)
      . msg (val "Blocking conversation")
  embed . void $ galleyRequest PUT req
  where
    req =
      paths
        [ "i",
          "conversations",
          toByteString' (qDomain qcnv),
          toByteString' (qUnqualified qcnv),
          "block"
        ]
        . zUser (tUnqualified lusr)
        . expect2xx

upsertOne2OneConversation ::
  ( MonadReader Env m,
    MonadUnliftIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  UpsertOne2OneConversationRequest ->
  m ()
upsertOne2OneConversation urequest = do
  response <- galleyRequest POST req
  case Bilge.statusCode response of
    200 -> pure ()
    _ -> throwM internalServerError
  where
    req =
      paths ["i", "conversations", "one2one", "upsert"]
        . header "Content-Type" "application/json"
        . lbytes (encode urequest)

-------------------------------------------------------------------------------
-- User management

-- | Calls Galley's endpoint with the internal route ID "delete-user", as well
-- as gundeck and cargohold.
rmUser ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  [Asset] ->
  Sem r ()
rmUser usr asts = do
  Log.debug $
    remote "gundeck"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  cleanupUser usr
  Log.debug $
    remote "galley"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  embed $ void $ galleyRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  Log.debug $
    remote "cargohold"
      . field "user" (toByteString usr)
      . msg (val "remove profile assets")
  -- Note that we _may_ not get a 2xx response code from cargohold (e.g., client has
  -- deleted the asset "directly" with cargohold; on our side, we just do our best to
  -- delete it in case it is still there
  embed $ forM_ asts $ \ast ->
    cargoholdRequest DELETE (paths ["assets/v3", toByteString' $ assetKey ast] . zUser usr)

-------------------------------------------------------------------------------
-- Client management

-- | Calls 'Galley.API.rmClientH', as well as gundeck.
rmClient ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
rmClient u c = do
  let cid = toByteString' c
  Log.debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "remove client")
  let p = paths ["i", "clients", cid]
  embed $ void $ galleyRequest DELETE (p . zUser u . expect expected)
  -- for_ clabel rmClientCookie
  Log.debug $
    remote "gundeck"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "unregister push client")
  unregisterPushClient u c
  where
    expected = [status200, status204, status404]

-------------------------------------------------------------------------------
-- Team Management

guardLegalhold ::
  LegalholdProtectee ->
  UserClients ->
  ExceptT ClientError (AppT r) ()
guardLegalhold protectee userClients = do
  res <- lift . wrapHttp $ galleyRequest PUT req
  case Bilge.statusCode res of
    200 -> pure ()
    403 -> case Bilge.responseJsonMaybe @Value res >>= (^? key "label") of
      Just "missing-legalhold-consent" -> throwE ClientMissingLegalholdConsent
      Just "missing-legalhold-consent-old-clients" -> throwE ClientMissingLegalholdConsentOldClients
      _ ->
        -- only happens if galley misbehaves (fisx: this could also be a parse error if we
        -- used a more constraining type to send back & forth between brig and galley, but
        -- merging brig and galley would make this train of thought go away more naturally).
        throwE ClientMissingLegalholdConsent
    404 -> pure () -- allow for galley not to be ready, so the set of valid deployment orders is non-empty.
    _ -> throwM internalServerError
  where
    req =
      paths ["i", "guard-legalhold-policy-conflicts"]
        . header "Content-Type" "application/json"
        . lbytes (encode $ GuardLegalholdPolicyConflicts protectee userClients)
