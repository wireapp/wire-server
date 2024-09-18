{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

module Stern.Intra
  ( backendApiVersion,
    putUserStatus,
    getContacts,
    getUserConnections,
    getUsersConnections,
    getUserProfiles,
    getUserProfilesByIdentity,
    getEjpdInfo,
    getUserProperties,
    getInvoiceUrl,
    revokeIdentity,
    changeEmail,
    deleteAccount,
    setStatusBindingTeam,
    deleteBindingTeam,
    deleteBindingTeamForce,
    getTeamInfo,
    getUserBindingTeam,
    isBlacklisted,
    setBlacklistStatus,
    getTeamFeatureFlag,
    setTeamFeatureFlag,
    patchTeamFeatureFlag,
    setTeamFeatureLockStatus,
    getTeamData,
    getSearchVisibility,
    setSearchVisibility,
    getTeamBillingInfo,
    setTeamBillingInfo,
    getEmailConsentLog,
    getUserConsentLog,
    getMarketoResult,
    getUserConsentValue,
    getUserConversations,
    getUserClients,
    getUserCookies,
    getUserNotifications,
    getSsoDomainRedirect,
    putSsoDomainRedirect,
    deleteSsoDomainRedirect,
    registerOAuthClient,
    getOAuthClient,
    updateOAuthClient,
    deleteOAuthClient,
  )
where

import Bilge hiding (head, options, patch, path, paths, requestId)
import Bilge qualified
import Bilge.RPC
import Brig.Types.Intra
import Control.Error
import Control.Lens (view, (^.))
import Data.Aeson hiding (Error)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (emptyArray)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion
import Data.ByteString.UTF8 qualified as UTF8
import Data.Handle (Handle)
import Data.Id
import Data.Int
import Data.List.Split (chunksOf)
import Data.Map qualified as Map
import Data.Qualified (qUnqualified)
import Data.Text (strip)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy as LT (pack)
import Data.Text.Lazy.Encoding qualified as TL
import Imports
import Network.HTTP.Types (urlEncode)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode, statusMessage)
import Network.Wai.Utilities (Error (..), mkError)
import Servant.API (toUrlPiece)
import Stern.App
import Stern.Types
import System.Logger.Class hiding (Error, name, (.=))
import System.Logger.Class qualified as Log
import UnliftIO.Exception hiding (Handler)
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.CustomBackend
import Wire.API.Internal.Notification
import Wire.API.OAuth (OAuthClient, OAuthClientConfig, OAuthClientCredentials)
import Wire.API.Properties
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Brig.EJPD qualified as EJPD
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Member
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Search

-------------------------------------------------------------------------------

backendApiVersion :: Version
backendApiVersion = V2

versionedPath :: ByteString -> Request -> Request
versionedPath = Bilge.path . ((encodeUtf8 (toUrlPiece backendApiVersion) <> "/") <>)

versionedPaths :: [ByteString] -> Request -> Request
versionedPaths = Bilge.paths . (encodeUtf8 (toUrlPiece backendApiVersion) :)

-------------------------------------------------------------------------------

putUserStatus :: AccountStatus -> UserId -> Handler ()
putUserStatus status uid = do
  info $ userMsg uid . msg "Changing user status"
  b <- view brig
  void $
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method PUT
            . Bilge.paths ["i", "users", toByteString' uid, "status"]
            . lbytes (encode payload)
            . contentJson
            . expect2xx
        )
  where
    payload = AccountStatusUpdate status

-- This won't work anymore once API version V1 is not supported anymore
getUserConnections :: UserId -> Handler [UserConnection]
getUserConnections uid = do
  info $ msg "Getting user connections"
  fetchAll [] Nothing
  where
    fetchAll :: [UserConnection] -> Maybe UserId -> Handler [UserConnection]
    fetchAll xs start = do
      userConnectionList <- fetchBatch start
      let batch = clConnections userConnectionList
      if (not . null) batch && clHasMore userConnectionList
        then fetchAll (batch ++ xs) (Just . qUnqualified . ucTo $ last batch)
        else pure (batch ++ xs)
    fetchBatch :: Maybe UserId -> Handler UserConnectionList
    fetchBatch start = do
      b <- view brig
      r <-
        catchRpcErrors $
          rpc'
            "brig"
            b
            ( method GET
                . header "Z-User" (toByteString' uid)
                . Bilge.paths ["v1", "connections"]
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "start" . toByteString') start
                . expect2xx
            )
      parseResponse (mkError status502 "bad-upstream") r
    batchSize = 100 :: Int

getUsersConnections :: List UserId -> Handler [ConnectionStatus]
getUsersConnections uids = do
  info $ msg "Getting user connections"
  b <- view brig
  let reqBody = ConnectionsStatusRequest (fromList uids) Nothing
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method POST
            . Bilge.path "i/users/connections-status"
            . Bilge.json reqBody
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  parseResponse (mkError status502 "bad-upstream") r

getUserProfiles :: Either [UserId] [Handle] -> Handler [UserAccount]
getUserProfiles uidsOrHandles = do
  info $ msg "Getting user accounts"
  b <- view brig
  concat <$> mapM (doRequest b) (prepareQS uidsOrHandles)
  where
    doRequest :: Request -> (Request -> Request) -> Handler [UserAccount]
    doRequest b qry = do
      r <-
        catchRpcErrors $
          rpc'
            "brig"
            b
            ( method GET
                . Bilge.path "i/users"
                . qry
                . expect2xx
            )
      parseResponse (mkError status502 "bad-upstream") r
    prepareQS :: Either [UserId] [Handle] -> [Request -> Request]
    prepareQS (Left uids) = fmap (queryItem "ids") (toQS uids)
    prepareQS (Right handles) = fmap (queryItem "handles") (toQS handles)
    toQS :: (ToByteString a) => [a] -> [ByteString]
    toQS =
      fmap (BS.intercalate "," . map toByteString')
        . chunksOf 50

getUserProfilesByIdentity :: EmailAddress -> Handler [UserAccount]
getUserProfilesByIdentity email = do
  info $ msg "Getting user accounts by identity"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . Bilge.path "i/users"
            . userKeyToParam email
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getEjpdInfo :: [Handle] -> Bool -> Handler EJPD.EJPDResponseBody
getEjpdInfo handles includeContacts = do
  info $ msg "Getting ejpd info on users by handle"
  b <- view brig
  let bdy :: Value
      bdy =
        object
          [ "EJPDRequest"
              .= (decodeUtf8With lenientDecode . toByteString' <$> handles)
          ]
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method POST
            . Bilge.path "i/ejpd-request"
            . Bilge.json bdy
            . (if includeContacts then queryItem "include_contacts" "true" else id)
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getContacts :: UserId -> Text -> Int32 -> Handler (SearchResult Contact)
getContacts u q s = do
  info $ msg "Getting user contacts"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . versionedPath "search/contacts"
            . header "Z-User" (toByteString' u)
            . queryItem "q" (toByteString' q)
            . queryItem "size" (toByteString' s)
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

revokeIdentity :: EmailAddress -> Handler ()
revokeIdentity email = do
  info $ msg "Revoking user identity"
  b <- view brig
  void
    . catchRpcErrors
    $ rpc'
      "brig"
      b
      ( method POST
          . Bilge.path "i/users/revoke-identity"
          . userKeyToParam email
          . expect2xx
      )

deleteAccount :: UserId -> Handler ()
deleteAccount uid = do
  info $ msg "Deleting account"
  b <- view brig
  void
    . catchRpcErrors
    $ rpc'
      "brig"
      b
      ( method DELETE
          . Bilge.paths ["i", "users", toByteString' uid]
          . expect2xx
      )

setStatusBindingTeam :: TeamId -> Team.TeamStatus -> Handler ()
setStatusBindingTeam tid status = do
  info $
    msg
      ( "Setting team status to "
          <> UTF8.toString (BS.toStrict . encode $ status)
      )
  g <- view galley
  void
    . catchRpcErrors
    $ rpc'
      "galley"
      g
      ( method PUT
          . Bilge.paths ["i", "teams", toByteString' tid, "status"]
          . Bilge.json (Team.TeamStatusUpdate status Nothing)
          . expect2xx
      )

deleteBindingTeam :: TeamId -> Handler ()
deleteBindingTeam tid = do
  info $ msg "Deleting team"
  g <- view galley
  void
    . catchRpcErrors
    $ rpc'
      "galley"
      g
      ( method DELETE
          . Bilge.paths ["i", "teams", toByteString' tid]
          . expect2xx
      )

-- | Caution! This may permanently delete all team members!
deleteBindingTeamForce :: TeamId -> Handler ()
deleteBindingTeamForce tid = do
  info $ msg "Deleting team with force flag"
  g <- view galley
  void
    . catchRpcErrors
    $ rpc'
      "galley"
      g
      ( method DELETE
          . Bilge.paths ["i", "teams", toByteString' tid]
          . queryItem "force" "true"
          . expect2xx
      )

changeEmail :: UserId -> EmailUpdate -> Handler ()
changeEmail u upd = do
  info $ msg "Updating email address"
  b <- view brig
  void
    . catchRpcErrors
    $ rpc'
      "brig"
      b
      ( method PUT
          . Bilge.path "i/self/email"
          . header "Z-User" (toByteString' u)
          . header "Z-Connection" (toByteString' "")
          . queryItem "validate" "true"
          . lbytes (encode upd)
          . contentJson
          . expect2xx
      )

getTeamInfo :: TeamId -> Handler TeamInfo
getTeamInfo tid = do
  d <- getTeamData tid
  m <- getTeamMembers tid
  pure $ TeamInfo d (map TeamMemberInfo (m ^. teamMembers))

getUserBindingTeam :: UserId -> Handler (Maybe TeamId)
getUserBindingTeam u = do
  info $ msg "Getting user binding team"
  g <- view galley
  r <-
    catchRpcErrors $
      rpc'
        "galley"
        g
        ( method GET
            . versionedPath "teams"
            . header "Z-User" (toByteString' u)
            . header "Z-Connection" (toByteString' "")
            . expect2xx
        )
  teams <- parseResponse (mkError status502 "bad-upstream") r
  pure $
    listToMaybe $
      fmap (view teamId) $
        filter ((== Binding) . view teamBinding) $
          teams
            ^. teamListTeams

getInvoiceUrl :: TeamId -> InvoiceId -> Handler ByteString
getInvoiceUrl tid iid = do
  info $ msg "Getting invoice"
  i <- view ibis
  r <-
    catchRpcErrors $
      rpc'
        "ibis"
        i
        ( method GET
            . Bilge.paths ["i", "team", toByteString' tid, "invoice", toByteString' iid]
            . noRedirect
            . expectStatus (== 307)
        )
  pure $ getHeader' "Location" r

getTeamBillingInfo :: TeamId -> Handler (Maybe TeamBillingInfo)
getTeamBillingInfo tid = do
  info $ msg "Getting team billing info"
  i <- view ibis
  resp <-
    catchRpcErrors $
      rpc'
        "ibis"
        i
        ( method GET
            . Bilge.paths ["i", "team", toByteString' tid, "billing"]
        )
  case Bilge.statusCode resp of
    200 -> Just <$> parseResponse (mkError status502 "bad-upstream") resp
    404 -> pure Nothing
    _ -> throwE (mkError status502 "bad-upstream" (errorMessage resp))

setTeamBillingInfo :: TeamId -> TeamBillingInfo -> Handler ()
setTeamBillingInfo tid tbu = do
  info $ msg "Setting team billing info"
  i <- view ibis
  void
    . catchRpcErrors
    $ rpc'
      "ibis"
      i
      ( method PUT
          . Bilge.paths ["i", "team", toByteString' tid, "billing"]
          . lbytes (encode tbu)
          . contentJson
          . expect2xx
      )

isBlacklisted :: EmailAddress -> Handler Bool
isBlacklisted email = do
  info $ msg "Checking blacklist"
  b <- view brig
  resp <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . Bilge.path "i/users/blacklist"
            . userKeyToParam email
        )
  case Bilge.statusCode resp of
    200 -> pure True
    404 -> pure False
    _ -> throwE (mkError status502 "bad-upstream" (errorMessage resp))

setBlacklistStatus :: Bool -> EmailAddress -> Handler ()
setBlacklistStatus status email = do
  info $ msg "Changing blacklist status"
  b <- view brig
  void
    . catchRpcErrors
    $ rpc'
      "brig"
      b
      ( method (statusToMethod status)
          . Bilge.path "i/users/blacklist"
          . userKeyToParam email
          . expect2xx
      )
  where
    statusToMethod False = DELETE
    statusToMethod True = POST

getTeamFeatureFlag ::
  forall cfg.
  (IsFeatureConfig cfg, Typeable cfg) =>
  TeamId ->
  Handler (Public.LockableFeature cfg)
getTeamFeatureFlag tid = do
  info $ msg "Getting team feature status"
  gly <- view galley
  let req =
        method GET
          . Bilge.paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
  resp <- catchRpcErrors $ rpc' "galley" gly req
  case Bilge.statusCode resp of
    200 -> pure $ responseJsonUnsafe @(Public.LockableFeature cfg) resp
    404 -> throwE (mkError status404 "bad-upstream" "team doesnt exist")
    _ -> throwE (mkError status502 "bad-upstream" (errorMessage resp))

setTeamFeatureFlag ::
  forall cfg.
  (IsFeatureConfig cfg) =>
  TeamId ->
  Public.Feature cfg ->
  Handler ()
setTeamFeatureFlag tid status = do
  info $ msg "Setting team feature status"
  galleyRpc $
    method PUT
      . Bilge.paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . Bilge.json status
      . contentJson

patchTeamFeatureFlag ::
  forall cfg.
  (IsFeatureConfig cfg) =>
  TeamId ->
  Public.LockableFeaturePatch cfg ->
  Handler ()
patchTeamFeatureFlag tid patch = do
  info $ msg "Patching team feature status"
  galleyRpc $
    method PATCH
      . Bilge.paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . Bilge.json patch
      . contentJson

galleyRpc :: (Bilge.Request -> Bilge.Request) -> Handler ()
galleyRpc req = do
  gly <- view galley
  resp <- catchRpcErrors $ rpc' "galley" gly req
  case statusCode resp of
    200 -> pure ()
    404 -> throwE (mkError status404 "bad-upstream" "team does not exist")
    403 -> throwE (mkError status403 "bad-upstream" "config cannot be changed")
    _ -> throwE (mkError status502 "bad-upstream" (errorMessage resp))

setTeamFeatureLockStatus ::
  forall cfg.
  (IsFeatureConfig cfg) =>
  TeamId ->
  LockStatus ->
  Handler ()
setTeamFeatureLockStatus tid lstat = do
  info $ msg ("Setting lock status: " <> featureName @cfg)
  gly <- view galley
  fromResponseBody
    <=< catchRpcErrors
    $ rpc'
      "galley"
      gly
      ( method PUT
          . Bilge.paths
            [ "i",
              "teams",
              toByteString' tid,
              "features",
              Public.featureNameBS @cfg,
              toByteString' lstat
            ]
      )
  where
    fromResponseBody :: Response (Maybe LByteString) -> Handler ()
    fromResponseBody resp = parseResponse (mkError status502 "bad-upstream") resp

getSearchVisibility :: TeamId -> Handler TeamSearchVisibilityView
getSearchVisibility tid = do
  info $ msg "Getting TeamSearchVisibilityView value"
  gly <- view galley
  fromResponseBody
    <=< catchRpcErrors
    $ rpc'
      "galley"
      gly
      ( method GET
          . Bilge.paths ["i", "teams", toByteString' tid, "search-visibility"]
          . expect2xx
      )
  where
    fromResponseBody :: Response (Maybe LByteString) -> Handler TeamSearchVisibilityView
    fromResponseBody resp = parseResponse (mkError status502 "bad-upstream") resp

setSearchVisibility :: TeamId -> TeamSearchVisibility -> Handler ()
setSearchVisibility tid typ = do
  info $ msg "Setting TeamSearchVisibility value"
  gly <- view galley
  resp <-
    catchRpcErrors $
      rpc'
        "galley"
        gly
        ( method PUT
            . Bilge.paths ["i", "teams", toByteString' tid, "search-visibility"]
            . lbytes (encode $ TeamSearchVisibilityView typ)
            . contentJson
        )
  case statusCode resp of
    200 -> pure ()
    204 -> pure ()
    403 ->
      throwE $
        mkError
          status403
          "team-search-visibility-unset"
          "This team does not have TeamSearchVisibility enabled. Ensure this is the correct TeamID or first enable the feature"
    _ -> throwE $ responseJsonUnsafe resp

--------------------------------------------------------------------------------
-- Helper functions
stripBS :: ByteString -> ByteString
stripBS = encodeUtf8 . strip . decodeUtf8

userKeyToParam :: EmailAddress -> Request -> Request
userKeyToParam e = queryItem "email" (stripBS $ toByteString' e)

errorMessage :: Response (Maybe LByteString) -> LText
errorMessage = maybe "" TL.decodeUtf8 . responseBody

-- | Run an App and catch any RPCException's which may occur, lifting them to ExceptT
-- This isn't an ideal set-up; but is required in certain cases because 'ExceptT' isn't
-- an instance of 'MonadUnliftIO'
catchRpcErrors :: forall a. App a -> ExceptT Error App a
catchRpcErrors action = ExceptT $ catch (Right <$> action) catchRPCException
  where
    catchRPCException :: RPCException -> App (Either Error a)
    catchRPCException rpcE = do
      Log.err $ rpcExceptionMsg rpcE
      pure . Left $ mkError status500 "io-error" (pack $ show rpcE)

getTeamData :: TeamId -> Handler TeamData
getTeamData tid = do
  info $ msg "Getting team information"
  g <- view galley
  r <-
    catchRpcErrors $
      rpc'
        "galley"
        g
        ( method GET
            . Bilge.paths ["i", "teams", toByteString' tid]
            . expectStatus (`elem` [200, 404])
        )
  case Bilge.statusCode r of
    200 -> parseResponse (mkError status502 "bad-upstream") r
    _ -> throwE (mkError status404 "no-team" "no such team")

getTeamMembers :: TeamId -> Handler TeamMemberList
getTeamMembers tid = do
  info $ msg "Getting team members"
  g <- view galley
  r <-
    catchRpcErrors $
      rpc'
        "galley"
        g
        ( method GET
            . Bilge.paths ["i", "teams", toByteString' tid, "members"]
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getEmailConsentLog :: EmailAddress -> Handler ConsentLog
getEmailConsentLog email = do
  info $ msg "Getting email consent log"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . Bilge.paths ["i", "consent", "logs", "emails", toByteString' email]
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

-- TODO: Temporary in stern -- All functions below this
--       will eventually be moved to a separate service
--       that will be accessible directly over our public facing API
getUserConsentValue :: UserId -> Handler ConsentValue
getUserConsentValue uid = do
  info $ msg "Getting user consent value"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . header "Z-User" (toByteString' uid)
            . versionedPath "self/consent"
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getMarketoResult :: EmailAddress -> Handler MarketoResult
getMarketoResult email = do
  info $ msg "Getting marketo results"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . Bilge.paths ["i", "marketo", "emails", toByteString' email]
            . expectStatus (`elem` [200, 404])
        )
  -- 404 is acceptable when marketo doesn't know about this user, return an empty result
  case statusCode r of
    200 -> do
      let responseOrError = responseJsonEither r
      case responseOrError of
        Left e -> do
          Log.err $ msg ("Error parsing marketo response: " ++ e)
          throwE (mkError status502 "bad-upstream" (pack e))
        Right res -> pure res
    404 -> pure noEmail
    otherStatus -> do
      Log.err $ msg ("Unexpected status code from marketo: " ++ show otherStatus)
      throwE (mkError status502 "bad-upstream" "")
  where
    noEmail = MarketoResult $ KeyMap.singleton "results" emptyArray

getUserConsentLog :: UserId -> Handler ConsentLog
getUserConsentLog uid = do
  info $ msg "Getting user consent log"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . Bilge.paths ["i", "consent", "logs", "users", toByteString' uid]
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getUserCookies :: UserId -> Handler CookieList
getUserCookies uid = do
  info $ msg "Getting user cookies"
  g <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        g
        ( method GET
            . header "Z-User" (toByteString' uid)
            . versionedPath "cookies"
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getUserConversations :: UserId -> Int -> Handler [Conversation]
getUserConversations uid maxConvs = do
  info $ msg "Getting user conversations"
  fetchAll [] Nothing maxConvs
  where
    fetchAll :: [Conversation] -> Maybe ConvId -> Int -> Handler [Conversation]
    fetchAll xs start remaining = do
      userConversationList <- fetchBatch start (min 100 remaining)
      let batch = convList userConversationList
          remaining' = remaining - length batch
      if (not . null) batch && convHasMore userConversationList && remaining' > 0
        then fetchAll (batch ++ xs) (Just . qUnqualified . cnvQualifiedId $ last batch) remaining'
        else pure (batch ++ xs)
    fetchBatch :: Maybe ConvId -> Int -> Handler (ConversationList Conversation)
    fetchBatch start batchSize = do
      baseReq <- view galley
      r <-
        catchRpcErrors $
          rpc'
            "galley"
            baseReq
            ( method GET
                . header "Z-User" (toByteString' uid)
                . versionedPath "conversations"
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "start" . toByteString') start
                . expect2xx
            )
      unVersioned @'V2 <$> parseResponse (mkError status502 "bad-upstream") r

getUserClients :: UserId -> Handler [Client]
getUserClients uid = do
  info $ msg "Getting user clients"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . header "Z-User" (toByteString' uid)
            . versionedPath "clients"
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  let resultOrError :: Either String [Versioned 'V6 Client] = responseJsonEither r
  case resultOrError of
    Left e -> do
      Log.err $ msg ("Error parsing client response: " ++ e)
      pure []
    Right res -> pure $ fmap unVersioned res

getUserProperties :: UserId -> Handler UserProperties
getUserProperties uid = do
  info $ msg "Getting user properties"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . header "Z-User" (toByteString' uid)
            . versionedPath "properties"
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  keys <- parseResponse (mkError status502 "bad-upstream") r :: Handler [PropertyKey]
  UserProperties <$> fetchProperty b keys mempty
  where
    fetchProperty _ [] acc = pure acc
    fetchProperty b (x : xs) acc = do
      r <-
        catchRpcErrors $
          rpc'
            "brig"
            b
            ( method GET
                . header "Z-User" (toByteString' uid)
                . versionedPaths ["properties", toByteString' x]
                . expect2xx
            )
      info $ msg ("Response" ++ show r)
      value <- parseResponse (mkError status502 "bad-upstream") r
      fetchProperty b xs (Map.insert x value acc)

getUserNotifications :: UserId -> Int -> Handler [QueuedNotification]
getUserNotifications uid maxNotifs = do
  info $ msg "Getting user notifications"
  fetchAll [] Nothing maxNotifs
  where
    fetchAll :: [QueuedNotification] -> Maybe NotificationId -> Int -> ExceptT Error App [QueuedNotification]
    fetchAll xs start remaining = do
      -- size must be within 100-1000
      mUserNotificationList <- fetchBatch start (max 100 (min 1000 remaining))
      case mUserNotificationList of
        Nothing -> pure xs
        Just userNotificationList -> do
          let batch = view queuedNotifications userNotificationList
              remaining' = remaining - length batch
          if (not . null) batch && view queuedHasMore userNotificationList && remaining' > 0
            then fetchAll (batch ++ xs) (Just . view queuedNotificationId $ last batch) remaining'
            else pure (batch ++ xs)
    fetchBatch :: Maybe NotificationId -> Int -> Handler (Maybe QueuedNotificationList)
    fetchBatch start batchSize = do
      baseReq <- view gundeck
      r <-
        catchRpcErrors $
          rpc'
            "gundeck"
            baseReq
            ( method GET
                . header "Z-User" (toByteString' uid)
                . versionedPath "notifications"
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "since" . toByteString') start
                . expectStatus (`elem` [200, 404])
            )
      -- 404 is an acceptable response, in case, for some reason,
      -- "start" is not found we still return a QueuedNotificationList
      case statusCode r of
        200 -> do
          let responseOrError = responseJsonEither r
          case responseOrError of
            Left e -> do
              Log.err $ msg ("Error parsing notification response: " ++ e)
              pure Nothing
            Right res -> pure $ Just res
        404 -> do
          let resultOrError = responseJsonEither r
          case resultOrError of
            Left e -> do
              Log.err $ msg ("Error parsing notification response: " ++ e)
              pure Nothing
            Right res -> pure $ Just res
        otherStatus -> do
          Log.err $ msg ("Unexpected status code from gundeck: " ++ show otherStatus)
          pure Nothing

getSsoDomainRedirect :: Text -> Handler (Maybe CustomBackend)
getSsoDomainRedirect domain = do
  info $ msg "getSsoDomainRedirect"
  -- curl  -XGET ${CLOUD_BACKEND}/custom-backend/by-domain/${DOMAIN_EXAMPLE}
  g <- view galley
  r <-
    catchRpcErrors $
      rpc'
        "galley"
        g
        ( method GET
            . versionedPaths ["custom-backend", "by-domain", encodeUtf8 domain]
            . expectStatus (`elem` [200, 404])
        )
  case statusCode r of
    200 -> Just <$> parseResponse (mkError status502 "bad-upstream") r
    404 -> pure Nothing
    _ -> error "impossible"

putSsoDomainRedirect :: Text -> Text -> Text -> Handler ()
putSsoDomainRedirect domain config welcome = do
  info $ msg "putSsoDomainRedirect"
  -- export DOMAIN_ENTRY='{ \
  --   "config_json_url": "https://wire-rest.https.example.com/config.json", \
  --   "webapp_welcome_url": "https://app.wire.example.com/" \
  -- }'
  -- curl -XPUT http://localhost/i/custom-backend/by-domain/${DOMAIN_EXAMPLE} -d "${DOMAIN_ENTRY}"
  g <- view galley
  void
    . catchRpcErrors
    $ rpc'
      "galley"
      g
      ( method PUT
          . Bilge.paths
            [ "i",
              "custom-backend",
              "by-domain",
              urlEncode True (encodeUtf8 domain)
            ]
          . Bilge.json (object ["config_json_url" .= config, "webapp_welcome_url" .= welcome])
          . expect2xx
      )

deleteSsoDomainRedirect :: Text -> Handler ()
deleteSsoDomainRedirect domain = do
  info $ msg "deleteSsoDomainRedirect"
  -- curl -XDELETE http://localhost/i/custom-backend/by-domain/${DOMAIN_EXAMPLE}
  g <- view galley
  void
    . catchRpcErrors
    $ rpc'
      "galley"
      g
      ( method DELETE
          . Bilge.paths
            [ "i",
              "custom-backend",
              "by-domain",
              urlEncode True (encodeUtf8 domain)
            ]
          . expect2xx
      )

registerOAuthClient :: OAuthClientConfig -> Handler OAuthClientCredentials
registerOAuthClient conf = do
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method POST
            . Bilge.paths ["i", "oauth", "clients"]
            . Bilge.json conf
            . contentJson
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

getOAuthClient :: OAuthClientId -> Handler OAuthClient
getOAuthClient cid = do
  b <- view brig
  r <-
    lift $
      rpc'
        "brig"
        b
        ( method GET
            . Bilge.paths ["i", "oauth", "clients", toByteString' cid]
        )
  case statusCode r of
    200 -> parseResponse (mkError status502 "bad-upstream") r
    404 -> throwE (mkError status404 "bad-upstream" "not-found")
    _ -> throwE (mkError status502 "bad-upstream" (LT.pack $ show r))

updateOAuthClient :: OAuthClientId -> OAuthClientConfig -> Handler OAuthClient
updateOAuthClient cid conf = do
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method PUT
            . Bilge.paths ["i", "oauth", "clients", toByteString' cid]
            . Bilge.json conf
            . contentJson
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r

deleteOAuthClient :: OAuthClientId -> Handler ()
deleteOAuthClient cid = do
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method DELETE
            . Bilge.paths ["i", "oauth", "clients", toByteString' cid]
            . expect2xx
        )
  parseResponse (mkError status502 "bad-upstream") r
