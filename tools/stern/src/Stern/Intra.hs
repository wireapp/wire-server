{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
  ( putUser,
    putUserStatus,
    getContacts,
    getUserConnections,
    getUsersConnections,
    getUserProfiles,
    getUserProfilesByIdentity,
    getUserProperties,
    getInvoiceUrl,
    revokeIdentity,
    changeEmail,
    changePhone,
    deleteAccount,
    deleteBindingTeam,
    getTeamInfo,
    getUserBindingTeam,
    isBlacklisted,
    setBlacklistStatus,
    getTeamFeatureFlag,
    setTeamFeatureFlag,
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
  )
where

import Bilge hiding (head, options, requestId)
import Bilge.RPC
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth
import Control.Error
import Control.Lens (view, (^.))
import Control.Monad.Reader
import Data.Aeson hiding (Error)
import Data.Aeson.Types (emptyArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Conversion
import Data.Handle (Handle)
import qualified Data.HashMap.Strict as M
import Data.Id
import Data.Int
import Data.List.Split (chunksOf)
import Data.Text (Text, strip)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (pack)
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Gundeck.Types
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wai.Utilities (Error (..))
import Stern.App
import Stern.Types
import System.Logger.Class hiding (Error, name, (.=))
import qualified System.Logger.Class as Log
import UnliftIO.Exception hiding (Handler)
import qualified Wire.API.Team.Feature as Public

-------------------------------------------------------------------------------

putUser :: UserId -> UserUpdate -> Handler ()
putUser uid upd = do
  info $ userMsg uid . msg "Changing user state"
  b <- view brig
  void $
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method PUT
            . path "/self"
            . header "Z-User" (toByteString' uid)
            . header "Z-Connection" (toByteString' "")
            . lbytes (encode upd)
            . contentJson
            . expect2xx
        )

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
            . paths ["/i/users", toByteString' uid, "status"]
            . lbytes (encode payload)
            . contentJson
            . expect2xx
        )
  where
    payload = AccountStatusUpdate status

getUserConnections :: UserId -> Handler [UserConnection]
getUserConnections uid = do
  info $ msg "Getting user connections"
  fetchAll [] Nothing
  where
    fetchAll xs start = do
      userConnectionList <- fetchBatch start
      let batch = clConnections userConnectionList
      if (not . null) batch && (clHasMore userConnectionList)
        then fetchAll (batch ++ xs) (Just . ucTo $ last batch)
        else return (batch ++ xs)
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
                . path "/connections"
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "start" . toByteString') start
                . expect2xx
            )
      parseResponse (Error status502 "bad-upstream") r
    batchSize = 100 :: Int

getUsersConnections :: List UserId -> Handler [ConnectionStatus]
getUsersConnections uids = do
  info $ msg "Getting user connections"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . path "/i/users/connections-status"
            . queryItem "users" users
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  parseResponse (Error status502 "bad-upstream") r
  where
    users = BS.intercalate "," $ map toByteString' (fromList uids)

getUserProfiles :: Either [UserId] [Handle] -> Handler [UserAccount]
getUserProfiles uidsOrHandles = do
  info $ msg "Getting user accounts"
  b <- view brig
  return . concat =<< mapM (doRequest b) (prepareQS uidsOrHandles)
  where
    doRequest :: Request -> (Request -> Request) -> Handler [UserAccount]
    doRequest b qry = do
      r <-
        catchRpcErrors $
          rpc'
            "brig"
            b
            ( method GET
                . path "/i/users"
                . qry
                . expect2xx
            )
      parseResponse (Error status502 "bad-upstream") r
    prepareQS :: Either [UserId] [Handle] -> [(Request -> Request)]
    prepareQS (Left uids) = fmap (queryItem "ids") (toQS uids)
    prepareQS (Right handles) = fmap (queryItem "handles") (toQS handles)
    toQS :: ToByteString a => [a] -> [ByteString]
    toQS =
      fmap (BS.intercalate "," . map toByteString')
        . chunksOf 50

getUserProfilesByIdentity :: Either Email Phone -> Handler [UserAccount]
getUserProfilesByIdentity emailOrPhone = do
  info $ msg "Getting user accounts by identity"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method GET
            . path "/i/users"
            . userKeyToParam emailOrPhone
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

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
            . path "/search/contacts"
            . header "Z-User" (toByteString' u)
            . queryItem "q" (toByteString' q)
            . queryItem "size" (toByteString' s)
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

revokeIdentity :: Either Email Phone -> Handler ()
revokeIdentity emailOrPhone = do
  info $ msg "Revoking user identity"
  b <- view brig
  void . catchRpcErrors $
    rpc'
      "brig"
      b
      ( method POST
          . path "/i/users/revoke-identity"
          . userKeyToParam emailOrPhone
          . expect2xx
      )

deleteAccount :: UserId -> Handler ()
deleteAccount uid = do
  info $ msg "Deleting account"
  b <- view brig
  void . catchRpcErrors $
    rpc'
      "brig"
      b
      ( method DELETE
          . paths ["/i/users", toByteString' uid]
          . expect2xx
      )

deleteBindingTeam :: TeamId -> Handler ()
deleteBindingTeam tid = do
  info $ msg "Deleting team"
  g <- view galley
  void . catchRpcErrors $
    rpc'
      "galley"
      g
      ( method DELETE
          . paths ["/i/teams", toByteString' tid]
          . expect2xx
      )

changeEmail :: UserId -> EmailUpdate -> Handler ()
changeEmail u upd = do
  info $ msg "Updating email address"
  b <- view brig
  void . catchRpcErrors $
    rpc'
      "brig"
      b
      ( method PUT
          . path "/self/email"
          . header "Z-User" (toByteString' u)
          . header "Z-Connection" (toByteString' "")
          . lbytes (encode upd)
          . contentJson
          . expect2xx
      )

changePhone :: UserId -> PhoneUpdate -> Handler ()
changePhone u upd = do
  info $ msg "Updating phone number"
  b <- view brig
  void . catchRpcErrors $
    rpc'
      "brig"
      b
      ( method PUT
          . path "/self/phone"
          . header "Z-User" (toByteString' u)
          . header "Z-Connection" (toByteString' "")
          . lbytes (encode upd)
          . contentJson
          . expect2xx
      )

getTeamInfo :: TeamId -> Handler TeamInfo
getTeamInfo tid = do
  d <- getTeamData tid
  m <- getTeamMembers tid
  return $ TeamInfo d (map TeamMemberInfo (m ^. teamMembers))

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
            . path "teams"
            . header "Z-User" (toByteString' u)
            . header "Z-Connection" (toByteString' "")
            . expect2xx
        )
  teams <- parseResponse (Error status502 "bad-upstream") r
  return $
    listToMaybe $
      fmap (view teamId) $
        filter ((== Binding) . view teamBinding) $
          teams ^. teamListTeams

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
            . paths ["i", "team", toByteString' tid, "invoice", toByteString' iid]
            . noRedirect
            . expectStatus (== 307)
        )
  return $ getHeader' "Location" r

getTeamBillingInfo :: TeamId -> Handler (Maybe TeamBillingInfo)
getTeamBillingInfo tid = do
  info $ msg "Getting team billing info"
  i <- view ibis
  r <-
    catchRpcErrors $
      rpc'
        "ibis"
        i
        ( method GET
            . paths ["i", "team", toByteString' tid, "billing"]
        )
  case Bilge.statusCode r of
    200 -> Just <$> parseResponse (Error status502 "bad-upstream") r
    404 -> return Nothing
    _ -> throwE (Error status502 "bad-upstream" "bad response")

setTeamBillingInfo :: TeamId -> TeamBillingInfo -> Handler ()
setTeamBillingInfo tid tbu = do
  info $ msg "Setting team billing info"
  i <- view ibis
  void . catchRpcErrors $
    rpc'
      "ibis"
      i
      ( method PUT
          . paths ["i", "team", toByteString' tid, "billing"]
          . lbytes (encode tbu)
          . contentJson
          . expect2xx
      )

isBlacklisted :: Either Email Phone -> Handler Bool
isBlacklisted emailOrPhone = do
  info $ msg "Checking blacklist"
  b <- view brig
  r <-
    catchRpcErrors $
      rpc'
        "brig"
        b
        ( method HEAD
            . path "i/users/blacklist"
            . userKeyToParam emailOrPhone
        )
  case Bilge.statusCode r of
    200 -> return True
    404 -> return False
    _ -> throwE (Error status502 "bad-upstream" "bad response")

setBlacklistStatus :: Bool -> Either Email Phone -> Handler ()
setBlacklistStatus status emailOrPhone = do
  info $ msg "Changing blacklist status"
  b <- view brig
  void . catchRpcErrors $
    rpc'
      "brig"
      b
      ( method (statusToMethod status)
          . path "i/users/blacklist"
          . userKeyToParam emailOrPhone
          . expect2xx
      )
  where
    statusToMethod False = DELETE
    statusToMethod True = POST

getTeamFeatureFlag :: TeamId -> Public.TeamFeatureName -> Handler Public.TeamFeatureStatus
getTeamFeatureFlag tid feature = do
  info $ msg "Getting team feature status"
  gly <- view galley
  let req =
        method GET
          . paths ["/i/teams", toByteString' tid, "features", toByteString' feature]
          . expect2xx
  responseJsonUnsafe <$> catchRpcErrors (rpc' "galley" gly req)

setTeamFeatureFlag :: TeamId -> Public.TeamFeatureName -> Public.TeamFeatureStatusValue -> Handler ()
setTeamFeatureFlag tid feature status = do
  info $ msg "Setting team feature status"
  gly <- view galley
  let req =
        method PUT
          . paths ["/i/teams", toByteString' tid, "features", toByteString' feature]
          . Bilge.json (Public.TeamFeatureStatus status)
          . contentJson
  resp <- catchRpcErrors $ rpc' "galley" gly req
  case statusCode resp of
    204 -> pure ()
    _ -> throwE $ responseJsonUnsafe resp

getSearchVisibility :: TeamId -> Handler TeamSearchVisibilityView
getSearchVisibility tid = do
  info $ msg "Getting TeamSearchVisibilityView value"
  gly <- view galley
  (>>= fromResponseBody) . catchRpcErrors $
    rpc'
      "galley"
      gly
      ( method GET
          . paths ["/i/teams", toByteString' tid, "search-visibility"]
          . expect2xx
      )
  where
    fromResponseBody :: Response (Maybe LByteString) -> Handler TeamSearchVisibilityView
    fromResponseBody resp = parseResponse (Error status502 "bad-upstream") resp

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
            . paths ["/i/teams", toByteString' tid, "search-visibility"]
            . lbytes (encode $ TeamSearchVisibilityView typ)
            . contentJson
        )
  case statusCode resp of
    204 -> pure ()
    403 ->
      throwE $
        Error
          status403
          "team-search-visibility-unset"
          "This team does not have TeamSearchVisibility enabled. Ensure this is the correct TeamID or first enable the feature"
    _ -> throwE $ responseJsonUnsafe resp

--------------------------------------------------------------------------------
-- Helper functions
stripBS :: ByteString -> ByteString
stripBS = encodeUtf8 . strip . decodeUtf8

userKeyToParam :: Either Email Phone -> Request -> Request
userKeyToParam (Left e) = queryItem "email" (stripBS $ toByteString' e)
userKeyToParam (Right p) = queryItem "phone" (stripBS $ toByteString' p)

-- | Run an App and catch any RPCException's which may occur, lifting them to ExceptT
-- This isn't an ideal set-up; but is required in certain cases because 'ExceptT' isn't
-- an instance of 'MonadUnliftIO'
catchRpcErrors :: forall a. App a -> ExceptT Error App a
catchRpcErrors action = ExceptT $ catch (Right <$> action) catchRPCException
  where
    catchRPCException :: RPCException -> App (Either Error a)
    catchRPCException rpcE = do
      Log.err $ rpcExceptionMsg rpcE
      pure . Left $ Error status500 "io-error" (pack $ show rpcE)

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
            . paths ["i", "teams", toByteString' tid]
            . expectStatus (`elem` [200, 404])
        )
  case Bilge.statusCode r of
    200 -> parseResponse (Error status502 "bad-upstream") r
    _ -> throwE (Error status404 "no-team" "no such team")

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
            . paths ["i", "teams", toByteString' tid, "members"]
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

getEmailConsentLog :: Email -> Handler ConsentLog
getEmailConsentLog email = do
  info $ msg "Getting email consent log"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . paths ["/i/consent/logs/emails", toByteString' email]
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

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
            . path "/self/consent"
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

getMarketoResult :: Email -> Handler MarketoResult
getMarketoResult email = do
  info $ msg "Getting marketo results"
  g <- view galeb
  r <-
    catchRpcErrors $
      rpc'
        "galeb"
        g
        ( method GET
            . paths ["/i/marketo/emails", toByteString' email]
            . expectStatus (`elem` [200, 404])
        )
  -- 404 is acceptable when marketo doesn't know about this user, return an empty result
  case statusCode r of
    200 -> parseResponse (Error status502 "bad-upstream") r
    404 -> return noEmail
    _ -> throwE (Error status502 "bad-upstream" "")
  where
    noEmail = MarketoResult $ M.singleton "results" emptyArray

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
            . paths ["/i/consent/logs/users", toByteString' uid]
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

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
            . path "/cookies"
            . expect2xx
        )
  parseResponse (Error status502 "bad-upstream") r

getUserConversations :: UserId -> Handler [Conversation]
getUserConversations uid = do
  info $ msg "Getting user conversations"
  fetchAll [] Nothing
  where
    fetchAll xs start = do
      userConversationList <- fetchBatch start
      let batch = convList userConversationList
      if (not . null) batch && (convHasMore userConversationList)
        then fetchAll (batch ++ xs) (Just . cnvId $ last batch)
        else return (batch ++ xs)
    fetchBatch :: Maybe ConvId -> Handler (ConversationList Conversation)
    fetchBatch start = do
      b <- view galley
      r <-
        catchRpcErrors $
          rpc'
            "galley"
            b
            ( method GET
                . header "Z-User" (toByteString' uid)
                . path "/conversations"
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "start" . toByteString') start
                . expect2xx
            )
      parseResponse (Error status502 "bad-upstream") r
    batchSize = 100 :: Int

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
            . path "/clients"
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  parseResponse (Error status502 "bad-upstream") r

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
            . path "/properties"
            . expect2xx
        )
  info $ msg ("Response" ++ show r)
  keys <- parseResponse (Error status502 "bad-upstream") r :: Handler [PropertyKey]
  UserProperties <$> fetchProperty b keys M.empty
  where
    fetchProperty _ [] acc = return acc
    fetchProperty b (x : xs) acc = do
      r <-
        catchRpcErrors $
          rpc'
            "brig"
            b
            ( method GET
                . header "Z-User" (toByteString' uid)
                . paths ["/properties", toByteString' x]
                . expect2xx
            )
      info $ msg ("Response" ++ show r)
      value <- parseResponse (Error status502 "bad-upstream") r
      fetchProperty b xs (M.insert x value acc)

getUserNotifications :: UserId -> Handler [QueuedNotification]
getUserNotifications uid = do
  info $ msg "Getting user notifications"
  fetchAll [] Nothing
  where
    fetchAll xs start = do
      userNotificationList <- fetchBatch start
      let batch = view queuedNotifications userNotificationList
      if (not . null) batch && (view queuedHasMore userNotificationList)
        then fetchAll (batch ++ xs) (Just . (view queuedNotificationId) $ last batch)
        else return (batch ++ xs)
    fetchBatch :: Maybe NotificationId -> Handler QueuedNotificationList
    fetchBatch start = do
      b <- view gundeck
      r <-
        catchRpcErrors $
          rpc'
            "galley"
            b
            ( method GET
                . header "Z-User" (toByteString' uid)
                . path "/notifications"
                . queryItem "size" (toByteString' batchSize)
                . maybe id (queryItem "since" . toByteString') start
                . expectStatus (`elem` [200, 404])
            )
      -- 404 is an acceptable response, in case, for some reason,
      -- "start" is not found we still return a QueuedNotificationList
      case statusCode r of
        200 -> parseResponse (Error status502 "bad-upstream") r
        404 -> parseResponse (Error status502 "bad-upstream") r
        _ -> throwE (Error status502 "bad-upstream" "")
    batchSize = 100 :: Int
