{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Stern.API
  ( start,
  )
where

import Brig.Types
import Brig.Types.Intra
import Control.Applicative ((<|>))
import Control.Error
import Control.Lens ((^.))
import Control.Monad (liftM, unless, void, when)
import Control.Monad.Catch (throwM)
import Data.Aeson hiding (Error, json)
import Data.Aeson.Types (emptyArray)
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Handle (Handle)
import Data.Id
import Data.Predicate
import Data.Range
import Data.Swagger.Build.Api hiding (Response, def, min, response)
import qualified Data.Swagger.Build.Api as Doc
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Imports hiding (head)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Predicate hiding (Error, reason, setStatus)
import Network.Wai.Routing hiding (trace)
import Network.Wai.Utilities
import qualified Network.Wai.Utilities.Server as Server
import Network.Wai.Utilities.Swagger (document, mkSwaggerApi)
import Stern.API.Predicates
import Stern.App
import qualified Stern.Intra as Intra
import Stern.Options
import qualified Stern.Swagger as Doc
import Stern.Types
import System.Logger.Class hiding ((.=), Error, name, trace)
import Util.Options
import qualified Wire.Swagger as Doc

default (ByteString)

start :: Opts -> IO ()
start o = do
  e <- newEnv o
  s <- Server.newSettings (server e)
  runSettings s (pipeline e)
  where
    server e = Server.defaultServer (unpack $ (stern o) ^. epHost) ((stern o) ^. epPort) (e ^. applog) (e ^. metrics)
    pipeline e = GZip.gzip GZip.def $ serve e
    serve e r k = runHandler e r (Server.route (Server.compile sitemap) r k) k

sitemap :: Routes Doc.ApiBuilder Handler ()
sitemap = do
  ---------------------------------------------------------------------------
  -- Begin Internal

  get "/i/status" (continue $ const $ return empty) true
  head "/i/status" (continue $ const $ return empty) true
  -- End Internal

  post "/users/:uid/suspend" (continue suspendUser) $
    capture "uid"
  document "POST" "users/:uid/suspend" $ do
    Doc.summary "Suspends user with this ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User successfully suspended" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  post "/users/:uid/unsuspend" (continue unsuspendUser) $
    capture "uid"
  document "POST" "users/:uid/unsuspend" $ do
    Doc.summary "Unsuspends user with this ID"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.response 200 "User successfully unsuspended" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  get "/users" (continue usersByEmail) $
    param "email"
  document "GET" "users" $ do
    Doc.summary "Displays user's info given an email address"
    Doc.parameter Doc.Query "email" Doc.string' $
      Doc.description "Email address"
    Doc.response 200 "List of users" Doc.end
  get
    "/users"
    (continue usersByPhone)
    phoneParam
  document "GET" "users" $ do
    Doc.summary "Displays user's info given a phone number"
    Doc.parameter Doc.Query "phone" Doc.string' $
      Doc.description "Phone number"
    Doc.response 200 "List of users" Doc.end
  get "/users" (continue usersByIds) $
    param "ids"
  document "GET" "users" $ do
    Doc.summary "Displays active users info given a list of ids"
    Doc.parameter Doc.Query "ids" Doc.string' $
      Doc.description "ID of the user"
    Doc.response 200 "List of users" Doc.end
  get "/users" (continue usersByHandles) $
    param "handles"
  document "GET" "users" $ do
    Doc.summary "Displays active users info given a list of handles"
    Doc.parameter Doc.Query "handles" Doc.string' $
      Doc.description "Handle of the user"
    Doc.response 200 "List of users" Doc.end
  get "/users/:uid/connections" (continue userConnections) $
    capture "uid"
  document "GET" "users/:uid/connections" $ do
    Doc.summary "Displays user's connections"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.response 200 "List of user's connections" Doc.end
  get "/users/connections" (continue usersConnections) $
    param "ids"
  document "GET" "users/connections" $ do
    Doc.summary "Displays users connections given a list of ids"
    Doc.parameter Doc.Query "ids" Doc.string' $
      Doc.description "IDs of the users"
    Doc.response 200 "List of users connections" Doc.end
  get "/users/:uid/search" (continue searchOnBehalf) $
    capture "uid"
      .&. def "" (query "q")
      .&. def (unsafeRange 10) (query "size")
  document "GET" "search" $ do
    summary "Search for users on behalf of"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.parameter Query "q" string' $ do
      description "Search query"
      optional
    Doc.parameter Query "size" int32' $ do
      description "Number of results to return"
      optional
    Doc.response 200 "List of users" Doc.end
  post "/users/revoke-identity" (continue revokeIdentity) $
    param "email" ||| phoneParam
  document "POST" "revokeIdentity" $ do
    Doc.summary "Revoke a verified user identity."
    Doc.notes
      "Forcefully revokes a verified user identity. \
      \WARNING: If the given identity is the only verified \
      \user identity of an account, the account will be \
      \deactivated (\"wireless\") and might thus become inaccessible. \
      \If the given identity is not taken / verified, this is a no-op."
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "A verified email address"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A verified phone number (E.164 format)."
      Doc.optional
    Doc.response 200 "Identity revoked or not verified / taken." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  put "/users/:uid/email" (continue changeEmail) $
    contentType "application" "json"
      .&. capture "uid"
      .&. jsonRequest @EmailUpdate
  document "PUT" "changeEmail" $ do
    Doc.summary "Change a user's email address."
    Doc.notes
      "The new e-mail address must be verified \
      \before the change takes effect."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.body (Doc.ref Doc.emailUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Change of email address initiated." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  put "/users/:uid/phone" (continue changePhone) $
    contentType "application" "json"
      .&. capture "uid"
      .&. jsonRequest @PhoneUpdate
  document "PUT" "changePhone" $ do
    Doc.summary "Change a user's phone number."
    Doc.notes
      "The new phone number must be verified \
      \before the change takes effect."
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      Doc.description "User ID"
    Doc.body (Doc.ref Doc.phoneUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Change of phone number initiated." Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  delete "/users/:uid" (continue deleteUser) $
    capture "uid"
      .&. (query "email" ||| phoneParam)
  document "DELETE" "deleteUser" $ do
    summary "Delete a user (irrevocable!)"
    Doc.notes "Email or Phone must match UserId's (to prevent copy/paste mistakes)"
    Doc.parameter Doc.Path "uid" Doc.bytes' $
      description "User ID"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "Matching verified email address"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "Matching verified phone number (E.164 format)."
      Doc.optional
    Doc.response 200 "Account deleted" Doc.end
    Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
  head "/users/blacklist" (continue isUserKeyBlacklisted) $
    (query "email" ||| phoneParam)
  document "HEAD" "checkBlacklistStatus" $ do
    summary "Fetch blacklist information on a email/phone"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to check"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to check"
      Doc.optional
    Doc.response 200 "The email/phone IS blacklisted" Doc.end
    Doc.response 404 "The email/phone is NOT blacklisted" Doc.end
  post "/users/blacklist" (continue addBlacklist) $
    (query "email" ||| phoneParam)
  document "POST" "addToBlacklist" $ do
    summary "Add the email/phone to our blacklist"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to add"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to add"
      Doc.optional
    Doc.response 200 "Operation succeeded" Doc.end
  delete "/users/blacklist" (continue deleteFromBlacklist) $
    (query "email" ||| phoneParam)
  document "DELETE" "deleteFromBlacklist" $ do
    summary "Remove the email/phone from our blacklist"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address to remove"
      Doc.optional
    Doc.parameter Doc.Query "phone" Doc.string' $ do
      Doc.description "A phone to remove"
      Doc.optional
    Doc.response 200 "Operation succeeded" Doc.end
  get "/teams" (continue getTeamInfoByMemberEmail) $
    param "email"
  document "GET" "getTeamInfoByMemberEmail" $ do
    summary "Fetch a team information given a member's email"
    Doc.parameter Doc.Query "email" Doc.string' $
      Doc.description "A verified email address"
    Doc.response 200 "Team Information" Doc.end
  get "/teams/:tid" (continue getTeamInfo) $
    capture "tid"
  document "GET" "getTeamInfo" $ do
    summary "Gets information about a team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Information" Doc.end
  get "/teams/:tid/admins" (continue getTeamAdminInfo) $
    capture "tid"
  document "GET" "getTeamAdminInfo" $ do
    summary "Gets information about a team's owners and admins only"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Information about Owners and Admins" Doc.end
  -- feature flags

  get "/teams/:tid/features/legalhold" (continue (liftM json . Intra.getLegalholdStatus)) $
    capture "tid"
  document "GET" "getLegalholdStatus" $ do
    summary "Shows whether legalhold feature is enabled for team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.returns Doc.docSetLegalHoldStatus
    Doc.response 200 "Legalhold status" Doc.end
    Doc.returns Doc.bool'
  put "/teams/:tid/features/legalhold" (continue setLegalholdStatus) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @SetLegalHoldStatus
  document "PUT" "setLegalholdStatus" $ do
    summary "Disable / enable legalhold feature for team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body Doc.docSetLegalHoldStatus $
      Doc.description "JSON body"
    Doc.response 200 "Legalhold status" Doc.end
  get "/teams/:tid/features/sso" (continue (liftM json . Intra.getSSOStatus)) $
    capture "tid"
  document "GET" "getSSOStatus" $ do
    summary "Shows whether SSO feature is enabled for team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.returns Doc.docSetSSOStatus
    Doc.response 200 "SSO status" Doc.end
  put "/teams/:tid/features/sso" (continue setSSOStatus) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @SetSSOStatus
  document "PUT" "setSSOStatus" $ do
    summary "Disable / enable SSO feature for team"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body Doc.docSetSSOStatus $
      Doc.description "JSON body"
    Doc.response 200 "SSO status" Doc.end
  --- Swagger ---
  get
    "/stern/api-docs"
    ( \(_ ::: url) k ->
        let doc = mkSwaggerApi (decodeLatin1 url) Doc.sternModels sitemap
         in k $ json doc
    )
    $ accept "application" "json"
      .&. query "base_url"
  -- The following endpoint are only relevant internally at Wire

  get "/teams/:tid/invoices/:inr" (continue getTeamInvoice) $
    capture "tid"
      .&. capture "inr"
      .&. accept "application" "json"
  document "GET" "getTeamInvoice" $ do
    summary "Get a specific invoice by Number"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.parameter Doc.Path "inr" Doc.string' $
      Doc.description "Invoice Number"
    Doc.response 307 "Redirect to PDF download" Doc.end
  get "/teams/:tid/billing" (continue getTeamBillingInfo) $
    capture "tid"
  document "GET" "getTeamBillingInfo" $ do
    summary "Gets billing information about a team"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.response 200 "Team Billing Information" Doc.end
    Doc.response 404 "No team or no billing info for given team" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)
  put "/teams/:tid/billing" (continue updateTeamBillingInfo) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @TeamBillingInfoUpdate
  document "PUT" "updateTeamBillingInfo" $ do
    summary
      "Updates billing information about a team. Non \
      \specified fields will NOT be updated"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body (Doc.ref Doc.teamBillingInfoUpdate) $
      Doc.description "JSON body"
    Doc.response 200 "Updated Team Billing Information" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)
  post "/teams/:tid/billing" (continue setTeamBillingInfo) $
    contentType "application" "json"
      .&. capture "tid"
      .&. jsonRequest @TeamBillingInfo
  document "POST" "setTeamBillingInfo" $ do
    summary
      "Sets billing information about a team. Can \
      \only be used on teams that do NOT have any \
      \billing information set. To update team billing \
      \info, use the update endpoint"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      description "Team ID"
    Doc.body (Doc.ref Doc.teamBillingInfo) $
      Doc.description "JSON body"
    Doc.response 200 "Updated Team Billing Information" Doc.end
    Doc.returns (Doc.ref Doc.teamBillingInfo)
  get "/i/consent" (continue getConsentLog) $
    param "email"
  document "GET" "getConsentLog" $ do
    summary "Fetch the consent log given an email address of a non-user"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Query "email" Doc.string' $ do
      Doc.description "An email address"
    Doc.response 200 "Consent Log" Doc.end
    Doc.response 403 "Access denied! There is a user with this email address" Doc.end
  get "/i/user/meta-info" (continue getUserData) $
    param "id"
  document "GET" "getUserMetaInfo" $ do
    summary "Fetch a user's meta info given a user id: TEMPORARY!"
    notes "Relevant only internally at Wire"
    Doc.parameter Doc.Query "id" Doc.bytes' $ do
      Doc.description "A user's ID"
    Doc.response 200 "Meta Info" Doc.end

-----------------------------------------------------------------------------
-- Handlers

type JSON = Media "application" "json"

suspendUser :: UserId -> Handler Response
suspendUser uid = do
  Intra.putUserStatus Suspended uid
  return empty

unsuspendUser :: UserId -> Handler Response
unsuspendUser uid = Intra.putUserStatus Active uid >> return empty

usersByEmail :: Email -> Handler Response
usersByEmail = liftM json . Intra.getUserProfilesByIdentity . Left

usersByPhone :: Phone -> Handler Response
usersByPhone = liftM json . Intra.getUserProfilesByIdentity . Right

usersByIds :: List UserId -> Handler Response
usersByIds = liftM json . Intra.getUserProfiles . Left . fromList

usersByHandles :: List Handle -> Handler Response
usersByHandles = liftM json . Intra.getUserProfiles . Right . fromList

userConnections :: UserId -> Handler Response
userConnections uid = do
  conns <- Intra.getUserConnections uid
  return . json $ groupByStatus conns

usersConnections :: List UserId -> Handler Response
usersConnections = liftM json . Intra.getUsersConnections

searchOnBehalf :: UserId ::: T.Text ::: Range 1 100 Int32 -> Handler Response
searchOnBehalf (uid ::: q ::: s) =
  liftM json $ Intra.getContacts uid q (fromRange s)

revokeIdentity :: Either Email Phone -> Handler Response
revokeIdentity emailOrPhone = Intra.revokeIdentity emailOrPhone >> return empty

changeEmail :: JSON ::: UserId ::: JsonRequest EmailUpdate -> Handler Response
changeEmail (_ ::: uid ::: req) = do
  upd <- parseBody req !>> Error status400 "client-error"
  Intra.changeEmail uid upd
  return empty

changePhone :: JSON ::: UserId ::: JsonRequest PhoneUpdate -> Handler Response
changePhone (_ ::: uid ::: req) = do
  upd <- parseBody req !>> Error status400 "client-error"
  Intra.changePhone uid upd
  return empty

deleteUser :: UserId ::: Either Email Phone -> Handler Response
deleteUser (uid ::: emailOrPhone) = do
  usrs <- Intra.getUserProfilesByIdentity emailOrPhone
  case usrs of
    ((accountUser -> u) : _) ->
      if checkUUID u
        then do
          canBeDeleted <- Intra.getUserBindingTeam uid >>= \case
            Nothing -> pure True
            Just tid -> Intra.canBeDeleted uid tid
          unless canBeDeleted
            $ throwE
            $ Error
              status403
              "no-other-owner"
              "You are trying to remove or downgrade the last owner. \
              \Promote another team member before proceeding."
          info $ userMsg uid . msg (val "Deleting account")
          void $ Intra.deleteAccount uid
          return empty
        else throwE $ Error status400 "match-error" "email or phone did not match UserId"
    _ -> return $ setStatus status404 empty
  where
    checkUUID u = userId u == uid

isUserKeyBlacklisted :: Either Email Phone -> Handler Response
isUserKeyBlacklisted emailOrPhone = do
  bl <- Intra.isBlacklisted emailOrPhone
  if bl
    then response status200 "The given user key IS blacklisted"
    else response status404 "The given user key is NOT blacklisted"
  where
    response st reason =
      return
        . setStatus st
        . json
        $ object ["status" .= (reason :: Text)]

addBlacklist :: Either Email Phone -> Handler Response
addBlacklist emailOrPhone = do
  Intra.setBlacklistStatus True emailOrPhone
  return empty

deleteFromBlacklist :: Either Email Phone -> Handler Response
deleteFromBlacklist emailOrPhone = do
  Intra.setBlacklistStatus False emailOrPhone
  return empty

getTeamInfo :: TeamId -> Handler Response
getTeamInfo = liftM json . Intra.getTeamInfo

getTeamAdminInfo :: TeamId -> Handler Response
getTeamAdminInfo = liftM (json . toAdminInfo) . Intra.getTeamInfo

setLegalholdStatus :: JSON ::: TeamId ::: JsonRequest SetLegalHoldStatus -> Handler Response
setLegalholdStatus (_ ::: tid ::: req) = do
  status <- parseBody req !>> Error status400 "client-error"
  liftM json $ Intra.setLegalholdStatus tid status

setSSOStatus :: JSON ::: TeamId ::: JsonRequest SetSSOStatus -> Handler Response
setSSOStatus (_ ::: tid ::: req) = do
  status :: SetSSOStatus <- parseBody req !>> Error status400 "client-error"
  liftM json $ Intra.setSSOStatus tid status

getTeamBillingInfo :: TeamId -> Handler Response
getTeamBillingInfo tid = do
  ti <- Intra.getTeamBillingInfo tid
  case ti of
    Just t -> return $ json t
    Nothing -> throwE (Error status404 "no-team" "No team or no billing info for team")

updateTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfoUpdate -> Handler Response
updateTeamBillingInfo (_ ::: tid ::: req) = do
  update <- parseBody req !>> Error status400 "client-error"
  current <- Intra.getTeamBillingInfo tid >>= handleNoTeam
  let changes = parse update current
  Intra.setTeamBillingInfo tid changes
  liftM json $ Intra.getTeamBillingInfo tid
  where
    handleNoTeam = ifNothing (Error status404 "no-team" "No team or no billing info for team")
    parse :: TeamBillingInfoUpdate -> TeamBillingInfo -> TeamBillingInfo
    parse TeamBillingInfoUpdate {..} tbi =
      tbi
        { tbiFirstname = fromMaybe (tbiFirstname tbi) (fromRange <$> tbiuFirstname),
          tbiLastname = fromMaybe (tbiLastname tbi) (fromRange <$> tbiuLastname),
          tbiStreet = fromMaybe (tbiStreet tbi) (fromRange <$> tbiuStreet),
          tbiZip = fromMaybe (tbiZip tbi) (fromRange <$> tbiuZip),
          tbiCity = fromMaybe (tbiCity tbi) (fromRange <$> tbiuCity),
          tbiCountry = fromMaybe (tbiCountry tbi) (fromRange <$> tbiuCountry),
          tbiCompany = (fromRange <$> tbiuCompany) <|> tbiCompany tbi,
          tbiState = (fromRange <$> tbiuState) <|> tbiState tbi
        }

setTeamBillingInfo :: JSON ::: TeamId ::: JsonRequest TeamBillingInfo -> Handler Response
setTeamBillingInfo (_ ::: tid ::: req) = do
  billingInfo <- parseBody req !>> Error status400 "client-error"
  current <- Intra.getTeamBillingInfo tid
  when (isJust current) $
    throwE (Error status403 "existing-team" "Cannot set info on existing team, use update instead")
  Intra.setTeamBillingInfo tid billingInfo
  getTeamBillingInfo tid

getTeamInfoByMemberEmail :: Email -> Handler Response
getTeamInfoByMemberEmail e = do
  acc <- (listToMaybe <$> Intra.getUserProfilesByIdentity (Left e)) >>= handleUser
  tid <- (Intra.getUserBindingTeam . userId . accountUser $ acc) >>= handleTeam
  liftM json $ Intra.getTeamInfo tid
  where
    handleUser = ifNothing (Error status404 "no-user" "No such user with that email")
    handleTeam = ifNothing (Error status404 "no-binding-team" "No such binding team")

getTeamInvoice :: TeamId ::: InvoiceId ::: JSON -> Handler Response
getTeamInvoice (tid ::: iid ::: _) = do
  url <- Intra.getInvoiceUrl tid iid
  return $ plain (fromStrict url)

getConsentLog :: Email -> Handler Response
getConsentLog e = do
  acc <- (listToMaybe <$> Intra.getUserProfilesByIdentity (Left e))
  when (isJust acc)
    $ throwE
    $ Error status403 "user-exists" "Trying to access consent log of existing user!"
  consentLog <- Intra.getEmailConsentLog e
  marketo <- Intra.getMarketoResult e
  return . json $
    object
      [ "consent_log" .= consentLog,
        "marketo" .= marketo
      ]

-- TODO: This will be removed as soon as this is ported to another tool
getUserData :: UserId -> Handler Response
getUserData uid = do
  account <- (listToMaybe <$> Intra.getUserProfiles (Left [uid])) >>= noSuchUser
  conns <- Intra.getUserConnections uid
  convs <- Intra.getUserConversations uid
  clts <- Intra.getUserClients uid
  notfs <- Intra.getUserNotifications uid
  consent <- Intra.getUserConsentValue uid
  consentLog <- Intra.getUserConsentLog uid
  cookies <- Intra.getUserCookies uid
  properties <- Intra.getUserProperties uid
  -- Get all info from Marketo too
  let em = userEmail $ accountUser account
  marketo <- maybe (return noEmail) Intra.getMarketoResult em
  return . json $
    object
      [ "account" .= account,
        "cookies" .= cookies,
        "connections" .= conns,
        "conversations" .= convs,
        "clients" .= clts,
        "notifications" .= notfs,
        "consent" .= consent,
        "consent_log" .= consentLog,
        "marketo" .= marketo,
        "properties" .= properties
      ]
  where
    noEmail = let Object o = object ["results" .= emptyArray] in MarketoResult o

-- Utilities

groupByStatus :: [UserConnection] -> Value
groupByStatus conns =
  object
    [ "accepted" .= byStatus Accepted conns,
      "sent" .= byStatus Sent conns,
      "pending" .= byStatus Pending conns,
      "blocked" .= byStatus Blocked conns,
      "ignored" .= byStatus Ignored conns,
      "total" .= length conns
    ]
  where
    byStatus :: Relation -> [UserConnection] -> Int
    byStatus s = length . filter ((==) s . ucStatus)

ifNothing :: Error -> Maybe a -> Handler a
ifNothing e = maybe (throwM e) return

noSuchUser :: Maybe a -> Handler a
noSuchUser = ifNothing (Error status404 "no-user" "No such user")
