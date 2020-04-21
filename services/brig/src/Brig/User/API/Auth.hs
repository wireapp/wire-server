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

module Brig.User.API.Auth
  ( routes,
  )
where

import Brig.API.Error
import Brig.API.Handler
import qualified Brig.API.User as User
import Brig.App (AppIO)
import Brig.Phone
import Brig.Types.Intra (ReAuthUser, reAuthPassword)
import qualified Brig.Types.Swagger as Doc
import Brig.Types.User.Auth
import qualified Brig.User.Auth as Auth
import qualified Brig.User.Auth.Cookie as Auth
import qualified Brig.ZAuth as ZAuth
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Either.Combinators (leftToMaybe, rightToMaybe)
import Data.Id
import Data.Predicate
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate
import qualified Network.Wai.Predicate as P
import Network.Wai.Predicate.Request
import Network.Wai.Routing
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Request (JsonRequest, jsonRequest)
import Network.Wai.Utilities.Response (empty, json)
import Network.Wai.Utilities.Swagger (document)
import qualified Network.Wai.Utilities.Swagger as Doc

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
  post "/access" (continue renewH) $
    accept "application" "json"
      .&. tokenRequest
  document "POST" "newAccessToken" $ do
    Doc.summary "Obtain an access tokens for a cookie."
    Doc.notes
      "You can provide only a cookie or a cookie and token. \
      \Every other combination is invalid. \
      \Access tokens can be given as query parameter or authorisation \
      \header, with the latter being preferred."
    Doc.returns (Doc.ref Doc.accessToken)
    Doc.parameter Doc.Header "cookie" Doc.bytes' $ do
      Doc.description "The 'zuid' cookie header"
      Doc.optional
    Doc.parameter Doc.Header "Authorization" Doc.bytes' $ do
      Doc.description "The access-token as 'Authorization' header."
      Doc.optional
    Doc.parameter Doc.Query "access_token" Doc.bytes' $ do
      Doc.description "The access-token as query parameter."
      Doc.optional
    Doc.errorResponse badCredentials
  --

  post "/login/send" (continue sendLoginCodeH) $
    jsonRequest @SendLoginCode
  document "POST" "sendLoginCode" $ do
    Doc.summary "Send a login code to a verified phone number."
    Doc.notes
      "This operation generates and sends a login code. \
      \A login code can be used only once and times out after \
      \10 minutes. Only one login code may be pending at a time."
    Doc.body (Doc.ref Doc.sendLoginCode) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Doc.loginCodeResponse)
    Doc.response 200 "Login code sent." Doc.end
    Doc.errorResponse invalidPhone
    Doc.errorResponse passwordExists
    Doc.errorResponse' loginCodePending Doc.pendingLoginError
  --

  post "/login" (continue loginH) $
    jsonRequest @Login
      .&. def False (query "persist")
      .&. accept "application" "json"
  document "POST" "login" $ do
    Doc.summary "Authenticate a user to obtain a cookie and first access token."
    Doc.notes "Logins are throttled at the server's discretion."
    Doc.body (Doc.ref Doc.login) $
      Doc.description
        "The optional label can later be used to delete all \
        \cookies matching this label (cf. /cookies/remove)."
    Doc.parameter Doc.Query "persist" (Doc.bool $ Doc.def False) $ do
      Doc.description "Request a persistent cookie instead of a session cookie."
      Doc.optional
    Doc.errorResponse badCredentials
    Doc.errorResponse accountSuspended
    Doc.errorResponse accountPending
    Doc.errorResponse loginsTooFrequent
  --

  post "/access/logout" (continue logoutH) $
    accept "application" "json" .&. tokenRequest
  document "POST" "logout" $ do
    Doc.summary "Log out in order to remove a cookie from the server."
    Doc.notes
      "Calling this endpoint will effectively revoke the given cookie \
      \and subsequent calls to /access with the same cookie will \
      \result in a 403."
    Doc.parameter Doc.Header "cookie" Doc.bytes' $
      Doc.description "The 'zuid' cookie header"
    Doc.parameter Doc.Header "Authorization" Doc.bytes' $ do
      Doc.description "The access-token as 'Authorization' header."
      Doc.optional
    Doc.parameter Doc.Query "access_token" Doc.bytes' $ do
      Doc.description "The access-token as query parameter."
      Doc.optional
    Doc.errorResponse badCredentials
  --

  get "/cookies" (continue listCookiesH) $
    header "Z-User"
      .&. opt (query "labels")
      .&. accept "application" "json"
  document "GET" "getCookies" $ do
    Doc.summary "Retrieve the list of cookies currently stored for the user."
    Doc.returns (Doc.ref Doc.cookieList)
    Doc.parameter Doc.Query "labels" Doc.bytes' $ do
      Doc.description "Filter by label (comma-separated list)"
      Doc.optional
  --

  post "/cookies/remove" (continue rmCookiesH) $
    header "Z-User"
      .&. jsonRequest @RemoveCookies
  document "POST" "rmCookies" $ do
    Doc.summary "Revoke stored cookies."
    Doc.body (Doc.ref Doc.removeCookies) Doc.end
    Doc.errorResponse badCredentials
  -- Internal

  -- galley can query this endpoint at the right moment in the LegalHold flow
  post "/i/legalhold-login" (continue legalHoldLoginH) $
    jsonRequest @LegalHoldLogin
      .&. accept "application" "json"
  post "/i/sso-login" (continue ssoLoginH) $
    jsonRequest @SsoLogin
      .&. def False (query "persist")
      .&. accept "application" "json"
  get "/i/users/login-code" (continue getLoginCodeH) $
    accept "application" "json"
      .&. param "phone"
  get "/i/users/:uid/reauthenticate" (continue reAuthUserH) $
    capture "uid"
      .&. jsonRequest @ReAuthUser

-- Handlers

sendLoginCodeH :: JsonRequest SendLoginCode -> Handler Response
sendLoginCodeH req = do
  json <$> (sendLoginCode =<< parseJsonBody req)

sendLoginCode :: SendLoginCode -> Handler LoginCodeTimeout
sendLoginCode (SendLoginCode phone call force) = do
  checkWhitelist (Right phone)
  c <- Auth.sendLoginCode phone call force !>> sendLoginCodeError
  return $ LoginCodeTimeout (pendingLoginTimeout c)

getLoginCodeH :: JSON ::: Phone -> Handler Response
getLoginCodeH (_ ::: phone) = json <$> getLoginCode phone

getLoginCode :: Phone -> Handler PendingLoginCode
getLoginCode phone = do
  code <- lift $ Auth.lookupLoginCode phone
  maybe (throwStd loginCodeNotFound) return code

reAuthUserH :: UserId ::: JsonRequest ReAuthUser -> Handler Response
reAuthUserH (uid ::: req) = do
  reAuthUser uid =<< parseJsonBody req
  return empty

reAuthUser :: UserId -> ReAuthUser -> Handler ()
reAuthUser uid body = do
  User.reauthenticate uid (reAuthPassword body) !>> reauthError

loginH :: JsonRequest Login ::: Bool ::: JSON -> Handler Response
loginH (req ::: persist ::: _) = do
  lift . tokenResponse =<< flip login persist =<< parseJsonBody req

login :: Login -> Bool -> Handler (Auth.Access ZAuth.User)
login l persist = do
  let typ = if persist then PersistentCookie else SessionCookie
  Auth.login l typ !>> loginError

ssoLoginH :: JsonRequest SsoLogin ::: Bool ::: JSON -> Handler Response
ssoLoginH (req ::: persist ::: _) = do
  lift . tokenResponse =<< flip ssoLogin persist =<< parseJsonBody req

ssoLogin :: SsoLogin -> Bool -> Handler (Auth.Access ZAuth.User)
ssoLogin l persist = do
  let typ = if persist then PersistentCookie else SessionCookie
  Auth.ssoLogin l typ !>> loginError

legalHoldLoginH :: JsonRequest LegalHoldLogin ::: JSON -> Handler Response
legalHoldLoginH (req ::: _) = do
  lift . tokenResponse =<< legalHoldLogin =<< parseJsonBody req

legalHoldLogin :: LegalHoldLogin -> Handler (Auth.Access ZAuth.LegalHoldUser)
legalHoldLogin l = do
  let typ = PersistentCookie -- Session cookie isn't a supported use case here
  Auth.legalHoldLogin l typ !>> legalHoldLoginError

logoutH :: JSON ::: Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) -> Handler Response
logoutH (_ ::: ut ::: at) = empty <$ logout ut at

-- TODO: add legalhold test checking cookies are revoked (/access/logout is called) when legalhold device is deleted.
logout ::
  Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ->
  Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) ->
  Handler ()
logout Nothing Nothing = throwStd authMissingCookieAndToken
logout Nothing (Just _) = throwStd authMissingCookie
logout (Just _) Nothing = throwStd authMissingToken
logout (Just (Left _)) (Just (Right _)) = throwStd authTokenMismatch
logout (Just (Right _)) (Just (Left _)) = throwStd authTokenMismatch
logout (Just (Left ut)) (Just (Left at)) = Auth.logout ut at !>> zauthError
logout (Just (Right ut)) (Just (Right at)) = Auth.logout ut at !>> zauthError

listCookiesH :: UserId ::: Maybe (List CookieLabel) ::: JSON -> Handler Response
listCookiesH (u ::: ll ::: _) = json <$> lift (listCookies u ll)

listCookies :: UserId -> Maybe (List CookieLabel) -> AppIO CookieList
listCookies u ll = do
  CookieList <$> Auth.listCookies u (maybe [] fromList ll)

rmCookiesH :: UserId ::: JsonRequest RemoveCookies -> Handler Response
rmCookiesH (uid ::: req) = do
  empty <$ (rmCookies uid =<< parseJsonBody req)

rmCookies :: UserId -> RemoveCookies -> Handler ()
rmCookies uid (RemoveCookies pw lls ids) = do
  Auth.revokeAccess uid pw ids lls !>> authError

renewH :: JSON ::: Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) -> Handler Response
renewH (_ ::: ut ::: at) = lift . either tokenResponse tokenResponse =<< renew ut at

-- | renew access for either:
-- * a user with user token and optional access token, or
-- * a legalhold user with legalhold user token and optional legalhold access token.
--
-- Other combinations of provided inputs will cause an error to be raised.
renew ::
  Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ->
  Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) ->
  Handler (Either (Auth.Access ZAuth.User) (Auth.Access ZAuth.LegalHoldUser))
renew = \case
  Nothing ->
    const $ throwStd authMissingCookie
  (Just (Left userToken)) ->
    -- normal UserToken, so we want a normal AccessToken
    fmap Left . renewAccess userToken <=< matchingOrNone leftToMaybe
  (Just (Right legalholdUserToken)) ->
    -- LegalholdUserToken, so we want a LegalholdAccessToken
    fmap Right . renewAccess legalholdUserToken <=< matchingOrNone rightToMaybe
  where
    renewAccess ut mat =
      Auth.renewAccess ut mat !>> zauthError
    matchingOrNone :: (a -> Maybe b) -> Maybe a -> Handler (Maybe b)
    matchingOrNone matching = traverse $ \accessToken ->
      case matching accessToken of
        Just m -> pure m
        Nothing -> throwStd authTokenMismatch

-- Utilities
--

-- | A predicate that captures user and access tokens for a request handler.
tokenRequest ::
  forall r.
  (HasCookies r, HasHeaders r, HasQuery r) =>
  Predicate r P.Error
    ( Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken)
        ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken)
    )
tokenRequest = opt (userToken ||| legalHoldUserToken) .&. opt (accessToken ||| legalHoldAccessToken)
  where
    userToken = cookieErr @ZAuth.User <$> cookie "zuid"
    legalHoldUserToken = cookieErr @ZAuth.LegalHoldUser <$> cookie "zuid"
    accessToken = parse @ZAuth.Access <$> (tokenHeader .|. tokenQuery)
    legalHoldAccessToken = parse @ZAuth.LegalHoldAccess <$> (tokenHeader .|. tokenQuery)
    tokenHeader = bearer <$> header "authorization"
    tokenQuery = query "access_token"
    cookieErr :: ZAuth.UserTokenLike u => Result P.Error (ZAuth.Token u) -> Result P.Error (ZAuth.Token u)
    cookieErr x@Okay {} = x
    cookieErr (Fail x) = Fail (setMessage "Invalid user token" (P.setStatus status403 x))
    -- Extract the access token from the Authorization header.
    bearer :: Result P.Error ByteString -> Result P.Error ByteString
    bearer (Fail x) = Fail x
    bearer (Okay _ b) =
      let (prefix, suffix) = BS.splitAt 7 b
       in if prefix == "Bearer "
            then return suffix
            else
              Fail
                ( setReason
                    TypeError
                    (setMessage "Invalid authorization scheme" (err status403))
                )
    -- Parse the access token
    parse :: ZAuth.AccessTokenLike a => Result P.Error ByteString -> Result P.Error (ZAuth.Token a)
    parse (Fail x) = Fail x
    parse (Okay _ b) = case fromByteString b of
      Nothing ->
        Fail
          ( setReason
              TypeError
              (setMessage "Invalid access token" (err status403))
          )
      Just t -> return t

tokenResponse :: ZAuth.UserTokenLike u => Auth.Access u -> AppIO Response
tokenResponse (Auth.Access t Nothing) = pure $ json t
tokenResponse (Auth.Access t (Just c)) = Auth.setResponseCookie c (json t)
