module Brig.User.API.Auth (routes) where

import Imports
import Brig.API.Error
import Brig.API.Handler
import Brig.Phone
import Brig.App (AppIO)
import Brig.Types.Intra (reAuthPassword, ReAuthUser)
import Brig.Types.User.Auth
import Data.ByteString.Conversion
import Data.Id
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Routing
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Response (json, empty)
import Network.Wai.Utilities.Request (jsonRequest, JsonRequest)
import Network.Wai.Utilities.Swagger (document)

import qualified Brig.API.User                 as User
import qualified Brig.User.Auth                as Auth
import qualified Brig.User.Auth.Cookie         as Auth
import qualified Data.ByteString               as BS
import qualified Brig.Types.Swagger            as Doc
import qualified Data.Swagger.Build.Api        as Doc
import qualified Network.Wai.Utilities.Swagger as Doc
import qualified Brig.ZAuth                    as ZAuth
import qualified Data.ZAuth.Token              as ZAuth
import qualified Network.Wai.Predicate         as P

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
    post "/access" (continue renewH) $
        accept "application" "json" .&. tokenRequest

    document "POST" "newAccessToken" $ do
        Doc.summary "Obtain an access tokens for a cookie."
        Doc.notes "You can provide only a cookie or a cookie and token. \
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
        Doc.notes "This operation generates and sends a login code. \
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
            Doc.description "The optional label can later be used to delete all \
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
        Doc.notes "Calling this endpoint will effectively revoke the given cookie \
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
    l <- parseJsonBody req
    let typ = if persist then PersistentCookie else SessionCookie
    _a <- Auth.ssoLogin l typ !>> loginError
    undefined -- tokenResponse a

legalHoldLoginH :: JsonRequest LegalHoldLogin ::: JSON -> Handler Response
legalHoldLoginH (req ::: _) = do
    l <- parseJsonBody req
    let typ = PersistentCookie -- Session cookie isn't a supported use case here
    _a <- Auth.legalHoldLogin l typ !>> legalHoldLoginError
    undefined -- tokenResponse a

-- TODO: add legalhold test checking cookies are revoked (/access/logout is called) when legalhold device is deleted.
logoutH :: JSON ::: Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) -> Handler Response
logoutH (_ ::: Nothing ::: Nothing) = throwStd authMissingCookieAndToken
logoutH (_ ::: Nothing ::: Just _ ) = throwStd authMissingCookie
logoutH (_ ::: Just _  ::: Nothing) = throwStd authMissingToken
logoutH (_ ::: Just (Left _) ::: Just (Right _)) = throwStd authTokenMismatch
logoutH (_ ::: Just (Right _) ::: Just (Left _)) = throwStd authTokenMismatch
logoutH (_ ::: Just (Left ut) ::: Just (Left at)) = do
    Auth.logout ut at !>> zauthError
    return empty
logoutH (_ ::: Just (Right ut) ::: Just (Right at)) = do
    Auth.logout ut at !>> zauthError
    return empty

listCookiesH :: UserId ::: Maybe (List CookieLabel) ::: JSON -> Handler Response
listCookiesH (u ::: ll ::: _) = do
    cs <- lift $ Auth.listCookies u (maybe [] fromList ll)
    return . json $ CookieList cs

rmCookiesH :: UserId ::: JsonRequest RemoveCookies -> Handler Response
rmCookiesH (uid ::: req) = do
    RemoveCookies pw lls ids <- parseJsonBody req
    Auth.revokeAccess uid pw ids lls !>> authError
    return empty

renewH :: JSON ::: Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken) ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) -> Handler Response
renewH (_ ::: Nothing :::  _) = throwStd authMissingCookie
renewH (_ ::: Just userToken ::: accessToken) = do
    case (userToken, accessToken) of
         (Left ut, Just (Left at)) -> (Auth.renewAccess ut (Just at) !>> zauthError) >>= lift . tokenResponse
         (Left ut, Nothing) -> Auth.renewAccess @ZAuth.User @ZAuth.Access ut Nothing !>> zauthError >>= lift . tokenResponse
         (Right lut, Just (Right lat)) -> Auth.renewAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess lut (Just lat) !>> zauthError >>= lift . tokenResponse
         (Right lut, Nothing) -> Auth.renewAccess @ZAuth.LegalHoldUser @ZAuth.LegalHoldAccess lut Nothing !>> zauthError >>= lift . tokenResponse
         (_, _) -> throwStd authTokenMismatch

-- Utilities
--

-- | A predicate that captures user and access tokens for a request handler.
tokenRequest :: forall r . (HasCookies r, HasHeaders r, HasQuery r)
    => Predicate r P.Error (Maybe (Either ZAuth.UserToken ZAuth.LegalHoldUserToken)
    ::: Maybe (Either ZAuth.AccessToken ZAuth.LegalHoldAccessToken) )
tokenRequest = opt (userToken ||| legalHoldUserToken) .&. opt (accessToken ||| legalHoldAccessToken)
  where
    userToken            = cookieErr @ZAuth.User            <$> cookie "zuid"
    legalHoldUserToken   = cookieErr @ZAuth.LegalHoldUser   <$> cookie "zuid"
    accessToken          = parse @ZAuth.Access              <$> (tokenHeader .|. tokenQuery)
    legalHoldAccessToken = parse @ZAuth.LegalHoldAccess     <$> (tokenHeader .|. tokenQuery)
    tokenHeader          = bearer <$> header "authorization"
    tokenQuery           = query "access_token"

    cookieErr :: ZAuth.UserTokenLike u => Result P.Error (ZAuth.Token u) -> Result P.Error (ZAuth.Token u)
    cookieErr x@Okay{} = x
    cookieErr (Fail x) = Fail (setMessage "Invalid user token" (P.setStatus status403 x))

    -- Extract the access token from the Authorization header.
    bearer :: Result P.Error ByteString -> Result P.Error ByteString
    bearer (Fail   x) = Fail x
    bearer (Okay _ b) = let (prefix, suffix) = BS.splitAt 7 b in
        if prefix == "Bearer "
            then return suffix
            else Fail (setReason TypeError
                      (setMessage "Invalid authorization scheme" (err status403)))

    -- Parse the access token
    parse :: ZAuth.AccessTokenLike a => Result P.Error ByteString -> Result P.Error (ZAuth.Token a)
    parse (Fail   x) = Fail x
    parse (Okay _ b) = case fromByteString b of
        Nothing -> Fail (setReason TypeError
                        (setMessage "Invalid access token" (err status403)))
        Just  t -> return t

tokenResponse :: ZAuth.UserTokenLike u => Auth.Access u -> AppIO Response
tokenResponse (Auth.Access t  Nothing) = pure $ json t
tokenResponse (Auth.Access t (Just c)) = Auth.setResponseCookie c (json t)
