{-# LANGUAGE OverloadedStrings          #-}

module API.User.Auth (tests) where

import Bilge hiding (body)
import Bilge.Assert hiding (assert)
import Brig.Types.Intra
import Brig.Types.User
import Brig.Types.User.Auth
import Brig.ZAuth (ZAuth, runZAuth)
import Control.Concurrent
import Control.Lens ((^?), set)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.List (sort, partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import System.Logger (Logger)
import System.Random (randomIO)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options.Common

import qualified Brig.Options         as Opts
import qualified Brig.Types.User.Auth as Auth
import qualified Brig.Types.Code      as Code
import qualified Bilge                as Http
import qualified Data.ByteString.Lazy as Lazy
import qualified Brig.ZAuth           as ZAuth
import qualified Data.Text            as Text
import qualified Data.UUID.V4         as UUID

import qualified Network.Wai.Utilities.Error as Error

tests :: Maybe Opts.Opts -> Manager -> Logger -> Brig -> IO TestTree
tests conf m _ b = do
    z <- mkZAuthEnv conf
    return $ testGroup "auth"
        [ testGroup "login"
            [ test m "email" (testEmailLogin b)
            , test m "phone" (testPhoneLogin b)
            , test m "handle" (testHandleLogin b)
            , test m "email-untrusted-domain" (testLoginUntrustedDomain b)
            , test m "send-phone-code" (testSendLoginCode b)
            , test m "failure" (testLoginFailure b)
            , test m "throttle" (testThrottleLogins conf b)
            ]
        , testGroup "refresh"
            [ test m "invalid-cookie" (testInvalidCookie z b)
            , test m "invalid-token" (testInvalidToken b)
            , test m "missing-cookie" (testMissingCookie z b)
            , test m "unknown-cookie" (testUnknownCookie z b)
            , test m "new-persistent-cookie" (testNewPersistentCookie conf b)
            , test m "new-session-cookie" (testNewSessionCookie conf b)
            ]
        , testGroup "cookies"
            [ test m "list" (testListCookies b)
            , test m "remove-by-label" (testRemoveCookiesByLabel b)
            , test m "remove-by-label-id" (testRemoveCookiesByLabelAndId b)
            , test m "limit" (testTooManyCookies conf b)
            , test m "logout" (testLogout b)
            ]
        , testGroup "reauth"
            [ test m "reauthorisation" (testReauthorisation b)
            ]
        ]

--------------------------------------------------------------------------------
-- ZAuth test environment for generating arbitrary tokens.

mkZAuthEnv :: Maybe Opts.Opts -> IO ZAuth.Env
mkZAuthEnv config = do
    Just (sk :| sks) <- join $ optOrEnv (ZAuth.readKeys . Opts.privateKeys . Opts.zauth) config ZAuth.readKeys "ZAUTH_PRIVKEYS"
    Just (pk :| pks) <- join $ optOrEnv (ZAuth.readKeys . Opts.privateKeys . Opts.zauth) config ZAuth.readKeys "ZAUTH_PUBKEYS"
    ZAuth.mkEnv (sk :| sks) (pk :| pks) ZAuth.defSettings

randomAccessToken :: ZAuth ZAuth.AccessToken
randomAccessToken = randomUserToken >>= ZAuth.newAccessToken

randomUserToken :: ZAuth ZAuth.UserToken
randomUserToken = (Id <$> liftIO UUID.nextRandom) >>= ZAuth.newUserToken

-------------------------------------------------------------------------------
-- Login

testEmailLogin :: Brig -> Http ()
testEmailLogin brig = do
    u <- randomUser brig
    let Just email = userEmail u
    now <- liftIO getCurrentTime

    -- Login with email
    _rs <- login brig (defEmailLogin email) PersistentCookie
        <!! const 200 === statusCode

    -- Check basic cookie attributes
    let ck = decodeCookie _rs
    liftIO $ do
        assertBool "type" (cookie_persistent ck)
        assertBool "http-only" (cookie_http_only ck)
        assertBool "expiry" (cookie_expiry_time ck > cookie_creation_time ck)
        assertBool "domain" (cookie_domain ck /= "")
        assertBool "path" (cookie_path ck /= "")

    -- Check basic access token attributes
    let tk = decodeToken  _rs
    liftIO $ do
        assertEqual "user" (userId u) (ZAuth.accessTokenOf tk)
        assertBool "expiry" (ZAuth.tokenExpiresUTC tk > now)

    -- Login again, but with capitalised email address
    let Email loc dom = email
    let email' = Email (Text.toUpper loc) dom
    login brig (defEmailLogin email') PersistentCookie !!!
        const 200 === statusCode

testPhoneLogin :: Brig -> Http ()
testPhoneLogin brig = do
    p <- randomPhone
    let newUser = RequestBodyLBS . encode $ object
                [ "name"  .= ("Alice" :: Text)
                , "phone" .= fromPhone p
                ]
    post (brig . path "/i/users" . contentJson . Http.body newUser) !!!
        const 201 === statusCode
    sendLoginCode brig p LoginCodeSMS False !!! const 200 === statusCode
    code <- getPhoneLoginCode brig p
    case code of
        Nothing -> liftIO $ assertFailure "missing login code"
        Just c  -> login brig (SmsLogin p c Nothing) PersistentCookie
            !!! const 200 === statusCode

testHandleLogin :: Brig -> Http ()
testHandleLogin brig = do
    usr <- userId <$> randomUser brig
    hdl <- randomHandle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    put (brig . path "/self/handle" . contentJson . zUser usr . zConn "c" . Http.body update) !!!
        const 200 === statusCode
    let l = PasswordLogin (LoginByHandle (Handle hdl)) defPassword Nothing
    login brig l PersistentCookie !!! const 200 === statusCode

testLoginUntrustedDomain :: Brig -> Http ()
testLoginUntrustedDomain brig = do
    -- NOTE: local part cannot be longer than 64 octets
    rd <- liftIO (randomIO :: IO Integer)
    let email = (Text.pack $ show rd) <> "@zinfra.io"
    Just (Email loc dom) <- userEmail <$> createUser "Homer" email brig
    -- login without "+" suffix
    let email' = Email (Text.takeWhile (/= '+') loc) dom
    login brig (defEmailLogin email') PersistentCookie !!!
        const 200 === statusCode

testSendLoginCode :: Brig -> Http ()
testSendLoginCode brig = do
    p <- randomPhone
    let newUser = RequestBodyLBS . encode $ object
                [ "name"     .= ("Alice" :: Text)
                , "phone"    .= fromPhone p
                , "password" .= ("secret" :: Text)
                ]
    post (brig . path "/i/users" . contentJson . Http.body newUser) !!!
        const 201 === statusCode

    -- Unless forcing it, SMS/voice code login is not permitted if
    -- the user has a password.
    sendLoginCode brig p LoginCodeSMS False !!! do
        const 403 === statusCode
        const (Just "password-exists") === fmap Error.label . decodeBody
    rsp1 <- sendLoginCode brig p LoginCodeSMS True
        <!! const 200 === statusCode

    let _timeout = fromLoginCodeTimeout <$> decodeBody rsp1
    liftIO $ assertEqual "timeout" (Just (Code.Timeout 600)) _timeout

    -- Retry with a voice call
    rsp2 <- sendLoginCode brig p LoginCodeVoice True
        <!! const 200 === statusCode

    -- Timeout is reset
    let _timeout = fromLoginCodeTimeout <$> decodeBody rsp2
    liftIO $ assertEqual "timeout" (Just (Code.Timeout 600)) _timeout

testLoginFailure :: Brig -> Http ()
testLoginFailure brig = do
    Just email <- userEmail <$> randomUser brig

    -- login with wrong password
    let badpw = PlainTextPassword "wrongpassword"
    login brig (PasswordLogin (LoginByEmail email) badpw Nothing) PersistentCookie
        !!! const 403 === statusCode

    -- login with wrong / non-existent email
    let badmail = Email "wrong" "wire.com"
    login brig (PasswordLogin (LoginByEmail badmail) defPassword Nothing) PersistentCookie
        !!! const 403 === statusCode

    -- Create user with only phone number
    p <- randomPhone
    let newUser = RequestBodyLBS . encode $ object
                [ "name"  .= ("Alice" :: Text)
                , "phone" .= fromPhone p
                ]
    res <- post (brig . path "/i/users" . contentJson . Http.body newUser) <!!
        const 201 === statusCode
    let uid = fromMaybe (error "Failed to parse user") (userId <$> decodeBody res)
    eml <- randomEmail

    -- Add email
    let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
    put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . Http.body emailUpdate) !!!
        (const 202 === statusCode)

    -- Activate
    act <- getActivationCode brig (Left eml)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> do
            activate brig kc !!! const 200 === statusCode
            -- Attempt to log in without having set a password
            login brig (defEmailLogin eml) PersistentCookie !!! do
                const 403                          === statusCode
                const (Just "invalid-credentials") === fmap Error.label . decodeBody

testThrottleLogins :: Maybe Opts.Opts -> Brig -> Http ()
testThrottleLogins conf b = do
    l <- liftIO $ optOrEnv (Opts.setUserCookieLimit . Opts.optSettings) conf read "USER_COOKIE_LIMIT"
    u <- randomUser b
    let Just e = userEmail u
    replicateM_ l (login b (defEmailLogin e) SessionCookie)
    x <- login b (defEmailLogin e) SessionCookie <!!
        const 429 === statusCode
    let Just n = fromByteString =<< getHeader "Retry-After" x
    liftIO $ do
        assertBool "throttle delay" (n > 0)
        threadDelay (1000000 * (n + 1))
    void $ login b (defEmailLogin e) SessionCookie

-------------------------------------------------------------------------------
-- Token Refresh

testInvalidCookie :: ZAuth.Env -> Brig -> Http ()
testInvalidCookie z b = do
    -- Syntactically invalid
    post (b . path "/access" . cookieRaw "zuid" "xxx") !!! do
        const 403 === statusCode
        const (Just "Invalid user token") =~= responseBody

    -- Expired
    u <- userId <$> randomUser b
    let f = set ZAuth.userTokenTimeout (ZAuth.UserTokenTimeout 0)
    t <- toByteString' <$> runZAuth z (ZAuth.localSettings f (ZAuth.newUserToken u))
    liftIO $ threadDelay 1000000
    post (b . path "/access" . cookieRaw "zuid" t) !!! do
        const 403 === statusCode
        const (Just "expired") =~= responseBody

testInvalidToken :: Brig -> Http ()
testInvalidToken r = do
    -- Syntactically invalid
    post (r . path "/access" . queryItem "access_token" "xxx")
        !!! errResponse
    post (r . path "/access" . header "Authorization" "Bearer xxx")
        !!! errResponse
  where
    errResponse = do
        const 403 === statusCode
        const (Just "Invalid access token") =~= responseBody

testMissingCookie :: ZAuth.Env -> Brig -> Http ()
testMissingCookie z r = do
    -- Missing cookie, i.e. token refresh mandates a cookie.
    post (r . path "/access")
        !!! errResponse
    t <- toByteString' <$> runZAuth z randomAccessToken
    post (r . path "/access" . header "Authorization" ("Bearer " <> t))
        !!! errResponse
    post (r . path "/access" . queryItem "access_token" t)
        !!! errResponse
  where
    errResponse = do
        const 403 === statusCode
        const (Just "Missing cookie") =~= responseBody
        const (Just "invalid-credentials") =~= responseBody

testUnknownCookie :: ZAuth.Env -> Brig -> Http ()
testUnknownCookie z r = do
    -- Valid cookie but unknown to the server.
    t <- toByteString' <$> runZAuth z randomUserToken
    post (r . path "/access" . cookieRaw "zuid" t) !!! do
        const 403 === statusCode
        const (Just "invalid-credentials") =~= responseBody

testNewPersistentCookie :: Maybe Opts.Opts -> Brig -> Http ()
testNewPersistentCookie config b = do
    u <- randomUser b
    renewAge <- liftIO $ optOrEnv (Opts.setUserCookieRenewAge . Opts.optSettings) config read "USER_COOKIE_RENEW_AGE"
    let minAge = fromIntegral $  renewAge * 1000000 + 1
        Just email = userEmail u
    _rs <- login b (emailLogin email defPassword (Just "nexus1")) PersistentCookie
        <!! const 200 === statusCode
    let c = decodeCookie _rs

    -- Wait for the cookie to be eligible for renewal
    liftIO $ threadDelay minAge

    -- Refresh tokens
    _rs <- post (b . path "/access" . cookie c) <!! do
        const 200 === statusCode
        const Nothing =/= getHeader "Set-Cookie"
        const (Just "access_token") =~= responseBody
    let c' = decodeCookie _rs
    liftIO $ assertBool "expiry" (cookie_expiry_time c' > cookie_expiry_time c)

    -- Refresh with the old cookie should still work for the
    -- duration of another BRIG_COOKIE_RENEW_AGE seconds,
    -- but the response should keep advertising the new cookie.
    _rs <- post (b . path "/access" . cookie c) <!! do
        const 200 === statusCode
        const Nothing =/= getHeader "Set-Cookie"
        const (Just "access_token") =~= responseBody
    liftIO $ assertEqual "cookie" c' (decodeCookie _rs)

    -- Refresh with the new cookie should succeed
    -- (without advertising yet another new cookie).
    post (b . path "/access" . cookie c') !!! do
        const 200 === statusCode
        const Nothing === getHeader "Set-Cookie"
        const (Just "access_token") =~= responseBody

    -- Check cookies
    _cs <- listCookies b (userId u)
    let (old, new) = partition (isJust . cookieSucc) _cs
    liftIO $ do
        2 @=? length _cs
        mapMaybe cookieSucc old @=? map cookieId new
        [PersistentCookie, PersistentCookie] @=? map cookieType _cs

    -- Wait for the old cookie to disappear
    liftIO $ threadDelay minAge
    _cs <- listCookies b (userId u)
    liftIO $ do
        [PersistentCookie] @=? map cookieType _cs
        [Nothing] @=? map cookieSucc _cs

testNewSessionCookie :: Maybe Opts.Opts -> Brig -> Http ()
testNewSessionCookie config b = do
    u <- randomUser b
    renewAge <- liftIO $ optOrEnv (Opts.setUserCookieRenewAge . Opts.optSettings) config read "USER_COOKIE_RENEW_AGE"
    let minAge = fromIntegral $  renewAge * 1000000 + 1
        Just email = userEmail u
    _rs <- login b (emailLogin email defPassword (Just "nexus1")) SessionCookie
        <!! const 200 === statusCode
    let c = decodeCookie _rs
    liftIO $ threadDelay minAge
    -- Session cookies are never renewed
    post (b . path "/access" . cookie c) !!! do
        const 200     === statusCode
        const Nothing === getHeader "Set-Cookie"

-------------------------------------------------------------------------------
-- Cookie Management

testListCookies :: Brig -> Http ()
testListCookies b = do
    u <- randomUser b
    let Just e = userEmail u
    void $ login b (emailLogin e defPassword (Just "nexus1")) SessionCookie
    _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
    liftIO $ ["nexus1"] @=? _cookies

    void $ login b (emailLogin e defPassword (Just "nexus2")) SessionCookie
    void $ login b (emailLogin e defPassword (Just "nexus3")) SessionCookie

    _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
    liftIO $ ["nexus1", "nexus2", "nexus3"] @=? sort _cookies

testRemoveCookiesByLabel :: Brig -> Http ()
testRemoveCookiesByLabel b = do
    u <- randomUser b
    let Just e = userEmail u
    void $ login b (emailLogin e defPassword (Just "nexus1")) SessionCookie
    void $ login b (emailLogin e defPassword (Just "nexus2")) SessionCookie
    void $ login b (emailLogin e defPassword (Just "nexus3")) SessionCookie

    _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
    liftIO $ ["nexus1", "nexus2", "nexus3"] @=? sort _cookies

    let rem1 = encode $ remJson defPassword (Just ["nexus1"]) Nothing
    post (b . path "/cookies/remove"
        . content "application/json"
        . lbytes rem1
        . zUser (userId u)) !!! const 200 === statusCode

    _cookies <- mapMaybe cookieLabel <$> listCookies b (userId u)
    liftIO $ ["nexus2", "nexus3"] @=? sort _cookies

    let rem2 = encode $ remJson defPassword Nothing Nothing
    post (b .
        path "/cookies/remove"
        . content "application/json"
        . lbytes rem2
        . zUser (userId u)) !!!  const 200 === statusCode

    listCookies b (userId u) >>= liftIO . ([] @=?) . mapMaybe cookieLabel

testRemoveCookiesByLabelAndId :: Brig -> Http ()
testRemoveCookiesByLabelAndId b = do
    u <- randomUser b
    let Just e = userEmail u
    -- Create some cookies
    forM_ ["nexus1", "nexus2", "nexus3", "nexus4"] $ \l ->
        login b (emailLogin e defPassword (Just l)) SessionCookie
    [c1, c2, c3, c4] <- listCookies b (userId u)
    -- Remove 2 cookies by ID and 1 by label, leaving 1
    let rmLabel = maybeToList (cookieLabel c2)
    let rmIds = map cookieId [c1, c3]
    let rmJs  = encode $ remJson defPassword (Just rmLabel) (Just rmIds)
    post (b .
        path "/cookies/remove"
        . content "application/json"
        . lbytes rmJs
        . zUser (userId u)) !!! const 200 === statusCode
    -- Check the remaining cookie
    let lbl = cookieLabel c4
    listCookies b (userId u) >>= liftIO . ([lbl] @=?) . map cookieLabel

testTooManyCookies :: Maybe Opts.Opts -> Brig -> Http ()
testTooManyCookies config b = do
    u <- randomUser b
    l <- liftIO $ optOrEnv (Opts.setUserCookieLimit . Opts.optSettings) config read "USER_COOKIE_LIMIT"
    let Just e = userEmail u
        carry = 4
        pwl = emailLogin e defPassword (Just "nexus1")
    -- Session logins
    tcs <- replicateM (l + carry) $ (decodeCookie <$> login b pwl SessionCookie) <* wait
    cs <- listCookies b (userId u)
    liftIO $ map cookieId cs @=? map getCookieId (drop carry tcs)
    -- Persistent logins
    tcs' <- replicateM (l + carry) $ (decodeCookie <$> login b pwl PersistentCookie) <* wait
    cs'  <- listCookies b (userId u)
    liftIO $ map cookieId cs' @=? map getCookieId (drop carry tcs ++ drop carry tcs')

testLogout :: Brig -> Http ()
testLogout b = do
    Just email <- userEmail <$> randomUser b
    _rs <- login b (defEmailLogin email) SessionCookie
    let (t, c) = (decodeToken _rs, decodeCookie _rs)

    post (b . path "/access" . cookie c) !!!
        const 200 === statusCode

    post (b . path "/access/logout" . cookie c . queryItem "access_token" (toByteString' t)) !!!
        const 200 === statusCode

    post (b . path "/access" . cookie c) !!!
        const 403 === statusCode

testReauthorisation :: Brig -> Http ()
testReauthorisation b = do
    u <- userId <$> randomUser b

    let js = Http.body . RequestBodyLBS . encode $ object ["foo" .= ("bar" :: Text) ]
    get (b . paths [ "/i/users", toByteString' u, "reauthenticate"] . contentJson . js) !!! do
        const 400 === statusCode

    get (b . paths [ "/i/users", toByteString' u, "reauthenticate"] . contentJson . payload (PlainTextPassword "123456")) !!! do
        const 403 === statusCode
        const (Just "invalid-credentials") === fmap Error.label . decodeBody

    get (b . paths [ "/i/users", toByteString' u, "reauthenticate"] . contentJson . payload defPassword) !!! do
        const 200 === statusCode

    setStatus u Suspended

    get (b . paths [ "/i/users", toByteString' u, "reauthenticate"] . contentJson . payload defPassword) !!! do
        const 403 === statusCode
        const (Just "suspended") === fmap Error.label . decodeBody
  where
    payload = Http.body . RequestBodyLBS . encode . ReAuthUser

    setStatus u s =
        let js = RequestBodyLBS . encode $ AccountStatusUpdate s
        in put ( b . paths ["i", "users", toByteString' u, "status"]
               . contentJson . Http.body js
               ) !!! const 200 === statusCode

-----------------------------------------------------------------------------
-- Helpers

decodeCookie :: Response a -> Http.Cookie
decodeCookie = fromMaybe (error "missing zuid cookie") . getCookie "zuid"

decodeToken :: Response (Maybe Lazy.ByteString) -> ZAuth.AccessToken
decodeToken r = fromMaybe (error "invalid access_token") $ do
    x <- responseBody r
    t <- x ^? key "access_token" . _String
    fromByteString (encodeUtf8 t)

getCookieId :: Http.Cookie -> CookieId
getCookieId c = maybe (error "no cookie value")
                      (CookieId . ZAuth.userTokenRand)
                      (fromByteString (cookie_value c))

listCookies :: Brig -> UserId -> Http [Auth.Cookie ()]
listCookies b u = do
    rs <- get (b . path "/cookies" . header "Z-User" (toByteString' u)) <!!
        const 200 === statusCode
    let Just cs = cookieList <$> decodeBody rs
    return cs

remJson :: PlainTextPassword -> Maybe [CookieLabel] -> Maybe [CookieId] -> Value
remJson p l ids = object
    [ "password" .= p
    , "labels"   .= l
    , "ids"      .= ids
    ]

wait :: MonadIO m => m ()
wait = liftIO $ threadDelay 1000000

