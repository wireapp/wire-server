{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module API.User.Account (tests) where

import API.Search.Util (assertSearchable)
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.AWS.Types
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth hiding (user)
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently_)
import Control.Lens ((^?), (^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Char8 (pack, intercalate)
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.List1 (singleton)
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Monoid ((<>))
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Data.Text (Text)
import Data.Vector (Vector)
import Gundeck.Types.Notification
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.HUnit
import System.Random (randomIO)
import Web.Cookie (parseSetCookie)
import Util as Util
import Util.AWS as Util

import qualified API.Search.Util             as Search
import qualified Brig.AWS                    as AWS
import qualified Brig.Options                as Opt
import qualified CargoHold.Types.V3          as CHV3
import qualified Data.List1                  as List1
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import qualified Data.Vector                 as Vec
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon           as WS

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> AWS.Env -> TestTree
tests _cl at _conf p b c ch g aws = testGroup "account"
    [ test' aws p "post /register - 201 (with preverified)"  $ testCreateUserWithPreverified b aws
    , test' aws p "post /register - 201"                     $ testCreateUser b g
    , test' aws p "post /register - 201 + no email"          $ testCreateUserNoEmailNoPassword b
    , test' aws p "post /register - 201 anonymous"           $ testCreateUserAnon b g
    , test' aws p "post /register - 201 anonymous expiry"    $ testCreateUserAnonExpiry b
    , test' aws p "post /register - 201 pending"             $ testCreateUserPending b
    , test' aws p "post /register - 201 existing activation" $ testCreateAccountPendingActivationKey b
    , test' aws p "post /register - 409 conflict"            $ testCreateUserConflict b
    , test' aws p "post /register - 400"                     $ testCreateUserInvalidPhone b
    , test' aws p "post /register - 403 blacklist"           $ testCreateUserBlacklist b aws
    , test' aws p "post /register - 400 external-SSO"        $ testCreateUserExternalSSO b
    , test' aws p "post /activate - 200/204 + expiry"        $ testActivateWithExpiry b at
    , test' aws p "get /users/:id - 404"                     $ testNonExistingUser b
    , test' aws p "get /users/:id - 200"                     $ testExistingUser b
    , test' aws p "get /users?:id=.... - 200"                $ testMultipleUsers b
    , test' aws p "put /self - 200"                          $ testUserUpdate b c aws
    , test' aws p "put /self/email - 202"                    $ testEmailUpdate b aws
    , test' aws p "put /self/phone - 202"                    $ testPhoneUpdate b
    , test' aws p "head /self/password - 200/404"            $ testPasswordSet b
    , test' aws p "put /self/password - 200"                 $ testPasswordChange b
    , test' aws p "put /self/locale - 200"                   $ testUserLocaleUpdate b aws
    , test' aws p "post /activate/send - 200"                $ testSendActivationCode b
    , test' aws p "put /i/users/:id/status (suspend)"        $ testSuspendUser b
    , test' aws p "get /i/users?:(email|phone) - 200"        $ testGetByIdentity b
    , test' aws p "delete/phone-email"                       $ testEmailPhoneDelete b c
    , test' aws p "delete/by-password"                       $ testDeleteUserByPassword b c
    , test' aws p "delete/by-code"                           $ testDeleteUserByCode b
    , test' aws p "delete/anonymous"                         $ testDeleteAnonUser b
    , test' aws p "delete /i/users/:id - 202"                $ testDeleteInternal b c aws
    , test' aws p "delete with profile pic"                  $ testDeleteWithProfilePic b ch
    ]

testCreateUserWithPreverified :: Brig -> AWS.Env -> Http ()
testCreateUserWithPreverified brig aws = do
    -- Register (pre verified) user with phone
    p <- randomPhone
    let phoneReq = RequestBodyLBS . encode $ object [ "phone" .= fromPhone p ]
    post (brig . path "/activate/send" . contentJson . body phoneReq) !!!
        (const 200 === statusCode)
    getActivationCode brig (Right p) >>= \case
        Nothing     -> liftIO $ assertFailure "missing activation key/code"
        Just (_, c) -> do
            let reg = RequestBodyLBS . encode $ object
                    [ "name"       .= Name "Alice"
                    , "phone"      .= fromPhone p
                    , "phone_code" .= c
                    ]
            rs <- post (brig . path "/register" . contentJson . body reg) <!!
                const 201 === statusCode
            let Just uid = userId <$> decodeBody rs
            get (brig . path "/self" . zUser uid) !!! do
                const 200 === statusCode
                const (Just p) === (userPhone <=< decodeBody)

            liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled uid)

    -- Register (pre verified) user with email
    e <- randomEmail
    let emailReq = RequestBodyLBS . encode $ object [ "email" .= fromEmail e ]
    post (brig . path "/activate/send" . contentJson . body emailReq) !!!
        (const 200 === statusCode)
    getActivationCode brig (Left e) >>= \case
        Nothing     -> liftIO $ assertFailure "missing activation key/code"
        Just (_, c) -> do
            let reg = RequestBodyLBS . encode $ object
                    [ "name"       .= Name "Alice"
                    , "email"      .= fromEmail e
                    , "email_code" .= c
                    ]
            rs <- post (brig . path "/register" . contentJson . body reg) <!!
                const 201 === statusCode
            let Just uid = userId <$> decodeBody rs
            get (brig . path "/self" . zUser uid) !!! do
                const 200 === statusCode
                const (Just e) === (userEmail <=< decodeBody)

            liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled uid)

    liftIO $ Util.assertEmptyUserJournalQueue aws

testCreateUser :: Brig -> Galley -> Http ()
testCreateUser brig galley = do
    uid <- userId <$> randomUser brig
    get (galley . path "conversations" . zAuthAccess uid "conn") !!! do
        const 200 === statusCode

        -- check number of conversations:
        const (Just 1) === \r -> do
            b <- responseBody r
            c <- b ^? key "conversations"
            Vec.length <$> (maybeFromJSON c :: Maybe (Vector Value))

        -- check conversation type:
        const (Just (1 :: Integer)) === \r -> do
            b <- responseBody r
            b ^? key "conversations" . nth 0 . key "type" >>= maybeFromJSON

testCreateUserAnon :: Brig -> Galley -> Http ()
testCreateUserAnon brig galley = do
    let p = RequestBodyLBS . encode $ object
            [ "name" .= ("Mr. Pink" :: Text) ]
    rs <- post (brig . path "/register" . contentJson . body p) <!!
        const 201 === statusCode

    -- Every registered user gets a cookie.
    let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
    liftIO $ assertBool "Missing zuid cookie" (isJust zuid)

    -- Every registered user gets a self conversation.
    let Just uid = userId <$> decodeBody rs
    get (galley . path "conversations" . zAuthAccess uid "conn") !!! do
        const 200 === statusCode

        -- check number of conversations:
        const (Just 1) === \r -> do
            b <- responseBody r
            c <- b ^? key "conversations"
            Vec.length <$> (maybeFromJSON c :: Maybe (Vector Value))

        -- check conversation type:
        const (Just (1 :: Integer)) === \r -> do
            b <- responseBody r
            b ^? key "conversations" . nth 0 . key "type" >>= maybeFromJSON

    -- should not appear in search
    suid <- userId <$> randomUser brig
    Search.refreshIndex brig
    Search.assertCan'tFind brig suid uid "Mr. Pink"

testCreateUserPending :: Brig -> Http ()
testCreateUserPending brig = do
    e <- randomEmail
    let p = RequestBodyLBS . encode $ object
            [ "name"     .= ("Mr. Pink" :: Text)
            , "email"    .= fromEmail e
            , "password" .= defPassword
            ]
    rs <- post (brig . path "/register" . contentJson . body p) <!!
        const 201 === statusCode
    -- Even though activation is pending, the user gets an access cookie,
    -- i.e. every user starts out as a "Wireless" user.
    let zuid = parseSetCookie <$> getHeader "Set-Cookie" rs
    liftIO $ assertBool "Missing zuid cookie" (isJust zuid)

    -- Cannot login via email (pending activation)
    login brig (defEmailLogin e) PersistentCookie !!! do
        const 403 === statusCode
        const (Just "pending-activation") === fmap Error.label . decodeBody

    -- The user has no verified / activated identity yet
    let Just uid = userId <$> decodeBody rs
    get (brig . path "/self" . zUser uid) !!! do
        const 200  === statusCode
        const (Just True) === \rs' -> do
            self <- decodeBody rs'
            return $! isNothing (userIdentity (selfUser self))

    -- should not appear in search
    suid <- userId <$> randomUser brig
    Search.refreshIndex brig
    Search.assertCan'tFind brig suid uid "Mr. Pink"

testCreateUserNoEmailNoPassword :: Brig -> Http ()
testCreateUserNoEmailNoPassword brig = do
    p <- randomPhone
    let newUser = RequestBodyLBS . encode $ object
                [ "name"  .= ("Alice" :: Text)
                , "phone" .= fromPhone p
                ]
    rs <- post (brig . path "/i/users" . contentJson . body newUser) <!!
            const 201 === statusCode

    let Just uid = userId <$> decodeBody rs

    e <- randomEmail
    let setEmail = RequestBodyLBS . encode $ EmailUpdate e
    put (brig . path "/self/email" . contentJson . zUser uid . zConn "conn" . body setEmail) !!!
        const 202 === statusCode

testCreateUserConflict :: Brig -> Http ()
testCreateUserConflict brig = do
    u <- createUser "conflict" "test@simulator.amazonses.com" brig
    let p = RequestBodyLBS . encode $ object
            [ "name"     .= ("conflict1" :: Text)
            , "email"    .= (fromEmail <$> userEmail u) -- dup. email
            , "password" .= defPassword
            ]
    post (brig . path "/register" . contentJson . body p) !!! do
        const 409 === statusCode
        const (Just "key-exists") === fmap Error.label . decodeBody

    -- Untrusted domain and thus "<anything>@zinfra.io" considered equal
    -- to "<anything>+<uuid>@zinfra.io"
    -- NOTE: local part cannot be longer than 64 octets
    rd <- liftIO (randomIO :: IO Integer)
    let email = (T.pack $ show rd) <> "@zinfra.io"
    u2 <- createUser "conflict" email brig
    let Just (Email loc dom) = userEmail u2
    let p2 = RequestBodyLBS . encode $ object
            [ "name"     .= ("conflict2" :: Text)
            , "email"    .= (T.takeWhile (/= '+') loc <> "@" <> dom) -- dup. email
            , "password" .= defPassword
            ]
    post (brig . path "/register" . contentJson . body p2) !!! do
        const 409 === statusCode
        const (Just "key-exists") === fmap Error.label . decodeBody

testCreateUserInvalidPhone :: Brig -> Http ()
testCreateUserInvalidPhone brig = do
    email <- mkEmail "test@wearezeta.com"
    let p = RequestBodyLBS . encode $ object
            [ "name"     .= ("foo" :: Text)
            , "email"    .= fromEmail email
            , "password" .= defPassword
            , "phone"    .= ("123456" :: Text) -- invalid phone nr
            ]
    post (brig . path "/register" . contentJson . body p) !!!
        const 400 === statusCode

testCreateUserBlacklist :: Brig -> AWS.Env -> Http ()
testCreateUserBlacklist brig aws =
    mapM_ ensureBlacklist ["bounce", "complaint"]
  where
    ensureBlacklist typ = do
        e <- mkSimulatorEmail typ
        flip finally (removeBlacklist brig e) $ do
            post (brig . path "/register" . contentJson . body (p e)) !!! const 201 === statusCode
            -- If we are using a local env, we need to fake this bounce
            unless (Util.isRealSESEnv aws) $
                publishMessage typ e aws
            -- Typically bounce/complaint messages arrive instantaneously
            awaitBlacklist 30 e
            post (brig . path "/register" . contentJson . body (p e)) !!! do
                const 403                        === statusCode
                const (Just "blacklisted-email") === fmap Error.label . decodeBody

    p email = RequestBodyLBS . encode $ object [ "name"     .= ("Alice" :: Text)
                                               , "email"    .= email
                                               , "password" .= defPassword
                                               ]

    publishMessage :: Text -> Email -> AWS.Env -> Http ()
    publishMessage typ em env = do
        let bdy = encode $ case typ of
                        "bounce"    -> MailBounce BouncePermanent [em]
                        "complaint" -> MailComplaint [em]
                        x           -> error ("Unsupported message type: " ++ show x)
        let queue = env^.AWS.sesQueue
        void $ AWS.execute env (AWS.enqueueStandard queue bdy)

    awaitBlacklist :: Int -> Email -> Http ()
    awaitBlacklist n e = do
        r <- Bilge.head (brig . path "i/users/blacklist" . queryItem "email" (toByteString' e))
        when (statusCode r == 404 && n > 0) $ do
            liftIO $ threadDelay 1000000
            awaitBlacklist (n-1) e

testCreateUserExternalSSO :: Brig -> Http ()
testCreateUserExternalSSO brig = do
    teamid <- Id <$> liftIO UUID.nextRandom
    let ssoid = UserSSOId "nil" "nil"
        p withsso withteam = RequestBodyLBS . encode . object $
              [ "name"  .= ("foo" :: Text) ] <>
              [ "sso_id" .= Just ssoid | withsso ] <>
              [ "team_id" .= Just teamid | withteam ]
    post (brig . path "/register" . contentJson . body (p False True)) !!!
      const 400 === statusCode
    post (brig . path "/register" . contentJson . body (p True False)) !!!
      const 400 === statusCode
    post (brig . path "/register" . contentJson . body (p True True)) !!!
      const 400 === statusCode

testActivateWithExpiry :: Brig -> Opt.Timeout -> Http ()
testActivateWithExpiry brig timeout = do
    Just u <- decodeBody <$> registerUser "dilbert" "success@simulator.amazonses.com" brig
    let email = fromMaybe (error "missing email") (userEmail u)
    act <- getActivationCode brig (Left email)
    case act of
        Nothing -> liftIO $ assertFailure "activation key/code not found"
        Just kc -> do
            activate brig kc !!! do
                const 200 === statusCode
                const (Just (userIdentity u, True)) === actualBody
            -- Note: This value must be larger than the option passed as `activation-timeout`
            awaitExpiry (round timeout + 5) kc
            activate brig kc !!! const 404 === statusCode
  where
    actualBody rs = do
        a <- decodeBody rs
        Just (Just (activatedIdentity a), activatedFirst a)

    awaitExpiry :: Int -> ActivationPair -> Http ()
    awaitExpiry n kc = do
        liftIO $ threadDelay 1000000
        r <- activate brig kc
        when (statusCode r == 204 && n > 0) $
            awaitExpiry (n-1) kc

testNonExistingUser :: Brig -> Http ()
testNonExistingUser brig = do
    uid <- liftIO $ Id <$> UUID.nextRandom
    get (brig . paths ["users", pack $ show uid] . zUser uid) !!!
        const 404 === statusCode

testExistingUser :: Brig -> Http ()
testExistingUser brig = do
    uid <- userId <$> randomUser brig
    get (brig . paths ["users", pack $ show uid] . zUser uid) !!! do
        const 200 === statusCode
        const (Just uid) === (\r -> do
            b <- responseBody r
            b ^? key "id" >>= maybeFromJSON)

testMultipleUsers :: Brig -> Http ()
testMultipleUsers brig = do
    u1 <- randomUser brig
    u2 <- randomUser brig
    u3 <- createAnonUser "a" brig
    let uids = intercalate "," $ map (toByteString' . userId) [u1, u2, u3]
        -- n.b. email addresses and phone numbers are never returned
        -- on this endpoint, only from the self profile (/self).
        expected = Set.fromList
                   [ (Just $ userName u1, Nothing :: Maybe Email)
                   , (Just $ userName u2, Nothing)
                   , (Just $ userName u3, Nothing)
                   ]
    get (brig . zUser (userId u1) . path "users" . queryItem "ids" uids) !!! do
        const 200 === statusCode
        const (Just expected) === result
  where
    result r =  Set.fromList
             .  map (field "name" &&& field "email")
            <$> decodeBody r

    field :: FromJSON a => Text -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON


testCreateUserAnonExpiry :: Brig -> Http ()
testCreateUserAnonExpiry b = do
    u1 <- randomUser b
    alice <- randomUser b
    bob <- createAnonUserExpiry (Just 2) "bob" b
    liftIO $ assertBool "expiry not set on regular creation" (not $ isJust $ userExpire alice)
    ensureExpiry (userExpire bob) "bob/register"
    resAlice <- getProfile (userId u1) (userId alice)
    resBob <- getProfile (userId u1) (userId bob)
    selfBob <- get (b . zUser (userId bob) . path "self") <!! const 200 === statusCode
    liftIO $ assertBool "Bob must not be in a deleted state initially" (fromMaybe True (not <$> deleted selfBob))
    liftIO $ assertBool "Regular user should not have any expiry" (null $ expire resAlice)
    ensureExpiry (expire resBob) "bob/public"
    ensureExpiry (expire selfBob) "bob/self"
    awaitExpiry 5 (userId u1) (userId bob)
    resBob' <- getProfile (userId u1) (userId bob)
    liftIO $ assertBool "Bob must be in deleted state" (fromMaybe False $ deleted resBob')
  where
    getProfile :: UserId -> UserId -> Http ResponseLBS
    getProfile zusr uid = get (b . zUser zusr . paths ["users", toByteString' uid]) <!! const 200 === statusCode

    awaitExpiry :: Int -> UserId -> UserId -> Http ()
    awaitExpiry n zusr uid = do
        -- after expiration, a profile lookup should trigger garbage collection of ephemeral users
        r <- getProfile zusr uid
        when (statusCode r == 200 && deleted r == Nothing && n > 0) $ do
            liftIO $ threadDelay 1000000
            awaitExpiry (n-1) zusr uid

    ensureExpiry :: Maybe UTCTime -> String -> Http ()
    ensureExpiry expiry s = case expiry of
                       Nothing -> liftIO $ assertFailure ("user must have an expiry" <> s)
                       Just a -> do
                          now <- liftIO getCurrentTime
                          let diff = diffUTCTime a now
                              minExp = 1 :: Integer -- 1 second
                              maxExp = 60 * 60 * 24 * 10 :: Integer -- 10 days
                          liftIO $ assertBool "expiry must in be the future" (diff >= fromIntegral minExp)
                          liftIO $ assertBool "expiry must be less than 10 days" (diff < fromIntegral maxExp)

    expire :: ResponseLBS -> Maybe UTCTime
    expire r = join $ field "expires_at" <$> decodeBody r

    deleted :: ResponseLBS -> Maybe Bool
    deleted r = join $ field "deleted" <$> decodeBody r

    field :: FromJSON a => Text -> Value -> Maybe a
    field f u = u ^? key f >>= maybeFromJSON


testUserUpdate :: Brig -> Cannon -> AWS.Env -> Http ()
testUserUpdate brig cannon aws = do
    alice <- userId <$> randomUser brig
    liftIO $ Util.assertUserJournalQueue "user create alice" aws (userActivateJournaled alice)
    bob <- userId <$> randomUser brig
    liftIO $ Util.assertUserJournalQueue "user create bob" aws (userActivateJournaled bob)
    connectUsers brig alice (singleton bob)
    let newColId   = Just 5
        newAssets  = Just [ImageAsset "abc" (Just AssetComplete)]
        newName    = Just $ Name "dogbert"
        newPic     = Nothing -- Legacy
        userUpdate = UserUpdate newName newPic newAssets newColId
        update     = RequestBodyLBS . encode $ userUpdate

    -- Update profile & receive notification
    WS.bracketRN cannon [alice, bob] $ \[aliceWS, bobWS] -> do
        put (brig . path "/self" . contentJson . zUser alice . zConn "c" . body update) !!!
            const 200 === statusCode

        liftIO $ mapConcurrently_ (\ws -> assertUpdateNotification ws alice userUpdate) [aliceWS, bobWS]
        -- Should not generate any user update journaled messages
        liftIO $ Util.assertEmptyUserJournalQueue aws

    -- get the updated profile
    get (brig . path "/self" . zUser alice) !!! do
        const 200 === statusCode
        const (newName, newColId, newAssets) === (\u ->
            ( fmap userName u
            , fmap userAccentId u
            , fmap userAssets u
            )) . decodeBody

    -- get only the new name
    get (brig . path "/self/name" . zUser alice) !!! do
        const 200 === statusCode
        const (String . fromName <$> newName) === (\r -> do
            b <- responseBody r
            b ^? key "name")

    -- should appear in search by 'newName'
    suid <- userId <$> randomUser brig
    Search.refreshIndex brig
    assertSearchable "alice should be searchable" brig alice True
    Search.assertCanFind brig suid alice "dogbert"

testEmailUpdate :: Brig -> AWS.Env -> Http ()
testEmailUpdate brig aws = do
    uid <- userId <$> randomUser brig
    liftIO $ Util.assertUserJournalQueue "user create random" aws (userActivateJournaled uid)
    eml <- randomEmail
    -- update email
    initiateEmailUpdate brig eml uid !!! const 202 === statusCode
    -- activate
    activateEmail brig eml
    checkEmail brig uid eml
    liftIO $ Util.assertUserJournalQueue "user update" aws (userUpdateJournaled uid)

testPhoneUpdate :: Brig -> Http ()
testPhoneUpdate brig = do
    uid <- userId <$> randomUser brig
    phn <- randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
    put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        (const 202 === statusCode)
    -- activate
    act <- getActivationCode brig (Right phn)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! do
            const 200 === statusCode
            const (Just False) === fmap activatedFirst . decodeBody
    -- check new phone
    get (brig . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just phn) === (userPhone <=< decodeBody)

testCreateAccountPendingActivationKey :: Brig -> Http ()
testCreateAccountPendingActivationKey brig = do
    uid <- userId <$> randomUser brig
    phn <- randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
    put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        (const 202 === statusCode)
    -- create a new user with that phone/code
    act <- getActivationCode brig (Right phn)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc@(_, c) -> do
            let p = RequestBodyLBS . encode $ object
                                        [ "name"       .= ("foo" :: Text)
                                        , "phone"      .= phn
                                        , "phone_code" .= c
                                        ]
            post (brig . path "/register" . contentJson . body p) !!!
                const 201 === statusCode
            -- try to activate already active phone
            activate brig kc !!! const 409 === statusCode

testUserLocaleUpdate :: Brig -> AWS.Env -> Http ()
testUserLocaleUpdate brig aws = do
    uid <- userId <$> randomUser brig
    liftIO $ Util.assertUserJournalQueue "user create random" aws (userActivateJournaled uid)
    -- update locale info with locale supported in templates
    put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale "en-US") !!!
        const 200 === statusCode
    -- update locale info with locale NOT supported in templates
    put (brig . path "/self/locale" . contentJson . zUser uid . zConn "c" . locale "pt-PT") !!!
        const 200 === statusCode
    -- get the updated locale
    get (brig . path "/self" . zUser uid) !!! do
        const 200                   === statusCode
        const (parseLocale "pt-PT") === (Just . userLocale . selfUser <=< decodeBody)
    liftIO $ Util.assertUserJournalQueue "user update" aws (userUpdateJournaled uid)
  where
    locale l = body . RequestBodyLBS . encode $ object ["locale" .= (l :: Text)]

testSuspendUser :: Brig -> Http ()
testSuspendUser brig = do
    u <- randomUser brig
    let uid        = userId u
        Just email = userEmail u
    setStatus uid Suspended
    -- login fails
    login brig (defEmailLogin email) PersistentCookie !!! do
        const 403 === statusCode
        const (Just "suspended") === fmap Error.label . decodeBody
    -- check status
    chkStatus brig uid Suspended
    -- should not appear in search
    suid <- userId <$> randomUser brig
    Search.refreshIndex brig
    Search.assertCan'tFind brig suid uid (fromName (userName u))

    -- re-activate
    setStatus uid Active
    chkStatus brig uid Active
    -- should appear in search again
    Search.refreshIndex brig
    Search.assertCanFind brig suid uid (fromName (userName u))
  where
    setStatus u s =
        let js = RequestBodyLBS . encode $ AccountStatusUpdate s
        in put ( brig . paths ["i", "users", toByteString' u, "status"]
               . contentJson . body js
               ) !!! const 200 === statusCode

testGetByIdentity :: Brig -> Http ()
testGetByIdentity brig = do
    p <- randomPhone
    e <- mkEmail "test@wearezeta.com"
    let emailBs = T.encodeUtf8 $ fromEmail e
        phoneBs = T.encodeUtf8 $ fromPhone p
        newUser = RequestBodyLBS . encode $ object
                [ "name"  .= ("Alice" :: Text)
                , "phone" .= fromPhone p
                , "email" .= fromEmail e
                ]

    rs <- post (brig . path "/i/users" . contentJson . body newUser) <!!
            const 201 === statusCode

    let Just uid = userId <$> decodeBody rs

    get (brig . zUser uid . path "i/users" . queryItem "email" emailBs) !!! do
        const 200                === statusCode
        const (Just [uid]) === getUids
    get (brig . zUser uid . path "i/users" . queryItem "phone" phoneBs) !!! do
        const 200                === statusCode
        const (Just [uid]) === getUids
  where
    getUids r = return . fmap (userId . accountUser) =<< decodeBody r

testPasswordSet :: Brig -> Http ()
testPasswordSet brig = do
    p <- randomPhone
    let newUser = RequestBodyLBS . encode $ object
                [ "name"  .= ("Alice" :: Text)
                , "phone" .= fromPhone p
                ]
    rs <- post (brig . path "/i/users" . contentJson . body newUser) <!!
            const 201 === statusCode

    let Just uid = userId <$> decodeBody rs
    -- No password set yet
    Bilge.head (brig . path "/self/password" . zUser uid) !!!
        const 404 === statusCode
    -- Since there is no password set, we can just set one
    put (brig . path "/self/password" . contentJson . zUser uid . body pwSet) !!!
        const 200 === statusCode
    -- Now we should have a password
    Bilge.head (brig . path "/self/password" . zUser uid) !!!
        const 200 === statusCode
  where
    pwSet = RequestBodyLBS . encode $ object
        [ "new_password" .= ("a_very_long_password" :: Text)
        ]

testPasswordChange :: Brig -> Http ()
testPasswordChange brig = do
    (uid, Just email) <- (userId &&& userEmail) <$> randomUser brig
    put (brig . path "/self/password" . contentJson . zUser uid . body pwChange) !!!
        (const 200 === statusCode)

    -- login with new password
    login brig (PasswordLogin (LoginByEmail email) newPass Nothing) PersistentCookie
        !!! const 200 === statusCode

    -- Setting a password for an anonymous / unverified user should fail
    uid2 <- userId <$> createAnonUser "foo2" brig
    put (brig . path "/self/password" . contentJson . zUser uid2 . body pwSet) !!!
        (const 403 === statusCode)
  where
    newPass = PlainTextPassword "topsecret"

    pwChange = RequestBodyLBS . encode $ object
        [ "old_password" .= defPassword
        , "new_password" .= newPass
        ]

    pwSet = RequestBodyLBS . encode $ object
        [ "new_password" .= newPass
        ]

testSendActivationCode :: Brig -> Http ()
testSendActivationCode brig = do
    -- Code for phone pre-verification
    requestActivationCode brig . Right =<< randomPhone
    -- Code for email pre-verification
    requestActivationCode brig . Left =<< randomEmail
    -- Standard email registration flow
    r <- registerUser "Alice" "success@simulator.amazonses.com" brig <!! const 201 === statusCode
    let Just email = userEmail =<< decodeBody r
    -- Re-request existing activation code
    requestActivationCode brig (Left email)

testEmailPhoneDelete :: Brig -> Cannon -> Http ()
testEmailPhoneDelete brig cannon = do
    user <- randomUser brig
    let      uid   = userId user
    let Just email = userEmail user

    -- Cannot remove the only identity
    delete (brig . path "/self/email" . zUser uid . zConn "c") !!!
        const 403 === statusCode

    -- Add a phone number
    phone <- randomPhone
    let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phone
    put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        const 202 === statusCode
    act <- getActivationCode brig (Right phone)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! const 200 === statusCode

    -- Remove the email
    WS.bracketR cannon uid $ \ws -> do
        delete (brig . path "/self/email" . zUser uid . zConn "c") !!!
            (const 200 === statusCode)
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype  = j ^? key "type" . _String
            let euser  = j ^? key "user" . key "id" . _String
            let eemail = j ^? key "user" . key "email" . _String
            etype  @?= Just "user.identity-remove"
            euser  @?= Just (UUID.toText (toUUID uid))
            eemail @?= Just (fromEmail email)

    get (brig . path "/self" . zUser uid) !!! do
        const 200     === statusCode
        const Nothing === (userEmail <=< decodeBody)

    -- Cannot remove the only remaining identity
    delete (brig . path "/self/phone" . zUser uid . zConn "c") !!!
        const 403 === statusCode

    -- Add back a new email address
    eml <- randomEmail
    let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
    put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . body emailUpdate) !!!
        const 202 === statusCode
    act' <- getActivationCode brig (Left eml)
    case act' of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! const 200 === statusCode

    -- Remove the phone number
    WS.bracketR cannon uid $ \ws -> do
        delete (brig . path "/self/phone" . zUser uid . zConn "c") !!!
            const 200 === statusCode
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype  = j ^? key "type" . _String
            let euser  = j ^? key "user" . key "id" . _String
            let ephone = j ^? key "user" . key "phone" . _String
            etype  @?= Just "user.identity-remove"
            euser  @?= Just (UUID.toText (toUUID uid))
            ephone @?= Just (fromPhone phone)
    get (brig . path "/self" . zUser uid) !!! do
        const 200     === statusCode
        const Nothing === (userPhone <=< decodeBody)

testDeleteUserByPassword :: Brig -> Cannon -> Http ()
testDeleteUserByPassword brig cannon = do
    u <- randomUser brig
    let uid1  = userId u
    let email = fromMaybe (error "Missing email") (userEmail u)

    -- Initiate a change of email address, to verify that activation
    -- does not work after the account has been deleted.
    eml <- randomEmail
    let emailUpdate = RequestBodyLBS . encode $ EmailUpdate eml
    put (brig . path "/self/email" . contentJson . zUser uid1 . zConn "c" . body emailUpdate) !!!
        const 202 === statusCode

    -- Establish some connections
    uid2 <- userId <$> randomUser brig
    uid3 <- userId <$> randomUser brig
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    postConnection brig uid1 uid3 !!! const 201 === statusCode
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    putConnection brig uid3 uid1 Accepted !!! const 200 === statusCode
    postConnection brig uid2 uid3 !!! const 201 === statusCode
    con32 <- putConnection brig uid3 uid2 Accepted <!! const 200 === statusCode
    con23 <- getConnection brig uid2 uid3 <!! const 200 === statusCode

    -- Register a client
    addClient brig u (defNewClient PermanentClient [somePrekeys !! 0] (someLastPrekeys !! 0))
        !!! const 201 === statusCode

    -- Initial login
    login brig (defEmailLogin email) PersistentCookie
        !!! const 200 === statusCode
    n1 <- countCookies brig uid1 defCookieLabel
    liftIO $ Just 1 @=? n1

    setHandleAndDeleteUser brig cannon u [] $
        \uid -> deleteUser uid (Just defPassword) brig !!! const 200 === statusCode

    -- Activating the new email address now should not work
    act <- getActivationCode brig (Left eml)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! do
            const 404 === statusCode
            const (Just "invalid-code") === fmap Error.label . decodeBody

    -- Connections involving uid1 are gone (uid2 <-> uid3 remains)
    let u1Conns = UserConnectionList [] False
    let u2Conns = UserConnectionList (maybeToList (decodeBody con23)) False
    let u3Conns = UserConnectionList (maybeToList (decodeBody con32)) False
    listConnections brig uid1 !!! do
        const 200 === statusCode
        const (Just u1Conns) === decodeBody
    listConnections brig uid2 !!! do
        const 200 === statusCode
        const (Just u2Conns) === decodeBody
    listConnections brig uid3 !!! do
        const 200 === statusCode
        const (Just u3Conns) === decodeBody

testDeleteUserByCode :: Brig -> Http ()
testDeleteUserByCode brig = do
    u <- randomUser brig
    deleteUser (userId u) Nothing brig
        !!! const 202 === statusCode

    -- (Syntactically) invalid key / code
    let _key  = "" :: Text
    let _code = "123" :: Text
    send _key _code !!! do
        const 400 === statusCode
        const (Just "bad-request") === fmap Error.label . decodeBody

    -- (Semantically) invalid key / code
    let _key  = T.replicate 20 "x"
    let _code = "idontknow" :: Text
    send _key _code !!! do
        const 403 === statusCode
        const (Just "invalid-code") === fmap Error.label . decodeBody
  where
    send k c = post (brig . path "/delete" . contentJson . body (payload k c))
    payload k c = RequestBodyLBS . encode $ object
        [ "key" .= k, "code" .= c ]

testDeleteAnonUser :: Brig -> Http ()
testDeleteAnonUser brig = do
    uid <- userId <$> createAnonUser "anon" brig
    deleteUser uid Nothing brig
        !!! const 200 === statusCode

testDeleteInternal :: Brig -> Cannon -> AWS.Env -> Http ()
testDeleteInternal brig cannon aws = do
    u <- randomUser brig
    liftIO $ Util.assertUserJournalQueue "user activate" aws (userActivateJournaled $ userId u)
    setHandleAndDeleteUser brig cannon u [] $
        \uid -> delete (brig . paths ["/i/users", toByteString' uid]) !!! const 202 === statusCode
    -- Check that user deletion is also triggered
    liftIO $ Util.assertUserJournalQueue "user deletion" aws (userDeleteJournaled $ userId u)

testDeleteWithProfilePic :: Brig -> CargoHold -> Http ()
testDeleteWithProfilePic brig cargohold = do
    uid <- userId <$> createAnonUser "anon" brig
    ast <- uploadAsset cargohold uid "this is my profile pic"
    -- Ensure that the asset is there
    downloadAsset cargohold uid (toByteString' (ast^.CHV3.assetKey)) !!! const 200 === statusCode

    let newAssets  = Just [ImageAsset (T.decodeLatin1 $ toByteString' (ast^.CHV3.assetKey)) (Just AssetComplete)]
        userUpdate = UserUpdate Nothing Nothing newAssets Nothing
        update     = RequestBodyLBS . encode $ userUpdate

    -- Update profile with the uploaded asset
    put (brig . path "/self" . contentJson . zUser uid . zConn "c" . body update) !!!
            const 200 === statusCode

    deleteUser uid Nothing brig !!! const 200 === statusCode

    -- Check that the asset gets deleted
    downloadAsset cargohold uid (toByteString' (ast^.CHV3.assetKey)) !!! const 404 === statusCode

setHandleAndDeleteUser :: Brig -> Cannon -> User -> [UserId] -> (UserId -> HttpT IO ()) -> Http ()
setHandleAndDeleteUser brig cannon u others execDelete = do
    let uid   = userId u
        email = fromMaybe (error "Must have an email set") (userEmail u)

    -- First set a unique handle (to verify freeing of the handle)
    hdl <- randomHandle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode

    -- Delete the user
    WS.bracketRN cannon (uid : others) $ \wss -> do
        execDelete uid
        void . liftIO $ WS.assertMatchN (5 # Second) wss $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype = j ^? key "type" . _String
            let euser = j ^? key "id" . _String
            etype @?= Just "user.delete"
            euser @?= Just (UUID.toText (toUUID uid))

    -- Cookies are gone
    n2 <- countCookies brig uid defCookieLabel
    liftIO $ Just 0 @=? n2

    -- Clients are gone
    get (brig . path "clients" . zUser (userId u)) !!! do
        const 200 === statusCode
        const (Just [] :: Maybe [Client]) === decodeBody

    -- Can no longer log in
    login brig (defEmailLogin email) PersistentCookie !!! do
        const 403 === statusCode
        const (Just "invalid-credentials") === fmap Error.label . decodeBody

    -- Deleted flag appears in self profile; email, handle and picture are gone
    get (brig . path "/self" . zUser uid) !!! assertDeletedProfileSelf

    Search.refreshIndex brig
    -- Does not appear in search; public profile shows the user as deleted
    forM_ others $ \usr -> do
        get (brig . paths ["users", toByteString' uid] . zUser usr) !!! assertDeletedProfilePublic
        Search.assertCan'tFind brig usr uid (fromName (userName u))
        Search.assertCan'tFind brig usr uid hdl

    -- Email address is available again
    let p = RequestBodyLBS . encode $ object
            [ "name"     .= ("Someone Else" :: Text)
            , "email"    .= fromEmail email
            , "password" .= defPassword
            ]
    post (brig . path "/i/users" . contentJson . body p) !!!
        const 201 === statusCode

    -- Handle is available again
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 404 === statusCode
  where
    assertDeletedProfileSelf = do
        const 200 === statusCode
        const (Just noPict, Just Nothing, Just True, Just [], Nothing) === (\u' ->
            ( fmap userPict u'
            , fmap userEmail u'
            , fmap userDeleted u'
            , fmap userAssets u'
            , userHandle =<< u'
            )) . decodeBody

    assertDeletedProfilePublic = do
        const 200 === statusCode
        const (Just noPict, Just True, Nothing) === (\u' ->
            ( fmap profilePict u'
            , fmap profileDeleted u'
            , profileHandle =<< u'
            )) . decodeBody
