{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module API (tests, ConnectionLimit (..)) where

import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth hiding (user)
import Brig.Data.PasswordReset
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently, mapConcurrently_)
import Control.Lens ((^?), (^?!), preview)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, intercalate)
import Data.ByteString.Conversion
import Data.Function (on)
import Data.Id hiding (client)
import Data.Int (Int64)
import Data.List (sort, sortBy, nub)
import Data.List1 (List1, singleton)
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Monoid ((<>))
import Data.Range (unsafeRange)
import Data.Text (Text)
import Data.Vector (Vector)
import Galley.Types
import Gundeck.Types.Notification
import Gundeck.Types.Push.V2
import OpenSSL.EVP.Digest (getDigestByName, digestBS)
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.HUnit
import Safe
import System.Random (randomIO)
import Web.Cookie (parseSetCookie, setCookieName)
import Util
import Util.Options.Common

import qualified API.Search.Util             as Search
import qualified Brig.Options                as Opt
import qualified Data.ByteString.Char8       as C
import qualified Data.List1                  as List1
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.Ascii             as Ascii
import qualified Data.Text.Encoding          as T
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import qualified Data.Vector                 as Vec
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon           as WS

newtype ConnectionLimit = ConnectionLimit Int64

tests :: Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> IO TestTree
tests conf p b c g = do
    l <- optOrEnv (ConnectionLimit . Opt.setUserMaxConnections . Opt.optSettings) conf (ConnectionLimit . read) "USER_CONNECTION_LIMIT"
    return $ testGroup "user"
        [ testGroup "account"
            [ test p "post /register - 201"                     $ testCreateUser b g
            , test p "post /register - 201 + no email"          $ testCreateUserNoEmailNoPassword b
            , test p "post /register - 201 anonymous"           $ testCreateUserAnon b g
            , test p "post /register - 201 pending"             $ testCreateUserPending b
            , test p "post /register - 201 existing activation" $ testCreateAccountPendingActivationKey b
            , test p "post /register - 409 conflict"            $ testCreateUserConflict b
            , test p "post /register - 400"                     $ testCreateUserInvalidPhone b
            , test p "post /register - 403"                     $ testCreateUserBlacklist b
            , test p "post /activate - 200/204 + expiry"        $ testActivateWithExpiry b
            , test p "get /users/:id - 404"                     $ testNonExistingUser b
            , test p "get /users/:id - 200"                     $ testExistingUser b
            , test p "get /users?:id=.... - 200"                $ testMultipleUsers b
            , test p "put /self - 200"                          $ testUserUpdate b c
            , test p "put /self/email - 202"                    $ testEmailUpdate b
            , test p "put /self/phone - 202"                    $ testPhoneUpdate b
            , test p "put /self/password - 200"                 $ testPasswordChange b
            , test p "put /self/locale - 200"                   $ testUserLocaleUpdate b
            , test p "post /activate/send - 200"                $ testSendActivationCode b
            , test p "put /i/users/:id/status (suspend)"        $ testSuspendUser b
            , test p "get /i/users?:(email|phone) - 200"        $ testGetByIdentity b
            , test p "delete/phone-email"                       $ testEmailPhoneDelete b c
            , test p "delete/by-password"                       $ testDeleteUserByPassword b c
            , test p "delete/by-code"                           $ testDeleteUserByCode b
            , test p "delete/anonymous"                         $ testDeleteAnonUser b
            , test p "delete /i/users/:id - 202"                $ testDeleteInternal b c
            ]
        , testGroup "connection"
            [ test p "post /connections"                    $ testCreateManualConnections b
            , test p "post /connections mutual"             $ testCreateMutualConnections b g
            , test p "post /connections (bad user)"         $ testCreateConnectionInvalidUser b
            , test p "put /connections/:id accept"          $ testAcceptConnection b
            , test p "put /connections/:id ignore"          $ testIgnoreConnection b
            , test p "put /connections/:id cancel"          $ testCancelConnection b
            , test p "put /connections/:id cancel"          $ testCancelConnection2 b g
            , test p "put /connections/:id block"           $ testBlockConnection b
            , test p "put /connections/:id block-resend"    $ testBlockAndResendConnection b g
            , test p "put /connections/:id unblock pending" $ testUnblockPendingConnection b
            , test p "put /connections/:id accept blocked"  $ testAcceptWhileBlocked b
            , test p "put /connections/:id bad update"      $ testBadUpdateConnection b
            , test p "put /connections/:id noop"            $ testUpdateConnectionNoop b
            , test p "get /connections - 200 (paging)"      $ testConnectionPaging b
            , test p "post /connections - 400 (max conns)"  $ testConnectionLimit b l
            , test p "post /i/users/auto-connect"                   $ testAutoConnectionOK b g
            , test p "post /i/users/auto-connect - existing conn"   $ testAutoConnectionNoChanges b
            , test p "post /i/users/auto-connect - 400 (bad range)" $ testAutoConnectionBadRequest b
            ]
        , testGroup "invitation"
            [ test p "post /invitations - 201 accepted"     $ testInvitationEmail b g c
            , test p "post /invitations - 400 inactive"     $ testInvitationNotActivated b
            , test p "post /register - 400 invitee exists"  $ testInvitationCodeExists b
            , test p "post /register - 400 bad code"        $ testInvitationInvalidCode b
            , test p "post /register - 400 no wireless"     $ testInvitationCodeNoIdentity b
            , test p "get /invitations - 200 (paging)"      $ testInvitationPaging b
            , test p "get /invitations/info - 200"          $ testInvitationInfo b
            , test p "get /invitations/info - 400"          $ testInvitationInfoBadCode b
            ]
        , testGroup "client"
            [ test p "get /users/:user/prekeys - 200"         $ testGetUserPrekeys b
            , test p "get /users/:user/prekeys/:client - 200" $ testGetClientPrekey b
            , test p "post /clients - 201"                    $ testAddGetClient b c
            , test p "post /clients - 403"                    $ testClientReauthentication b
            , test p "get /clients - 200"                     $ testListClients b
            , test p "get /clients/:client/prekeys - 200"     $ testListPrekeyIds b
            , test p "post /clients - 400"                    $ testTooManyClients b
            , test p "delete /clients/:client - 200"          $ testRemoveClient b c
            , test p "put /clients/:client - 200"             $ testUpdateClient b
            , test p "post /clients - 200 multiple temporary" $ testAddMultipleTemporary b g
            , test p "client/prekeys/race"                    $ testPreKeyRace b
            ]
        , testGroup "property"
            [ test p "put/get /properties/:key - 200" $ testSetGetProperty b
            , test p "delete /properties/:key - 200"  $ testDeleteProperty b
            , test p "get /properties - 200"          $ testListPropertyKeys b
            , test p "delete /properties - 200"       $ testClearProperties b
            , test p "put /properties/:key - 403"     $ testPropertyLimits b
            ]
        , testGroup "password-reset"
            [ test p "post /password-reset[/complete] - 201[/200]"  $ testPasswordReset b
            , test p "post /password-reset & put /self/email - 400" $ testPasswordResetAfterEmailUpdate b
            ]
        , testGroup "handles"
            [ test p "handles/update" $ testHandleUpdate b c
            , test p "handles/race"   $ testHandleRace b
            , test p "handles/query"  $ testHandleQuery b
            ]
        , testGroup "onboarding"
            [ test p "post /onboarding/v3 - 200" $ testOnboarding b
            ]
        ]

-------------------------------------------------------------------------------
-- User Account/Profile Tests

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

testCreateUserBlacklist :: Brig -> Http ()
testCreateUserBlacklist brig =
    mapM_ ensureBlacklist ["bounce", "complaint"]
  where
    ensureBlacklist typ = do
        e <- mkSimulatorEmail typ
        flip finally (removeBlacklist brig e) $ do
            post (brig . path "/register" . contentJson . body (p e)) !!! const 201 === statusCode
            -- Typically bounce/complaint messages arrive instantaneously
            awaitBlacklist 30 e
            post (brig . path "/register" . contentJson . body (p e)) !!! do
                const 403                        === statusCode
                const (Just "blacklisted-email") === fmap Error.label . decodeBody

    p email = RequestBodyLBS . encode $ object [ "name"     .= ("Alice" :: Text)
                                               , "email"    .= email
                                               , "password" .= defPassword
                                               ]

    awaitBlacklist :: Int -> Email -> Http ()
    awaitBlacklist n e = do
        r <- Bilge.head (brig . path "i/users/blacklist" . queryItem "email" (toByteString' e))
        when (statusCode r == 404 && n > 0) $ do
            liftIO $ threadDelay 1000000
            awaitBlacklist (n-1) e

testActivateWithExpiry :: Brig -> Http ()
testActivateWithExpiry brig = do
    Just u <- decodeBody <$> registerUser "dilbert" "success@simulator.amazonses.com" brig
    let email = fromMaybe (error "missing email") (userEmail u)
    act <- getActivationCode brig (Left email)
    case act of
        Nothing -> liftIO $ assertFailure "activation key/code not found"
        Just kc -> do
            activate brig kc !!! do
                const 200 === statusCode
                const (Just (userIdentity u, True)) === actualBody
            -- Note: This value must be larger than BRIG_ACTIVATION_TIMEOUT
            awaitExpiry 10 kc
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

testUserUpdate :: Brig -> Cannon -> Http ()
testUserUpdate brig cannon = do
    alice <- userId <$> randomUser brig
    bob <- userId <$> randomUser brig
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
    Search.assertCanFind brig suid alice "dogbert"

testEmailUpdate :: Brig -> Http ()
testEmailUpdate brig = do
    uid <- userId <$> randomUser brig
    eml <- randomEmail
    -- update email
    initiateEmailUpdate brig eml uid !!! const 202 === statusCode
    -- activate
    activateEmail brig eml
    checkEmail brig uid eml

testPasswordResetAfterEmailUpdate :: Brig -> Http ()
testPasswordResetAfterEmailUpdate brig = do
    u <- randomUser brig
    let uid = userId u
    let Just email = userEmail u
    eml <- randomEmail
    initiateEmailUpdate brig eml uid !!! const 202 === statusCode
    initiatePasswordReset brig email !!! const 201 === statusCode
    passwordResetData <- preparePasswordReset brig email uid (PlainTextPassword "newsecret")
    -- activate new email
    activateEmail brig eml
    checkEmail brig uid eml
    -- attempting to complete password reset should fail
    completePasswordReset brig passwordResetData !!! const 400 === statusCode

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

testUserLocaleUpdate :: Brig -> Http ()
testUserLocaleUpdate brig = do
    uid <- userId <$> randomUser brig
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
    p <- randomPhone
    send $ RequestBodyLBS . encode $ object ["phone" .= fromPhone p]
    r <- registerUser "Alice" "success@simulator.amazonses.com" brig <!! const 201 === statusCode
    let Just email = userEmail =<< decodeBody r
    send $ RequestBodyLBS . encode $ object ["email" .= fromEmail email]
  where
    send p = post (brig . path "/activate/send" . contentJson . body p) !!!
        const 200 === statusCode

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

testDeleteInternal :: Brig -> Cannon -> Http ()
testDeleteInternal brig cannon = do
    u <- randomUser brig
    setHandleAndDeleteUser brig cannon u [] $
        \uid -> delete (brig . paths ["/i/users", toByteString' uid]) !!! const 202 === statusCode

-------------------------------------------------------------------------------
-- Connection Tests

testCreateConnectionInvalidUser :: Brig -> Http ()
testCreateConnectionInvalidUser brig = do
    uid1 <- userId <$> randomUser brig
    -- user does not exist
    uid2 <- Id <$> liftIO UUID.nextRandom
    postConnection brig uid1 uid2 !!! do
        const 400 === statusCode
        const (Just "invalid-user") === fmap Error.label . decodeBody
    -- cannot create a connection with yourself
    postConnection brig uid1 uid1 !!! do
        const 400 === statusCode
        const (Just "invalid-user") === fmap Error.label . decodeBody

testCreateManualConnections :: Brig -> Http ()
testCreateManualConnections brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
    -- Test that no connections to anonymous users can be created,
    -- as well as that anonymous users cannot create connections.
    uid3 <- userId <$> createAnonUser "foo3" brig
    postConnection brig uid1 uid3 !!! const 400 === statusCode
    postConnection brig uid3 uid1 !!! const 403 === statusCode

testCreateMutualConnections :: Brig -> Galley -> Http ()
testCreateMutualConnections brig galley = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]
    rsp  <- postConnection brig uid2 uid1 <!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
    case decodeBody rsp >>= ucConvId of
        Nothing  -> liftIO $ assertFailure "incomplete connection"
        Just cnv -> do
            getConversation galley uid1 cnv !!! do
                const 200                === statusCode
                const (Just One2OneConv) === fmap cnvType . decodeBody
            getConversation galley uid2 cnv !!! do
                const 200                === statusCode
                const (Just One2OneConv) === fmap cnvType . decodeBody

testAcceptConnection :: Brig -> Http ()
testAcceptConnection brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- B accepts
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

    -- Mutual connection request with a user C
    uid3 <- userId <$> randomUser brig
    postConnection brig uid1 uid3 !!! const 201 === statusCode
    postConnection brig uid3 uid1 !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid3 Accepted]
    assertConnections brig uid3 [ConnectionStatus uid3 uid1 Accepted]

testIgnoreConnection :: Brig -> Http ()
testIgnoreConnection brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- B ignores A
    putConnection brig uid2 uid1 Ignored !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Ignored]

    -- B accepts after all
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

testCancelConnection :: Brig -> Http ()
testCancelConnection brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- A cancels the request
    putConnection brig uid1 uid2 Cancelled !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]

    -- A changes his mind again
    postConnection brig uid1 uid2 !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Sent]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Pending]

testCancelConnection2 :: Brig -> Galley -> Http ()
testCancelConnection2 brig galley = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- A cancels the request
    rsp <- putConnection brig uid1 uid2 Cancelled <!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Cancelled]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Cancelled]

    let Just cnv = ucConvId =<< decodeBody rsp

    -- A cannot see the conversation (due to cancelling)
    getConversation galley uid1 cnv !!! do
        const 404 === statusCode

    -- B cannot see the conversation
    getConversation galley uid2 cnv !!! const 404 === statusCode

    -- B initiates a connection request himself
    postConnection brig uid2 uid1 !!! const 200 === statusCode
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Sent]
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Pending]

    -- B is now a current member of the connect conversation
    getConversation galley uid2 cnv !!! do
        const 200 === statusCode
        const (Just ConnectConv) === \rs -> do
            conv <- decodeBody rs
            Just (cnvType conv)

    -- A is a past member, cannot see the conversation
    getConversation galley uid1 cnv !!! do
        const 404 === statusCode

    -- A finally accepts
    putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
    getConversation galley uid1 cnv !!! do
        const 200 === statusCode
    getConversation galley uid2 cnv !!! do
        const 200 === statusCode

testBlockConnection :: Brig -> Http ()
testBlockConnection brig = do
    u1 <- randomUser brig
    u2 <- randomUser brig

    let uid1 = userId u1
    let uid2 = userId u2

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- Even connected users cannot see each other's email
    -- (or phone number for that matter).
    assertEmailVisibility brig u2 u1 False
    assertEmailVisibility brig u1 u2 False

    -- B blocks A
    putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode

    -- A does not notice that he got blocked
    postConnection brig uid1 uid2 !!! do
        const 200 === statusCode
        const (Just Sent) === fmap ucStatus . decodeBody
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]

    -- B accepts after all
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

    assertEmailVisibility brig u1 u2 False

    -- B blocks A again
    putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]

    assertEmailVisibility brig u1 u2 False

    -- B accepts again
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

    assertEmailVisibility brig u1 u2 False

testBlockAndResendConnection :: Brig -> Galley -> Http ()
testBlockAndResendConnection brig galley = do
    u1 <- randomUser brig
    u2 <- randomUser brig

    let uid1 = userId u1
    let uid2 = userId u2

    -- Initiate a new connection (A -> B)
    postConnection brig uid1 uid2 !!! const 201 === statusCode

    -- B blocks A
    putConnection brig uid2 uid1 Blocked !!! const 200 === statusCode

    -- A blocks B
    putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode

    -- Cannot resend while blocked, need to unblock first
    postConnection brig uid1 uid2 !!! const 403 === statusCode

    -- Unblock
    putConnection brig uid1 uid2 Accepted !!! const 200 === statusCode

    -- Try to resend the connection request
    -- B is not actually notified, since he blocked.
    rsp <- postConnection brig uid1 uid2 <!! const 200 === statusCode
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Blocked]

    -- B never accepted and thus does not see the conversation
    let Just cnv = ucConvId =<< decodeBody rsp
    getConversation galley uid2 cnv !!! const 404 === statusCode

    -- A can see the conversation and is a current member
    getConversation galley uid1 cnv !!! do
        const 200 === statusCode

testUnblockPendingConnection :: Brig -> Http ()
testUnblockPendingConnection brig = do
    u1 <- userId <$> randomUser brig
    u2 <- userId <$> randomUser brig
    postConnection brig u1 u2 !!! const 201 === statusCode
    putConnection brig u1 u2 Blocked !!! const 200 === statusCode
    assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
    assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
    putConnection brig u1 u2 Accepted !!! const 200 === statusCode
    assertConnections brig u1 [ConnectionStatus u1 u2 Sent]
    assertConnections brig u2 [ConnectionStatus u2 u1 Pending]

testAcceptWhileBlocked :: Brig -> Http ()
testAcceptWhileBlocked brig = do
    u1 <- userId <$> randomUser brig
    u2 <- userId <$> randomUser brig
    postConnection brig u1 u2 !!! const 201 === statusCode
    putConnection brig u1 u2 Blocked !!! const 200 === statusCode
    assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
    assertConnections brig u2 [ConnectionStatus u2 u1 Pending]
    putConnection brig u2 u1 Accepted !!! const 200 === statusCode
    assertConnections brig u1 [ConnectionStatus u1 u2 Blocked]
    assertConnections brig u2 [ConnectionStatus u2 u1 Accepted]

testUpdateConnectionNoop :: Brig -> Http ()
testUpdateConnectionNoop brig = do
    u1 <- randomUser brig
    u2 <- randomUser brig
    let uid1 = userId u1
    let uid2 = userId u2
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode
    putConnection brig uid2 uid1 Accepted !!! const 204 === statusCode

testBadUpdateConnection :: Brig -> Http ()
testBadUpdateConnection brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig

    postConnection brig uid1 uid2 !!! const 201 === statusCode

    assertBadUpdate uid1 uid2 Pending
    assertBadUpdate uid1 uid2 Ignored
    assertBadUpdate uid1 uid2 Accepted

    assertBadUpdate uid2 uid1 Sent
  where
    assertBadUpdate u1 u2 s = putConnection brig u1 u2 s !!! do
        const 403 === statusCode
        const (Just "bad-conn-update") === fmap Error.label . decodeBody

testConnectionPaging :: Brig -> Http ()
testConnectionPaging b = do
    u <- userId <$> randomUser b
    replicateM_ total $ do
        u2 <- userId <$> randomUser b
        postConnection b u u2 !!! const 201 === statusCode
    foldM_ (next u 2) (0, Nothing) [2,2,1,0]
    foldM_ (next u total) (0, Nothing) [total,0]
  where
    total = 5

    next u step (count, start) n = do
        let count' = count + step
        let range = queryRange (toByteString' <$> start) (Just step)
        r <- get (b . path "/connections" . zUser u . range) <!!
            const 200 === statusCode
        let (conns, more) = (fmap clConnections &&& fmap clHasMore) $ decodeBody r
        liftIO $ assertEqual "page size" (Just n) (length <$> conns)
        liftIO $ assertEqual "has more" (Just (count' < total)) more
        return . (count',) $ (conns >>= fmap ucTo . listToMaybe . reverse)

testConnectionLimit :: Brig -> ConnectionLimit -> Http ()
testConnectionLimit brig (ConnectionLimit l) = do
    uid1 <- userId <$> randomUser brig
    (uid2:_) <- replicateM (fromIntegral l) (newConn uid1)
    uidX <- userId <$> randomUser brig
    postConnection brig uid1 uidX !!! assertLimited

    -- blocked connections do not count towards the limit
    putConnection brig uid1 uid2 Blocked !!! const 200 === statusCode
    postConnection brig uid1 uidX !!! const 201 === statusCode

    -- the next send/accept hits the limit again
    uidY <- userId <$> randomUser brig
    postConnection brig uid1 uidY !!! assertLimited

    -- (re-)sending an already accepted connection does not affect the limit
    postConnection brig uid1 uidX !!! const 200 === statusCode
  where
    newConn from = do
        to <- userId <$> randomUser brig
        postConnection brig from to !!! const 201 === statusCode
        return to

    assertLimited = do
        const 403 === statusCode
        const (Just "connection-limit") === fmap Error.label . decodeBody

testAutoConnectionOK :: Brig -> Galley -> Http ()
testAutoConnectionOK brig galley = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig
    bdy  <- postAutoConnection brig uid1 [uid2] <!! do
        const 200      === statusCode
        const (Just 2) === \r -> do
            b <- responseBody r
            Vec.length <$> (decode b :: Maybe (Vector UserConnection))
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
    case decodeBody bdy >>= headMay >>= ucConvId of
        Nothing  -> liftIO $ assertFailure "incomplete connection"
        Just cnv -> do
            getConversation galley uid1 cnv !!! do
                const 200                === statusCode
                const (Just One2OneConv) === fmap cnvType . decodeBody
            getConversation galley uid2 cnv !!! do
                const 200                === statusCode
                const (Just One2OneConv) === fmap cnvType . decodeBody

testAutoConnectionNoChanges :: Brig -> Http ()
testAutoConnectionNoChanges brig = do
    uid1 <- userId <$> randomUser brig
    uid2 <- userId <$> randomUser brig
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    -- This is effectively a no-op
    postAutoConnection brig uid1 [uid2] !!! do
        const 200 === statusCode
        const (Just 0) === \r -> do
            b <- responseBody r
            Vec.length <$> (decode b :: Maybe (Vector UserConnection))

testAutoConnectionBadRequest :: Brig -> Http ()
testAutoConnectionBadRequest brig = do
    uid1 <- userId <$> randomUser brig
    -- no users
    postAutoConnection brig uid1 []   !!! const 400 === statusCode
    -- too many users
    uids <- replicateM 26 (liftIO $ Id <$> UUID.nextRandom)
    postAutoConnection brig uid1 uids !!! const 400 === statusCode
    -- unactivated / unverified self user
    uid2 <- userId <$> createAnonUser "foo2" brig
    postAutoConnection brig uid2 (take 1 uids) !!! do
        const 403 === statusCode
        const (Just "no-identity") === fmap Error.label . decodeBody
    -- unactivated / unverified target users simply get filtered out
    postAutoConnection brig uid1 [uid2] !!! do
        const 200 === statusCode
        const (Just 0) === \r -> do
            b <- responseBody r
            Vec.length <$> (decode b :: Maybe (Vector UserConnection))

-------------------------------------------------------------------------------
-- Invitation Tests

testInvitationEmail :: Brig -> Galley -> Cannon -> Http ()
testInvitationEmail brig galley cannon = do
    email <- randomEmail
    alice <- userId <$> randomUser brig
    bdy <- decodeBody <$> postInvitation brig alice (invite email)
    let Just invRef = inInvitation <$> bdy
    invCode <- getInvitationCode brig alice invRef

    WS.bracketR cannon alice $ \wsA -> do
        rsp2 <- post ( brig . path "/register"
                     . contentJson
                     . body (accept email invCode)
                     ) <!! const 201 === statusCode

        let Just (bob, Just email2) = (userId &&& userEmail) <$> decodeBody rsp2
        let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
        liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)

        -- Verify that they got connected
        assertConnections brig bob   [ConnectionStatus bob alice Accepted]
        assertConnections brig alice [ConnectionStatus alice bob Accepted]

        -- Verify that the invited user is active
        login brig (defEmailLogin email2) PersistentCookie
            !!! const 200 === statusCode

        -- Verify that the conversation IDs are set
        a2b <- decodeBody <$> get (brig . paths ["connections", toByteString' bob] . zUser alice)
        b2a <- decodeBody <$> get (brig . paths ["connections", toByteString' alice] . zUser bob)
        liftIO $ assertEqual "conv ids not equal" (ucConvId =<< a2b) (ucConvId =<< b2a)

        -- Verify that the 1-1 conversation exists
        cnv <- maybe (error "no conv ID") (fmap decodeBody . getConversation galley alice) (ucConvId =<< a2b)
        liftIO $ assertEqual "wrong conv type" (Just One2OneConv) (cnvType <$> cnv)

        -- WS receive timeout
        let t = 5 # Second
        -- Ensure alice gets the right events
        void . liftIO $ WS.assertMatch t wsA $ \n -> do
            -- conversation.member-join
            let e = List1.head (WS.unpackPayload n :: List1 Event)
            evtType e @?= MemberJoin
            evtFrom e @?= alice
        void . liftIO $ WS.assertMatch t wsA $ \n -> do
            -- user.connection
            let (typ, nm, uc) = decodeUCEvent . Object $ List1.head (ntfPayload n)
            typ @?= (Just "user.connection")
            nm  @?= (Just $ Name "Bob")
            uc  @?= a2b
  where
    decodeUCEvent :: Value -> (Maybe Text, Maybe Name, Maybe UserConnection)
    decodeUCEvent e = do
        let ty = e ^? key "type" . _String
        let nm = e ^? key "user" . key "name" . _String
        let uc = maybeFromJSON =<< (e ^? key "connection")
        (ty, Name <$> nm, uc)

    invite email = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing

    accept email code = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail email
        , "password"        .= defPassword
        , "invitation_code" .= code
        ]

testInvitationNotActivated :: Brig -> Http ()
testInvitationNotActivated brig = do
    u <- createAnonUser "Mr. Black" brig
    email <- randomEmail
    postInvitation brig (userId u) (invite email) !!! do
        const 403 === statusCode
        const (Just "no-identity") === fmap Error.label . decodeBody
  where
    invite email = InvitationRequest email (Name "Mr. Pink") (Message ".") Nothing

testInvitationCodeExists :: Brig -> Http ()
testInvitationCodeExists brig = do
    email <- randomEmail

    uid1 <- userId <$> randomUser brig
    rsp <- postInvitation brig uid1 (invite email) <!! const 201 === statusCode

    let Just invRef = inInvitation <$> decodeBody rsp
    invCode <- getInvitationCode brig uid1 invRef

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!!
        const 201 === statusCode

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!! do
        const 409                 === statusCode
        const (Just "key-exists") === fmap Error.label . decodeBody

    postUser "dilbert" "someoneelse@wearezeta.com" invCode brig !!! do
        const 400                              === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody
  where
    invite email = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing

    accept email code = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail email
        , "password"        .= defPassword
        , "invitation_code" .= code
        ]

testInvitationInvalidCode :: Brig -> Http ()
testInvitationInvalidCode brig = do
    -- Syntactically invalid
    let code1 = InvitationCode (Ascii.unsafeFromText "8z6JVcO1o4o¿9kFeb4Y3N-BmhIjH6b33")
    postUser "dilbert" "foo7@wearezeta.com" (Just code1) brig !!! do
        const 400 === statusCode
        const (Just "bad-request") === fmap Error.label . decodeBody
    -- Syntactically valid but semantically invalid
    iid <- liftIO $ randomBytes 24
    let code2 = InvitationCode (Ascii.encodeBase64Url iid)
    postUser "dilbert" "foo7@wearezeta.com" (Just code2) brig !!! do
        const 400 === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody

testInvitationCodeNoIdentity :: Brig -> Http ()
testInvitationCodeNoIdentity brig = do
    uid <- liftIO $ Id <$> UUID.nextRandom
    post (brig . path "/register" . contentJson . body (user uid)) !!! do
        const 403                       === statusCode
        const (Just "missing-identity") === fmap Error.label . decodeBody
  where
    user u = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "invitation_code" .= u
        ]

testInvitationPaging :: Brig -> Http ()
testInvitationPaging b = do
    u <- userId <$> randomUser b
    replicateM_ total $ do
        email <- randomEmail
        postInvitation b u (invite email) !!! const 201 === statusCode
    foldM_ (next u 2) (0, Nothing) [2,2,1,0]
    foldM_ (next u total) (0, Nothing) [total,0]
  where
    total = 5

    next :: UserId -> Int -> (Int, Maybe InvitationId) -> Int -> Http (Int, Maybe InvitationId)
    next u step (count, start) n = do
        let count' = count + step
        let range = queryRange (toByteString' <$> start) (Just step)
        r <- get (b . path "/invitations" . zUser u . range) <!!
            const 200 === statusCode
        let (invs, more) = (fmap ilInvitations &&& fmap ilHasMore) $ decodeBody r
        liftIO $ assertEqual "page size" (Just n) (length <$> invs)
        liftIO $ assertEqual "has more" (Just (count' < total)) more
        return . (count',) $ invs >>= fmap inInvitation . listToMaybe . reverse

    invite email = InvitationRequest email (Name "You") (Message "sth") Nothing

testInvitationInfo :: Brig -> Http ()
testInvitationInfo brig = do
    email    <- randomEmail
    uid      <- userId <$> randomUser brig

    let invite = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing
    Just inv <- decodeBody <$> postInvitation brig uid invite

    Just invCode    <- getInvitationCode brig uid (inInvitation inv)
    Just invitation <- getInvitation brig invCode

    liftIO $ assertEqual "Invitations differ" inv invitation

testInvitationInfoBadCode :: Brig -> Http ()
testInvitationInfoBadCode brig = do
    -- The code contains non-ASCII characters after url-decoding
    let icode = "8z6JVcO1o4o%C2%BF9kFeb4Y3N-BmhIjH6b33"
    get (brig . path ("/invitations/info?code=" <> icode)) !!!
        const 400 === statusCode

-------------------------------------------------------------------------------
-- Client & Prekey Tests

testAddGetClient :: Brig -> Cannon -> Http ()
testAddGetClient brig cannon = do
    u <- randomUser brig
    let rq = addClientReq brig u (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
           . header "X-Forwarded-For" "127.0.0.1" -- Fake IP to test IpAddr parsing.
    c <- WS.bracketR cannon (userId u) $ \ws -> do
        Just c <- decodeBody <$> (post rq <!! do
            const 201  === statusCode
            const True === isJust . getHeader "Location")
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype = j ^? key "type" . _String
            let eclient = j ^? key "client"
            etype @?= Just "user.client-add"
            fmap fromJSON eclient @?= Just (Success c)
        return c
    getClient brig (userId u) (clientId c) !!! do
        const 200      === statusCode
        const (Just c) === decodeBody

testClientReauthentication :: Brig -> Http ()
testClientReauthentication brig = do
    let (pk1, lk1) = (somePrekeys !! 0, someLastPrekeys !! 0)
    let (pk2, lk2) = (somePrekeys !! 1, someLastPrekeys !! 1)
    let (pk3, lk3) = (somePrekeys !! 2, someLastPrekeys !! 2)

    let payload1 = (defNewClient PermanentClient [pk1] lk1)
                 { newClientPassword = Nothing }
    let payload2 = (defNewClient PermanentClient [pk2] lk2)
                 { newClientPassword = Nothing }
    let payload3 = (defNewClient TemporaryClient [pk3] lk3)
                 { newClientPassword = Nothing }

    -- User with password
    u <- randomUser brig
    -- The first client never requires authentication
    Just c <- decodeBody <$> (addClient brig u payload1 <!! const 201 === statusCode)
    -- Adding a second client requires reauthentication, if a password is set.
    addClient brig u payload2 !!! do
        const 403 === statusCode
        const (Just "missing-auth") === (fmap Error.label . decodeBody)
    -- Removing a client requires reauthentication, if a password is set.
    deleteClient brig (userId u) (clientId c) Nothing !!! const 403 === statusCode

    -- User without a password
    u2 <- createAnonUser "Mr. X" brig
    Just c2 <- decodeBody <$> (addClient brig u2 payload1 <!! const 201 === statusCode)
    Just c3 <- decodeBody <$> (addClient brig u2 payload2 <!! const 201 === statusCode)
    deleteClient brig (userId u2) (clientId c2) Nothing !!! const 200 === statusCode
    deleteClient brig (userId u2) (clientId c3) Nothing !!! const 200 === statusCode

    -- Temporary client can always be deleted without a password
    Just c4 <- decodeBody <$> addClient brig u payload3
    deleteClient brig (userId u) (clientId c4) Nothing !!! const 200 === statusCode
    Just c5 <- decodeBody <$> addClient brig u2 payload3
    deleteClient brig (userId u2) (clientId c5) Nothing !!! const 200 === statusCode

testListClients :: Brig -> Http ()
testListClients brig = do
    u  <- randomUser brig
    let (pk1, lk1) = (somePrekeys !! 0, (someLastPrekeys !! 0))
    let (pk2, lk2) = (somePrekeys !! 1, (someLastPrekeys !! 1))
    let (pk3, lk3) = (somePrekeys !! 2, (someLastPrekeys !! 2))
    c1 <- decodeBody <$> addClient brig u (defNewClient PermanentClient [pk1] lk1)
    c2 <- decodeBody <$> addClient brig u (defNewClient PermanentClient [pk2] lk2)
    c3 <- decodeBody <$> addClient brig u (defNewClient TemporaryClient [pk3] lk3)
    let cs = sortBy (compare `on` clientId) $ catMaybes [c1, c2, c3]
    get ( brig
        . path "clients"
        . zUser (userId u)
        ) !!! do
            const 200       === statusCode
            const (Just cs) === decodeBody

testListPrekeyIds :: Brig -> Http ()
testListPrekeyIds brig = do
    u <- randomUser brig
    let new = defNewClient PermanentClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    let pks = [PrekeyId 1, lastPrekeyId]
    get ( brig
        . paths ["clients", toByteString' (clientId c), "prekeys"]
        . zUser (userId u)
        ) !!! do
            const 200        === statusCode
            const (Just pks) === fmap sort . decodeBody

testGetUserPrekeys :: Brig -> Http ()
testGetUserPrekeys brig = do
    u <- randomUser brig
    let new = defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    let cpk = ClientPrekey (clientId c) (somePrekeys !! 0)
    get (brig . paths ["users", toByteString' (userId u), "prekeys"]) !!! do
        const 200 === statusCode
        const (Just $ PrekeyBundle (userId u) [cpk]) === decodeBody

    -- prekeys are deleted when retrieved, except the last one
    let lpk = ClientPrekey (clientId c) (unpackLastPrekey (someLastPrekeys !! 0))
    replicateM_ 2 $ get (brig . paths ["users", toByteString' (userId u), "prekeys"]) !!! do
        const 200 === statusCode
        const (Just $ PrekeyBundle (userId u) [lpk]) === decodeBody

testGetClientPrekey :: Brig -> Http ()
testGetClientPrekey brig = do
    u <- randomUser brig
    let new = defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    Just c <- decodeBody <$> addClient brig u new
    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === decodeBody

testTooManyClients :: Brig -> Http ()
testTooManyClients brig = do
    u <- randomUser brig

    -- There is only one temporary client, adding a new one
    -- replaces the previous one.
    forM_ [0..(9 :: Int)] $ \i ->
        let pk = somePrekeys !! i
            lk = someLastPrekeys !! i
        in addClient brig u (defNewClient TemporaryClient [pk] lk) !!! const 201 === statusCode

    -- But there can be only up to 7 permanent clients
    forM_ [10..(16 :: Int)] $ \i ->
        let pk = somePrekeys !! i
            lk = someLastPrekeys !! i
        in addClient brig u (defNewClient PermanentClient [pk] lk) !!! const 201 === statusCode

    addClient brig u (defNewClient PermanentClient [somePrekeys !! 17] (someLastPrekeys !! 17)) !!! do
        const 403 === statusCode
        const (Just "too-many-clients") === fmap Error.label . decodeBody

testRemoveClient :: Brig -> Cannon -> Http ()
testRemoveClient brig cannon = do
    u <- randomUser brig
    let Just email = userEmail u

    -- Permanent client with attached cookie
    login brig (defEmailLogin email) PersistentCookie
        !!! const 200 === statusCode
    numCookies <- countCookies brig (userId u) defCookieLabel
    liftIO $ Just 1 @=? numCookies
    Just c <- decodeBody <$> addClient brig u (client PermanentClient (someLastPrekeys !! 10))
    -- Missing password
    deleteClient brig (userId u) (clientId c) Nothing !!! const 403 === statusCode

    -- Success
    WS.bracketR cannon (userId u) $ \ws -> do
        deleteClient brig (userId u) (clientId c) (Just defPassword)
            !!! const 200 === statusCode
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            let etype = j ^? key "type" . _String
            let eclient = j ^? key "client" . key "id" . _String
            etype @?= Just "user.client-remove"
            fmap ClientId eclient @?= Just (clientId c)

    -- Not found on retry
    deleteClient brig (userId u) (clientId c) Nothing !!! const 404 === statusCode

    -- Prekeys are gone
    getPreKey brig (userId u) (clientId c) !!! const 404 === statusCode

    -- Cookies are gone
    numCookies' <- countCookies brig (userId u) defCookieLabel
    liftIO $ Just 0 @=? numCookies'
  where
    client ty lk = (defNewClient ty [somePrekeys !! 0] lk)
        { newClientLabel  = Just "Nexus 5x"
        , newClientCookie = Just defCookieLabel
        }

testUpdateClient :: Brig -> Http ()
testUpdateClient brig = do
    u <- randomUser brig
    let clt = (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone"
                }
    Just c <- decodeBody <$> addClient brig u clt
    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) (somePrekeys !! 0)) === decodeBody

    getClient brig (userId u) (clientId c) !!! do
        const 200                   === statusCode
        const (Just "Test Device")  === (clientLabel <=< decodeBody)
        const (Just PhoneClient)    === (clientClass <=< decodeBody)
        const (Just "featurephone") === (clientModel <=< decodeBody)

    let newPrekey = somePrekeys !! 2
    let update    = UpdateClient [newPrekey] Nothing (Nothing :: Maybe SignalingKeys) (Just "label")

    put ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser (userId u)
        . contentJson
        . body (RequestBodyLBS $ encode update)
        ) !!! const 200 === statusCode

    get (brig . paths ["users", toByteString' (userId u), "prekeys", toByteString' (clientId c)]) !!! do
        const 200 === statusCode
        const (Just $ ClientPrekey (clientId c) newPrekey) === decodeBody

    -- check if label has been updated
    getClient brig (userId u) (clientId c) !!! do
        const 200            === statusCode
        const (Just "label") === (clientLabel <=< decodeBody)

    -- via `/users/:user/clients/:client`, only `id` and `class` are visible:
    get (brig . paths ["users", toByteString' (userId u), "clients", toByteString' (clientId c)]) !!! do
        const 200                 === statusCode
        const (Just $ clientId c) === (fmap pubClientId . decodeBody)
        const (Just PhoneClient)  === (pubClientClass <=< decodeBody)
        const Nothing             === (preview (key "label") <=< asValue)

    let update' = UpdateClient [] Nothing (Nothing :: Maybe SignalingKeys) Nothing

    -- empty update should be a no-op
    put ( brig
        . paths ["clients", toByteString' (clientId c)]
        . zUser (userId u)
        . contentJson
        . body (RequestBodyLBS $ encode update')
        ) !!! const 200 === statusCode

    -- check if label is still present
    getClient brig (userId u) (clientId c) !!! do
        const 200            === statusCode
        const (Just "label") === (clientLabel <=< decodeBody)

-- Legacy (galley)
testAddMultipleTemporary :: Brig -> Galley -> Http ()
testAddMultipleTemporary brig galley = do
    u <- randomUser brig

    let clt1 = (defNewClient TemporaryClient [somePrekeys !! 0] (someLastPrekeys !! 0))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone1"
                }
    _ <- addClient brig u clt1
    brigClients1   <- numOfBrigClients (userId u)
    galleyClients1 <- numOfGalleyClients (userId u)
    liftIO $ assertEqual "Too many clients found" (Just 1) brigClients1
    liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients1

    let clt2 = (defNewClient TemporaryClient [somePrekeys !! 1] (someLastPrekeys !! 1))
                { newClientClass = Just PhoneClient
                , newClientModel = Just "featurephone2"
                }
    _ <- addClient brig u clt2

    brigClients2   <- numOfBrigClients (userId u)
    galleyClients2 <- numOfGalleyClients (userId u)
    liftIO $ assertEqual "Too many clients found" (Just 1) brigClients2
    liftIO $ assertEqual "Too many clients found" (Just 1) galleyClients2
  where
    numOfBrigClients u = do
        r <- get $ brig
                 . path "clients"
                 . zUser u
        return $ Vec.length <$> (preview _Array =<< asValue r)

    numOfGalleyClients u = do
        r <- get $ galley
                 . path "i/test/clients"
                 . zUser u
        return $ Vec.length <$> (preview _Array =<< asValue r)

testPreKeyRace :: Brig -> Http ()
testPreKeyRace brig = do
    u <- randomUser brig
    let pks = map (\i -> somePrekeys !! i) [1..10]
    Just c <- decodeBody <$> addClient brig u (defNewClient PermanentClient pks (someLastPrekeys !! 0))
    pks' <- flip mapConcurrently pks $ \_ -> do
        rs <- getPreKey brig (userId u) (clientId c) <!! const 200 === statusCode
        return $ prekeyId . prekeyData <$> decodeBody rs
    -- We should not hand out regular prekeys more than once (i.e. at most once).
    let actual = catMaybes pks'
    liftIO $ assertEqual "insufficient prekeys" (length pks) (length actual)
    let regular = filter (/= lastPrekeyId) actual
    liftIO $ assertEqual "duplicate prekeys" (length regular) (length (nub regular))
    deleteClient brig (userId u) (clientId c) (Just defPassword) !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- Property Tests

testSetGetProperty :: Brig -> Http ()
testSetGetProperty brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" objectProp !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200               === statusCode
        const (Just objectProp) === decodeBody
    -- String Literals
    setProperty brig (userId u) "foo" (String "foo") !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200              === statusCode
        const (Just "\"foo\"") === responseBody
    -- Boolean Literals
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200           === statusCode
        const (Just "true") === responseBody
    -- Numeric Literals
    setProperty brig (userId u) "foo" (Number 42) !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200         === statusCode
        const (Just "42") === responseBody
  where
    objectProp = object
        [ "key.1" .= ("val1" :: Text)
        , "key.2" .= ("val2" :: Text)
        ]

testDeleteProperty :: Brig -> Http ()
testDeleteProperty brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    deleteProperty brig (userId u) "foo" !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!!
        const 404 === statusCode

testListPropertyKeys :: Brig -> Http ()
testListPropertyKeys brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!!
        const 200 === statusCode
    let keys = toJSON ["bar" :: Text, "foo"]
    get (brig . path "/properties" . zUser (userId u)) !!! do
        const 200         === statusCode
        const (Just keys) === decodeBody

testClearProperties :: Brig -> Http ()
testClearProperties brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!!
        const 200 === statusCode
    delete (brig . path "/properties" . zUser (userId u) . zConn "conn") !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!!
        const 404 === statusCode
    getProperty brig (userId u) "bar" !!!
        const 404 === statusCode

testPropertyLimits :: Brig -> Http ()
testPropertyLimits brig = do
    u <- randomUser brig
    -- Maximum key length
    setProperty brig (userId u) (C.replicate 257 'x') (String "y") !!! do
        const 403 === statusCode
        const (Just "property-key-too-large") === fmap Error.label . decodeBody

    -- Maximum value length
    setProperty brig (userId u) "foo" (String (T.replicate 513 "x")) !!! do
        const 403 === statusCode
        const (Just "property-value-too-large") === fmap Error.label . decodeBody

    -- Maximum count
    forM_ [1..16 :: Int] $ \i ->
        setProperty brig (userId u) ("foo" <> C.pack (show i)) (Number (fromIntegral i)) !!!
            const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!! do
        const 403 === statusCode
        const (Just "too-many-properties") === fmap Error.label . decodeBody

-------------------------------------------------------------------------------
-- Password Reset Tests

testPasswordReset :: Brig -> Http ()
testPasswordReset brig = do
    u <- randomUser brig
    let Just email = userEmail u
    let uid = userId u
    -- initiate reset
    initiatePasswordReset brig email !!! const 201 === statusCode
    let newpw = PlainTextPassword "newsecret"
    passwordResetData <- preparePasswordReset brig email uid newpw
    completePasswordReset brig passwordResetData !!! const 200 === statusCode
    -- try login
    login brig (defEmailLogin email) PersistentCookie
        !!! const 403 === statusCode
    login brig (PasswordLogin (LoginByEmail email) newpw Nothing) PersistentCookie
        !!! const 200 === statusCode

--------------------------------------------------------------------------------
-- Handle Tests

testHandleUpdate :: Brig -> Cannon -> Http ()
testHandleUpdate brig cannon = do
    uid <- userId <$> randomUser brig

    -- Invalid handles are rejected
    let badHandles = ["ca$h", "w", "Capital", "wire"]
    forM_ badHandles $ \h -> do
        let upd = RequestBodyLBS . encode $ HandleUpdate h
        put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body upd) !!! do
            const 400 === statusCode
            const (Just "invalid-handle") === fmap Error.label . decodeBody

    -- Claim a valid handle & receive notification
    hdl <- randomHandle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    WS.bracketR cannon uid $ \ws -> do
        put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
            const 200 === statusCode
        void . liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let j = Object $ List1.head (ntfPayload n)
            j ^? key "type" . _String @?= Just "user.update"
            let u = j ^?! key "user"
            u ^? key "id" . _String     @?= Just (UUID.toText (toUUID uid))
            u ^? key "handle" . _String @?= Just hdl

    -- The owner of the handle can always retry the update
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 200 === statusCode

    -- For other users, the handle is unavailable
    uid2 <- userId <$> randomUser brig
    put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
        const 409 === statusCode
        const (Just "handle-exists") === fmap Error.label . decodeBody

    -- The owner appears by that handle in search
    Search.refreshIndex brig
    Search.assertCanFind brig uid2 uid hdl

    -- Change the handle again, thus freeing the old handle
    hdl2 <- randomHandle
    let update2 = RequestBodyLBS . encode $ HandleUpdate hdl2
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update2) !!!
        const 200 === statusCode
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 404 === statusCode

    -- The owner appears by the new handle in search
    Search.refreshIndex brig
    Search.assertCan'tFind brig uid2 uid hdl
    Search.assertCanFind   brig uid2 uid hdl2

    -- Other users cannot immediately claim the old handle since the previous claim
    -- is still active.
    put (brig . path "/self/handle" . contentJson . zUser uid2 . zConn "c" . body update) !!! do
        const 409 === statusCode
        const (Just "handle-exists") === fmap Error.label . decodeBody

    -- The old handle can be claimed again immediately by the user who previously
    -- owned it (since the claim is either still active but his own, or expired).
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode

testHandleRace :: Brig -> Http ()
testHandleRace brig = do
    us <- replicateM 10 (userId <$> randomUser brig)
    -- 10 races. In each race, 10 users try to claim the same handle.
    -- At most one of them should get the handle in each race
    -- (usually no-one due to the contention).
    void $ replicateM 10 $ do
        hdl <- randomHandle
        let update = RequestBodyLBS . encode $ HandleUpdate hdl
        void $ flip mapConcurrently us $ \u ->
            put (brig . path "/self/handle" . contentJson . zUser u . zConn "c" . body update)
        ps <- forM us $ \u -> decodeBody <$> get (brig . path "/self" . zUser u)
        let owners = catMaybes $ filter (maybe False ((== Just (Handle hdl)) . userHandle)) ps
        liftIO $ assertBool "More than one owner of a handle" (length owners <= 1)

testHandleQuery :: Brig -> Http ()
testHandleQuery brig = do
    uid <- userId <$> randomUser brig
    hdl <- randomHandle

    -- Query for the handle availability (must be free)
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 404 === statusCode

    -- Set handle
    let update = RequestBodyLBS . encode $ HandleUpdate hdl
    put (brig . path "/self/handle" . contentJson . zUser uid . zConn "c" . body update) !!!
        const 200 === statusCode

    -- Query the updated profile
    get (brig . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just (Handle hdl)) === (>>= userHandle) . decodeBody

    -- Query for the handle availability (must be taken)
    Bilge.head (brig . paths ["users", "handles", toByteString' hdl] . zUser uid) !!!
        const 200 === statusCode

    -- Query user profiles by handles
    get (brig . path "/users" . queryItem "handles" (toByteString' hdl) . zUser uid) !!! do
        const 200 === statusCode
        const (Just (Handle hdl)) === (>>= (listToMaybe >=> userHandle)) . decodeBody

    -- Bulk availability check
    hdl2 <- randomHandle
    hdl3 <- randomHandle
    checkHandles brig uid [hdl, hdl2, "InVa£iD", hdl3] 1 !!! do
        const 200 === statusCode
        const (Just [hdl2]) === decodeBody
    checkHandles brig uid [hdl2, hdl, hdl3] 3 !!! do
        const 200 === statusCode
        const (Just [hdl2, hdl3]) === decodeBody

-------------------------------------------------------------------------------
-- Onboarding Tests

testOnboarding :: Brig -> Http ()
testOnboarding brig = do
    usr1 <- randomUser brig
    let uid1 = userId usr1
        em1 = fromEmail $ fromMaybe (error "Should have an email!") (userEmail usr1)
    (uid2, phn2) <- createRandomPhoneUser brig

    -- We do not match on emails (nor on other phone numbers obviously)
    ab2 <- liftIO $ toAddressBook [ ("random1", [em1]), ("random2", ["+0123456789"]) ]
    let expect2 = toMatchingResult []
    uploadAddressBook brig uid1 ab2 expect2

    -- Simple test with a single user, single entry
    ab3 <- liftIO $ toAddressBook [ ("random", [fromPhone phn2]) ]
    let expect3 = toMatchingResult [(uid2, "random")]
    uploadAddressBook brig uid1 ab3 expect3
    -- Ensure we really got auto-connected
    assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
    assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]

    -- Ensure we only auto-connect once
    uploadAddressBook brig uid1 ab3 (toMatchingResult [])

    -- Single user, multiple entries
    (uid4, ph4) <- createRandomPhoneUser brig
    ab4 <- liftIO $ toAddressBook [ ("first",  [fromPhone ph4])
                                  , ("second", [fromPhone ph4])
                                  ]
    let expect4 = toMatchingResult [(uid4, "first"), (uid4, "second")]
    uploadAddressBook brig uid1 ab4 expect4

    -- Multiple user, multiple entries
    (uid5, ph5) <- createRandomPhoneUser brig
    (uid6, ph6) <- createRandomPhoneUser brig
    ab5 <- liftIO $ toAddressBook [ ("first",  [fromPhone ph5])
                                  , ("second", [fromPhone ph5])
                                  , ("third",  [fromPhone ph6])
                                  , ("fourth", [fromPhone ph6])
                                  ]
    let expect5 = toMatchingResult [ (uid5, "first"), (uid5, "second")
                                   , (uid6, "third"), (uid6, "fourth")
                                   ]

    -- Check upload and results
    uploadAddressBook brig uid1 ab5 expect5

-------------------------------------------------------------------------------
-- Utilities

checkHandles :: Brig -> UserId -> [Text] -> Word -> Http ResponseLBS
checkHandles brig uid hs num =
    let hs'  = unsafeRange hs
        num' = unsafeRange num
        js   = RequestBodyLBS $ encode $ CheckHandles hs' num'
    in post (brig . path "/users/handles" . contentJson . zUser uid . body js)

-- Note: This actually _will_ send out an email so make sure we don't use any
--       inexistent email addresses or ones that bounce! Perhaps we should
--       ensure that the email used here has a domain 'simulator.amazonses.com'
-- TODO: register
registerUser :: Text -> Text -> Brig -> Http ResponseLBS
registerUser name email brig = do
    e <- mkEmail email
    let p = RequestBodyLBS . encode $ object
            [ "name"     .= name
            , "email"    .= fromEmail e
            , "password" .= defPassword
            ]
    post (brig . path "/register" . contentJson . body p)

createRandomPhoneUser :: Brig -> Http (UserId, Phone)
createRandomPhoneUser brig = do
    usr <- randomUser brig
    let uid = userId usr
    phn <- liftIO randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . encode $ PhoneUpdate phn
    put (brig . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        (const 202 === statusCode)
    -- activate
    act <- getActivationCode brig (Right phn)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! const 200 === statusCode
    -- check new phone
    get (brig . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just phn) === (userPhone <=< decodeBody)

    return (uid, phn)

initiatePasswordReset :: Brig -> Email -> Http ResponseLBS
initiatePasswordReset brig email =
    post ( brig
         . path "/password-reset"
         . contentJson
         . body (RequestBodyLBS . encode $ NewPasswordReset (Left email))
         )

activateEmail :: Brig -> Email -> HttpT IO ()
activateEmail brig email = do
    act <- getActivationCode brig (Left email)
    case act of
        Nothing -> liftIO $ assertFailure "missing activation key/code"
        Just kc -> activate brig kc !!! do
            const 200 === statusCode
            const(Just False) === fmap activatedFirst . decodeBody

checkEmail :: Brig -> UserId -> Email -> HttpT IO ()
checkEmail brig uid expectedEmail =
    get (brig . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just expectedEmail) === (userEmail <=< decodeBody)

initiateEmailUpdate :: Brig -> Email -> UserId -> Http ResponseLBS
initiateEmailUpdate brig email uid =
    let emailUpdate = RequestBodyLBS . encode $ EmailUpdate email in
    put (brig . path "/self/email" . contentJson . zUser uid . zConn "c" . body emailUpdate)

preparePasswordReset :: Brig -> Email -> UserId -> PlainTextPassword -> Http CompletePasswordReset
preparePasswordReset brig email uid newpw = do
    let qry = queryItem "email" (toByteString' email)
    r <- get $ brig . path "/i/users/password-reset-code" . qry
    let lbs = fromMaybe "" $ responseBody r
    let Just pwcode = PasswordResetCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
    ident <- PasswordResetIdentityKey <$> mkPasswordResetKey uid
    let complete = CompletePasswordReset ident pwcode newpw
    return complete

completePasswordReset :: Brig -> CompletePasswordReset -> Http ResponseLBS
completePasswordReset brig passwordResetData =
    post ( brig
         . path "/password-reset/complete"
         . contentJson
         . body (RequestBodyLBS $ encode passwordResetData)
         )

removeBlacklist :: Brig -> Email -> Http ()
removeBlacklist brig email =
    void $ delete (brig . path "/i/users/blacklist" . queryItem "email" (toByteString' email))

getInvitationCode :: Brig -> UserId -> InvitationId -> Http (Maybe InvitationCode)
getInvitationCode brig u ref = do
    r <- get ( brig
             . path "/i/users/invitation-code"
             . queryItem "inviter" (toByteString' u)
             . queryItem "invitation_id" (toByteString' ref)
             )
    let lbs   = fromMaybe "" $ responseBody r
    return $ fromByteString . fromMaybe (error "No code?") $ T.encodeUtf8 <$> (lbs ^? key "code"  . _String)

getInvitation :: Brig -> InvitationCode -> Http (Maybe Invitation)
getInvitation brig c = do
    r <- get $ brig
             . path "/invitations/info"
             . queryItem "code" (toByteString' c)
    return . decode . fromMaybe "" $ responseBody r

getClient :: Brig -> UserId -> ClientId -> Http ResponseLBS
getClient brig u c = get $ brig
    . paths ["clients", toByteString' c]
    . zUser u

deleteClient :: Brig -> UserId -> ClientId -> Maybe PlainTextPassword -> Http ResponseLBS
deleteClient brig u c pw = delete $ brig
    . paths ["clients", toByteString' c]
    . zUser u
    . zConn "conn"
    . contentJson
    . body payload
  where
    payload = RequestBodyLBS . encode $ object
        [ "password" .= pw
        ]

listConnections :: Brig -> UserId -> Http ResponseLBS
listConnections brig u = get $ brig
    . path "connections"
    . zUser u

postInvitation :: Brig -> UserId -> InvitationRequest -> Http ResponseLBS
postInvitation brig u i = post $ brig
    . path "invitations"
    . contentJson
    . body (RequestBodyLBS $ encode i)
    . zUser u
    . zConn "conn"

postAutoConnection :: Brig -> UserId -> [UserId] -> Http ResponseLBS
postAutoConnection brig from to = post $ brig
    . paths ["/i/users", toByteString' from, "auto-connect"]
    . contentJson
    . body payload
    . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ AutoConnect (Set.fromList to)

setProperty :: Brig -> UserId -> ByteString -> Value -> Http ResponseLBS
setProperty brig u k v = put $ brig
    . paths ["/properties", k]
    . zUser u
    . zConn "conn"
    . contentJson
    . body (RequestBodyLBS $ encode v)

getProperty :: Brig -> UserId -> ByteString -> Http ResponseLBS
getProperty brig u k = get $ brig
    . paths ["/properties", k]
    . zUser u

deleteProperty :: Brig -> UserId -> ByteString -> Http ResponseLBS
deleteProperty brig u k = delete $ brig
    . paths ["/properties", k]
    . zConn "conn"
    . zUser u

countCookies :: Brig -> UserId -> CookieLabel -> Http (Maybe Int)
countCookies brig u label = do
    r <- get ( brig
             . path "/cookies"
             . queryItem "labels" (toByteString' label)
             . header "Z-User" (toByteString' u)
             ) <!! const 200 === statusCode
    return $ Vec.length <$> (preview (key "cookies" . _Array) =<< asValue r)

assertConnections :: Brig -> UserId -> [ConnectionStatus] -> Http ()
assertConnections brig u cs = listConnections brig u !!! do
    const 200 === statusCode
    const (Just True) === fmap (check . map status . clConnections) . decodeBody
  where
    check xs = all (`elem` xs) cs
    status c = ConnectionStatus (ucFrom c) (ucTo c) (ucStatus c)

assertEmailVisibility :: Brig -> User -> User -> Bool -> Http ()
assertEmailVisibility brig a b visible =
    get (brig . paths ["users", pack . show $ userId b] . zUser (userId a)) !!! do
        const 200 === statusCode
        if visible
            then const (Just (userEmail b)) === fmap userEmail . decodeBody
            else const Nothing === (userEmail <=< decodeBody)

uploadAddressBook :: Brig -> UserId -> AddressBook -> MatchingResult -> Http ()
uploadAddressBook b u a m =
    post ( b
         . path "/onboarding/v3"
         . contentJson
         . zUser u
         . body (RequestBodyLBS $ encode a)
         ) !!! do
            const 200 === statusCode
            const (Just (f m)) === (fmap f . decodeBody)
  where
    f :: MatchingResult -> MatchingResult
    f (MatchingResult x y) = MatchingResult (sort x) (sort y)

-- Builds expectations on the matched users/cards
toMatchingResult :: [(UserId, Text)] -> MatchingResult
toMatchingResult xs = MatchingResult
                      (map (\(u, c) -> Match u (Just (CardId c)) [CardId c]) xs)
                      (Set.toList $ Set.fromList (map fst xs))

-- Hashes each entry and builds an appropriate address book
toAddressBook :: [(Text, [Text])] -> IO AddressBook
toAddressBook xs = do
    Just sha <- liftIO $ getDigestByName "SHA256"
    return . AddressBook $ fmap (toCard sha) xs
  where
    toCard sha (cardId, entries) = Card (Just $ CardId cardId)
                                        (map (Entry . digestBS sha . T.encodeUtf8) entries)

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
