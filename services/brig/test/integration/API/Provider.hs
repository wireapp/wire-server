{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module API.Provider (tests, Config) where

import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Brig.Types hiding (NewPasswordReset (..), CompletePasswordReset(..), EmailUpdate (..), PasswordReset (..), PasswordChange (..))
import Brig.Types.Provider
import Brig.Types.Provider.Tag
import Control.Concurrent.Chan
import Control.Concurrent.Timeout
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Conversion
import Data.Foldable (toList)
import Data.Id hiding (client)
import Data.List1 (List1)
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Monoid ((<>))
import Data.PEM
import Data.Range
import Data.Set (Set)
import Data.Text (Text, isPrefixOf, toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Timeout (Timeout, TimeoutUnit (..), (#), TimedOut (..))
import Galley.Types (NewConv (..), NewConvUnmanaged (..), Conversation (..), Members (..))
import Galley.Types (ConvMembers (..), OtherMember (..))
import Galley.Types (Event (..), EventType (..), EventData (..), OtrMessage (..))
import Galley.Types.Bot (ServiceRef, newServiceRef, serviceRefId, serviceRefProvider)
import GHC.Generics hiding (to, from)
import GHC.Stack (HasCallStack)
import Gundeck.Types.Notification
import Network.HTTP.Types.Status (status200, status201, status400)
import Network.Wai (Application, responseLBS, strictRequestBody)
import OpenSSL.PEM (writePublicKey)
import OpenSSL.RSA (generateRSAKey')
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit
import Web.Cookie (SetCookie (..), parseSetCookie)
import Util
import Util.Options.Common (optOrEnv)

import qualified API.Team.Util                     as Team
import qualified Brig.Code                         as Code
import qualified Brig.Types.Intra                  as Intra
import qualified Brig.Types.Provider.External      as Ext
import qualified Cassandra                         as DB
import qualified Control.Concurrent.Async          as Async
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as C8
import qualified Data.ByteString.Lazy.Char8        as LC8
import qualified Data.HashMap.Strict               as HashMap
import qualified Data.List1                        as List1
import qualified Data.Set                          as Set
import qualified Data.Text.Ascii                   as Ascii
import qualified Data.Text.Encoding                as Text
import qualified Data.UUID                         as UUID
import qualified Data.ZAuth.Token                  as ZAuth
import qualified Galley.Types.Teams                as Team
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.WarpTLS       as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Route                 as Wai
import qualified Network.Wai.Utilities.Error       as Error
import qualified Test.Tasty.Cannon                 as WS

data Config = Config
    { privateKey   :: FilePath
    , publicKey    :: FilePath
    , cert         :: FilePath
    , botHost      :: Text
    , botPort      :: Int
    } deriving (Show, Generic)

instance FromJSON Config

tests :: Maybe Config -> Manager -> DB.ClientState -> Brig -> Cannon -> Galley -> IO TestTree
tests conf p db b c g = do
    crt <- optOrEnv cert conf id "TEST_CERT"
    return $ testGroup "provider"
        [ testGroup "account"
            [ test p "register"       $ testRegisterProvider db b
            , test p "login"          $ testLoginProvider db b
            , test p "update"         $ testUpdateProvider db b
            , test p "delete"         $ testDeleteProvider db b
            , test p "password-reset" $ testPasswordResetProvider db b
            , test p "email/password update with password reset"
                                      $ testPasswordResetAfterEmailUpdateProvider db b
            ]
        , testGroup "service"
            [ test p "add-get fail (bad key)" $ testAddGetServiceBadKey conf db b
            , test p "add-get"                $ testAddGetService conf db b
            , test p "update"                 $ testUpdateService conf db b
            , test p "update-conn"            $ testUpdateServiceConn conf db b
            , test p "search (tag/prefix)"    $ testListServices conf db b
            , test p "delete"                 $ testDeleteService conf db b
            ]
        , testGroup "bot"
            [ test p "add-remove" $ testAddRemoveBot conf crt db b g c
            , test p "message"    $ testMessageBot conf crt db b g c
            ]
        , testGroup "bot-teams"
            [ test p "add-remove"  $ testAddRemoveBotTeam conf crt db b g c
            , test p "message"     $ testMessageBotTeam conf crt db b g c
            , test p "delete conv" $ testDeleteConvBotTeam conf crt db b g c
            , test p "delete team" $ testDeleteTeamBotTeam conf crt db b g c
            ]
        ]

-------------------------------------------------------------------------------
-- Provider Accounts

-- | Step-by-step registration procedure with verification
-- of pre- and post-conditions.
testRegisterProvider :: DB.ClientState -> Brig -> Http ()
testRegisterProvider db brig = do
    email <- mkSimulatorEmail "success"
    gen   <- Code.mkGen (Code.ForEmail email)

    let new = defNewProvider email

    _rs <- registerProvider brig new <!!
        const 201 === statusCode

    let Just npr = decodeBody _rs :: Maybe NewProviderResponse
    -- Since a password was given, none should have been generated
    liftIO $ assertBool "password" (isNothing (rsNewProviderPassword npr))
    let pid = rsNewProviderId npr

    -- No login possible directly after registration
    loginProvider brig email defProviderPassword !!!
        const 403 === statusCode

    -- Activate email
    Just vcode <- lookupCode db gen Code.IdentityVerification
    activateProvider brig (Code.codeKey vcode) (Code.codeValue vcode) !!!
        const 200 === statusCode

    -- Login succeeds after activation (due to auto-approval)
    loginProvider brig email defProviderPassword !!!
        const 200 === statusCode

    -- Email address is now taken
    registerProvider brig new !!!
        const 409 === statusCode

    -- Retrieve full account and public profile
    -- (these are identical for now).
    uid <- randomId
    _rs <- getProvider brig pid <!! const 200 === statusCode
    let Just p  = decodeBody _rs
    _rs <- getProviderProfile brig pid uid <!! const 200 === statusCode
    let Just pp = decodeBody _rs
    liftIO $ do
        assertEqual "id" pid (providerId p)
        assertEqual "name" defProviderName (providerName p)
        assertEqual "email" email (providerEmail p)
        assertEqual "url" defProviderUrl (providerUrl p)
        assertEqual "description" defProviderDescr (providerDescr p)
        assertEqual "profile" (ProviderProfile p) pp

testLoginProvider :: DB.ClientState -> Brig -> Http ()
testLoginProvider db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    let email = providerEmail prv
    _rs <- loginProvider brig email defProviderPassword <!! do
        const 200 === statusCode
        const Nothing === responseBody
    let Just cok = parseSetCookie <$> getHeader "Set-Cookie" _rs
    now <- liftIO getCurrentTime
    let ttl = (`diffUTCTime` now) <$> setCookieExpires cok
    liftIO $ do
        assertEqual "cookie name" "zprovider" (setCookieName cok)
        assertEqual "cookie http-only" True (setCookieHttpOnly cok)
        assertBool  "cookie timeout" (ttl > Just 0)
    let Just tok = fromByteString (setCookieValue cok)
    liftIO $ assertEqual "principal" pid (Id (tok^.ZAuth.body.ZAuth.provider))

testUpdateProvider :: DB.ClientState -> Brig -> Http ()
testUpdateProvider db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    let newName = Name "All New"
    let Just newUrl = fromByteString "https://new.localhost/"
    let newDescr = "Totally new description"
    let upd = UpdateProvider
            { updateProviderName  = Just newName
            , updateProviderUrl   = Just newUrl
            , updateProviderDescr = Just newDescr
            }
    updateProvider brig pid upd !!! const 200 === statusCode
    _rs <- getProvider brig pid <!! const 200 === statusCode
    let Just prv' = decodeBody _rs
    liftIO $ do
        assertEqual "name" newName (providerName prv')
        assertEqual "url" newUrl (providerUrl prv')
        assertEqual "description" newDescr (providerDescr prv')

testDeleteProvider :: DB.ClientState -> Brig -> Http ()
testDeleteProvider db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    deleteProvider brig pid defProviderPassword !!!
        const 200 === statusCode
    getProvider brig pid !!! const 404 === statusCode
    -- The email address must be available again
    let new = defNewProvider (providerEmail prv)
    registerProvider brig new !!!
        const 201 === statusCode

testPasswordResetProvider :: DB.ClientState -> Brig -> Http ()
testPasswordResetProvider db brig = do
    prv <- randomProvider db brig

    let email = providerEmail prv
    initiatePasswordResetProvider brig (PasswordReset email) !!! const 201 === statusCode
    let newPw = PlainTextPassword "newsupersecret"

    -- Get the code directly from the DB
    gen <- Code.mkGen (Code.ForEmail email)
    Just vcode <- lookupCode db gen Code.PasswordReset

    let passwordResetData = CompletePasswordReset (Code.codeKey vcode)
                                                  (Code.codeValue vcode)
                                                  newPw
    completePasswordResetProvider brig passwordResetData !!! const 200 === statusCode

    loginProvider brig email defProviderPassword !!!
        const 403 === statusCode
    loginProvider brig email newPw !!!
        const 200 === statusCode

testPasswordResetAfterEmailUpdateProvider :: DB.ClientState -> Brig -> Http ()
testPasswordResetAfterEmailUpdateProvider db brig = do
    newEmail <- mkSimulatorEmail "success"
    prv <- randomProvider db brig
    let pid = providerId prv
    let origEmail = providerEmail prv
    initiateEmailUpdateProvider brig pid (EmailUpdate newEmail) !!! const 202 === statusCode
    initiatePasswordResetProvider brig (PasswordReset origEmail) !!! const 201 === statusCode

    -- Get password reset code directly from the DB
    genOrig <- Code.mkGen (Code.ForEmail origEmail)
    Just vcodePw <- lookupCode db genOrig Code.PasswordReset

    let passwordResetData = CompletePasswordReset (Code.codeKey vcodePw)
                                                  (Code.codeValue vcodePw)
                                                  (PlainTextPassword "doesnotmatter")

    -- Activate the new email
    genNew <- Code.mkGen (Code.ForEmail newEmail)
    Just vcodeEm <- lookupCode db genNew Code.IdentityVerification
    activateProvider brig (Code.codeKey vcodeEm) (Code.codeValue vcodeEm) !!!
        const 200 === statusCode

    Just p <- decodeBody <$> (getProvider brig pid <!! const 200 === statusCode)
    liftIO $ assertEqual "email" newEmail (providerEmail p)

    -- attempting to complete password reset should fail
    completePasswordResetProvider brig passwordResetData !!! const 403 === statusCode

    -- ensure you can login with the new email address and not with the old one
    loginProvider brig origEmail defProviderPassword !!! const 403 === statusCode
    loginProvider brig newEmail defProviderPassword !!! const 200 === statusCode

    -- exercise the password change endpoint
    let newPass = PlainTextPassword "newpass"
    let pwChangeFail = PasswordChange (PlainTextPassword "notcorrect") newPass
    updateProviderPassword brig pid pwChangeFail !!! const 403 === statusCode
    let pwChange = pwChangeFail { cpOldPassword = defProviderPassword }
    updateProviderPassword brig pid pwChange !!! const 200 === statusCode

    -- Check the login process again
    loginProvider brig newEmail defProviderPassword !!! const 403 === statusCode
    loginProvider brig newEmail newPass !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- Provider Services

testAddGetServiceBadKey :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testAddGetServiceBadKey config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    -- Add service
    new <- defNewService config
    -- Specially crafted key that passes basic validation
    let Right [k] = pemParseBS "-----BEGIN PUBLIC KEY-----\n\n-----END PUBLIC KEY-----"
    let newBad = new { newServiceKey = ServiceKeyPEM k }
    addService brig pid newBad !!! const 400 === statusCode

testAddGetService :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testAddGetService config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    -- Add service
    new <- defNewService config
    _rs <- addService brig pid new <!! const 201 === statusCode
    let Just srs = decodeBody _rs
    let sid = rsNewServiceId srs
    -- Get service definition as seen by provider
    _rs <- getService brig pid sid <!! const 200 === statusCode
    let Just svc = decodeBody _rs
    liftIO $ do
        assertEqual "auth token" (List1.singleton <$> rsNewServiceToken srs) (Just (serviceTokens svc))
        assertEqual "name" defServiceName (serviceName svc)
        assertEqual "description" defServiceDescr (serviceDescr svc)
        assertEqual "url" defServiceUrl (serviceUrl svc)
        assertEqual "keys" (List1.singleton (newServiceKey new)) (serviceKeyPEM <$> serviceKeys svc)
        assertEqual "assets" defServiceAssets (serviceAssets svc)
        assertEqual "tags" (fromRange defServiceTags) (serviceTags svc)
        assertBool  "enabled" (not (serviceEnabled svc))
    -- Get public service profile
    uid <- randomId
    _rs <- getServiceProfile brig uid pid sid <!! const 200 === statusCode
    let Just svp = decodeBody _rs
    liftIO $ do
        assertEqual "id" (serviceId svc) (serviceProfileId svp)
        assertEqual "provider" pid (serviceProfileProvider svp)
        assertEqual "name" (serviceName svc) (serviceProfileName svp)
        assertEqual "description" (serviceDescr svc) (serviceProfileDescr svp)
        assertEqual "assets" (serviceAssets svc) (serviceProfileAssets svp)
        assertEqual "tags" (serviceTags svc) (serviceProfileTags svp)
        assertBool  "enabled" (not (serviceProfileEnabled svp))

    -- TODO: Check that disabled services can not be found via tag search?
    --       Need to generate a unique service name for that.

testUpdateService :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testUpdateService config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    _svc <- addGetService brig pid =<< defNewService config
    let sid = serviceId _svc
    let newTags    = Set.fromList [QuizTag, EducationTag]
    let newName    = Name "x"
    let newSummary = "short"
    let newDescr   = "looooooooooooong"
    let newAssets  = [] -- TODO
    -- Exercise all updateable attributes
    let upd = UpdateService
            { updateServiceName    = Just newName
            , updateServiceSummary = Just (unsafeRange newSummary)
            , updateServiceDescr   = Just (unsafeRange newDescr)
            , updateServiceAssets  = Just newAssets
            , updateServiceTags    = Just (unsafeRange newTags)
            }
    updateService brig pid sid upd !!! const 200 === statusCode
    _rs <- getService brig pid sid <!! const 200 === statusCode
    let Just _svc = decodeBody _rs
    liftIO $ do
        assertEqual "name" newName (serviceName _svc)
        assertEqual "description" newDescr (serviceDescr _svc)
        assertEqual "assets" newAssets (serviceAssets _svc)
        assertEqual "tags" newTags (serviceTags _svc)
    -- Excercise all individual tags
    forM_ [minBound ..] $ \tag -> do
        let t = Set.singleton tag
        let u = upd { updateServiceTags = Just (unsafeRange t) }
        updateService brig pid sid u !!! const 200 === statusCode
        _rs <- getService brig pid sid <!! const 200 === statusCode
        let Just _svc = decodeBody _rs
        liftIO $ assertEqual "tags" t (serviceTags _svc)

testUpdateServiceConn :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testUpdateServiceConn config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    _svc <- addGetService brig pid =<< defNewService config
    let sid = serviceId _svc
    let Just newUrl = fromByteString "https://other.localhost/test"
    key <- randServiceKey
    let newKeys   = key `List1.cons` (serviceKeyPEM <$> serviceKeys _svc)
    let tok = ServiceToken (Ascii.unsafeFromText "123456")
    let newTokens = tok `List1.cons` serviceTokens _svc
    let upd = UpdateServiceConn
            { updateServiceConnUrl      = Just newUrl
            , updateServiceConnKeys     = Just (unsafeRange (toList newKeys))
            , updateServiceConnTokens   = Just (unsafeRange (toList newTokens))
            , updateServiceConnEnabled  = Just True
            , updateServiceConnPassword = defProviderPassword
            }
    updateServiceConn brig pid sid upd !!!
        const 200 === statusCode
    _rs <- getService brig pid sid <!! const 200 === statusCode
    let Just _svc = decodeBody _rs
    liftIO $ do
        assertEqual "url" newUrl (serviceUrl _svc)
        assertEqual "keys" newKeys (fmap serviceKeyPEM (serviceKeys _svc))
        assertEqual "token" newTokens (serviceTokens _svc)
        assertBool  "enabled" (serviceEnabled _svc)

testListServices :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testListServices config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    uid <- randomId

    -- You need to supply at least one tag or a prefix
    get ( brig
        . path "/services"
        . header "Z-Type" "access"
        . header "Z-User" (toByteString' uid)) !!! const 400 === statusCode

    -- An empty prefix is not sufficient
    listServiceProfilesByPrefix brig uid (Name "") 10 !!! const 400 === statusCode

    -- nb. We use a random name prefix so tests can run concurrently
    -- (and repeatedly) against a shared database and thus a shared
    -- "name index" per tag.
    uniq <- UUID.toText . toUUID <$> randomId
    new  <- defNewService config
    let taggedNames = mkTaggedNames uniq
    svcs <- mapM (addGetService brig pid . mkNew new) (reverse taggedNames)
    mapM_ (enableService brig pid . serviceId) svcs

    let _tags = Just (match1 SocialTag)
    let names = fst (unzip taggedNames)

    -- List services with different start names, that all start with the
    -- same prefix.
    void $ searchAndAssert uid _tags (Name uniq) 20 names

    -- Search by exact name
    forM_ names $ \n -> searchAndAssert uid _tags n 10 [n]

    -- Chosen prefixes

    -- Only Bjørn should be returned
    let _search = mkName uniq "Bjø"
    void $ searchAndAssert uid _tags _search 10 (select _search names)

    -- Both Bjørn and Bjorn should be returned
    let _search = mkName uniq "Bj"
    void $ searchAndAssert uid _tags _search 10 (select _search names)

    -- CHRISTMAS should be returned
    let _search = mkName uniq "chris"
    void $ searchAndAssert uid _tags _search 10 (select _search names)

    -- Ensure name changes are also indexed properly
    forM_ svcs $ searchAndAssertNameChange pid uid _tags uniq
  where
    getPage :: UserId -> Maybe MatchAny -> Maybe Name -> Int -> HttpT IO [ServiceProfile]
    getPage uid (Just tag) start size = do
        rs <- listServiceProfilesByTag brig uid tag start size <!! const 200 === statusCode
        let Just ls = serviceProfilePageResults <$> decodeBody rs
        return ls
    getPage uid Nothing (Just start) size = do
        rs <- listServiceProfilesByPrefix brig uid start size <!! const 200 === statusCode
        let Just ls = serviceProfilePageResults <$> decodeBody rs
        return ls
    getPage _ Nothing Nothing _ = error "Query not supported"

    searchAndAssertNameChange :: ProviderId -> UserId -> Maybe MatchAny -> Text -> Service -> Http ()
    searchAndAssertNameChange pid uid tags uniq svc = do
        let sid = serviceId svc
        Just svp <- decodeBody <$> (getServiceProfile brig uid pid sid
                               <!!  const 200 === statusCode)
        let origName = serviceProfileName svp
        searchAndAssertWithSid uid tags origName 10 [origName] [sid]

        let newName = mkName uniq "Wire"
        let _upd = emptyUpdateService { updateServiceName = Just newName }
        updateService brig pid (serviceId svc) _upd !!! const 200 === statusCode

        -- Now we should find no such service with the original name, only with the new name
        searchAndAssertWithSid uid tags origName 10 []        []
        searchAndAssertWithSid uid tags newName  10 [newName] [sid]

        -- Let's rollback
        let _upd = emptyUpdateService { updateServiceName = Just origName }
        updateService brig pid (serviceId svc) _upd !!! const 200 === statusCode

        -- Searching the new name should return nothing
        searchAndAssertWithSid uid tags newName  10 []         []
        searchAndAssertWithSid uid tags origName 10 [origName] [sid]

    searchAndAssertWithSid uid tags qry size expectsNames expectsSids = do
        -- Test _with_ and _without_ tags and check we got the right sid back
        _ls <- searchAndAssert uid tags qry size expectsNames
        liftIO $ assertEqual ("check sid size: " ++ show qry) (length expectsSids) (length _ls)
        let _sids = map serviceProfileId _ls
        liftIO $ assertEqual ("check sid str: " ++ show qry) expectsSids _sids

    searchAndAssert uid tags qry size expectsNames = do
        -- Search with tag and name
        _ls <- getPage uid tags (Just qry) size
        liftIO $ assertEqual ("get page with tag, size: " ++ show qry) (length expectsNames) (length _ls)
        let _names = map serviceProfileName _ls
        liftIO $ assertEqual ("get page with tag, str: " ++ show qry) expectsNames _names
        -- Search with name only
        _ls <- getPage uid Nothing (Just qry) size
        liftIO $ assertEqual ("get page no tag, size: " ++ show qry) (length expectsNames) (length _ls)
        let _names = map serviceProfileName _ls
        liftIO $ assertEqual ("get page no tag: " ++ show qry) expectsNames _names
        return _ls

    -- 20 names, all using the given unique prefix
    mkTaggedNames uniq =
        [ (mkName uniq "Alpha",     [SocialTag, QuizTag, BusinessTag])
        , (mkName uniq "Beta",      [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Bjorn",     [SocialTag, QuizTag, TravelTag])
        , (mkName uniq "Bjørn",     [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "CHRISTMAS", [SocialTag, QuizTag, WeatherTag])
        , (mkName uniq "Delta",     [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Epsilon",   [SocialTag, QuizTag, BusinessTag])
        , (mkName uniq "Freer",     [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Gamma",     [SocialTag, QuizTag, WeatherTag])
        , (mkName uniq "Gramma",    [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Hera",      [SocialTag, QuizTag, TravelTag])
        , (mkName uniq "Io",        [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Jojo",      [SocialTag, QuizTag, WeatherTag])
        , (mkName uniq "Kuba",      [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Lawn",      [SocialTag, QuizTag, TravelTag])
        , (mkName uniq "Mango",     [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "North",     [SocialTag, QuizTag, WeatherTag])
        , (mkName uniq "Yak",       [SocialTag, MusicTag, LifestyleTag])
        , (mkName uniq "Zeta",      [SocialTag, QuizTag, TravelTag])
        , (mkName uniq "Zulu",      [SocialTag, MusicTag, LifestyleTag])
        ]

    mkName uniq n = Name (uniq <> n)

    mkNew new (n, t) = new { newServiceName = n
                           , newServiceTags = unsafeRange (Set.fromList t)
                           }
    select (Name prefix) nm = filter (isPrefixOf (toLower prefix) . toLower . fromName) nm

    emptyUpdateService = UpdateService
        { updateServiceName    = Nothing
        , updateServiceSummary = Nothing
        , updateServiceDescr   = Nothing
        , updateServiceAssets  = Nothing
        , updateServiceTags    = Nothing
        }

testDeleteService :: Maybe Config -> DB.ClientState -> Brig -> Http ()
testDeleteService config db brig = do
    prv <- randomProvider db brig
    let pid = providerId prv
    svc <- addGetService brig pid =<< defNewService config
    let sid = serviceId svc
    deleteService brig pid sid defProviderPassword !!!
        const 200 === statusCode
    getService brig pid sid !!!
        const 404 === statusCode
    uid <- randomId
    getServiceProfile brig uid pid sid !!!
        const 404 === statusCode

testAddRemoveBot :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBot config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    let pid = sref^.serviceRefProvider
    let sid = sref^.serviceRefId

    -- Prepare users
    u1 <- createUser "Ernie" "success@simulator.amazonses.com" brig
    u2 <- createUser "Bert"  "success@simulator.amazonses.com" brig
    let uid1 = userId u1
    let uid2 = userId u2
    h <- randomHandle

    putHandle brig uid1 h !!! const 200 === statusCode
    postConnection brig uid1 uid2 !!! const 201 === statusCode
    putConnection brig uid2 uid1 Accepted !!! const 200 === statusCode

    -- Create conversation
    _rs <- createConv galley uid1 [uid2] <!! const 201 === statusCode
    let Just cnv = decodeBody _rs
    let cid = cnvId cnv

    testAddRemoveBotUtil pid sid cid u1 u2 h sref buf brig galley cannon

testMessageBot :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBot config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    let pid = sref^.serviceRefProvider
    let sid = sref^.serviceRefId

    -- Prepare user with client
    usr <- createUser "User" "success@simulator.amazonses.com" brig
    let uid = userId usr
    let new = defNewClient PermanentClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    _rs <- addClient brig usr new <!! const 201 === statusCode
    let Just uc = clientId <$> decodeBody _rs

    -- Create conversation
    _rs <- createConv galley uid [] <!! const 201 === statusCode
    let Just cid = cnvId <$> decodeBody _rs

    testMessageBotUtil uid uc cid pid sid sref buf brig galley cannon

testAddRemoveBotTeam :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testAddRemoveBotTeam config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    (u1, u2, h, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
    let (uid1, uid2) = (userId u1, userId u2)
    -- Ensure cannot add bots to managed conversations
    cidFail <- Team.createManagedConv galley tid uid1 [uid2] Nothing
    addBot brig uid1 pid sid cidFail !!! do
        const 403 === statusCode
        const (Just "invalid-conversation") === fmap Error.label . decodeBody

    testAddRemoveBotUtil pid sid cid u1 u2 h sref buf brig galley cannon

testMessageBotTeam :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testMessageBotTeam config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    let pid = sref^.serviceRefProvider
    let sid = sref^.serviceRefId

    -- Prepare user with client
    usr <- createUser "User" "success@simulator.amazonses.com" brig
    let uid = userId usr
    let new = defNewClient PermanentClient [somePrekeys !! 0] (someLastPrekeys !! 0)
    _rs <- addClient brig usr new <!! const 201 === statusCode
    let Just uc = clientId <$> decodeBody _rs
    tid <- Team.createTeam uid galley

    -- Create conversation
    cid <- Team.createTeamConv galley tid uid [] Nothing

    testMessageBotUtil uid uc cid pid sid sref buf brig galley cannon

testDeleteConvBotTeam :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeleteConvBotTeam config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    -- Prepare users and the bot
    (u1, u2, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
    let (uid1, uid2) = (userId u1, userId u2)
    bid <- addBotConv brig cannon uid1 uid2 cid pid sid buf

    -- Delete the conversation and check that everyone is notified
    -- via an event, including the bot itself.
    WS.bracketR2 cannon uid1 uid2 $ \wss -> do
        -- 200 response on success
        Team.deleteTeamConv galley tid cid uid2
        -- Events for the users
        forM_ wss $ \ws -> wsAssertTeamConvDelete ws tid cid
        -- Event for the bot
        svcAssertConvDelete buf uid2 cid

    -- Check that the conversation no longer exists
    forM_ [uid1, uid2] $ \uid ->
        getConversation galley uid cid !!! const 404 === statusCode
    getBotConv galley bid cid !!! const 404 === statusCode

testDeleteTeamBotTeam :: Maybe Config -> FilePath -> DB.ClientState -> Brig -> Galley -> Cannon -> Http ()
testDeleteTeamBotTeam config crt db brig galley cannon = withTestService config crt db brig defServiceApp $ \sref buf -> do
    -- Prepare users and the bot
    (u1, u2, _, tid, cid, pid, sid) <- prepareBotUsersTeam brig galley sref
    let (uid1, uid2) = (userId u1, userId u2)
    bid <- addBotConv brig cannon uid1 uid2 cid pid sid buf

    -- Delete the team, and check that the bot (eventually)
    -- receives a notification via event
    Team.deleteTeam galley tid uid1
    -- NOTE: Due to the async nature of a team deletion, some
    -- events may or may not be sent (for instance, team members)
    -- leaving a conversation. Thus, we check _only_ for the relevant
    -- ones for the bot, which are the ConvDelete event
    svcAssertEventuallyConvDelete buf uid1 cid

    -- Wait until all users have been deleted (can take a while)
    forM_ [uid1, uid2] $ \uid -> do
        void $ retryWhileN 20 (/= Intra.Deleted) (getStatus brig uid)
        chkStatus brig uid Intra.Deleted
        getConversation galley uid cid !!! const 404 === statusCode
    -- Check the bot cannot see the conversation either
    getBotConv galley bid cid !!! const 404 === statusCode

--------------------------------------------------------------------------------
-- API Operations

registerProvider
    :: Brig
    -> NewProvider
    -> Http ResponseLBS
registerProvider brig new = post $ brig
    . path "/provider/register"
    . contentJson
    . body (RequestBodyLBS (encode new))

activateProvider
    :: Brig
    -> Code.Key
    -> Code.Value
    -> Http ResponseLBS
activateProvider brig key val = get $ brig
    . path "/provider/activate"
    . queryItem "key" (toByteString' key)
    . queryItem "code" (toByteString' val)

loginProvider
    :: Brig
    -> Email
    -> PlainTextPassword
    -> Http ResponseLBS
loginProvider brig email pw = post $ brig
    . path "/provider/login"
    . contentJson
    . body (RequestBodyLBS (encode (ProviderLogin email pw)))

updateProvider
    :: Brig
    -> ProviderId
    -> UpdateProvider
    -> Http ResponseLBS
updateProvider brig pid upd = put $ brig
    . path "/provider"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode upd))

updateProviderPassword
    :: Brig
    -> ProviderId
    -> PasswordChange
    -> Http ResponseLBS
updateProviderPassword brig pid upd = put $ brig
    . path "/provider/password"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode upd))

initiateEmailUpdateProvider
    :: Brig
    -> ProviderId
    -> EmailUpdate
    -> Http ResponseLBS
initiateEmailUpdateProvider brig pid upd = put $ brig
    . path "/provider/email"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode upd))

initiatePasswordResetProvider
    :: Brig
    -> PasswordReset
    -> Http ResponseLBS
initiatePasswordResetProvider brig npr = post $ brig
    . path "/provider/password-reset"
    . contentJson
    . body (RequestBodyLBS (encode npr))

completePasswordResetProvider
    :: Brig
    -> CompletePasswordReset
    -> Http ResponseLBS
completePasswordResetProvider brig e = post $ brig
    . path "/provider/password-reset/complete"
    . contentJson
    . body (RequestBodyLBS (encode e))

deleteProvider
    :: Brig
    -> ProviderId
    -> PlainTextPassword
    -> Http ResponseLBS
deleteProvider brig pid pw = delete $ brig
    . path "/provider"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode (DeleteProvider pw)))

getProvider
    :: Brig
    -> ProviderId
    -> Http ResponseLBS
getProvider brig pid = get $ brig
    . path "/provider"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)

getProviderProfile
    :: Brig
    -> ProviderId
    -> UserId
    -> Http ResponseLBS
getProviderProfile brig pid uid = get $ brig
    . paths ["providers", toByteString' pid]
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)

addService
    :: Brig
    -> ProviderId
    -> NewService
    -> Http ResponseLBS
addService brig pid new = post $ brig
    . path "/provider/services"
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode new))

getService
    :: Brig
    -> ProviderId
    -> ServiceId
    -> Http ResponseLBS
getService brig pid sid = get $ brig
    . paths ["provider", "services", toByteString' sid]
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)

getServiceProfile
    :: Brig
    -> UserId
    -> ProviderId
    -> ServiceId
    -> Http ResponseLBS
getServiceProfile brig uid pid sid = get $ brig
    . paths ["providers", toByteString' pid, "services", toByteString' sid]
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)

updateService
    :: Brig
    -> ProviderId
    -> ServiceId
    -> UpdateService
    -> Http ResponseLBS
updateService brig pid sid upd = put $ brig
    . paths ["provider", "services", toByteString' sid]
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode upd))

updateServiceConn
    :: Brig
    -> ProviderId
    -> ServiceId
    -> UpdateServiceConn
    -> Http ResponseLBS
updateServiceConn brig pid sid upd = put $ brig
    . paths ["provider", "services", toByteString' sid, "connection"]
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode upd))

deleteService
    :: Brig
    -> ProviderId
    -> ServiceId
    -> PlainTextPassword
    -> Http ResponseLBS
deleteService brig pid sid pw = delete $ brig
    . paths ["provider", "services", toByteString' sid]
    . header "Z-Type" "provider"
    . header "Z-Provider" (toByteString' pid)
    . contentJson
    . body (RequestBodyLBS (encode (DeleteService pw)))

listServiceProfilesByPrefix
    :: Brig
    -> UserId
    -> Name
    -> Int
    -> Http ResponseLBS
listServiceProfilesByPrefix brig uid start size = get $ brig
    . path "/services"
    . queryItem "start" (toByteString' start)
    . queryItem "size" (toByteString' size)
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)

listServiceProfilesByTag
    :: Brig
    -> UserId
    -> MatchAny
    -> Maybe Name
    -> Int
    -> Http ResponseLBS
listServiceProfilesByTag brig uid tags start size = get $ brig
    . path "/services"
    . queryItem "tags" (toByteString' cond)
    . maybe id (queryItem "start" . toByteString') start
    . queryItem "size" (toByteString' size)
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)
  where
    cond :: QueryAnyTags 1 3
    cond = fromMaybe (error "Too many tags in query")
         . queryAnyTags
         $ tags

addBot
    :: Brig
    -> UserId
    -> ProviderId
    -> ServiceId
    -> ConvId
    -> Http ResponseLBS
addBot brig uid pid sid cid = post $ brig
    . paths ["conversations", toByteString' cid, "bots"]
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)
    . header "Z-Connection" "conn"
    . contentJson
    . body (RequestBodyLBS (encode (AddBot pid sid Nothing)))

removeBot
    :: Brig
    -> UserId
    -> ConvId
    -> BotId
    -> Http ResponseLBS
removeBot brig uid cid bid = delete $ brig
    . paths ["conversations", toByteString' cid, "bots", toByteString' bid]
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' uid)
    . header "Z-Connection" "conn"

createConv
    :: Galley
    -> UserId
    -> [UserId]
    -> Http ResponseLBS
createConv g u us = post $ g
    . path "/conversations"
    . header "Z-User" (toByteString' u)
    . header "Z-Type" "access"
    . header "Z-Connection" "conn"
    . contentJson
    . body (RequestBodyLBS (encode (NewConvUnmanaged conv)))
  where
    conv = NewConv us Nothing Set.empty Nothing Nothing Nothing

postMessage
    :: Galley
    -> UserId
    -> ClientId
    -> ConvId
    -> [(UserId, ClientId, Text)]
    -> Http ResponseLBS
postMessage galley fromu fromc cid rcps = post $ galley
    . paths ["conversations", toByteString' cid, "otr", "messages"]
    . header "Z-Type" "access"
    . header "Z-User" (toByteString' fromu)
    . header "Z-Connection" "conn"
    . contentJson
    . body (RequestBodyLBS (encode (mkMessage fromc rcps)))

postBotMessage
    :: Galley
    -> BotId
    -> ClientId
    -> ConvId
    -> [(UserId, ClientId, Text)]
    -> Http ResponseLBS
postBotMessage galley fromb fromc cid rcps = post $ galley
    . path "/bot/messages"
    . header "Z-Type" "bot"
    . header "Z-Bot" (toByteString' fromb)
    . header "Z-Conversation" (toByteString' cid)
    . header "Z-Connection" "conn"
    . contentJson
    . body (RequestBodyLBS (encode (mkMessage fromc rcps)))

getBotConv
    :: Galley
    -> BotId
    -> ConvId
    -> Http ResponseLBS
getBotConv galley bid cid = get $ galley
    . path "/bot/conversation"
    . header "Z-Type" "bot"
    . header "Z-Bot" (toByteString' bid)
    . header "Z-Conversation" (toByteString' cid)

--------------------------------------------------------------------------------
-- DB Operations

lookupCode :: MonadIO m => DB.ClientState -> Code.Gen -> Code.Scope -> m (Maybe Code.Code)
lookupCode db gen = liftIO . DB.runClient db . Code.lookup (Code.genKey gen)

--------------------------------------------------------------------------------
-- Utilities

randomProvider :: HasCallStack => DB.ClientState -> Brig -> Http Provider
randomProvider db brig = do
    email <- mkSimulatorEmail "success"
    gen   <- Code.mkGen (Code.ForEmail email)
    -- Register
    let new = defNewProvider email
    _rs <- registerProvider brig new <!!
        const 201 === statusCode
    let Just pid = rsNewProviderId <$> decodeBody _rs
    -- Activate (auto-approval)
    Just vcode <- lookupCode db gen Code.IdentityVerification
    activateProvider brig (Code.codeKey vcode) (Code.codeValue vcode) !!!
        const 200 === statusCode
    -- Fetch
    _rs <- getProvider brig pid <!! const 200 === statusCode
    let Just prv = decodeBody _rs
    return prv

addGetService :: HasCallStack => Brig -> ProviderId -> NewService -> Http Service
addGetService brig pid new = do
    _rs <- addService brig pid new <!! const 201 === statusCode
    let Just srs = decodeBody _rs
    let sid = rsNewServiceId srs
    _rs <- getService brig pid sid <!! const 200 === statusCode
    let Just svc = decodeBody _rs
    return svc

enableService :: HasCallStack => Brig -> ProviderId -> ServiceId -> Http ()
enableService brig pid sid = do
    let upd = (mkUpdateServiceConn defProviderPassword)
            { updateServiceConnEnabled  = Just True
            }
    updateServiceConn brig pid sid upd !!!
        const 200 === statusCode

defNewService :: MonadIO m => Maybe Config -> m NewService
defNewService config = liftIO $ do
    key <- join $ optOrEnv (readServiceKey . publicKey) config readServiceKey "TEST_PUBKEY"
    return NewService
        { newServiceName    = defServiceName
        , newServiceSummary = unsafeRange defProviderSummary
        , newServiceDescr   = unsafeRange defServiceDescr
        , newServiceUrl     = defServiceUrl
        , newServiceKey     = key
        , newServiceToken   = Nothing
        , newServiceAssets  = defServiceAssets
        , newServiceTags    = defServiceTags
        }

defNewProvider :: Email -> NewProvider
defNewProvider email = NewProvider
    { newProviderEmail    = email
    , newProviderPassword = Just defProviderPassword
    , newProviderName     = defProviderName
    , newProviderUrl      = defProviderUrl
    , newProviderDescr    = unsafeRange defProviderDescr
    }

defProviderUrl :: HttpsUrl
defProviderUrl = fromJust (fromByteString "https://localhost/")

defProviderName :: Name
defProviderName = Name "Integration Test Provider"

defProviderSummary :: Text
defProviderSummary = "A short summary of the integration test provider"

defProviderDescr :: Text
defProviderDescr = "A long description of an integration test provider"

defProviderPassword :: PlainTextPassword
defProviderPassword = PlainTextPassword "password"

defServiceName :: Name
defServiceName = Name "Test Service"

defServiceDescr :: Text
defServiceDescr = "Test service description"

defServiceUrl :: HttpsUrl
defServiceUrl = fromJust (fromByteString "https://localhost/test")

defServiceTags :: Range 1 3 (Set ServiceTag)
defServiceTags = unsafeRange (Set.singleton SocialTag)

defServiceAssets :: [Asset]
defServiceAssets = [ImageAsset "key" (Just AssetComplete)]

-- TODO: defServiceToken :: ServiceToken

readServiceKey :: MonadIO m => FilePath -> m ServiceKeyPEM
readServiceKey fp = liftIO $ do
    bs <- BS.readFile fp
    let Right [k] = pemParseBS bs
    return (ServiceKeyPEM k)

randServiceKey :: MonadIO m => m ServiceKeyPEM
randServiceKey = liftIO $ do
    kp <- generateRSAKey' 4096 65537
    Right [k] <-  pemParseBS . C8.pack <$> writePublicKey kp
    return (ServiceKeyPEM k)

waitFor :: MonadIO m => Timeout -> (a -> Bool) -> m a -> m a
waitFor t f ma = do
    a <- ma
    if  | f a       -> return a
        | t <= 0    -> liftIO $ throwM TimedOut
        | otherwise -> do
            liftIO $ threadDelay (1 # Second)
            waitFor (t - 1 # Second) f ma

-- | Run a test case with an external service application.
withTestService
    :: Maybe Config
    -> FilePath
    -> DB.ClientState
    -> Brig
    -> (Chan e -> Application)
    -> (ServiceRef -> Chan e -> Http a)
    -> Http a
withTestService config crt db brig mkApp go = do
    sref <- registerService
    runService sref
  where
    h = fromMaybe "https://localhost" (encodeUtf8 . botHost <$> config)
    p = fromMaybe 9000 (botPort <$> config)
    registerService = do
        prv <- randomProvider db brig
        new <- defNewService config
        let Just url = fromByteString $ h <> ":" <> (C8.pack . show $ p)
        svc <- addGetService brig (providerId prv) (new { newServiceUrl = url })
        let pid = providerId prv
        let sid = serviceId svc
        enableService brig pid sid
        return (newServiceRef sid pid)

    runService sref = do
        key <- liftIO $ optOrEnv privateKey config id "TEST_KEY"
        let tlss = Warp.tlsSettings crt key
        let defs = Warp.defaultSettings { Warp.settingsPort = p }
        buf <- liftIO newChan
        srv <- liftIO . Async.async $
            Warp.runTLS tlss defs $
                mkApp buf
        go sref buf `finally` liftIO (Async.cancel srv)

data TestBot = TestBot
    { testBotId         :: !BotId
    , testBotClient     :: !ClientId
    , testBotConv       :: !Ext.BotConvView
    , testBotToken      :: !Text
    , testBotLastPrekey :: !LastPrekey
    , testBotPrekeys    :: ![Prekey]
    , testBotLocale     :: !Locale
    , testBotOrigin     :: !Ext.BotUserView
    } deriving (Eq, Show)

data TestBotEvent
    = TestBotCreated TestBot
    | TestBotMessage Event

-- TODO: Test that the authorization header is properly set
defServiceApp :: Chan TestBotEvent -> Application
defServiceApp buf = Wai.route
    [ ("/bots",                onBotCreate)
    , ("/bots/:bot/messages", onBotMessage)
    ]
  where
    onBotCreate _ rq k = do
        -- TODO: Match request method
        js <- strictRequestBody rq
        case eitherDecode js of
            Left  e   -> k $ responseLBS status400 [] (LC8.pack e)
            Right new -> do
                let pks = [somePrekeys !! 0]
                let lpk = someLastPrekeys !! 0
                let rsp = Ext.NewBotResponse
                        { Ext.rsNewBotPrekeys    = pks
                        , Ext.rsNewBotLastPrekey = lpk
                        , Ext.rsNewBotName       = Nothing -- TODO
                        , Ext.rsNewBotColour     = Nothing -- TODO
                        , Ext.rsNewBotAssets     = Nothing -- TODO
                        }
                let bot = TestBot
                        { testBotId         = Ext.newBotId new
                        , testBotClient     = Ext.newBotClient new
                        , testBotConv       = Ext.newBotConv new
                        , testBotToken      = Ext.newBotToken new
                        , testBotLastPrekey = lpk
                        , testBotPrekeys    = pks
                        , testBotLocale     = Ext.newBotLocale new
                        , testBotOrigin     = Ext.newBotOrigin new
                        }
                writeChan buf (TestBotCreated bot)
                k $ responseLBS status201 [] (encode rsp)

    onBotMessage _ rq k = do
        js <- strictRequestBody rq
        case eitherDecode js of
            Left  e -> k $ responseLBS status400 [] (LC8.pack e)
            Right ev -> do
                writeChan buf (TestBotMessage ev)
                k $ responseLBS status200 [] "success"

wsAssertMemberJoin :: MonadIO m => WS.WebSocket -> ConvId -> UserId -> [UserId] -> m ()
wsAssertMemberJoin ws conv usr new = void $ liftIO $
    WS.assertMatch (5 # Second) ws $ \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv      e @?= conv
        evtType      e @?= MemberJoin
        evtFrom      e @?= usr
        evtData      e @?= Just (EdMembers (Members new))

wsAssertMemberLeave :: MonadIO m => WS.WebSocket -> ConvId -> UserId -> [UserId] -> m ()
wsAssertMemberLeave ws conv usr old = void $ liftIO $
    WS.assertMatch (5 # Second) ws $ \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv      e @?= conv
        evtType      e @?= MemberLeave
        evtFrom      e @?= usr
        evtData      e @?= Just (EdMembers (Members old))

wsAssertTeamConvDelete :: MonadIO m => WS.WebSocket -> TeamId -> ConvId -> m ()
wsAssertTeamConvDelete ws tid conv = void $ liftIO $
    WS.assertMatch (5 # Second) ws $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        e^.(Team.eventType) @?= Team.ConvDelete
        e^.(Team.eventTeam) @?= tid
        e^.(Team.eventData) @?= Just (Team.EdConvDelete conv)

wsAssertMessage :: MonadIO m => WS.WebSocket -> ConvId -> UserId -> ClientId -> ClientId -> Text -> m ()
wsAssertMessage ws conv fromu fromc to txt = void $ liftIO $
    WS.assertMatch (5 # Second) ws $ \n -> do
        let e = List1.head (unpackEvents n)
        ntfTransient n @?= False
        evtConv      e @?= conv
        evtType      e @?= OtrMessageAdd
        evtFrom      e @?= fromu
        evtData      e @?= Just (EdOtrMessage (OtrMessage fromc to txt (Just "data")))

svcAssertMemberJoin :: MonadIO m => Chan TestBotEvent -> UserId -> [UserId] -> ConvId -> m ()
svcAssertMemberJoin buf usr new cnv = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotMessage e) -> do
            let msg = Members new
            assertEqual "event type" MemberJoin (evtType e)
            assertEqual "conv" cnv (evtConv e)
            assertEqual "user" usr (evtFrom e)
            assertEqual "event data" (Just (EdMembers msg)) (evtData e)
        _ -> assertFailure "Event timeout (TestBotMessage: member-join)"

svcAssertMemberLeave :: MonadIO m => Chan TestBotEvent -> UserId -> [UserId] -> ConvId -> m ()
svcAssertMemberLeave buf usr gone cnv = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotMessage e) -> do
            let msg = Members gone
            assertEqual "event type" MemberLeave (evtType e)
            assertEqual "conv" cnv (evtConv e)
            assertEqual "user" usr (evtFrom e)
            assertEqual "event data" (Just (EdMembers msg)) (evtData e)
        _ -> assertFailure "Event timeout (TestBotMessage: member-leave)"

svcAssertConvDelete :: MonadIO m => Chan TestBotEvent -> UserId -> ConvId -> m ()
svcAssertConvDelete buf usr cnv = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotMessage e) -> do
            assertEqual "event type" ConvDelete (evtType e)
            assertEqual "conv" cnv (evtConv e)
            assertEqual "user" usr (evtFrom e)
            assertEqual "event data" Nothing (evtData e)
        _ -> assertFailure "Event timeout (TestBotMessage: conv-delete)"

svcAssertBotCreated :: MonadIO m => Chan TestBotEvent -> BotId -> ConvId -> m TestBot
svcAssertBotCreated buf bid cid = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotCreated b) -> do
            assertEqual "bot ID" bid (testBotId   b)
            assertEqual "conv"   cid (testBotConv b ^. Ext.botConvId)
            -- TODO: Verify the conversation name
            -- TODO: Verify the list of members
            return b
        _ -> throwM $ HUnitFailure "Event timeout (TestBotCreated)"

svcAssertMessage :: MonadIO m => Chan TestBotEvent -> UserId -> OtrMessage -> ConvId -> m ()
svcAssertMessage buf from msg cnv = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotMessage e) -> do
            assertEqual "event type" OtrMessageAdd (evtType e)
            assertEqual "conv" cnv (evtConv e)
            assertEqual "user" from (evtFrom e)
            assertEqual "event data" (Just (EdOtrMessage msg)) (evtData e)
        _ -> assertFailure "Event timeout (TestBotMessage: otr-message-add)"

svcAssertEventuallyConvDelete :: MonadIO m => Chan TestBotEvent -> UserId -> ConvId -> m ()
svcAssertEventuallyConvDelete buf usr cnv = liftIO $ do
    evt <- timeout (5 # Second) $ readChan buf
    case evt of
        Just (TestBotMessage e) | evtType e == ConvDelete -> do
            assertEqual "event type" ConvDelete (evtType e)
            assertEqual "conv" cnv (evtConv e)
            assertEqual "user" usr (evtFrom e)
            assertEqual "event data" Nothing (evtData e)
        -- We ignore every other message type
        Just (TestBotMessage _) ->
            svcAssertEventuallyConvDelete buf usr cnv
        _ -> assertFailure "Event timeout (TestBotMessage: conv-delete)"

unpackEvents :: Notification -> List1 Event
unpackEvents = WS.unpackPayload

mkMessage :: ClientId -> [(UserId, ClientId, Text)] -> Value
mkMessage fromc rcps = object
    [ "sender"     .= fromc
    , "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mk rcps)
    , "data"       .= Just ("data" :: Text)
    ]
  where
    mk (u, c, m) = (text u, HashMap.singleton (text c) m)

    text :: (FromByteString a, ToByteString a) => a -> Text
    text = fromJust . fromByteString . toByteString'

testAddRemoveBotUtil :: ProviderId
                     -> ServiceId
                     -> ConvId
                     -> User
                     -> User
                     -> Text
                     -> ServiceRef
                     -> Chan TestBotEvent
                     -> Brig
                     -> Galley
                     -> WS.Cannon
                     -> Http ()
testAddRemoveBotUtil pid sid cid u1 u2 h sref buf brig galley cannon = do
    let uid1 = userId u1
    let uid2 = userId u2
    -- Add the bot and check that everyone is notified via an event,
    -- including the bot itself.
    (rs, bot) <- WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
        _rs <- addBot brig uid1 pid sid cid <!! const 201 === statusCode
        let Just rs = decodeBody _rs
        let bid = rsAddBotId rs
        bot <- svcAssertBotCreated buf bid cid
        liftIO $ assertEqual "bot client" (rsAddBotClient rs) (testBotClient bot)
        liftIO $ assertEqual "bot event" MemberJoin (evtType (rsAddBotEvent rs))
        -- Member join event for both users
        forM_ [ws1, ws2] $ \ws -> wsAssertMemberJoin ws cid uid1 [botUserId bid]
        -- Member join event for the bot
        svcAssertMemberJoin buf uid1 [botUserId bid] cid
        return (rs, bot)

    let bid  = rsAddBotId rs
    let buid = botUserId bid

    -- Check that the bot token grants access to the right user and conversation
    let Just tok = fromByteString (Text.encodeUtf8 (testBotToken bot))
    liftIO $ do
        assertEqual "principal"    bid (BotId (Id (tok^.ZAuth.body.ZAuth.bot)))
        assertEqual "conversation" cid (Id (tok^.ZAuth.body.ZAuth.conv))
        assertEqual "provider"     pid (Id (tok^.ZAuth.body.ZAuth.prov))

    let u1Handle = Ext.botUserViewHandle $ testBotOrigin bot

    -- Check that the preferred locale defaults to the locale of the
    -- user who requsted the bot.
    liftIO $ assertEqual "locale" (userLocale u1) (testBotLocale bot)
    liftIO $ assertEqual "handle" (Just (Handle h)) u1Handle

    -- Check that the bot has access to the conversation
    getBotConv galley bid cid !!! const 200 === statusCode

    -- Check that the bot user exists and can be identified as a bot
    _rs <- getUser brig uid1 buid <!! const 200 === statusCode
    let Just bp = decodeBody _rs
    liftIO $ do
        assertEqual "service" (Just sref) (profileService bp)
        assertEqual "name" defServiceName (profileName bp)
        assertEqual "colour" defaultAccentId (profileAccentId bp)
        assertEqual "assets" defServiceAssets (profileAssets bp)

    -- Check that the bot client exists and has prekeys
    let isBotPrekey = (`elem` testBotPrekeys bot) . prekeyData
    getPreKey brig buid (rsAddBotClient rs) !!! do
        const 200 === statusCode
        const (Just True) === fmap isBotPrekey . decodeBody

    -- Remove the bot and check that everyone is notified via an event,
    -- including the bot itself.
    WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
        -- 200 response with event on success
        _rs <- removeBot brig uid2 cid bid <!! const 200 === statusCode
        let Just ev = rsRemoveBotEvent <$> decodeBody _rs
        liftIO $ assertEqual "bot event" MemberLeave (evtType ev)
        -- Events for both users
        forM_ [ws1, ws2] $ \ws -> wsAssertMemberLeave ws cid uid2 [buid]
        -- Event for the bot
        svcAssertMemberLeave buf uid2 [buid] cid
        -- Empty 204 response if the bot is not in the conversation
        removeBot brig uid2 cid bid !!! const 204 === statusCode

    -- Check that the bot no longer has access to the conversation
    getBotConv galley bid cid !!! const 404 === statusCode

testMessageBotUtil :: UserId
                   -> ClientId
                   -> ConvId
                   -> ProviderId
                   -> ServiceId
                   -> ServiceRef
                   -> Chan TestBotEvent
                   -> Brig
                   -> Galley
                   -> WS.Cannon
                   -> Http ()
testMessageBotUtil uid uc cid pid sid sref buf brig galley cannon = do
    -- Add bot to conversation
    _rs <- addBot brig uid pid sid cid <!! const 201 === statusCode
    let Just ars = decodeBody _rs
    let bid  = rsAddBotId ars
    let buid = botUserId bid
    let bc   = rsAddBotClient ars
    _ <- svcAssertBotCreated buf bid cid
    svcAssertMemberJoin buf uid [buid] cid

    -- The bot can now fetch the conversation
    _rs <- getBotConv galley bid cid <!! const 200 === statusCode
    let Just bcnv = decodeBody _rs
    liftIO $ do
        assertEqual "id" cid (bcnv^.Ext.botConvId)
        assertEqual "members" [OtherMember uid Nothing] (bcnv^.Ext.botConvMembers)

    -- The user can identify the bot in the member list
    Just mems <- fmap cnvMembers . decodeBody <$> getConversation galley uid cid
    let other = listToMaybe (cmOthers mems)
    liftIO $ do
        assertEqual "id" (Just buid) (omId <$> other)
        assertEqual "service" (Just sref) (omService =<< other)

    -- The bot greets the user
    WS.bracketR cannon uid $ \ws -> do
        postBotMessage galley bid bc cid [(uid, uc, "Hi User!")] !!!
            const 201 === statusCode
        wsAssertMessage ws cid buid bc uc "Hi User!"

    -- The user replies
    postMessage galley uid uc cid [(buid, bc, "Hi Bot")] !!!
        const 201 === statusCode
    let msg = OtrMessage uc bc "Hi Bot" (Just "data")
    svcAssertMessage buf uid msg cid

    -- Remove the entire service; existing bots should remain where they are.
    deleteService brig pid sid defProviderPassword !!!
        const 200 === statusCode
    _im <- isMember galley buid cid
    liftIO $ assertBool "bot is not a member" _im

    -- Writing another message triggers orphaned bots to be auto-removed due
    -- to the service being gone.
    WS.bracketR cannon uid $ \ws -> do
        postMessage galley uid uc cid [(buid, bc, "Still there?")] !!!
            const 201 === statusCode
        _ <- waitFor (5 # Second) not (isMember galley buid cid)
        getBotConv galley bid cid !!!
            const 404 === statusCode
        wsAssertMemberLeave ws cid buid [buid]

prepareBotUsersTeam :: HasCallStack
                    => Brig
                    -> Galley
                    -> ServiceRef
                    -> Http (User, User, Text, TeamId, ConvId, ProviderId, ServiceId)
prepareBotUsersTeam brig galley sref = do
    let pid = sref^.serviceRefProvider
    let sid = sref^.serviceRefId

    -- Prepare users
    u1 <- createUser "Ernie" "success@simulator.amazonses.com" brig
    u2 <- createUser "Bert"  "success@simulator.amazonses.com" brig
    let uid1 = userId u1
    let uid2 = userId u2
    h <- randomHandle
    putHandle brig uid1 h !!! const 200 === statusCode

    tid <- Team.createTeam uid1 galley
    Team.addTeamMember galley tid $ Team.newNewTeamMember $ Team.newTeamMember uid2 Team.fullPermissions

    -- Create conversation
    cid <- Team.createTeamConv galley tid uid1 [uid2] Nothing

    return (u1, u2, h, tid, cid, pid, sid)

addBotConv :: HasCallStack
           => Brig
           -> WS.Cannon
           -> UserId
           -> UserId
           -> ConvId
           -> ProviderId
           -> ServiceId
           -> Chan TestBotEvent
           -> Http BotId
addBotConv brig cannon uid1 uid2 cid pid sid buf =
    -- Add the bot and check that everyone is notified via an event,
    -- including the bot itself.
    WS.bracketR2 cannon uid1 uid2 $ \(ws1, ws2) -> do
        _rs <- addBot brig uid1 pid sid cid <!! const 201 === statusCode
        let Just rs = decodeBody _rs
        let bid = rsAddBotId rs
        bot <- svcAssertBotCreated buf bid cid
        liftIO $ assertEqual "bot client" (rsAddBotClient rs) (testBotClient bot)
        liftIO $ assertEqual "bot event" MemberJoin (evtType (rsAddBotEvent rs))
        -- Member join event for both users
        forM_ [ws1, ws2] $ \ws -> wsAssertMemberJoin ws cid uid1 [botUserId bid]
        -- Member join event for the bot
        svcAssertMemberJoin buf uid1 [botUserId bid] cid
        return (rsAddBotId rs)
