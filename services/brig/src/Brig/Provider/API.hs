{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.Provider.API (routes) where

import Brig.App (settings)
import Brig.API.Error
import Brig.API.Handler
import Brig.Email (mkEmailKey, validateEmail)
import Brig.Options (Settings (..))
import Brig.Password
import Brig.Provider.DB (ServiceConn (..))
import Brig.Provider.Email
import Brig.Types.Intra (UserAccount (..), AccountStatus (..))
import Brig.Types.Client
import Brig.Types.User
import Brig.Types.Provider
import Control.Lens (view)
import Control.Exception.Enclosed (handleAny)
import Control.Monad (join, when, unless, (>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.ByteString.Conversion
import Data.Foldable (forM_)
import Data.Hashable (hash)
import Data.Id
import Data.Int
import Data.List1 (List1 (..))
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Misc (Fingerprint (..), Rsa)
import Data.Predicate
import Data.Range
import Galley.Types (Conversation (..), ConvType (..), ConvMembers (..))
import Galley.Types (OtherMember (..))
import Galley.Types (Event, userClients)
import Galley.Types.Bot (newServiceRef)
import Data.Traversable (forM)
import Network.HTTP.Types.Status
import Network.Wai (Request, Response)
import Network.Wai.Predicate (contentType, accept, request, query, def, opt)
import Network.Wai.Routing
import Network.Wai.Utilities.Error ((!>>))
import Network.Wai.Utilities.Response (json, empty, setStatus, addHeader)
import Network.Wai.Utilities.ZAuth
import OpenSSL.Random (randBytes)

import qualified Brig.API.Client              as Client
import qualified Brig.Code                    as Code
import qualified Web.Cookie                   as Cookie
import qualified Brig.Data.User               as User
import qualified Brig.Data.Client             as User
import qualified Brig.IO.Intra                as RPC
import qualified Brig.Provider.DB             as DB
import qualified Brig.Provider.RPC            as RPC
import qualified Brig.Types.Provider.External as Ext
import qualified Data.ByteString.Lazy.Char8   as LC8
import qualified Data.List                    as List
import qualified Data.Map.Strict              as Map
import qualified Data.Swagger.Build.Api       as Doc
import qualified Data.Text.Ascii              as Ascii
import qualified OpenSSL.PEM                  as SSL
import qualified OpenSSL.RSA                  as SSL
import qualified OpenSSL.EVP.Digest           as SSL
import qualified OpenSSL.EVP.PKey             as SSL
import qualified Network.HTTP.Client.OpenSSL  as SSL
import qualified Data.Text.Encoding           as Text
import qualified Network.Wai.Utilities.Error  as Wai
import qualified Brig.ZAuth                   as ZAuth


routes :: Routes Doc.ApiBuilder Handler ()
routes = do

    -- Public API --------------------------------------------------------------

    post "/provider/register" (continue newAccount) $
        contentType "application" "json"
        .&> accept "application" "json"
        .&> request

    get "/provider/activate" (continue activateAccountKey) $
        accept "application" "json"
        .&> query "key"
        .&. query "code"

    get "/provider/approve" (continue approveAccountKey) $
        accept "application" "json"
        .&> query "key"
        .&. query "code"

    post "/provider/login" (continue login) $
        contentType "application" "json"
        .&> request

    -- Provider API ------------------------------------------------------------

    delete "/provider" (continue deleteAccount) $
        contentType "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. request

    put "/provider" (continue updateAccountProfile) $
        contentType "application" "json"
        .&> accept "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. request

-- TODO
--    put "/provider/email" (continue changeEmail) $
--        contentType "application" "json"
--        .&. zauthProvider
--        .&. request

-- TODO
--    put "/provider/password" (continue changePassword) $
--        accept "application" "json"
--        .&. contentType "application" "json"
--        .&. zauthProvider
--        .&. request

    get "/provider" (continue getAccount) $
        accept "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId

    post "/provider/services" (continue addService) $
        accept "application" "json"
        .&> contentType "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. request

    get "/provider/services" (continue listServices) $
        accept "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId

    get "/provider/services/:sid" (continue getService) $
        accept "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. capture "sid"

    put "/provider/services/:sid" (continue updateService) $
        contentType "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. capture "sid"
        .&. request

    put "/provider/services/:sid/connection" (continue updateServiceConn) $
        contentType "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. capture "sid"
        .&. request

-- TODO
--     post "/provider/services/:sid/token" (continue genServiceToken) $
--         accept "application" "json"
--         .&. zauthProvider

    delete "/provider/services/:sid" (continue deleteService) $
        contentType "application" "json"
        .&> zauth ZAuthProvider
        .&> zauthProviderId
        .&. capture "sid"
        .&. request

    -- User API ----------------------------------------------------------------

    get "/providers/:pid" (continue getProviderProfile) $
        accept "application" "json"
        .&> zauth ZAuthAccess
        .&> capture "pid"

    get "/providers/:pid/services" (continue listServiceProfiles) $
        accept "application" "json"
        .&> zauth ZAuthAccess
        .&> capture "pid"

    get "/providers/:pid/services/:sid" (continue getServiceProfile) $
        accept "application" "json"
        .&> zauth ZAuthAccess
        .&> capture "pid"
        .&. capture "sid"

    get "/services" (continue listServiceProfilesByTag) $
        accept "application" "json"
        .&> zauth ZAuthAccess
        .&> query "tags"
        .&. opt (query "start")
        .&. def (unsafeRange 20) (query "size")

    get "/services/tags" (continue getServiceTagList) $
        accept "application" "json"
        .&> zauth ZAuthAccess

    post "/conversations/:cnv/bots" (continue addBot) $
        contentType "application" "json"
        .&> accept "application" "json"
        .&> zauth ZAuthAccess
        .&> zauthUserId
        .&. zauthConnId
        .&. capture "cnv"
        .&. request

    delete "/conversations/:cnv/bots/:bot" (continue removeBot) $
        zauth ZAuthAccess
        .&> zauthUserId
        .&. zauthConnId
        .&. capture "cnv"
        .&. capture "bot"

    -- Bot API -----------------------------------------------------------------

    get "/bot/self" (continue botGetSelf) $
        accept "application" "json"
        .&> zauth ZAuthBot
        .&> zauthBotId

    delete "/bot/self" (continue botDeleteSelf) $
        zauth ZAuthBot
        .&> zauthBotId
        .&. zauthConvId

    get "/bot/client/prekeys" (continue botListPrekeys) $
        accept "application" "json"
        .&> zauth ZAuthBot
        .&> zauthBotId

    post "/bot/client/prekeys" (continue botUpdatePrekeys) $
        contentType "application" "json"
        .&> zauth ZAuthBot
        .&> zauthBotId
        .&. request

    get "/bot/client" (continue botGetClient) $
        contentType "application" "json"
        .&> zauth ZAuthBot
        .&> zauthBotId

    post "/bot/users/prekeys" (continue botClaimUsersPrekeys) $
        accept "application" "json"
        .&> contentType "application" "json"
        .&> zauth ZAuthBot
        .&> request

    get "/bot/users" (continue botListUserProfiles) $
        accept "application" "json"
        .&> zauth ZAuthBot
        .&> query "ids"

    get "/bot/users/:uid/clients" (continue botGetUserClients) $
        accept "application" "json"
        .&> zauth ZAuthBot
        .&> capture "uid"

--------------------------------------------------------------------------------
-- Public API (Unauthenticated)

newAccount :: Request -> Handler Response
newAccount req = do
    new <- parseJsonBody req

    email <- case validateEmail (newProviderEmail new) of
        Just em -> return em
        Nothing -> throwStd invalidEmail

    let name  = newProviderName new
    let pass  = newProviderPassword new
    let descr = newProviderDescr new
    let url   = newProviderUrl new

    let emailKey = mkEmailKey email
    DB.lookupKey emailKey >>= mapM_ (const $ throwStd emailExists)

    (safePass, newPass) <- case pass of
        Just newPass -> (, Nothing) <$> mkSafePassword newPass
        Nothing -> do
            newPass  <- genPassword
            safePass <- mkSafePassword newPass
            return (safePass, Just newPass)

    pid <- DB.insertAccount name safePass url descr

    gen  <- Code.mkGen (Code.ForEmail email)
    code <- Code.generate gen Code.IdentityVerification
                (Code.Retries 3)
                (Code.Timeout (3600 * 24)) -- 24h
                (Just (toUUID pid))
    Code.insert code

    let key = Code.codeKey code
    let val = Code.codeValue code

    lift $ sendActivationMail name email key val

    return $ setStatus status201 $ json (NewProviderResponse pid newPass)

activateAccountKey :: Code.Key ::: Code.Value -> Handler Response
activateAccountKey (key ::: val) = do
    c <- Code.verify key Code.IdentityVerification val >>= maybeInvalidCode
    (pid, email) <- case (Code.codeAccount c, Code.codeForEmail c) of
        (Just p, Just e) -> return (Id p, e)
        _                -> throwStd invalidCode
    (name, memail, _url, _descr) <- DB.lookupAccountData pid >>= maybeInvalidCode
    case memail of
        Just email' | email == email' -> return $ setStatus status204 empty
        Just _ -> do
            activate pid email
            return $ json (ProviderActivationResponse email)
        -- Immediate approval for everybody (for now).
        Nothing -> do
            activate pid email
            lift $ sendApprovalConfirmMail name email
            return $ json (ProviderActivationResponse email)

approveAccountKey :: Code.Key ::: Code.Value -> Handler Response
approveAccountKey (key ::: val) = do
    c <- Code.verify key Code.AccountApproval val >>= maybeInvalidCode
    case (Code.codeAccount c, Code.codeForEmail c) of
        (Just pid, Just email) -> do
            (name, _, _, _) <- DB.lookupAccountData (Id pid) >>= maybeInvalidCode
            activate (Id pid) email
            lift $ sendApprovalConfirmMail name email
            return empty
        _ -> throwStd invalidCode

login :: Request -> Handler Response
login req = do
    l    <- parseJsonBody req
    pid  <- DB.lookupKey (mkEmailKey (providerLoginEmail l)) >>= maybeBadCredentials
    pass <- DB.lookupPassword pid >>= maybeBadCredentials
    unless (verifyPassword (providerLoginPassword l) pass) $
        throwStd badCredentials
    tok <- ZAuth.newProviderToken pid
    setProviderCookie tok empty

--------------------------------------------------------------------------------
-- Provider API

getAccount :: ProviderId -> Handler Response
getAccount pid = do
    mp <- DB.lookupAccount pid
    return $ case mp of
        Just  p -> json p
        Nothing -> setStatus status404 empty

updateAccountProfile :: ProviderId ::: Request -> Handler Response
updateAccountProfile (pid ::: req) = do
    _   <- DB.lookupAccount pid >>= maybeInvalidProvider
    upd <- parseJsonBody req
    DB.updateAccountProfile pid
        (updateProviderName upd)
        (updateProviderUrl upd)
        (updateProviderDescr upd)
    return empty

addService :: ProviderId ::: Request -> Handler Response
addService (pid ::: req) = do
    new <- parseJsonBody req
    _   <- DB.lookupAccount pid >>= maybeInvalidProvider

    let name    = newServiceName new
    let descr   = newServiceDescr new
    let baseUrl = newServiceUrl new
    let pubkey  = newServiceKey new
    let assets  = newServiceAssets new
    let tags    = fromRange (newServiceTags new)

    (pk, fp) <- validateServiceKey pubkey >>= maybeInvalidServiceKey
    token    <- maybe randServiceToken return (newServiceToken new)
    sid      <- DB.insertService pid name descr baseUrl token pk fp assets tags

    let rstoken = maybe (Just token) (const Nothing) (newServiceToken new)
    return $ setStatus status201
           $ json (NewServiceResponse sid rstoken)

listServices :: ProviderId -> Handler Response
listServices pid = json <$> DB.listServices pid

getService :: ProviderId ::: ServiceId -> Handler Response
getService (pid ::: sid) = do
    s <- DB.lookupService pid sid >>= maybeServiceNotFound
    return (json s)

updateService :: ProviderId ::: ServiceId ::: Request -> Handler Response
updateService (pid ::: sid ::: req) = do
    upd <- parseJsonBody req
    _   <- DB.lookupAccount pid >>= maybeInvalidProvider

    -- Update service profile
    svc <- DB.lookupService pid sid >>= maybeServiceNotFound
    let newName   = updateServiceName upd
    let newDescr  = updateServiceDescr upd
    let newAssets = updateServiceAssets upd
    let newTags   = updateServiceTags upd
    DB.updateService pid sid newName newDescr newAssets newTags

    -- Update tag index
    let name  = serviceName svc
    let tags  = unsafeRange (serviceTags svc)
    let name' = fromMaybe name newName
    let tags' = fromMaybe tags newTags
    when (serviceEnabled svc) $
        DB.updateServiceTags pid sid (name, rcast tags) (name', rcast tags')

    return empty

updateServiceConn :: ProviderId ::: ServiceId ::: Request -> Handler Response
updateServiceConn (pid ::: sid ::: req) = do
    upd <- parseJsonBody req

    pass <- DB.lookupPassword pid >>= maybeBadCredentials
    unless (verifyPassword (updateServiceConnPassword upd) pass) $
        throwStd badCredentials

    scon <- DB.lookupServiceConn pid sid >>= maybeServiceNotFound

    let newBaseUrl = updateServiceConnUrl upd
    let newTokens  = List1 <$> (nonEmpty . fromRange =<< updateServiceConnTokens upd)
    let newEnabled = updateServiceConnEnabled upd
    let newKeyPems = fromRange <$> updateServiceConnKeys upd
    keys <- forM newKeyPems (mapM (validateServiceKey >=> maybeInvalidServiceKey))
    let newKeys = List1 <$> (keys >>= nonEmpty)
    let newFps  = fmap snd <$> newKeys

    DB.updateServiceConn pid sid newBaseUrl newTokens newKeys newEnabled

    let scon' = scon
              { sconBaseUrl      = fromMaybe (sconBaseUrl scon) newBaseUrl
              , sconAuthTokens   = fromMaybe (sconAuthTokens scon) newTokens
              , sconFingerprints = fromMaybe (sconFingerprints scon) newFps
              , sconEnabled      = fromMaybe (sconEnabled scon) newEnabled
              }

    when (sconEnabled scon || sconEnabled scon') $ do
        lift $ RPC.setServiceConn scon'
        -- If the service got enabled or disabled, update the tag index.
        unless (sconEnabled scon && sconEnabled scon') $ do
            svc <- DB.lookupServiceProfile pid sid >>= maybeServiceNotFound
            let name = serviceProfileName svc
            let tags = unsafeRange (serviceProfileTags svc)
            if sconEnabled scon
                then DB.deleteServiceTags pid sid name tags
                else DB.insertServiceTags pid sid name tags

    -- TODO: Send informational email to provider.

    return empty

deleteService :: ProviderId ::: ServiceId ::: Request -> Handler Response
deleteService (pid ::: sid ::: req) = do
    del  <- parseJsonBody req
    pass <- DB.lookupPassword pid >>= maybeBadCredentials
    unless (verifyPassword (deleteServicePassword del) pass) $
        throwStd badCredentials
    svc  <- DB.lookupService pid sid >>= maybeServiceNotFound
    let tags = unsafeRange (serviceTags svc)
    DB.deleteServiceTags pid sid (serviceName svc) tags
    lift $ RPC.removeServiceConn pid sid
    DB.deleteService pid sid
    return empty

deleteAccount :: ProviderId ::: Request -> Handler Response
deleteAccount (pid ::: req) = do
    del  <- parseJsonBody req
    prov <- DB.lookupAccount pid >>= maybeInvalidProvider
    pass <- DB.lookupPassword pid >>= maybeBadCredentials
    unless (verifyPassword (deleteProviderPassword del) pass) $
        throwStd badCredentials
    svcs <- DB.listServices pid
    forM_ svcs $ \svc -> do
        let sid  = serviceId svc
        let tags = unsafeRange (serviceTags svc)
        DB.deleteServiceTags pid sid (serviceName svc) tags
        lift $ RPC.removeServiceConn pid sid
        DB.deleteService pid sid
    DB.deleteKey (mkEmailKey (providerEmail prov))
    DB.deleteAccount pid
    return empty

--------------------------------------------------------------------------------
-- User API

getProviderProfile :: ProviderId -> Handler Response
getProviderProfile pid = do
    p <- DB.lookupAccountProfile pid >>= maybeProviderNotFound
    return (json p)

listServiceProfiles :: ProviderId -> Handler Response
listServiceProfiles pid = do
    ss <- DB.listServiceProfiles pid
    return (json ss)

getServiceProfile :: ProviderId ::: ServiceId -> Handler Response
getServiceProfile (pid ::: sid) = do
    s <- DB.lookupServiceProfile pid sid >>= maybeServiceNotFound
    return (json s)

listServiceProfilesByTag :: QueryAnyTags 1 3 ::: Maybe Name ::: Range 10 100 Int32 -> Handler Response
listServiceProfilesByTag (tags ::: start ::: size) = do
    let size' = fromRange size
    ss <- DB.paginateServiceTags tags start size'
    return (json ss)

getServiceTagList :: () -> Handler Response
getServiceTagList _ = return (json (ServiceTagList allTags))
  where
    allTags = [(minBound :: ServiceTag) ..]

addBot :: UserId ::: ConnId ::: ConvId ::: Request -> Handler Response
addBot (zuid ::: zcon ::: cid ::: req) = do
    add  <- parseJsonBody req
    zusr <- lift (User.lookupUser zuid) >>= maybeInvalidUser

    let pid = addBotProvider add
    let sid = addBotService add

    -- Get the conversation and check preconditions
    cnv <- lift (RPC.getConv zuid cid) >>= maybeConvNotFound
    let mems = cnvMembers cnv
    unless (cnvType cnv == RegularConv) $
        throwStd invalidConv
    unless (length (cmOthers mems) < 127) $
        throwStd tooManyMembers

    -- Lookup the relevant service data
    scon <- DB.lookupServiceConn pid sid >>= maybeServiceNotFound
    unless (sconEnabled scon) $
        throwStd serviceDisabled
    svp <- DB.lookupServiceProfile pid sid >>= maybeServiceNotFound

    -- Prepare a user ID, client ID and token for the bot.
    bid <- BotId <$> randomId
    btk <- Text.decodeLatin1 . toByteString' <$> ZAuth.newBotToken pid bid cid
    let bcl = newClientId (fromIntegral (hash bid))

    -- Ask the external service to create a bot
    let origmem = OtherMember zuid Nothing
    let members = origmem : (cmOthers mems)
    let bcnv    = Ext.botConvView (cnvId cnv) (cnvName cnv) members
    let busr    = mkBotUserView zusr
    let bloc    = fromMaybe (userLocale zusr) (addBotLocale add)
    let botReq  = Ext.NewBotRequest bid bcl busr bcnv btk bloc
    rs <- RPC.createBot scon botReq !>> StdError . serviceError

    -- Insert the bot user and client
    locale <- setDefaultLocale <$> view settings
    let name   = fromMaybe (serviceProfileName   svp) (Ext.rsNewBotName   rs)
    let assets = fromMaybe (serviceProfileAssets svp) (Ext.rsNewBotAssets rs)
    let colour = fromMaybe defaultAccentId            (Ext.rsNewBotColour rs)
    let pict   = Pict [] -- Legacy
    let sref   = newServiceRef sid pid
    let usr    = User (botUserId bid) Nothing name pict assets colour False locale (Just sref) Nothing
    let newClt = (newClient PermanentClient (Ext.rsNewBotLastPrekey rs) ())
               { newClientPrekeys = Ext.rsNewBotPrekeys rs
               }
    lift $ User.insertAccount (UserAccount usr Active) Nothing True
    (clt, _, _) <- User.addClient (botUserId bid) bcl newClt Nothing Nothing
                   !>> const (StdError badGateway) -- MalformedPrekeys

    -- Add the bot to the conversation
    ev <- lift $ RPC.addBotMember zuid zcon cid bid (clientId clt) pid sid

    return $ setStatus status201 $ json AddBotResponse
        { rsAddBotId     = bid
        , rsAddBotClient = bcl
        , rsAddBotName   = name
        , rsAddBotColour = colour
        , rsAddBotAssets = assets
        , rsAddBotEvent  = ev
        }

removeBot :: UserId ::: ConnId ::: ConvId ::: BotId -> Handler Response
removeBot (zusr ::: zcon ::: cid ::: bid) = do
    -- Get the conversation and check preconditions
    cnv <- lift (RPC.getConv zusr cid) >>= maybeConvNotFound
    let mems = cnvMembers cnv
    unless (cnvType cnv == RegularConv) $
        throwStd invalidConv

    -- Find the bot in the member list and delete it
    let busr = botUserId bid
    let bot = List.find ((== busr) . omId) (cmOthers mems)
    case bot >>= omService of
        Nothing -> return (setStatus status204 empty)
        Just  _ -> do
            ev <- deleteBot zusr (Just zcon) bid cid
            return $ case ev of
                Just  e -> json (RemoveBotResponse e)
                Nothing -> setStatus status204 empty

--------------------------------------------------------------------------------
-- Bot API

botGetSelf :: BotId -> Handler Response
botGetSelf bot = do
    p <- lift $ User.lookupUser (botUserId bot)
    maybe (throwStd userNotFound) (return . json . publicProfile) p

botGetClient :: BotId -> Handler Response
botGetClient bot = do
    c <- lift $ listToMaybe <$> User.lookupClients (botUserId bot)
    maybe (throwStd clientNotFound) (return . json) c

botListPrekeys :: BotId -> Handler Response
botListPrekeys bot = do
    clt <- lift $ listToMaybe <$> User.lookupClients (botUserId bot)
    case clientId <$> clt of
        Nothing -> return $ json ([] :: [PrekeyId])
        Just ci -> json <$> lift (User.lookupPrekeyIds (botUserId bot) ci)

botUpdatePrekeys :: BotId ::: Request -> Handler Response
botUpdatePrekeys (bot ::: req) = do
    upd <- parseJsonBody req
    clt <- lift $ listToMaybe <$> User.lookupClients (botUserId bot)
    case clt of
        Nothing -> throwStd clientNotFound
        Just  c -> do
            let pks = updateBotPrekeyList upd
            User.updatePrekeys (botUserId bot) (clientId c) pks !>> clientDataError
    return empty

botClaimUsersPrekeys :: Request -> Handler Response
botClaimUsersPrekeys req = do
    body <- parseJsonBody req
    when (Map.size (userClients body) > 128) $
        throwStd tooManyClients
    json <$> lift (Client.claimMultiPrekeyBundles body)

botListUserProfiles :: List UserId -> Handler Response
botListUserProfiles uids = do
    us <- lift $ User.lookupUsers (fromList uids)
    return (json (map mkBotUserView us))

botGetUserClients :: UserId -> Handler Response
botGetUserClients uid =
    json <$> lift (fmap pubClient <$> User.lookupClients uid)
  where
    pubClient c = PubClient (clientId c) (clientClass c)

botDeleteSelf :: BotId ::: ConvId -> Handler Response
botDeleteSelf (bid ::: cid) = do
    bot <- lift $ User.lookupUser (botUserId bid)
    _   <- maybeInvalidBot (userService =<< bot)
    _   <- deleteBot (botUserId bid) Nothing bid cid
    return empty

--------------------------------------------------------------------------------
-- Utilities

minRsaKeySize :: Int
minRsaKeySize = 256 -- Bytes (= 2048 bits)

activate :: ProviderId -> Email -> Handler ()
activate pid email = do
    let emailKey = mkEmailKey email
    taken <- maybe False (/= pid) <$> DB.lookupKey emailKey
    when taken $
        throwStd emailExists
    DB.insertKey pid emailKey

deleteBot :: UserId -> Maybe ConnId -> BotId -> ConvId -> Handler (Maybe Event)
deleteBot zusr zcon bid cid = do
    -- Remove the bot from the conversation
    ev <- lift $ RPC.removeBotMember zusr zcon cid bid
    -- Delete the bot user and client
    let buid = botUserId bid
    lift $ User.lookupClients buid >>= mapM_ (User.rmClient buid . clientId)
    -- TODO: Consider if we can actually delete the bot user entirely,
    -- i.e. not just marking the account as deleted.
    lift $ User.updateStatus buid Deleted
    return ev

validateServiceKey :: MonadIO m => ServiceKeyPEM -> m (Maybe (ServiceKey, Fingerprint Rsa))
validateServiceKey pem = liftIO $ readPublicKey >>= \pk ->
    case join (SSL.toPublicKey <$> pk) of
        Nothing  -> return Nothing
        Just pk' -> do
            Just sha <- SSL.getDigestByName "SHA256"
            let size = SSL.rsaSize (pk' :: SSL.RSAPubKey)
            if size < minRsaKeySize
                then return Nothing
                else do
                    fpr <- Fingerprint <$> SSL.rsaFingerprint sha pk'
                    let bits = fromIntegral size * 8
                    let key = ServiceKey RsaServiceKey bits pem
                    return $ Just (key, fpr)
  where
    readPublicKey = handleAny
        (const $ return Nothing)
        (SSL.readPublicKey (LC8.unpack (toByteString pem)) >>= return . Just)

mkBotUserView :: User -> Ext.BotUserView
mkBotUserView u = Ext.BotUserView
    { Ext.botUserViewId     = userId u
    , Ext.botUserViewName   = userName u
    , Ext.botUserViewColour = userAccentId u
    , Ext.botUserViewHandle = userHandle u
    }

setProviderCookie :: ZAuth.ProviderToken -> Response -> Handler Response
setProviderCookie t r = do
    s <- view settings
    let hdr = toByteString' (Cookie.renderSetCookie (cookie s))
    return (addHeader "Set-Cookie" hdr r)
  where
    cookie s = Cookie.def
        { Cookie.setCookieName     = "zprovider"
        , Cookie.setCookieValue    = toByteString' t
        , Cookie.setCookieDomain   = Just $ Text.encodeUtf8 . setCookieDomain $ s
        , Cookie.setCookiePath     = Just "/provider"
        , Cookie.setCookieExpires  = Just (ZAuth.tokenExpiresUTC t)
        , Cookie.setCookieSecure   = not (setCookieInsecure s)
        , Cookie.setCookieHttpOnly = True
        }

maybeInvalidProvider :: Maybe a -> Handler a
maybeInvalidProvider = maybe (throwStd invalidProvider) return

maybeInvalidCode :: Maybe a -> Handler a
maybeInvalidCode = maybe (throwStd invalidCode) return

maybeServiceNotFound :: Maybe a -> Handler a
maybeServiceNotFound = maybe (throwStd (notFound "Service not found")) return

maybeProviderNotFound :: Maybe a -> Handler a
maybeProviderNotFound = maybe (throwStd (notFound "Provider not found")) return

maybeConvNotFound :: Maybe a -> Handler a
maybeConvNotFound = maybe (throwStd (notFound "Conversation not found")) return

maybeBadCredentials :: Maybe a -> Handler a
maybeBadCredentials = maybe (throwStd badCredentials) return

maybeInvalidServiceKey :: Maybe a -> Handler a
maybeInvalidServiceKey = maybe (throwStd invalidServiceKey) return

maybeInvalidBot :: Maybe a -> Handler a
maybeInvalidBot = maybe (throwStd invalidBot) return

maybeInvalidUser :: Maybe a -> Handler a
maybeInvalidUser = maybe (throwStd invalidUser) return

invalidServiceKey :: Wai.Error
invalidServiceKey = Wai.Error status400 "invalid-service-key" "Invalid service key."

invalidProvider :: Wai.Error
invalidProvider = Wai.Error status403 "invalid-provider" "The provider does not exist."

invalidBot :: Wai.Error
invalidBot = Wai.Error status403 "invalid-bot" "The targeted user is not a bot."

invalidConv :: Wai.Error
invalidConv = Wai.Error status403 "invalid-conversation" "The operation is not allowed in this conversation."

badGateway :: Wai.Error
badGateway = Wai.Error status502 "bad-gateway" "The upstream service returned an invalid response."

tooManyMembers :: Wai.Error
tooManyMembers = Wai.Error status403 "too-many-members" "Maximum number of members per conversation reached."

tooManyBots :: Wai.Error
tooManyBots = Wai.Error status409 "too-many-bots" "Maximum number of bots for the service reached."

serviceDisabled :: Wai.Error
serviceDisabled = Wai.Error status403 "service-disabled" "The desired service is currently disabled."

serviceError :: RPC.ServiceError -> Wai.Error
serviceError RPC.ServiceUnavailable = badGateway
serviceError RPC.ServiceBotConflict = tooManyBots

randServiceToken :: MonadIO m => m ServiceToken
randServiceToken = ServiceToken . Ascii.encodeBase64Url <$> liftIO (randBytes 18)

