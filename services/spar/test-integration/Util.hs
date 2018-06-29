{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | TODO: this is all copied from /services/galley/test/integration/API/Util.hs and some other
-- places; should we make this a new library?
module Util
  ( TestEnv(..), teMgr, teBrig, teGalley, teSpar, teNewIdp
  , Select, mkEnv
  , IntegrationConfig(..)
  , Brig
  , Galley
  , Spar
  , ResponseLBS
  , createUserWithTeam
  , createRandomPhoneUser
  , zUser
  ) where

import Bilge
import Bilge.Assert ((!!!), (===), (<!!))
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.Lens as Aeson
import Data.ByteString.Conversion
import Data.Id
import Data.Maybe
import Data.Misc (PlainTextPassword(..))
import Data.Range
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Data.UUID.V4 as UUID (nextRandom)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro
import Lens.Micro.TH
import Spar.API ()
import Spar.Types
import System.Random (randomRIO)
import Util.Options

import qualified Brig.Types.Activation as Brig
import qualified Brig.Types.User as Brig
import qualified Brig.Types.User.Auth as Brig
import qualified Data.Text.Ascii as Ascii
import qualified Galley.Types.Teams as Galley


-- things only used by spar so far.

data TestEnv = TestEnv
  { _teMgr    :: Manager
  , _teBrig   :: Brig
  , _teGalley :: Galley
  , _teSpar   :: Spar
  , _teNewIdp :: NewIdP
  }

type Select = TestEnv -> (Request -> Request)

mkEnv :: IntegrationConfig -> IO TestEnv
mkEnv opts = do
  mgr :: Manager <- newManager defaultManagerSettings
  let mkreq :: (IntegrationConfig -> Endpoint) -> (Request -> Request)
      mkreq selector = Bilge.host (selector opts ^. epHost . to cs)
                     . Bilge.port (selector opts ^. epPort)
  pure $ TestEnv mgr (mkreq brig) (mkreq galley) (mkreq spar) (cnfnewidp opts)


-- things copied from brig integration tests

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { brig      :: Endpoint
  , galley    :: Endpoint
  , spar      :: Endpoint
  , cnfnewidp :: NewIdP
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

type Brig = Request -> Request
type Galley = Request -> Request
type Spar = Request -> Request


type ResponseLBS = Response (Maybe LBS)

createUserWithTeam :: (HasCallStack, MonadHttp m, MonadIO m) => Brig -> Galley -> m (UserId, TeamId)
createUserWithTeam brg gly = do
    e <- randomEmail
    n <- pure ("randomName" :: String)  -- TODO!
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= n
            , "email"           .= Brig.fromEmail e
            , "password"        .= defPassword
            , "team"            .= newTeam
            ]
    bdy <- decodeBody <$> post (brg . path "/i/users" . contentJson . body p)
    let (Just uid, Just (Just tid)) = (Brig.userId <$> bdy, Brig.userTeam <$> bdy)
    (team:_) <- (^. Galley.teamListTeams) <$> getTeams uid gly
    () <- Control.Exception.assert {- "Team ID in registration and team table do not match" -} (tid ==  team ^. Galley.teamId)
          $ pure ()
    selfTeam <- Brig.userTeam . Brig.selfUser <$> getSelfProfile brg uid
    () <- Control.Exception.assert {- "Team ID in self profile and team table do not match" -} (selfTeam == Just tid)
          $ pure ()
    return (uid, tid)

createRandomPhoneUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => Brig -> m (UserId, Brig.Phone)
createRandomPhoneUser brig_ = do
    usr <- randomUser brig_
    let uid = Brig.userId usr
    phn <- liftIO randomPhone
    -- update phone
    let phoneUpdate = RequestBodyLBS . encode $ Brig.PhoneUpdate phn
    put (brig_ . path "/self/phone" . contentJson . zUser uid . zConn "c" . body phoneUpdate) !!!
        (const 202 === statusCode)
    -- activate
    act <- getActivationCode brig_ (Right phn)
    case act of
        Nothing -> liftIO . throwIO $ ErrorCall "missing activation key/code"
        Just kc -> activate brig_ kc !!! const 200 === statusCode
    -- check new phone
    get (brig_ . path "/self" . zUser uid) !!! do
        const 200 === statusCode
        const (Just phn) === (Brig.userPhone <=< decodeBody)

    return (uid, phn)

decodeBody :: (HasCallStack, FromJSON a) => ResponseLBS -> Maybe a
decodeBody = responseBody >=> decode'

getTeams :: (HasCallStack, MonadHttp m, MonadIO m) => UserId -> Galley -> m Galley.TeamList
getTeams u gly = do
    r <- get ( gly
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ fromMaybe (error "getTeams: failed to parse response") (decodeBody r)

getSelfProfile :: (HasCallStack, MonadHttp m, MonadIO m) => Brig -> UserId -> m Brig.SelfProfile
getSelfProfile brg usr = do
    rsp <- get $ brg . path "/self" . zUser usr
    return $ fromMaybe (error $ "getSelfProfile: failed to decode: " ++ show rsp) (decodeBody rsp)

zAuthAccess :: UserId -> SBS -> Request -> Request
zAuthAccess u c = header "Z-Type" "access" . zUser u . zConn c

newTeam :: Galley.BindingNewTeam
newTeam = Galley.BindingNewTeam $ Galley.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")

randomEmail :: MonadIO m => m Brig.Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Brig.Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

randomPhone :: MonadIO m => m Brig.Phone
randomPhone = liftIO $ do
    nrs <- map show <$> replicateM 14 (randomRIO (0,9) :: IO Int)
    let phone = Brig.parsePhone . cs $ "+0" ++ concat nrs
    return $ fromMaybe (error "Invalid random phone#") phone

randomUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m) => Brig -> m Brig.User
randomUser brig_ = do
    let n = "randomName"  -- TODO: see above
    createUser n "success@simulator.amazonses.com" brig_

createUser :: (HasCallStack, MonadCatch m, MonadIO m, MonadHttp m)
           => ST -> ST -> Brig -> m Brig.User
createUser name email brig_ = do
    r <- postUser name (Just email) Nothing Nothing brig_ <!! const 201 === statusCode
    return $ fromMaybe (error "createUser: failed to parse response") (decodeBody r)

-- more flexible variant of 'createUser' (see above).
postUser :: (HasCallStack, MonadIO m, MonadHttp m)
         => ST -> Maybe ST -> Maybe Brig.UserSSOId -> Maybe TeamId -> Brig -> m ResponseLBS
postUser name email ssoid teamid brig_ = do
    email' <- maybe (pure Nothing) (fmap (Just . Brig.fromEmail) . mkEmailRandomLocalSuffix) email
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= name
            , "email"           .= email'
            , "password"        .= defPassword
            , "cookie"          .= defCookieLabel
            , "sso_id"          .= ssoid
            , "team_id"         .= teamid
            ]
    post (brig_ . path "/i/users" . contentJson . body p)

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

defCookieLabel :: Brig.CookieLabel
defCookieLabel = Brig.CookieLabel "auth"

mkEmailRandomLocalSuffix :: MonadIO m => ST -> m Brig.Email
mkEmailRandomLocalSuffix e = do
    uid <- liftIO UUID.nextRandom
    case Brig.parseEmail e of
        Just (Brig.Email loc dom) -> return $ Brig.Email (loc <> "+" <> UUID.toText uid) dom
        Nothing              -> fail $ "Invalid email address: " ++ cs e

getActivationCode :: (HasCallStack, MonadIO m, MonadHttp m)
                  => Brig -> Either Brig.Email Brig.Phone -> m (Maybe (Brig.ActivationKey, Brig.ActivationCode))
getActivationCode brig_ ep = do
    let qry = either (queryItem "email" . toByteString') (queryItem "phone" . toByteString') ep
    r <- get $ brig_ . path "/i/users/activation-code" . qry
    let lbs   = fromMaybe "" $ responseBody r
    let akey  = Brig.ActivationKey  . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "key"  . Aeson._String)
    let acode = Brig.ActivationCode . Ascii.unsafeFromText <$> (lbs ^? Aeson.key "code" . Aeson._String)
    return $ (,) <$> akey <*> acode

activate :: (HasCallStack, MonadIO m, MonadHttp m)
         => Brig -> Brig.ActivationPair -> m ResponseLBS
activate brig_ (k, c) = get $ brig_
    . path "activate"
    . queryItem "key" (toByteString' k)
    . queryItem "code" (toByteString' c)

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: SBS -> Request -> Request
zConn = header "Z-Connection"


-- TH

makeLenses ''TestEnv
