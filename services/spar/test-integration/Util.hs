{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | TODO: this is all copied from /services/galley/test/integration/API/Util.hs and some other
-- places; should we make this a new library?
module Util where

import Bilge
import Bilge.Assert
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson as Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Data.Maybe (isJust)
import Data.Range
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Lens.Micro
import Spar.API ()
import Test.Hspec
import Util.Options

import qualified Brig.Types.Common as Brig
import qualified Galley.Types.Teams as Galley


data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { brig      :: Endpoint
  , galley    :: Endpoint
  , spar      :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

type Brig = Request -> Request
type Galley = Request -> Request

randomUser :: (HasCallStack, MonadCatch m, MonadHttp m, MonadIO m) => Brig -> m UserId
randomUser b = do
    e <- liftIO randomEmail
    let p = object [ "name" .= Brig.fromEmail e, "email" .= Brig.fromEmail e, "password" .= ("secret" :: ST) ]
    r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createTeam :: (HasCallStack, MonadCatch m, MonadHttp m, MonadIO m) => Galley -> ST -> UserId -> m TeamId
createTeam g name owner = do
    let mems :: [Galley.TeamMember] = []
    let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
    let nt_ = Galley.NonBindingNewTeam $ Galley.newNewTeam (unsafeRange name) (unsafeRange "icon") & Galley.newTeamMembers .~ mm
    resp <- post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt_) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    fromBS (getHeader' "Location" resp)

randomEmail :: MonadIO m => m Brig.Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Brig.Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

fromBS :: (HasCallStack, FromByteString a, Monad m) => SBS -> m a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: SBS -> Request -> Request
zConn = header "Z-Connection"

zType :: SBS -> Request -> Request
zType = header "Z-Type"
