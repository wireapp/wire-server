module API.User (tests, ConnectionLimit (..)) where

import Imports
import API.User.Util
import Bilge hiding (accept, timeout)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty hiding (Timeout)
import Util
import Util.Options.Common

import qualified API.User.Account
import qualified API.User.Auth
import qualified API.User.Client
import qualified API.User.Connection
import qualified API.User.Handles
import qualified API.User.Onboarding
import qualified API.User.PasswordReset
import qualified API.User.Property
import qualified API.User.RichInfo

import qualified Brig.AWS     as AWS
import qualified Brig.Options as Opt
import qualified Brig.ZAuth   as ZAuth

tests :: Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> Nginz -> AWS.Env -> IO TestTree
tests conf p b c ch g n aws = do
    let cl = ConnectionLimit $ Opt.setUserMaxConnections (Opt.optSettings conf)
    let at = Opt.setActivationTimeout (Opt.optSettings conf)
    z  <- mkZAuthEnv (Just conf)
    return $ testGroup "user"
        [ API.User.Client.tests        cl at conf p b c g
        , API.User.Account.tests       cl at conf p b c ch g aws
        , API.User.Auth.tests          conf p z b g n
        , API.User.Connection.tests    cl at conf p b c g
        , API.User.Handles.tests       cl at conf p b c g
        , API.User.Onboarding.tests    cl at conf p b c g
        , API.User.PasswordReset.tests cl at conf p b c g
        , API.User.Property.tests      cl at conf p b c g
        , API.User.RichInfo.tests      cl at conf p b c g
        ]

mkZAuthEnv :: Maybe Opt.Opts -> IO ZAuth.Env
mkZAuthEnv config = do
    Just (sk :| sks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PRIVKEYS"
    Just (pk :| pks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PUBKEYS"
    ZAuth.mkEnv (sk :| sks) (pk :| pks) ZAuth.defSettings
