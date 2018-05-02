{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module API.User (tests, ConnectionLimit (..)) where

import API.User.Util
import Bilge hiding (accept, timeout)
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty hiding (Timeout)
import Util
import Util.Options.Common

import qualified API.User.Account
import qualified API.User.Auth
import qualified API.User.Client
import qualified API.User.Connection
import qualified API.User.Handles
import qualified API.User.Invitation
import qualified API.User.Onboarding
import qualified API.User.PasswordReset
import qualified API.User.Property

import qualified Brig.AWS     as AWS
import qualified Brig.Options as Opt
import qualified Brig.ZAuth   as ZAuth

tests :: Maybe Opt.Opts -> Manager -> Brig -> Cannon -> CargoHold -> Galley -> Maybe AWS.Env -> IO TestTree
tests conf p b c ch g localAWS = do
    cl <- optOrEnv (ConnectionLimit . Opt.setUserMaxConnections . Opt.optSettings) conf (ConnectionLimit . read) "USER_CONNECTION_LIMIT"
    at <- optOrEnv (Opt.setActivationTimeout . Opt.optSettings)                    conf read                     "USER_ACTIVATION_TIMEOUT"
    z  <- mkZAuthEnv conf
    return $ testGroup "user"
        [ API.User.Account.tests       cl at conf p b c ch g localAWS
        , API.User.Auth.tests          conf p z b
        , API.User.Client.tests        cl at conf p b c g localAWS
        , API.User.Connection.tests    cl at conf p b c g localAWS
        , API.User.Handles.tests       cl at conf p b c g localAWS
        , API.User.Invitation.tests    cl at conf p b c g localAWS
        , API.User.Onboarding.tests    cl at conf p b c g localAWS
        , API.User.PasswordReset.tests cl at conf p b c g localAWS
        , API.User.Property.tests      cl at conf p b c g localAWS
        ]

mkZAuthEnv :: Maybe Opt.Opts -> IO ZAuth.Env
mkZAuthEnv config = do
    Just (sk :| sks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PRIVKEYS"
    Just (pk :| pks) <- join $ optOrEnv (ZAuth.readKeys . Opt.privateKeys . Opt.zauth) config ZAuth.readKeys "ZAUTH_PUBKEYS"
    ZAuth.mkEnv (sk :| sks) (pk :| pks) ZAuth.defSettings
