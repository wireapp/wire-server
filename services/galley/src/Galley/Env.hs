{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Galley.Env where

import Cassandra
import Control.Lens hiding ((.=))
import Data.Byteable (constEqBytes)
import Data.Id
import Data.Metrics.Middleware
import Data.Misc (Fingerprint, HttpsUrl, Rsa, fingerprintBytes)
import Data.Range
import Data.Time (getCurrentTime)
import Data.Time.Clock.DiffTime (millisecondsToDiffTime)
import Galley.Aws qualified as Aws
import Galley.Options
import Galley.Options qualified as O
import Galley.Queue qualified as Q
import HTTP2.Client.Manager (Http2Manager)
import Imports
import Network.AMQP qualified as Q
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.EVP.Digest
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Verify (VerifyStatus (..))
import OpenSSL.RSA
import OpenSSL.Session as Ssl
import OpenSSL.X509
import OpenSSL.X509.Store
import Ssl.Util
import System.Logger
import Util.Options
import Wire.API.MLS.Credential
import Wire.API.MLS.Keys
import Wire.API.Team.Member
import Wire.NotificationSubsystem.Interpreter

data DeleteItem = TeamItem TeamId UserId (Maybe ConnId)
  deriving (Eq, Ord, Show)

-- | Main application environment.
data Env = Env
  { _reqId :: RequestId,
    _monitor :: Metrics,
    _options :: Opts,
    _applog :: Logger,
    _manager :: Manager,
    _http2Manager :: Http2Manager,
    _federator :: Maybe Endpoint, -- FUTUREWORK: should we use a better type here? E.g. to avoid fresh connections all the time?
    _brig :: Endpoint, -- FUTUREWORK: see _federator
    _cstate :: ClientState,
    _deleteQueue :: Q.Queue DeleteItem,
    _extGetManager :: [Fingerprint Rsa] -> IO Manager,
    _aEnv :: Maybe Aws.Env,
    _mlsKeys :: SignaturePurpose -> MLSKeys,
    _rabbitmqChannel :: Maybe (MVar Q.Channel),
    _convCodeURI :: Either HttpsUrl (Map Text HttpsUrl)
  }

makeLenses ''Env

-- TODO: somewhat duplicates Brig.App.initExtGetManager
initExtEnv :: [Fingerprint Rsa] -> IO Manager
initExtEnv fingerprints = do
  ctx <- Ssl.context
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetDefaultVerifyPaths ctx
  Ssl.contextSetVerificationMode
    ctx
    Ssl.VerifyPeer
      { vpFailIfNoPeerCert = True,
        vpClientOnce = False,
        vpCallback = Just $ const extEnvCallback
      }
  newManager
    (opensslManagerSettings (pure ctx))
      { managerResponseTimeout = responseTimeoutMicro 10000000,
        managerConnCount = 100
      }
  where
    extEnvCallback :: X509StoreCtx -> IO Bool
    extEnvCallback store = do
      Just sha <- getDigestByName "SHA256"
      cert <- getStoreCtxCert store
      pk <- getPublicKey cert
      case toPublicKey @RSAPubKey pk of
        Nothing -> pure False
        Just k -> do
          fp <- rsaFingerprint sha k
          -- find at least one matching fingerprint to continue
          if not (any (constEqBytes fp . fingerprintBytes) fingerprints)
            then pure False
            else do
              -- Check if the certificate is self-signed.
              self <- verifyX509 cert pk
              if (self /= VerifySuccess)
                then pure False
                else do
                  -- For completeness, perform a date check as well.
                  now <- getCurrentTime
                  notBefore <- getNotBefore cert
                  notAfter <- getNotAfter cert
                  pure (now >= notBefore && now <= notAfter)

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

currentFanoutLimit :: Opts -> Range 1 HardTruncationLimit Int32
currentFanoutLimit o = do
  let optFanoutLimit = fromIntegral . fromRange $ fromMaybe defaultFanoutLimit (o ^. (O.settings . maxFanoutSize))
  let maxSize = fromIntegral (o ^. (O.settings . maxTeamSize))
  unsafeRange (min maxSize optFanoutLimit)

notificationSubssystemConfig :: Env -> NotificationSubsystemConfig
notificationSubssystemConfig env =
  NotificationSubsystemConfig
    { chunkSize = defaultChunkSize,
      fanoutLimit = currentFanoutLimit env._options,
      slowPushDelay =
        maybe
          defaultSlowPushDelay
          (millisecondsToDiffTime . toInteger)
          (env ^. options . O.settings . deleteConvThrottleMillis),
      requestId = env ^. reqId
    }
