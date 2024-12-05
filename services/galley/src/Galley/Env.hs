{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.Range
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
import OpenSSL.Session as Ssl
import Ssl.Util
import System.Logger
import Util.Options
import Wire.API.MLS.Keys
import Wire.API.Team.Member
import Wire.NotificationSubsystem.Interpreter
import Wire.RateLimit.Interpreter (RateLimitEnv)

data DeleteItem = TeamItem TeamId UserId (Maybe ConnId)
  deriving (Eq, Ord, Show)

-- | Main application environment.
data Env = Env
  { _reqId :: RequestId,
    _options :: Opts,
    _applog :: Logger,
    _manager :: Manager,
    _http2Manager :: Http2Manager,
    _federator :: Maybe Endpoint, -- FUTUREWORK: should we use a better type here? E.g. to avoid fresh connections all the time?
    _brig :: Endpoint, -- FUTUREWORK: see _federator
    _cstate :: ClientState,
    _deleteQueue :: Q.Queue DeleteItem,
    _extEnv :: ExtEnv,
    _aEnv :: Maybe Aws.Env,
    _mlsKeys :: Maybe (MLSKeysByPurpose MLSPrivateKeys),
    _rabbitmqChannel :: Maybe (MVar Q.Channel),
    _convCodeURI :: Either HttpsUrl (Map Text HttpsUrl),
    _passwordHashingRateLimitEnv :: RateLimitEnv
  }

-- | Environment specific to the communication with external
-- service providers.
data ExtEnv = ExtEnv
  { _extGetManager :: (Manager, [Fingerprint Rsa] -> Ssl.SSL -> IO ())
  }

makeLenses ''Env

makeLenses ''ExtEnv

-- TODO: somewhat duplicates Brig.App.initExtGetManager
initExtEnv :: IO ExtEnv
initExtEnv = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx Ssl.VerifyNone
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetDefaultVerifyPaths ctx
  mgr <-
    newManager
      (opensslManagerSettings (pure ctx))
        { managerResponseTimeout = responseTimeoutMicro 10000000,
          managerConnCount = 100
        }
  Just sha <- getDigestByName "SHA256"
  pure $ ExtEnv (mgr, mkVerify sha)
  where
    mkVerify sha fprs =
      let pinset = map toByteString' fprs
       in verifyRsaFingerprint sha pinset

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

currentFanoutLimit :: Opts -> Range 1 HardTruncationLimit Int32
currentFanoutLimit o = do
  let optFanoutLimit = fromIntegral . fromRange $ fromMaybe defaultFanoutLimit (o ^. (O.settings . maxFanoutSize))
  let maxSize = fromIntegral (o ^. (O.settings . maxTeamSize))
  unsafeRange (min maxSize optFanoutLimit)

notificationSubsystemConfig :: Env -> NotificationSubsystemConfig
notificationSubsystemConfig env =
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
