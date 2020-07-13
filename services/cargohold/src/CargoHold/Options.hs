{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module CargoHold.Options where

import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Lens hiding (Level)
import Data.Aeson.TH
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

-- | AWS CloudFront settings.
data CloudFrontOpts = CloudFrontOpts
  { -- | Domain
    _cfDomain :: Domain,
    -- | Keypair ID
    _cfKeyPairId :: KeyPairId,
    -- | Path to private key
    _cfPrivateKey :: FilePath
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CloudFrontOpts

makeLenses ''CloudFrontOpts

data AWSOpts = AWSOpts
  { _awsS3Endpoint :: !AWSEndpoint,
    -- | S3 endpoint for generating download links. Useful if Cargohold is configured to use
    -- an S3 replacement running inside the internal network (in which case internally we
    -- would use one hostname for S3, and when generating an asset link for a client app, we
    -- would use another hostname).
    _awsS3DownloadEndpoint :: !(Maybe AWSEndpoint),
    -- | S3 bucket name
    _awsS3Bucket :: !Text,
    -- | AWS CloudFront options
    _awsCloudFront :: !(Maybe CloudFrontOpts)
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts

makeLenses ''AWSOpts

data Settings = Settings
  { -- | Maximum allowed size for uploads, in bytes
    _setMaxTotalBytes :: !Int,
    -- | TTL for download links, in seconds
    _setDownloadLinkTTL :: !Word
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

data Opts = Opts
  { -- | Hostname and port to bind to
    _optCargohold :: !Endpoint,
    _optAws :: !AWSOpts,
    _optSettings :: !Settings,
    -- Logging

    -- | Log level (Debug, Info, etc)
    _optLogLevel :: !Level,
    -- | Use netstrings encoding:
    --   <http://cr.yp.to/proto/netstrings.txt>
    _optLogNetStrings :: !(Maybe (Last Bool)),
    _optLogFormat :: !(Maybe (Last LogFormat)) --- ^ Log format
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts
