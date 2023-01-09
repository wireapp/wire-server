{-# LANGUAGE StrictData #-}
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

module CargoHold.Options where

import Amazonka (S3AddressingStyle (..))
import qualified CargoHold.CloudFront as CF
import Control.Lens hiding (Level)
import Data.Aeson (FromJSON (..), withText)
import Data.Aeson.TH
import Data.Domain
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common
import Wire.API.Routes.Version

-- | AWS CloudFront settings.
data CloudFrontOpts = CloudFrontOpts
  { -- | Domain
    _cfDomain :: CF.Domain,
    -- | Keypair ID
    _cfKeyPairId :: CF.KeyPairId,
    -- | Path to private key
    _cfPrivateKey :: FilePath
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CloudFrontOpts

makeLenses ''CloudFrontOpts

newtype OptS3AddressingStyle = OptS3AddressingStyle
  { unwrapS3AddressingStyle :: S3AddressingStyle
  }
  deriving (Show)

instance FromJSON OptS3AddressingStyle where
  parseJSON =
    withText "S3AddressingStyle" $
      fmap OptS3AddressingStyle . \case
        "auto" -> pure S3AddressingStyleAuto
        "path" -> pure S3AddressingStylePath
        "virtual" -> pure S3AddressingStyleVirtual
        other -> fail $ "invalid S3AddressingStyle: " <> show other

data AWSOpts = AWSOpts
  { _awsS3Endpoint :: !AWSEndpoint,
    -- | S3 can either by addressed in path style, i.e.
    -- https://<s3-endpoint>/<bucket-name>/<object>, or vhost style, i.e.
    -- https://<bucket-name>.<s3-endpoint>/<object>. AWS's S3 offering has
    -- deprecated path style addressing for S3 and completely disabled it for
    -- buckets created after 30 Sep 2020:
    -- https://aws.amazon.com/blogs/aws/amazon-s3-path-deprecation-plan-the-rest-of-the-story/
    --
    -- However other object storage providers (specially self-deployed ones like
    -- MinIO) may not support vhost style addressing yet (or ever?). Users of
    -- such buckets should configure this option to "path".
    --
    -- Installations using S3 service provided by AWS, should use "auto", this
    -- option will ensure that vhost style is only used when it is possible to
    -- construct a valid hostname from the bucket name and the bucket name
    -- doesn't contain a '.'. Having a '.' in the bucket name causes TLS
    -- validation to fail, hence it is not used by default.
    --
    -- Using "virtual" as an option is only useful in situations where vhost
    -- style addressing must be used even if it is not possible to construct a
    -- valid hostname from the bucket name or the S3 service provider can ensure
    -- correct certificate is issued for bucket which contain one or more '.'s
    -- in the name.
    --
    -- When this option is unspecified, we default to path style addressing to
    -- ensure smooth transition for older deployments.
    _awsS3AddressingStyle :: !(Maybe OptS3AddressingStyle),
    -- | S3 endpoint for generating download links. Useful if Cargohold is configured to use
    -- an S3 replacement running inside the internal network (in which case internally we
    -- would use one hostname for S3, and when generating an asset link for a client app, we
    -- would use another hostname).
    _awsS3DownloadEndpoint :: !(Maybe AWSEndpoint),
    -- | S3 bucket name
    _awsS3Bucket :: !Text,
    -- | Enable this option for compatibility with specific S3 backends.
    _awsS3Compatibility :: !(Maybe S3Compatibility),
    -- | AWS CloudFront options
    _awsCloudFront :: !(Maybe CloudFrontOpts)
  }
  deriving (Show, Generic)

data S3Compatibility
  = -- | Scality RING, might also work for Zenko CloudServer
    -- <https://www.scality.com/products/ring/>
    S3CompatibilityScalityRing
  deriving (Eq, Show)

instance FromJSON S3Compatibility where
  parseJSON = withText "S3Compatibility" $ \case
    "scality-ring" -> pure S3CompatibilityScalityRing
    other -> fail $ "invalid S3Compatibility: " <> show other

deriveFromJSON toOptionFieldName ''AWSOpts

makeLenses ''AWSOpts

data Settings = Settings
  { -- | Maximum allowed size for uploads, in bytes
    _setMaxTotalBytes :: !Int,
    -- | TTL for download links, in seconds
    _setDownloadLinkTTL :: !Word,
    -- | FederationDomain is required, even when not wanting to federate with other backends
    -- (in that case the 'allowedDomains' can be set to empty in Federator)
    -- Federation domain is used to qualify local IDs and handles,
    -- e.g. 0c4d8944-70fa-480e-a8b7-9d929862d18c@wire.com and somehandle@wire.com.
    -- It should also match the SRV DNS records under which other wire-server installations can find this backend:
    --    _wire-server-federator._tcp.<federationDomain>
    -- Once set, DO NOT change it: if you do, existing users may have a broken experience and/or stop working
    -- Remember to keep it the same in Galley and in Brig.
    -- This is referred to as the 'backend domain' in the public documentation; See
    -- https://docs.wire.com/how-to/install/configure-federation.html#choose-a-backend-domain-name
    _setFederationDomain :: !Domain,
    _setDisabledAPIVersions :: !(Maybe (Set Version))
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

-- | Options consist of information the server needs to operate, and 'Settings'
-- modify the behavior.
data Opts = Opts
  { -- | Hostname and port to bind to
    _optCargohold :: !Endpoint,
    _optAws :: !AWSOpts,
    _optSettings :: !Settings,
    -- | Federator endpoint
    _optFederator :: Maybe Endpoint,
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
