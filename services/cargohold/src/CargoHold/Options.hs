{-# LANGUAGE StrictData #-}

module CargoHold.Options where

import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Lens hiding (Level)
import Data.Aeson.TH
import Imports
import qualified Ropes.Aws as Aws
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

-- | AWS CloudFront settings.
data CloudFrontOpts
  = CloudFrontOpts
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

data AWSOpts
  = AWSOpts
      { -- | Key ID; if 'Nothing', will be taken from the environment or from instance metadata
        -- (when running on an AWS instance)
        _awsKeyId :: !(Maybe Aws.AccessKeyId),
        -- | Secret key
        _awsSecretKey :: !(Maybe Aws.SecretAccessKey),
        -- | S3 endpoint
        _awsS3Endpoint :: !AWSEndpoint,
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

data Settings
  = Settings
      { -- | Maximum allowed size for uploads, in bytes
        _setMaxTotalBytes :: !Int,
        -- | TTL for download links, in seconds
        _setDownloadLinkTTL :: !Word
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

data Opts
  = Opts
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
