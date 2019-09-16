{-# LANGUAGE StrictData        #-}

module CargoHold.Options where

import Imports
import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Lens hiding (Level)
import Data.Aeson.TH
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

import qualified Ropes.Aws as Aws

-- | AWS CloudFront settings.
data CloudFrontOpts = CloudFrontOpts
    { _cfDomain     :: Domain     -- ^ Domain
    , _cfKeyPairId  :: KeyPairId  -- ^ Keypair ID
    , _cfPrivateKey :: FilePath   -- ^ Path to private key
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CloudFrontOpts
makeLenses ''CloudFrontOpts

data AWSOpts = AWSOpts
    {
    -- | Key ID; if 'Nothing', will be taken from the environment or from instance metadata
    -- (when running on an AWS instance)
      _awsKeyId              :: !(Maybe Aws.AccessKeyId)
    -- | Secret key
    , _awsSecretKey          :: !(Maybe Aws.SecretAccessKey)
    -- | S3 endpoint
    , _awsS3Endpoint         :: !AWSEndpoint
    -- | S3 endpoint for generating download links. Useful if Cargohold is configured to use
    -- an S3 replacement running inside the internal network (in which case internally we
    -- would use one hostname for S3, and when generating an asset link for a client app, we
    -- would use another hostname).
    , _awsS3DownloadEndpoint :: !(Maybe AWSEndpoint)
    -- | S3 bucket name
    , _awsS3Bucket           :: !Text
    -- | AWS CloudFront options
    , _awsCloudFront         :: !(Maybe CloudFrontOpts)
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data Settings = Settings
    {
    -- | Maximum allowed size for uploads, in bytes
      _setMaxTotalBytes   :: !Int
    -- | TTL for download links, in seconds
    , _setDownloadLinkTTL :: !Word
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data Opts = Opts
    { _optCargohold :: !Endpoint  -- ^ Hostname and port to bind to
    , _optAws       :: !AWSOpts
    , _optSettings  :: !Settings
    -- Logging
    , _optLogLevel      :: !Level  -- ^ Log level (Debug, Info, etc)
    , _optLogNetStrings :: !(Maybe (Last Bool))   -- ^ Use netstrings encoding:
                                                  --   <http://cr.yp.to/proto/netstrings.txt>
    , _optLogFormat :: !(Maybe (Last LogFormat))  --- ^ Log format
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts
