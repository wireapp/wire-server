{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module CargoHold.Options where

import Imports
import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Lens
import Data.Aeson.TH
import Util.Options
import Util.Options.Common

import qualified Ropes.Aws as Aws

-- | AWS CloudFront options
data CloudFrontOpts = CloudFrontOpts
    { _cfDomain     :: Domain     -- ^ Domain
    , _cfKeyPairId  :: KeyPairId  -- ^ Keypair ID
    , _cfPrivateKey :: FilePath   -- ^ Private key
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CloudFrontOpts
makeLenses ''CloudFrontOpts

data AWSOpts = AWSOpts
    { _awsKeyId        :: !(Maybe Aws.AccessKeyId)     -- ^ Access key ID
    , _awsSecretKey    :: !(Maybe Aws.SecretAccessKey) -- ^ Secret access key
    , _awsS3Endpoint   :: !AWSEndpoint                 -- ^ S3 endpoint
    , _awsS3DownloadEndpoint :: !(Maybe AWSEndpoint)   -- ^ Endpoint that will be
                                                       --   used to generate URLs
                                                       --   for external consumers
    , _awsS3Bucket     :: !Text                        -- ^ S3 bucket name
    , _awsCloudFront   :: !(Maybe CloudFrontOpts)      -- ^ CloudFront options
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data Settings = Settings
    { _setMaxTotalBytes   :: !Int   -- ^ Maximum allowed size for uploads, in bytes
    , _setDownloadLinkTTL :: !Word  -- ^ TTL for download links, in seconds
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data Opts = Opts
    { _optCargohold :: !Endpoint  -- ^ Cargohold endpoint
    , _optAws       :: !AWSOpts   -- ^ AWS settings
    , _optSettings  :: !Settings  -- ^ Other settings
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts
