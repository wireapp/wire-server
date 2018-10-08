{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module CargoHold.Options where

import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Lens
import Data.Aeson.TH
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Options.Applicative
import Util.Options
import Util.Options.Common

import qualified Data.Text as T

data CloudFrontOpts = CloudFrontOpts
    { _cfDomain     :: Domain
    , _cfKeyPairId  :: KeyPairId
    , _cfPrivateKey :: FilePath
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CloudFrontOpts
makeLenses ''CloudFrontOpts

data AWSOpts = AWSOpts
    { _awsS3Endpoint         :: !AWSEndpoint
    , _awsS3DownloadEndpoint :: !(Maybe AWSEndpoint)
    , _awsS3Bucket           :: !Text
    , _awsCloudFront         :: !(Maybe CloudFrontOpts)
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts
makeLenses ''AWSOpts

data Settings = Settings
    { _setMaxTotalBytes   :: !Int
    , _setDownloadLinkTTL :: !Word
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data Opts = Opts
    { _optCargohold :: !Endpoint
    , _optAws       :: !AWSOpts
    , _optSettings  :: !Settings
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "CargoHold - Asset Service" <> fullDesc

optsParser :: Parser Opts
optsParser = Opts <$>
    (Endpoint <$>
        (textOption $
            long "host"
            <> value "*4"
            <> showDefault
            <> metavar "HOSTNAME"
            <> help "Hostname or address to bind to")
        <*>
        (option auto $
            long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to listen on"))
    <*> awsParser
    <*> settingsParser
  where
    cloudFrontParser :: Parser CloudFrontOpts
    cloudFrontParser = CloudFrontOpts <$>
            (fmap Domain . textOption $
                long "aws-cloudfront-domain"
                <> metavar "STRING"
                <> help "AWS CloudFront Domain")

        <*> (fmap KeyPairId . textOption $
                long "aws-cloudfront-keypair-id"
                <> metavar "STRING"
                <> help "AWS CloudFront Keypair ID")

        <*> strOption
                (long "aws-cloudfront-private-key"
                <> metavar "FILE"
                <> help "AWS CloudFront Private Key")

    awsParser :: Parser AWSOpts
    awsParser = AWSOpts <$>
            (option parseAWSEndpoint $
                long "aws-s3-endpoint"
                <> value (AWSEndpoint "s3.eu-west-1.amazonaws.com" True 443)
                <> metavar "STRING"
                <> showDefault
                <> help "aws S3 endpoint")

        <*> optional (option parseAWSEndpoint $
                long "aws-s3-download-endpoint"
                <> metavar "STRING"
                <> showDefault
                <> help "aws S3 endpoint used for generating download links")

        <*> (fmap T.pack . strOption $
                long "aws-s3-bucket"
                <> metavar "STRING"
                <> help "S3 bucket name")
        <*> optional cloudFrontParser

    settingsParser :: Parser Settings
    settingsParser = Settings <$>
            option auto
            (long "max-total-bytes"
            <> metavar "INT"
            <> value (25 * 1024 * 1024)
            <> showDefault
            <> help "Maximum allowed size in bytes for uploads")
        <*> option auto
            (long "download-link-ttl"
            <> metavar "INT"
            <> value 300
            <> showDefault
            <> help "TTL for download links in seconds")
