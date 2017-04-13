{-# LANGUAGE StrictData #-}

module CargoHold.Options (Opts (..), parseOptions) where

import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Word
import Options.Applicative
import Ropes.Aws (AccessKeyId (..), SecretAccessKey (..))

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

data Opts = Opts
    { optHost            :: String
    , optPort            :: Word16
    , optAwsKeyId        :: Maybe AccessKeyId
    , optAwsSecKey       :: Maybe SecretAccessKey
    , optAwsS3Bucket     :: Text
    , optAwsCFDomain     :: Domain
    , optAwsCFKeyPairId  :: KeyPairId
    , optAwsCFPrivateKey :: FilePath
    , optMaxTotalBytes   :: Int
    } deriving (Eq)

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "CargoHold - Asset Service" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> strOption
                (long "host"
                <> metavar "HOSTNAME"
                <> help "hostname or address to bind to")

        <*> option auto
                (long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "port to listen on")

        <*> (optional . fmap AccessKeyId . bytesOption $
                long "aws-key-id"
                <> metavar "STRING"
                <> help "AWS Access Key ID")

        <*> (optional . fmap SecretAccessKey . bytesOption $
                long "aws-secret-key"
                <> metavar "STRING"
                <> help "AWS Secret Access Key")

        <*> (fmap T.pack . strOption $
                long "aws-s3-bucket"
                <> metavar "STRING"
                <> help "S3 bucket name")

        <*> (fmap Domain . bytesOption $
                long "aws-cloudfront-domain"
                <> metavar "STRING"
                <> help "AWS CloudFront Domain")

        <*> (fmap KeyPairId . bytesOption $
                long "aws-cloudfront-keypair-id"
                <> metavar "STRING"
                <> help "AWS CloudFront Keypair ID")

        <*> strOption
                (long "aws-cloudfront-private-key"
                <> metavar "FILE"
                <> help "AWS CloudFront Private Key")

        <*> option auto
                (long "max-total-bytes"
                <> metavar "INT"
                <> value (25 * 1024 * 1024)
                <> showDefault
                <> help "Maximum allowed size in bytes for uploads")

    bytesOption :: Mod OptionFields String -> Parser ByteString
    bytesOption = fmap C.pack . strOption

