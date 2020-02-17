module Main (main) where

import Data.ByteString.Char8 (pack)
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Ropes.Aws
import qualified System.Logger as Logger
import Test.Tasty
import qualified Tests.Ropes.Aws.Ses as SES

main :: IO ()
main = do
  l <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  k <- pack <$> getEnv "AWS_ACCESS_KEY"
  s <- pack <$> getEnv "AWS_SECRET_KEY"
  m <- newManager tlsManagerSettings
  e <- newEnv l m $ Just (AccessKeyId k, SecretAccessKey s)
  defaultMain $ tests e
  where
    tests = SES.tests
