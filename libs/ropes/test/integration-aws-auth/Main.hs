module Main where

import Data.Time.Clock
import Imports
import Network.HTTP.Client
import Ropes.Aws
import qualified System.Logger as Logger

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  l <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  m <- newManager defaultManagerSettings
  e <- newEnv l m Nothing
  forever $ do
    now <- getCurrentTime
    crd <- getCredentials e
    putStrLn $ "Time: " ++ show now ++ " - Credentials: " ++ showCreds crd
    threadDelay $ 60 * 1000 * 1000 -- every minute
  where
    showCreds (Credentials k s _ t) =
      "AccessKeyId: " ++ show k ++ " - "
        ++ "SecretAccessKey: "
        ++ show s
        ++ " - "
        ++ "SessionToken: "
        ++ show t
