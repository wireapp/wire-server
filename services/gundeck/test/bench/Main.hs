
module Main (main) where

import Imports
import Criterion.Main
import Data.Id (randomId, ConnId (..), ClientId (..))
import Gundeck.Types.Push
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Network.AWS (Region (Ireland))
import OpenSSL (withOpenSSL)

import qualified Data.Text.Lazy      as LT

main :: IO ()
main = withOpenSSL $ do
    defaultMain [
        bgroup "notice"
            [ bench "32"   $ nfIO notice
            ]
        ]

-----------------------------------------------------------------------------
-- Benchmarks

notice :: IO Text
notice = do
    i <- randomId
    a <- mkAddress GCM
    let msg   = NativePush i HighPriority Nothing
    Right txt <- serialise msg a
    return $! LT.toStrict txt

-----------------------------------------------------------------------------
-- Utilities

mkAddress :: Transport -> IO Address
mkAddress t = do
    u <- randomId
    let app = AppName "test"
    let ept = mkEndpoint t app
    let tok = Token "test"
    let con = ConnId "conn"
    let clt = ClientId "client"
    return $! Address u t app tok ept con clt

mkEndpoint :: Transport -> AppName -> EndpointArn
mkEndpoint t a = mkSnsArn Ireland (Account "test") topic
  where
    topic = mkEndpointTopic (ArnEnv "test") t a (EndpointId "test")
