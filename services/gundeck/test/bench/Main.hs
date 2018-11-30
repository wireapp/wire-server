{-# LANGUAGE OverloadedStrings #-}

module Main where

import Imports
import Criterion.Main
import Data.Aeson
import Data.Id (randomId, ConnId (..), ClientId (..))
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Gundeck.Push.Native.Serialise
import Gundeck.Push.Native.Types
import Network.AWS (Region (Ireland))
import OpenSSL.EVP.Cipher (Cipher, getCipherByName)
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import OpenSSL (withOpenSSL)

import qualified Data.ByteString     as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List1          as List1
import qualified Data.Text           as Text
import qualified Data.Text.Lazy      as LT

main :: IO ()
main = withOpenSSL $ do
    c <- aes256
    d <- sha256
    defaultMain [
        bgroup "plain"
            [ bench "32"   $ nfIO (plaintext 32)
            , bench "64"   $ nfIO (plaintext 64)
            , bench "128"  $ nfIO (plaintext 128)
            , bench "256"  $ nfIO (plaintext 256)
            , bench "512"  $ nfIO (plaintext 512)
            , bench "1024" $ nfIO (plaintext 1024)
            , bench "1536" $ nfIO (plaintext 1536)
            , bench "2048" $ nfIO (plaintext 2048)
            , bench "3072" $ nfIO (plaintext 3072)
            ]
        , bgroup "cipher"
            [ bench "32"   $ nfIO (ciphertext c d 32)
            , bench "64"   $ nfIO (ciphertext c d 64)
            , bench "128"  $ nfIO (ciphertext c d 128)
            , bench "256"  $ nfIO (ciphertext c d 256)
            , bench "512"  $ nfIO (ciphertext c d 512)
            , bench "1024" $ nfIO (ciphertext c d 1024)
            , bench "1536" $ nfIO (ciphertext c d 1536)
            , bench "2048" $ nfIO (ciphertext c d 2048)
            ]
        ]

-----------------------------------------------------------------------------
-- Benchmarks

plaintext :: Int -> IO Text
plaintext l = do
    i <- randomId
    a <- mkAddress GCM
    let pload = List1.singleton (payload l)
    let notif = Notification i False pload
    let msg   = Plaintext notif HighPriority Nothing
    Right txt <- serialise msg a
    return $! LT.toStrict txt

ciphertext :: Cipher -> Digest -> Int -> IO Text
ciphertext c d l = do
    i <- randomId
    a <- mkAddress GCM
    let pload = List1.singleton (payload l)
    let notif = Notification i False pload
    let msg   = Ciphertext notif c d HighPriority Nothing
    Right txt <- serialise msg a
    return $! LT.toStrict txt

-----------------------------------------------------------------------------
-- Utilities

keys :: SignalingKeys
keys = SignalingKeys (EncKey (BS.replicate 32 0)) (MacKey (BS.replicate 32 0))

payload :: Int -> Object
payload n = HashMap.fromList ["a" .= Text.replicate n "b"]

mkAddress :: Transport -> IO (Address s)
mkAddress t = do
    u <- randomId
    let app = AppName "test"
    let ept = mkEndpoint t app
    let tok = Token "test"
    let con = ConnId "conn"
    let clt = ClientId "client"
    return $! Address u t app tok ept con clt (Just keys)

mkEndpoint :: Transport -> AppName -> EndpointArn
mkEndpoint t a = mkSnsArn Ireland (Account "test") topic
  where
    topic = mkEndpointTopic (ArnEnv "test") t a (EndpointId "test")

sha256 :: IO Digest
sha256 = maybe (error "SHA256 not found") return
     =<< getDigestByName "SHA256"

aes256 :: IO Cipher
aes256 = maybe (error "AES256 not found") return
     =<< getCipherByName "AES-256-CBC"
