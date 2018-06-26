{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Serialise
    ( serialise
    , maxPayloadSize
    ) where

import Control.Lens ((^.), (^?), _Just)
import Data.Aeson (object, (.=), Value, encode)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Int (Int64)
import Data.Json.Util
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Gundeck.Push.Native.Crypto
import Gundeck.Push.Native.Types
import Gundeck.Types

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB

serialise :: Message s -> Address s -> IO (Either Failure LT.Text)
serialise m a = do
    rs <- prepare m a
    case rs of
        Left  failure        -> return $! Left $! failure
        Right (v, prio, aps) -> case renderText (a^.addrTransport) aps prio v of
            Nothing  -> return $ Left PayloadTooLarge
            Just txt -> return $ Right txt

prepare :: Message s -> Address s -> IO (Either Failure (Value, Priority, Maybe ApsData))
prepare m a = case m of
    Plaintext ntf prio aps ->
        let o = object
              [ "type" .= ("plain" :: Text)
              , "data" .= ntf
              , "user" .= (a^.addrUser)
              ]
        in return $ Right (o, prio, aps)

    Ciphertext ntf ci dg prio aps -> case a^.addrKeys of
        Nothing -> return $ Left MissingKeys
        Just  k -> do
            let o = object ["data" .= ntf]
            let j = encode o
            let l = maxPayloadSize (a^.addrTransport)
            if LB.length j > l
                then return $ Left PayloadTooLarge
                else do
                    c <- encrypt (LB.toStrict j) k ci dg
                    let mac = B64.encode (cipherMac c)
                    let dat = B64.encode (cipherData c)
                    if BS.length mac + BS.length dat > fromIntegral l
                        then return $ Left PayloadTooLarge
                        else do
                            let o' = object
                                   [ "type" .= ("cipher" :: Text)
                                   , "mac"  .= decodeUtf8 mac
                                   , "data" .= decodeUtf8 dat
                                   , "user" .= (a^.addrUser)
                                   ]
                            return $ Right (o', prio, aps)

    Notice nid prio aps ->
        let o = object
              [ "type" .= ("notice" :: Text)
              , "data" .= object ["id" .= nid]
              , "user" .= (a^.addrUser)
              ]
        in return $ Right (o, prio, aps)

-- | Assemble a final SNS JSON string for transmission.
renderText :: Transport -> Maybe ApsData -> Priority -> Value -> Maybe LT.Text
renderText t aps prio x = case t of
    GCM             -> trim "GCM"               (jsonString gcmJson)
    APNS            -> trim "APNS"              (jsonString stdApnsJson)
    APNSSandbox     -> trim "APNS_SANDBOX"      (jsonString stdApnsJson)
    APNSVoIP        -> trim "APNS_VOIP"         (jsonString voipApnsJson)
    APNSVoIPSandbox -> trim "APNS_VOIP_SANDBOX" (jsonString voipApnsJson)
  where
    gcmJson = object
        [ "data"     .= x
        , "priority" .= gcmPriority prio
        ]

    stdApnsJson = object
        [ "aps"  .= apsDict prio
        , "data" .= x
        ]

    voipApnsJson = object
        [ "aps"  .= object []
        , "data" .= x
        ]

    apsDict HighPriority = object
        $ "alert" .= object (
              "loc-key"  .= (aps^?_Just.apsLocKey)
            # "loc-args" .= (aps^?_Just.apsLocArgs)
            # [])
        # "sound" .= (aps^?_Just.apsSound)
        # "content-available" .= '1'
        # []
    apsDict LowPriority = object
        $ "content-available" .= '1'
        # []

    maxLen = maxPayloadSize t

    -- see <https://github.com/wireapp/wire-server/issues/341>.
    trim k j = let j' = LT.toStrict (LT.take (maxLen + 1) j) in
        if BS.length (encodeUtf8 j') > fromIntegral maxLen
            then Nothing
            else Just $! jsonString $! object [k .= j']

maxPayloadSize :: Transport -> Int64
maxPayloadSize GCM             = 4096
maxPayloadSize APNS            = 2048
maxPayloadSize APNSSandbox     = 2048
maxPayloadSize APNSVoIP        = 4096
maxPayloadSize APNSVoIPSandbox = 4096

gcmPriority :: Priority -> Text
gcmPriority LowPriority  = "normal"
gcmPriority HighPriority = "high"

jsonString :: Value -> LT.Text
jsonString = LTB.toLazyTextWith 512 . encodeToTextBuilder
