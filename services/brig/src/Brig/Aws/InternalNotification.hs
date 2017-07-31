{-# LANGUAGE OverloadedStrings #-}

module Brig.Aws.InternalNotification(onEvent, publish) where

import Aws.Sqs
import Brig.API.User as API
import Brig.App
import Brig.Aws
import Control.Lens (view)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text (Text)
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class (field, msg, (~~), val)

import qualified Aws.Sqs              as Sqs
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding   as T
import qualified System.Logger.Class  as Log

onEvent :: InternalNotification -> AppIO ()
onEvent (DeleteUser uid) = do
    Log.info $ field "user" (toByteString uid) ~~ msg (val "Processing delete event")
    API.lookupAccount uid >>= mapM_ API.deleteAccount

publish :: InternalNotification -> AppIO Bool
publish n = do
    queue   <- view (awsConfig.sqsInternalQueue)
    calcMd5 <- digest <$> view digestMD5
    let bdy = encode n
    let req = Sqs.SendMessage
                { Sqs.smMessage      = T.decodeLatin1 (BL.toStrict bdy)
                , Sqs.smQueueName    = queue
                , Sqs.smDelaySeconds = Nothing
                , Sqs.smAttributes   = []
                }
    (_, r) <- sendSqs req
    return (smrMD5OfMessageBody r == calcMd5 bdy)
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
