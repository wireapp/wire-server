{-# LANGUAGE OverloadedStrings #-}

module Brig.Aws.InternalNotification(onEvent, enqueue) where

import Brig.App
import Brig.Aws
import Control.Lens (view, (^.))
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text (Text)
import Network.AWS.SQS
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class (field, msg, (~~), val)

import qualified Brig.API.User          as API
import qualified Brig.AwsAmazonka       as Aws
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding     as T
import qualified System.Logger.Class    as Log

onEvent :: InternalNotification -> AppIO ()
onEvent (DeleteUser uid) = do
    Log.info $ field "user" (toByteString uid) ~~ msg (val "Processing delete event")
    API.lookupAccount uid >>= mapM_ API.deleteAccount

enqueue :: InternalNotification -> AppIO Bool
enqueue n = do
    env     <- view amazonkaEnv
    calcMd5 <- digest <$> view digestMD5
    let bdy = encode n
    resp    <- Aws.execute env (Aws.enqueue (env^.Aws.internalQueue) bdy)
    return (resp^.smrsMD5OfMessageBody == (Just $ calcMd5 bdy))
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
