{-# LANGUAGE OverloadedStrings #-}

module Brig.InternalEvent.Publish where

import Brig.App
import Brig.InternalEvent.Types
import Control.Lens (view, (^.))
import Data.Aeson
import Data.Text (Text)
import Network.AWS.SQS
import OpenSSL.EVP.Digest (Digest, digestLBS)

import qualified Brig.AWS               as AWS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as T

publish :: InternalNotification -> AppIO Bool
publish n = view awsEnv >>= \env -> do
    let bdy   = encode n
        queue = env^.AWS.internalQueue
    bdyMD5 <- digest <$> view digestMD5 <*> pure bdy
    resp   <- AWS.execute env (AWS.enqueueStandard queue bdy)
    return (resp^.smrsMD5OfMessageBody == Just bdyMD5)
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
