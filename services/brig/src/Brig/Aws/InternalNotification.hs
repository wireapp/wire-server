{-# LANGUAGE OverloadedStrings #-}

module Brig.AWS.InternalNotification where

import Brig.App
import Brig.AWS.Types (InternalNotification (..))
import Control.Lens (view, (^.))
import Data.Aeson
import Data.ByteString.Conversion
import Data.Text (Text)
import Network.AWS.SQS
import OpenSSL.EVP.Digest (Digest, digestLBS)
import System.Logger.Class (field, msg, (~~), val)

import qualified Brig.API.User          as API
import qualified Brig.AWS               as AWS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text.Encoding     as T
import qualified System.Logger.Class    as Log

onEvent :: InternalNotification -> AppIO ()
onEvent (DeleteUser uid) = do
    Log.info $ field "user" (toByteString uid)
            ~~ msg (val "Processing delete event")
    API.lookupAccount uid >>= mapM_ API.deleteAccount

publish :: InternalNotification -> AppIO Bool
publish n = view awsEnv >>= \env -> do
    let bdy   = encode n
        queue = env^.AWS.internalQueue
    bdyMD5 <- digest <$> view digestMD5 <*> pure bdy
    resp   <- AWS.execute env (AWS.enqueue queue bdy)
    return (resp^.smrsMD5OfMessageBody == Just bdyMD5)
  where
    digest :: Digest -> BL.ByteString -> Text
    digest d = T.decodeLatin1 . B16.encode . digestLBS d
