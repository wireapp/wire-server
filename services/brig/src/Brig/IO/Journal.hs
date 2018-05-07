{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.IO.Journal
    ( userActivate
    , userUpdate
    , userDelete
    , bytes
    , nowInt
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Id
import Data.ProtoLens.Encoding (encodeMessage)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Brig.App
import Proto.UserEvents

import qualified Data.ByteString.Base64 as B64
import qualified Data.UUID as UUID
import qualified Brig.AWS as AWS

-- [Note: journaling]
-- User journal operations to SQS are a no-op when the service is started
-- without journaling arguments for user updates

userActivate :: UserId -> AppIO ()
userActivate = journalEvent UserEvent'USER_ACTIVATE

userUpdate :: UserId -> AppIO ()
userUpdate = journalEvent UserEvent'USER_UPDATE

userDelete :: UserId -> AppIO ()
userDelete = journalEvent UserEvent'USER_DELETE

journalEvent :: UserEvent'EventType -> UserId -> AppIO ()
journalEvent typ uid = view awsEnv >>= \env -> for_ (view AWS.journalQueue env) $ \queue -> do
    ts <- nowInt
    let ev = fromStrict . B64.encode . encodeMessage
           $ UserEvent typ (bytes uid) ts Nothing Nothing Nothing
    AWS.execute env (AWS.enqueue queue ev)

----------------------------------------------------------------------------
-- TODO: Move to types-common
bytes :: Id a -> ByteString
bytes = toStrict . UUID.toByteString . toUUID

nowInt :: MonadIO m => m Int64
nowInt = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
