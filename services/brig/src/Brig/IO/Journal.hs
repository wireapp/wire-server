{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.IO.Journal
    ( userActivate
    , userUpdate
    , userDelete
    , userEmailRemove
    ) where

import Brig.App
import Brig.Types
import Control.Lens
import Data.Foldable (for_)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Id
import Data.Proto
import Data.Proto.Id
import Data.ProtoLens.Encoding (encodeMessage)
import Proto.UserEvents hiding (userId)

import qualified Data.ByteString.Base64 as B64
import qualified Brig.AWS as AWS

-- [Note: journaling]
-- User journal operations to SQS are a no-op when the service is started
-- without journaling arguments for user updates

userActivate :: User -> AppIO ()
userActivate u = journalEvent UserEvent'USER_ACTIVATE (userId u) (userEmail u) (Just $ userLocale u)

userUpdate :: UserId -> Maybe Email -> Maybe Locale -> AppIO ()
userUpdate uid em loc = journalEvent UserEvent'USER_UPDATE uid em loc

userEmailRemove :: UserId -> Email -> AppIO ()
userEmailRemove uid em = journalEvent UserEvent'USER_EMAIL_REMOVE uid (Just em) Nothing

userDelete :: UserId -> AppIO ()
userDelete uid = journalEvent UserEvent'USER_DELETE uid Nothing Nothing

journalEvent :: UserEvent'EventType -> UserId -> Maybe Email -> Maybe Locale -> AppIO ()
journalEvent typ uid em loc = view awsEnv >>= \env -> for_ (view AWS.journalQueue env) $ \queue -> do
    ts <- now
    let encoded = fromStrict . B64.encode . encodeMessage
                $ UserEvent typ (toBytes uid) ts (toByteString' <$> em) (pack . show <$> loc)
    AWS.execute env (AWS.enqueue queue encoded)
