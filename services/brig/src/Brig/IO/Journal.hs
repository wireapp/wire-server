{-# LANGUAGE RecordWildCards   #-}

module Brig.IO.Journal
    ( userActivate
    , userUpdate
    , userDelete
    , userEmailRemove
    ) where

import Imports
import Brig.App
import Brig.Types
import Control.Lens
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Id
import Data.Proto
import Data.ProtoLens (defMessage)
import Data.Proto.Id
import Data.ProtoLens.Encoding (encodeMessage)
import Data.UUID.V4 (nextRandom)
import Proto.UserEvents (UserEvent, UserEvent'EventType(..))
import qualified Proto.UserEvents_Fields as U

import qualified Data.ByteString.Base64 as B64
import qualified Brig.AWS as AWS

-- [Note: journaling]
-- User journal operations to SQS are a no-op when the service is started
-- without journaling arguments for user updates

userActivate :: User -> AppIO ()
userActivate u@User{..} = journalEvent UserEvent'USER_ACTIVATE userId (userEmail u) (Just userLocale) userTeam (Just userName)

userUpdate :: UserId -> Maybe Email -> Maybe Locale -> Maybe Name -> AppIO ()
userUpdate uid em loc nm = journalEvent UserEvent'USER_UPDATE uid em loc Nothing nm

userEmailRemove :: UserId -> Email -> AppIO ()
userEmailRemove uid em = journalEvent UserEvent'USER_EMAIL_REMOVE uid (Just em) Nothing Nothing Nothing

userDelete :: UserId -> AppIO ()
userDelete uid = journalEvent UserEvent'USER_DELETE uid Nothing Nothing Nothing Nothing

journalEvent :: UserEvent'EventType -> UserId -> Maybe Email -> Maybe Locale -> Maybe TeamId -> Maybe Name -> AppIO ()
journalEvent typ uid em loc tid nm = view awsEnv >>= \env -> for_ (view AWS.userJournalQueue env) $ \queue -> do
    ts  <- now
    rnd <- liftIO nextRandom
    let userEvent :: UserEvent = defMessage
                                 & U.eventType .~ typ
                                 & U.userId .~ (toBytes uid)
                                 & U.utcTime .~ ts
                                 & U.maybe'email .~ (toByteString' <$> em)
                                 & U.maybe'locale .~ (pack . show <$> loc)
                                 & U.maybe'teamId .~ (toBytes <$> tid)
                                 & U.maybe'name .~ (toByteString' <$> nm) -- []
        encoded = fromStrict $ B64.encode $ encodeMessage  userEvent
    AWS.execute env (AWS.enqueueFIFO queue "user.events" rnd encoded)
