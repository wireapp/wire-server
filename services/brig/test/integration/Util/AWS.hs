-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Util.AWS where

import Brig.AWS qualified as AWS
import Control.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as Lazy
import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Imports
import Proto.UserEvents qualified as PU
import Proto.UserEvents_Fields qualified as PU
import Test.Tasty.HUnit
import Util.Test.SQS qualified as SQS
import Wire.API.User

type UserJournalWatcher = Maybe (SQS.SQSWatcher PU.UserEvent)

assertMessage :: (MonadUnliftIO m, HasCallStack) => UserJournalWatcher -> String -> (PU.UserEvent -> Bool) -> (String -> Maybe PU.UserEvent -> m ()) -> m ()
assertMessage mWatcher label predicate callback = for_ mWatcher $ \watcher -> SQS.assertMessage watcher label predicate callback

isRealSESEnv :: AWS.Env -> Bool
isRealSESEnv env = case view AWS.userJournalQueue env of
  Just url | "amazonaws.com" `Text.isSuffixOf` url -> True
  _ -> False

userActivateJournaled :: (HasCallStack, MonadIO m) => User -> String -> Maybe PU.UserEvent -> m ()
userActivateJournaled u l (Just ev) = liftIO $ do
  assertEventType l PU.UserEvent'USER_ACTIVATE ev
  assertUserId l (userId u) ev
  assertTeamId l (userTeam u) ev
  assertName l (Just $ userDisplayName u) ev
  assertEmail l (userEmail u) ev
  assertLocale l (Just $ userLocale u) ev
userActivateJournaled _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 UserActivate, got nothing"

assertUserActivateJournaled :: forall m. (HasCallStack, MonadUnliftIO m) => UserJournalWatcher -> User -> String -> m ()
assertUserActivateJournaled userJournalWatcher u label =
  assertMessage userJournalWatcher label userActivateMatcher (userActivateJournaled u)
  where
    userActivateMatcher :: PU.UserEvent -> Bool
    userActivateMatcher ev =
      ev ^. PU.eventType == PU.UserEvent'USER_ACTIVATE
        && decodeIdFromBS (ev ^. PU.userId) == userId u

-- | Check for user update event in journal queue.
nameUpdateJournaled :: (HasCallStack, MonadIO m) => UserId -> Name -> String -> Maybe PU.UserEvent -> m ()
nameUpdateJournaled uid name l (Just ev) = liftIO $ do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertName l (Just name) ev
nameUpdateJournaled _ _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

assertNameUpdateJournaled :: (HasCallStack, MonadUnliftIO m) => UserJournalWatcher -> UserId -> Name -> String -> m ()
assertNameUpdateJournaled userJournalWatcher uid name label =
  assertMessage userJournalWatcher label (userUpdateMatcher uid) (nameUpdateJournaled uid name)

userUpdateMatcher :: UserId -> PU.UserEvent -> Bool
userUpdateMatcher uid ev =
  ev ^. PU.eventType == PU.UserEvent'USER_UPDATE
    && decodeIdFromBS (ev ^. PU.userId) == uid

userLocaleUpdateJournaled :: (HasCallStack, MonadIO m) => UserId -> Locale -> String -> Maybe PU.UserEvent -> m ()
userLocaleUpdateJournaled uid loc l (Just ev) = liftIO $ do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertLocale l (Just loc) ev
userLocaleUpdateJournaled _ _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

assertLocaleUpdateJournaled :: (HasCallStack, MonadUnliftIO m) => UserJournalWatcher -> UserId -> Locale -> String -> m ()
assertLocaleUpdateJournaled userJournalWatcher uid loc label =
  assertMessage userJournalWatcher label (userUpdateMatcher uid) (userLocaleUpdateJournaled uid loc)

userEmailUpdateJournaled :: (HasCallStack, MonadIO m) => UserId -> EmailAddress -> String -> Maybe PU.UserEvent -> m ()
userEmailUpdateJournaled uid em l (Just ev) = liftIO $ do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertEmail l (Just em) ev
userEmailUpdateJournaled _ _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

assertEmailUpdateJournaled :: (HasCallStack, MonadUnliftIO m) => UserJournalWatcher -> UserId -> EmailAddress -> String -> m ()
assertEmailUpdateJournaled userJournalWatcher uid em label =
  assertMessage userJournalWatcher label (userUpdateMatcher uid) (userEmailUpdateJournaled uid em)

-- | Check for user deletion event in journal queue.
userDeleteJournaled :: (HasCallStack, MonadIO m) => UserId -> String -> Maybe PU.UserEvent -> m ()
userDeleteJournaled uid l (Just ev) = liftIO $ do
  assertEventType l PU.UserEvent'USER_DELETE ev
  assertUserId l uid ev
userDeleteJournaled _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 UserDelete, got nothing"

assertDeleteJournaled :: (HasCallStack, MonadUnliftIO m) => UserJournalWatcher -> UserId -> String -> m ()
assertDeleteJournaled userJournalWatcher uid label =
  assertMessage userJournalWatcher label (userDeleteMatcher uid) (userDeleteJournaled uid)

userDeleteMatcher :: UserId -> PU.UserEvent -> Bool
userDeleteMatcher uid ev =
  ev ^. PU.eventType == PU.UserEvent'USER_DELETE
    && decodeIdFromBS (ev ^. PU.userId) == uid

assertEventType :: String -> PU.UserEvent'EventType -> PU.UserEvent -> IO ()
assertEventType l et ev = assertEqual (l <> "eventType") et (ev ^. PU.eventType)

assertUserId :: (HasCallStack) => String -> UserId -> PU.UserEvent -> IO ()
assertUserId l uid ev = assertEqual (l <> "userId") uid (decodeIdFromBS (ev ^. PU.userId))

assertTeamId :: String -> Maybe TeamId -> PU.UserEvent -> IO ()
assertTeamId l (Just tid) ev = assertEqual (l <> "teamId should exist") tid ((Id . fromMaybe (error "failed to parse teamId")) ((UUID.fromByteString . Lazy.fromStrict) =<< (ev ^? PU.teamId)))
assertTeamId l Nothing ev = assertEqual (l <> "teamId should not exist") Nothing (ev ^. PU.maybe'teamId)

assertName :: String -> Maybe Name -> PU.UserEvent -> IO ()
assertName l (Just nm) ev = assertEqual (l <> "name should exist") nm (Name $ fromMaybe "failed to decode name" $ fromByteString $ ev ^. PU.name)
assertName l Nothing ev = assertEqual (l <> "name should not exist") Nothing (ev ^. PU.maybe'name)

assertEmail :: String -> Maybe EmailAddress -> PU.UserEvent -> IO ()
assertEmail l (Just em) ev = assertEqual (l <> "email should exist") em (fromMaybe (error "Failed to convert to email") $ emailAddressText $ Text.decodeLatin1 $ fromMaybe "failed to decode email value" $ fromByteString $ ev ^. PU.email)
assertEmail l Nothing ev = assertEqual (l <> "email should not exist") Nothing (ev ^. PU.maybe'email)

assertLocale :: String -> Maybe Locale -> PU.UserEvent -> IO ()
assertLocale l (Just loc) ev = assertEqual (l <> "locale should exist") loc (fromMaybe (error "Failed to convert to locale") $ parseLocale $ Text.decodeLatin1 $ fromMaybe "failed to decode locale value" $ fromByteString $ ev ^. PU.locale)
assertLocale l Nothing ev = assertEqual (l <> "locale should not exist") Nothing (ev ^. PU.maybe'locale)

decodeIdFromBS :: ByteString -> Id a
decodeIdFromBS = Id . fromMaybe (error "failed to decode userId") . UUID.fromByteString . Lazy.fromStrict
