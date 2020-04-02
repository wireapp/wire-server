module Util.AWS where

import qualified Brig.AWS as AWS
import Brig.Types
import Control.Lens
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import Data.Id
import qualified Data.ProtoLens as DP
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Imports
import qualified Proto.UserEvents as PU
import qualified Proto.UserEvents_Fields as PU
import Test.Tasty.HUnit
import qualified Util.Test.SQS as SQS

isRealSESEnv :: AWS.Env -> Bool
isRealSESEnv env = case view AWS.userJournalQueue env of
  Just url | "amazonaws.com" `Text.isSuffixOf` url -> True
  _ -> False

purgeJournalQueue :: AWS.Env -> IO ()
purgeJournalQueue env =
  for_ (view AWS.userJournalQueue env) $
    SQS.execute (view AWS.amazonkaEnv env)
      . SQS.purgeQueue

-- | Fail unless journal queue is empty.
assertEmptyUserJournalQueue :: AWS.Env -> IO ()
assertEmptyUserJournalQueue env =
  for_ (view AWS.userJournalQueue env) $ \url -> do
    let awsEnv = view AWS.amazonkaEnv env
    SQS.assertNoMessages url awsEnv

-- | Fail unless journal queue passes a given check.
assertUserJournalQueue ::
  DP.Message a =>
  String ->
  AWS.Env ->
  (String -> Maybe a -> IO ()) ->
  IO ()
assertUserJournalQueue label env check = do
  for_ (view AWS.userJournalQueue env) $ \url -> do
    let awsEnv = view AWS.amazonkaEnv env
    SQS.assertQueue url label awsEnv check

userActivateJournaled :: HasCallStack => User -> String -> Maybe PU.UserEvent -> IO ()
userActivateJournaled u l (Just ev) = do
  assertEventType l PU.UserEvent'USER_ACTIVATE ev
  assertUserId l (userId u) ev
  assertTeamId l (userTeam u) ev
  assertName l (Just $ userDisplayName u) ev
  assertEmail l (userEmail u) ev
  assertLocale l (Just $ userLocale u) ev
userActivateJournaled _ l Nothing = assertFailure $ l <> ": Expected 1 UserActivate, got nothing"

-- | Check for user update event in journal queue.
userUpdateJournaled :: HasCallStack => UserId -> UserUpdate -> String -> Maybe PU.UserEvent -> IO ()
userUpdateJournaled uid update l (Just ev) = do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertName l (uupName update) ev
userUpdateJournaled _ _ l Nothing = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

userLocaleUpdateJournaled :: HasCallStack => UserId -> Locale -> String -> Maybe PU.UserEvent -> IO ()
userLocaleUpdateJournaled uid loc l (Just ev) = do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertLocale l (Just loc) ev
userLocaleUpdateJournaled _ _ l Nothing = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

userEmailUpdateJournaled :: HasCallStack => UserId -> Email -> String -> Maybe PU.UserEvent -> IO ()
userEmailUpdateJournaled uid em l (Just ev) = do
  assertEventType l PU.UserEvent'USER_UPDATE ev
  assertUserId l uid ev
  assertEmail l (Just em) ev
userEmailUpdateJournaled _ _ l Nothing = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

-- | Check for user deletion event in journal queue.
userDeleteJournaled :: HasCallStack => UserId -> String -> Maybe PU.UserEvent -> IO ()
userDeleteJournaled uid l (Just ev) = do
  assertEventType l PU.UserEvent'USER_DELETE ev
  assertUserId l uid ev
userDeleteJournaled _ l Nothing = assertFailure $ l <> ": Expected 1 UserDelete, got nothing"

assertEventType :: String -> PU.UserEvent'EventType -> PU.UserEvent -> IO ()
assertEventType l et ev = assertEqual (l <> "eventType") et (ev ^. PU.eventType)

assertUserId :: String -> UserId -> PU.UserEvent -> IO ()
assertUserId l uid ev = assertEqual (l <> "userId") uid (Id $ fromMaybe (error "failed to decode userId") $ UUID.fromByteString $ Lazy.fromStrict (ev ^. PU.userId))

assertTeamId :: String -> Maybe TeamId -> PU.UserEvent -> IO ()
assertTeamId l (Just tid) ev = assertEqual (l <> "teamId should exist") tid (Id . fromMaybe (error "failed to parse teamId") . join $ fmap (UUID.fromByteString . Lazy.fromStrict) (ev ^? PU.teamId))
assertTeamId l Nothing ev = assertEqual (l <> "teamId should not exist") Nothing (ev ^. PU.maybe'teamId)

assertName :: String -> Maybe Name -> PU.UserEvent -> IO ()
assertName l (Just nm) ev = assertEqual (l <> "name should exist") nm (Name $ fromMaybe "failed to decode name" $ fromByteString $ ev ^. PU.name)
assertName l Nothing ev = assertEqual (l <> "name should not exist") Nothing (ev ^. PU.maybe'name)

assertEmail :: String -> Maybe Email -> PU.UserEvent -> IO ()
assertEmail l (Just em) ev = assertEqual (l <> "email should exist") em (fromMaybe (error "Failed to convert to email") $ parseEmail $ Text.decodeLatin1 $ fromMaybe "failed to decode email value" $ fromByteString $ ev ^. PU.email)
assertEmail l Nothing ev = assertEqual (l <> "email should not exist") Nothing (ev ^. PU.maybe'email)

assertLocale :: String -> Maybe Locale -> PU.UserEvent -> IO ()
assertLocale l (Just loc) ev = assertEqual (l <> "locale should exist") loc (fromMaybe (error "Failed to convert to locale") $ parseLocale $ Text.decodeLatin1 $ fromMaybe "failed to decode locale value" $ fromByteString $ ev ^. PU.locale)
assertLocale l Nothing ev = assertEqual (l <> "locale should not exist") Nothing (ev ^. PU.maybe'locale)
