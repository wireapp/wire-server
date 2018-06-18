{-# LANGUAGE OverloadedStrings #-}

module Util.AWS where

import Brig.Types
import Control.Lens
import Control.Monad (join)
import Data.ByteString.Conversion
import Data.Foldable (for_)
import Data.Id
import Data.Maybe
import Data.Monoid
import GHC.Stack
import Test.Tasty.HUnit

import qualified Brig.AWS             as AWS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ProtoLens       as DP
import qualified Data.Text            as Text
import qualified Data.UUID            as UUID
import qualified Proto.UserEvents     as PU
import qualified Util.Test.SQS        as SQS

isRealSESEnv :: AWS.Env -> Bool
isRealSESEnv env = case view AWS.userJournalQueue env of
    Just url | "amazonaws.com" `Text.isSuffixOf` url -> True
    _                                                -> False

purgeJournalQueue :: AWS.Env -> IO ()
purgeJournalQueue env = for_ (view AWS.userJournalQueue env)
                      $ SQS.execute (view AWS.amazonkaEnv env)
                      . SQS.purgeQueue

-- | Fail unless journal queue is empty.
assertEmptyUserJournalQueue :: AWS.Env -> IO ()
assertEmptyUserJournalQueue env =
    for_ (view AWS.userJournalQueue env) $ \url -> do
        let awsEnv = view AWS.amazonkaEnv env
        SQS.assertNoMessages url awsEnv

-- | Fail unless journal queue passes a given check.
assertUserJournalQueue :: DP.Message a
                       => String
                       -> AWS.Env
                       -> (String -> Maybe a -> IO ())
                       -> IO ()
assertUserJournalQueue label env check = do
    for_ (view AWS.userJournalQueue env) $ \url -> do
        let awsEnv = view AWS.amazonkaEnv env
        SQS.assertQueue url label awsEnv check

-- | Check for user activation event in journal queue.
userActivateJournaled' :: HasCallStack => UserId -> Name -> Maybe TeamId -> String -> Maybe PU.UserEvent -> IO ()
userActivateJournaled' uid nm tid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_ACTIVATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                        (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
    assertEqual (l <> "teamId")    tid                        (Id <$> join (fmap (UUID.fromByteString . Lazy.fromStrict) (ev^?PU.teamId)))
    assertEqual (l <> "name")      nm                         (Name $ fromMaybe "failed to decode name" $ fromByteString $ ev^.PU.name)
userActivateJournaled' _   _   _  l Nothing   = assertFailure $ l <> ": Expected 1 UserActivate, got nothing"

userActivateJournaled :: HasCallStack => User -> String -> Maybe PU.UserEvent -> IO ()
userActivateJournaled u l (Just ev) = do
    let uid = userId u
    assertEqual (l <> "eventType") PU.UserEvent'USER_ACTIVATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                        (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userActivateJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserActivate, got nothing"

-- | Check for user update event in journal queue.
userUpdateJournaled' :: HasCallStack => UserId -> Maybe Name -> String -> Maybe PU.UserEvent -> IO ()
userUpdateJournaled' uid nm l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_UPDATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                      (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
    assertEqual (l <> "name")      nm                       (Name <$> join (fmap fromByteString (ev^?PU.name)))
userUpdateJournaled' _   _  l  Nothing  = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

userUpdateJournaled :: HasCallStack => UserId -> String -> Maybe PU.UserEvent -> IO ()
userUpdateJournaled uid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_UPDATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                      (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userUpdateJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

-- | Check for user deletion event in journal queue.
userDeleteJournaled :: HasCallStack => UserId -> String -> Maybe PU.UserEvent -> IO ()
userDeleteJournaled uid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_DELETE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                      (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userDeleteJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserDelete, got nothing"
