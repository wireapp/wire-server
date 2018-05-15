{-# LANGUAGE OverloadedStrings #-}

module Util.AWS where

import Control.Lens
import Data.Foldable (for_)
import Data.Id
import Data.Maybe
import Data.Monoid
import Test.Tasty.HUnit

import qualified Brig.AWS             as AWS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ProtoLens       as DP
import qualified Data.Text            as Text
import qualified Data.UUID            as UUID
import qualified Proto.UserEvents     as PU
import qualified Util.Test.SQS        as SQS

isRealSESEnv :: AWS.Env -> Bool
isRealSESEnv env = case view AWS.journalQueue env of
    Just url | "amazonaws.com" `Text.isSuffixOf` url -> True
    _                                                -> False

purgeJournalQueue :: AWS.Env -> IO ()
purgeJournalQueue env = for_ (view AWS.journalQueue env)
                      $ SQS.execute (view AWS.amazonkaEnv env)
                      . SQS.purgeQueue

assertEmptyUserQueue :: AWS.Env -> IO ()
assertEmptyUserQueue env =
    for_ (view AWS.journalQueue env) $ \url -> do
        let awsEnv = view AWS.amazonkaEnv env
        SQS.assertNoMessages url awsEnv

assertUserQueue :: DP.Message a
                => String
                -> AWS.Env
                -> (String -> Maybe a -> IO ())
                -> IO ()
assertUserQueue label env check =
    for_ (view AWS.journalQueue env) $ \url -> do
        let awsEnv = view AWS.amazonkaEnv env
        SQS.assertQueue url label awsEnv check

userActivateJournaled :: UserId -> String -> Maybe PU.UserEvent -> IO ()
userActivateJournaled uid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_ACTIVATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                        (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userActivateJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserActivate, got nothing"

userUpdateJournaled :: UserId -> String -> Maybe PU.UserEvent -> IO ()
userUpdateJournaled uid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_UPDATE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                      (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userUpdateJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserUpdate, got nothing"

userDeleteJournaled :: UserId -> String -> Maybe PU.UserEvent -> IO ()
userDeleteJournaled uid l (Just ev) = do
    assertEqual (l <> "eventType") PU.UserEvent'USER_DELETE (ev^.PU.eventType)
    assertEqual (l <> "userId")    uid                      (Id $ fromMaybe (error "failed to decode") $ UUID.fromByteString $ Lazy.fromStrict (ev^.PU.userId))
userDeleteJournaled _   l Nothing   = assertFailure $ l <> ": Expected 1 UserDelete, got nothing"
