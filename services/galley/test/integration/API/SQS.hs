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
-- Disabling for HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | TODO: most of this module is deprecated; use "Util.Test.SQS" from the types-common-aws package
-- instead.
module API.SQS where

import Control.Lens hiding ((.=))
import Data.ByteString.Lazy (fromStrict)
import Data.Currency qualified as Currency
import Data.Id
import Data.Set qualified as Set
import Data.Text (pack)
import Data.UUID qualified as UUID
import Galley.Aws qualified as Aws
import Galley.Options (JournalOpts)
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session as Ssl
import Proto.TeamEvents as E
import Proto.TeamEvents_Fields as E
import Ssl.Util
import System.Logger.Class qualified as L
import Test.Tasty.HUnit
import TestSetup
import Util.Test.SQS qualified as SQS

withTeamEventWatcher :: (HasCallStack) => (SQS.SQSWatcher TeamEvent -> TestM ()) -> TestM ()
withTeamEventWatcher action = do
  view tsTeamEventWatcher >>= \case
    Nothing -> pure ()
    Just w -> action w

assertIfWatcher :: (HasCallStack) => String -> (TeamEvent -> Bool) -> (String -> Maybe TeamEvent -> TestM ()) -> TestM ()
assertIfWatcher l matcher assertion =
  view tsTeamEventWatcher >>= \case
    Nothing -> pure ()
    Just w -> SQS.assertMessage w l matcher assertion

tActivateWithCurrency :: (HasCallStack, MonadIO m) => Maybe Currency.Alpha -> String -> Maybe E.TeamEvent -> m ()
tActivateWithCurrency c l (Just e) = liftIO $ do
  assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e ^. eventType)
  assertEqual "count" 1 (e ^. eventData . memberCount)
  -- NOTE: protobuf used to decodes absent, optional fields as (Just "") but not when using `maybe'<field>`
  let cur = pack . show <$> c
  assertEqual "currency" cur (e ^. eventData . maybe'currency)
tActivateWithCurrency _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

assertTeamActivateWithCurrency :: (HasCallStack) => String -> TeamId -> Maybe Currency.Alpha -> TestM ()
assertTeamActivateWithCurrency l tid c =
  assertIfWatcher l (teamActivateMatcher tid) (tActivateWithCurrency c)

tActivate :: (HasCallStack, MonadIO m) => String -> Maybe E.TeamEvent -> m ()
tActivate l (Just e) = liftIO $ do
  assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e ^. eventType)
  assertEqual "count" 1 (e ^. eventData . memberCount)
tActivate l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

assertTeamActivate :: (HasCallStack) => String -> TeamId -> TestM ()
assertTeamActivate l tid =
  assertIfWatcher l (teamActivateMatcher tid) tActivate

teamActivateMatcher :: TeamId -> TeamEvent -> Bool
teamActivateMatcher tid e = e ^. eventType == E.TeamEvent'TEAM_ACTIVATE && decodeIdFromBS (e ^. teamId) == tid

tDelete :: (HasCallStack, MonadIO m) => String -> Maybe E.TeamEvent -> m ()
tDelete l (Just e) = liftIO $ assertEqual (l <> ": eventType") E.TeamEvent'TEAM_DELETE (e ^. eventType)
tDelete l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 TeamDelete, got nothing"

assertTeamDelete :: (HasCallStack) => Int -> String -> TeamId -> TestM ()
assertTeamDelete maxWaitSeconds l tid =
  withTeamEventWatcher $ \w -> do
    mEvent <- SQS.waitForMessage w maxWaitSeconds (\e -> e ^. eventType == E.TeamEvent'TEAM_DELETE && decodeIdFromBS (e ^. teamId) == tid)
    tDelete l mEvent

tSuspend :: (HasCallStack, MonadIO m) => String -> Maybe E.TeamEvent -> m ()
tSuspend l (Just e) = liftIO $ assertEqual (l <> "eventType") E.TeamEvent'TEAM_SUSPEND (e ^. eventType)
tSuspend l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 TeamSuspend, got nothing"

assertTeamSuspend :: (HasCallStack) => String -> TeamId -> TestM ()
assertTeamSuspend l tid =
  assertIfWatcher l (\e -> e ^. eventType == E.TeamEvent'TEAM_SUSPEND && decodeIdFromBS (e ^. teamId) == tid) tSuspend

tUpdate :: (HasCallStack, MonadIO m) => Int32 -> [UserId] -> String -> Maybe E.TeamEvent -> m ()
tUpdate expectedCount uids l (Just e) = liftIO $ do
  assertEqual (l <> ": eventType") E.TeamEvent'TEAM_UPDATE (e ^. eventType)
  assertEqual (l <> ": member count") expectedCount (e ^. eventData . memberCount)
  let maybeBillingUserIds = map (UUID.fromByteString . fromStrict) (e ^. eventData . billingUser)
  assertBool "Invalid UUID found" (all isJust maybeBillingUserIds)
  let billingUserIds = catMaybes maybeBillingUserIds
  assertEqual
    (l <> ": billing users")
    (Set.fromList $ toUUID <$> uids)
    (Set.fromList $ billingUserIds)
tUpdate _ _ l Nothing = liftIO $ assertFailure $ l <> ": Expected 1 TeamUpdate, got nothing"

updateMatcher :: TeamId -> TeamEvent -> Bool
updateMatcher tid e = e ^. eventType == E.TeamEvent'TEAM_UPDATE && decodeIdFromBS (e ^. teamId) == tid

assertTeamUpdate :: (HasCallStack) => String -> TeamId -> Int32 -> [UserId] -> TestM ()
assertTeamUpdate l tid c uids =
  assertIfWatcher l (\e -> e ^. eventType == E.TeamEvent'TEAM_UPDATE && decodeIdFromBS (e ^. teamId) == tid) $ tUpdate c uids

initHttpManager :: IO Manager
initHttpManager = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetDefaultVerifyPaths ctx
  newManager
    (opensslManagerSettings (pure ctx)) -- see Note [SSL context]
      { managerResponseTimeout = responseTimeoutMicro 10000000,
        managerConnCount = 100,
        managerIdleConnectionCount = 300
      }

mkAWSEnv :: JournalOpts -> IO Aws.Env
mkAWSEnv opts = do
  l <- L.new $ L.setOutput L.StdOut . L.setFormat Nothing $ L.defSettings -- TODO: use mkLogger'?
  mgr <- initHttpManager
  Aws.mkEnv l mgr opts

decodeIdFromBS :: ByteString -> Id a
decodeIdFromBS = Id . fromMaybe (error "failed to decode userId") . UUID.fromByteString . fromStrict
