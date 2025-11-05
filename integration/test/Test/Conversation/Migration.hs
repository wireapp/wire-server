-- | The migration has these phases.
-- 1. Write to cassandra (before any migration activity)
-- 2. Galley is prepared for migrations (new things created in PG, old things are in Cassandra)
-- 3. Backgound worker starts migration
-- 4. Background worker finishes migration, galley is still configured to think migration is on going
-- 5. Background worker is configured to not do anything, galley is configured to only use PG
--
-- The comments and variable names call these phases by number i.e. Phase1, Phase2, and so on.
--
-- The tests are from the perspective of mel, a user on the dynamic backend,
-- called backendM (migrating backend). There are also users called mark and mia
-- on this backend.
module Test.Conversation.Migration where

import API.Galley
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack
import MLS.Util
import Notifications
import SetupHelpers hiding (deleteUser)
import Testlib.Prelude
import Testlib.ResourcePool
import Text.Regex.TDFA ((=~))
import UnliftIO

-- | Our test setup cannot process updates to many MLS convs concurrently, so we
-- run this will only 1 conv per type per phase and use no concurrency.
testMigrationToPostgresMLS :: App ()
testMigrationToPostgresMLS = do
  resourcePool <- asks (.resourcePool)
  (alice, aliceTid, _) <- createTeam OwnDomain 1
  (bob, bobTid, _) <- createTeam OtherDomain 1
  [aliceC, bobC] <- traverse (createMLSClient def) [alice, bob]

  runCodensity (acquireResources 1 resourcePool) $ \[migratingBackend] -> do
    let domainM = migratingBackend.berDomain
    (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs) <- runCodensity (startDynamicBackend migratingBackend phase1Overrides) $ \_ -> do
      [mel, mark] <- createUsers [domainM, domainM]
      (mia, miaTid, _) <- createTeam domainM 1
      [melC, markC, miaC] <- traverse (createMLSClient def) [mel, mark, mia]
      connectUsers [alice, bob, mel, mark, mia]
      otherMelConvs <- getAllConvIds mel 100

      domainAConvs <- createTestConvs aliceC aliceTid melC markC []
      domainBConvs <- createTestConvs bobC bobTid melC markC []
      domainMConvs <- createTestConvs miaC miaTid melC markC []
      pure (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs)

    newConvsRef <- newIORef []
    addUsersToFailureContext [("alice", alice), ("bob", bob), ("mel", mel), ("mark", mark), ("mia", mia)]
      $ addJSONToFailureContext "convIds" (domainAConvs <> domainBConvs <> domainMConvs)
      $ addJSONToFailureContext "otherMelConvs" otherMelConvs
      $ do
        let runPhase :: (HasCallStack) => Int -> App ()
            runPhase phase = do
              runCodensity (startDynamicBackend migratingBackend (phaseOverrides IntMap.! phase)) $ \_ -> do
                newDomainAConvs <- runPhaseOperations phase aliceC aliceTid domainAConvs melC markC
                newDomainBConvs <- runPhaseOperations phase bobC bobTid domainBConvs melC markC
                newDomainCConvs <- runPhaseOperations phase miaC miaTid domainMConvs melC markC
                let newConvs = newDomainAConvs <> newDomainBConvs <> newDomainCConvs
                modifyIORef newConvsRef (newConvs <>)
                allNewConvs <- readIORef newConvsRef
                actualConvs <- getAllConvIds mel n
                let expectedConvsFrom dom =
                      dom.unmodifiedConvs
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.kickMelConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems dom.kickMarkConvs)
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.delConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.addMelConvs (IntSet.fromList [1 .. phase])))
                    expectedConvs =
                      expectedConvsFrom domainAConvs
                        <> expectedConvsFrom domainBConvs
                        <> expectedConvsFrom domainMConvs
                        <> allNewConvs

                actualConvs `shouldMatchSet` ((convIdToQidObject <$> expectedConvs) <> otherMelConvs)

                when (phase == 3) $ waitForMigration domainM
        runPhase 1
        runPhase 2
        runPhase 3
        runPhase 4
        runPhase 5
  where
    n = 1
    createTestConvs :: (HasCallStack) => ClientIdentity -> String -> ClientIdentity -> ClientIdentity -> [ClientIdentity] -> App TestConvList
    createTestConvs creatorC tid melC markC othersC = do
      unmodifiedConvs <- replicateM n $ do
        createTestConv creatorC tid (melC : markC : othersC)

      kickMelConvs <- forPhase $ createTestConv creatorC tid (melC : othersC)
      kickMarkConvs <- forPhase $ createTestConv creatorC tid (melC : markC : othersC)
      delConvs <- forPhase $ createTestConv creatorC tid (melC : markC : othersC)
      addMelConvs <- forPhase $ createTestConv creatorC tid othersC
      pure $ TestConvList {..}

    createTestConv :: (HasCallStack) => ClientIdentity -> String -> [ClientIdentity] -> App ConvId
    createTestConv creatorC tid membersC = do
      conv <- createNewGroupWith def creatorC defMLS {team = Just tid}
      traverse_ (uploadNewKeyPackage def) membersC
      void $ createAddCommit creatorC conv ((.qualifiedUserId) <$> membersC) >>= sendAndConsumeCommitBundle
      pure conv

    forPhase :: App a -> App (IntMap [a])
    forPhase action =
      fmap IntMap.fromList . for [1 .. 5] $ \phase -> do
        convs <- replicateM n $ action
        pure (phase, convs)

    runPhaseOperations :: (HasCallStack) => Int -> ClientIdentity -> String -> TestConvList -> ClientIdentity -> ClientIdentity -> App [ConvId]
    runPhaseOperations phase convAdmin tid TestConvList {..} melC markC = do
      for_ (IntMap.findWithDefault [] phase kickMelConvs) $ \convId -> do
        mp <- createRemoveCommit convAdmin convId [melC]
        void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      for_ (IntMap.findWithDefault [] phase kickMarkConvs) $ \convId -> do
        mp <- createRemoveCommit convAdmin convId [markC]
        void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      for_ (IntMap.findWithDefault [] phase delConvs) $ \convId -> do
        deleteTeamConversation tid convId convAdmin >>= assertSuccess
        getConversation convAdmin convId `bindResponse` \resp ->
          resp.status `shouldMatchInt` 404

      for_ (IntMap.findWithDefault [] phase addMelConvs) $ \convId -> do
        void $ uploadNewKeyPackage def melC
        void $ createAddCommit convAdmin convId [melC.qualifiedUserId] >>= sendAndConsumeCommitBundle

      replicateM n $ createTestConv convAdmin tid [melC]

testMigrationToPostgresProteus :: App ()
testMigrationToPostgresProteus = do
  resourcePool <- asks (.resourcePool)
  (alice, aliceTid, _) <- createTeam OwnDomain 1
  (bob, bobTid, _) <- createTeam OtherDomain 1

  runCodensity (acquireResources 1 resourcePool) $ \[migratingBackend] -> do
    let domainM = migratingBackend.berDomain
    (mel, _melC, mark, _markC, mia, _miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs) <- runCodensity (startDynamicBackend migratingBackend phase1Overrides) $ \_ -> do
      [mel, mark] <- createUsers [domainM, domainM]
      (mia, miaTid, _) <- createTeam domainM 1
      [melC, markC, miaC] <- traverse (createMLSClient def) [mel, mark, mia]
      connectUsers [alice, bob, mel, mark, mia]
      otherMelConvs <- getAllConvIds mel 100

      -- Other convs which just exist
      pooledReplicateConcurrentlyN_ parallellism 100 $ createTestConv mia miaTid []
      pooledReplicateConcurrentlyN_ parallellism 100 $ createTestConv alice aliceTid [mia]
      pooledReplicateConcurrentlyN_ parallellism 100 $ createTestConv bob bobTid [mia]

      domainAConvs <- createTestConvs alice aliceTid mel mark []
      domainBConvs <- createTestConvs bob bobTid mel mark []
      domainMConvs <- createTestConvs mia miaTid mel mark []
      pure (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs)

    newConvsRef <- newIORef []
    addUsersToFailureContext [("alice", alice), ("bob", bob), ("mel", mel), ("mark", mark), ("mia", mia)]
      $ addJSONToFailureContext "convIds" (domainAConvs <> domainBConvs <> domainMConvs)
      $ addJSONToFailureContext "otherMelConvs" otherMelConvs
      $ do
        let runPhase :: (HasCallStack) => Int -> App ()
            runPhase phase = do
              runCodensity (startDynamicBackend migratingBackend (phaseOverrides IntMap.! phase)) $ \_ -> do
                newDomainAConvs <- runPhaseOperations phase alice aliceTid domainAConvs mel mark
                newDomainBConvs <- runPhaseOperations phase bob bobTid domainBConvs mel mark
                newDomainCConvs <- runPhaseOperations phase mia miaTid domainMConvs mel mark
                let newConvs = newDomainAConvs <> newDomainBConvs <> newDomainCConvs
                modifyIORef newConvsRef (newConvs <>)
                allNewConvs <- readIORef newConvsRef
                actualConvs <- getAllConvIds mel n
                let expectedConvsFrom dom =
                      dom.unmodifiedConvs
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.kickMelConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems dom.kickMarkConvs)
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.delConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.addMelConvs (IntSet.fromList [1 .. phase])))
                    expectedConvs =
                      expectedConvsFrom domainAConvs
                        <> expectedConvsFrom domainBConvs
                        <> expectedConvsFrom domainMConvs
                        <> allNewConvs

                actualConvs `shouldMatchSet` ((convIdToQidObject <$> expectedConvs) <> otherMelConvs)

                when (phase == 3) $ waitForMigration domainM
        runPhase 1
        runPhase 2
        runPhase 3
        runPhase 4
        runPhase 5
  where
    n = 20
    parallellism = 8
    createTestConvs :: (HasCallStack) => Value -> String -> Value -> Value -> [Value] -> App TestConvList
    createTestConvs creatorC tid mel mark others = do
      unmodifiedConvs <- pooledReplicateConcurrentlyN parallellism n $ do
        createTestConv creatorC tid (mel : mark : others)

      kickMelConvs <- forPhase $ createTestConv creatorC tid (mel : others)
      kickMarkConvs <- forPhase $ createTestConv creatorC tid (mel : mark : others)
      delConvs <- forPhase $ createTestConv creatorC tid (mel : mark : others)
      addMelConvs <- forPhase $ createTestConv creatorC tid others
      pure $ TestConvList {..}

    createTestConv :: (HasCallStack) => Value -> String -> [Value] -> App ConvId
    createTestConv creator tid members = do
      postConversation creator defProteus {team = Just tid, qualifiedUsers = members}
        >>= getJSON 201
        >>= objConvId

    forPhase :: App a -> App (IntMap [a])
    forPhase action =
      fmap IntMap.fromList . pooledForConcurrentlyN parallellism [1 .. 5] $ \phase -> do
        convs <- replicateM n $ action
        pure (phase, convs)

    retry500Once :: App Response -> App Response
    retry500Once action = do
      action `bindResponse` \resp -> do
        if resp.status == 500
          then action
          else pure resp

    runPhaseOperations :: (HasCallStack) => Int -> Value -> String -> TestConvList -> Value -> Value -> App [ConvId]
    runPhaseOperations phase convAdmin tid TestConvList {..} mel mark = do
      withWebSocket mel $ \melWS -> do
        pooledForConcurrentlyN_ parallellism (IntMap.findWithDefault [] phase kickMelConvs) $ \convId -> do
          retry500Once (removeMember convAdmin convId mel) >>= assertSuccess

        void $ awaitNMatches n isConvLeaveNotif melWS

        pooledForConcurrentlyN_ parallellism (IntMap.findWithDefault [] phase kickMarkConvs) $ \convId -> do
          retry500Once (removeMember convAdmin convId mark) >>= assertSuccess

        void $ awaitNMatches n isConvLeaveNotif melWS

        pooledForConcurrentlyN_ parallellism (IntMap.findWithDefault [] phase delConvs) $ \convId -> do
          retry500Once (deleteTeamConversation tid convId convAdmin) >>= assertSuccess

        pooledForConcurrentlyN_ parallellism (IntMap.findWithDefault [] phase addMelConvs) $ \convId -> do
          retry500Once (addMembers convAdmin convId (def {users = [mel]})) >>= assertSuccess

        void $ awaitNMatches n isConvDeleteNotif melWS
        pooledReplicateConcurrentlyN parallellism n
          $ createTestConv convAdmin tid [mel]

-- Test Helpers

data TestConvList = TestConvList
  { unmodifiedConvs :: [ConvId],
    kickMelConvs :: IntMap [ConvId],
    kickMarkConvs :: IntMap [ConvId],
    delConvs :: IntMap [ConvId],
    addMelConvs :: IntMap [ConvId]
  }

instance ToJSON TestConvList where
  toJSON convList = do
    object
      [ fromString "unmodifiedConvs" .= (mkId <$> convList.unmodifiedConvs),
        fromString "kickMelConvs" .= (mkId <$$> convList.kickMelConvs),
        fromString "kickMarkConvs" .= (mkId <$$> convList.kickMarkConvs),
        fromString "delConvs" .= (mkId <$$> convList.delConvs),
        fromString "addMelConvs" .= (mkId <$$> convList.addMelConvs)
      ]
    where
      mkId :: ConvId -> String
      mkId cid = cid.id_ <> "@" <> cid.domain

instance Semigroup TestConvList where
  l1 <> l2 =
    TestConvList
      { unmodifiedConvs = l1.unmodifiedConvs <> l2.unmodifiedConvs,
        kickMelConvs = IntMap.unionWith (<>) l1.kickMelConvs l2.kickMelConvs,
        kickMarkConvs = IntMap.unionWith (<>) l1.kickMarkConvs l2.kickMarkConvs,
        delConvs = IntMap.unionWith (<>) l1.delConvs l2.delConvs,
        addMelConvs = IntMap.unionWith (<>) l1.addMelConvs l2.addMelConvs
      }

waitForMigration :: (HasCallStack) => String -> App ()
waitForMigration domainM = do
  metrics <-
    getMetrics domainM BackgroundWorker `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      pure $ Text.decodeUtf8 resp.body
  let (_, _, _, convFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_local_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
  let (_, _, _, userFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_user_remote_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
  when (convFinishedMatches /= [Text.pack "1.0"] || userFinishedMatches /= [Text.pack "1.0"]) $ do
    liftIO $ threadDelay 100_000
    waitForMigration domainM

phase1Overrides, phase2Overrides, phase3Overrides, phase4Overrides, phase5Overrides :: ServiceOverrides
phase1Overrides =
  def
    { galleyCfg = setField "postgresMigration.conversation" "cassandra",
      backgroundWorkerCfg = setField "migrateConversations" False
    }
phase2Overrides =
  def
    { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
      backgroundWorkerCfg = setField "migrateConversations" False
    }
phase3Overrides =
  def
    { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
      backgroundWorkerCfg = setField "migrateConversations" True
    }
phase4Overrides =
  def
    { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
      backgroundWorkerCfg = setField "migrateConversations" False
    }
phase5Overrides =
  def
    { galleyCfg = setField "postgresMigration.conversation" "postgresql",
      backgroundWorkerCfg = setField "migrateConversations" False
    }

phaseOverrides :: IntMap ServiceOverrides
phaseOverrides =
  IntMap.fromList
    [ (1, phase1Overrides),
      (2, phase2Overrides),
      (3, phase3Overrides),
      (4, phase4Overrides),
      (5, phase5Overrides)
    ]
