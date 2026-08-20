{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
module Test.Migration.User where

import API.Brig
import qualified API.BrigInternal as I
import API.Common
import API.Galley
import qualified API.GalleyInternal as I
import API.Spar
import Control.Applicative
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.String.Conversions
import Data.Tuple.Extra
import qualified Data.Vector as Vector
import GHC.Stack
import Notifications
import SetupHelpers hiding (deleteUser)
import Test.Bot (mkBotService)
import Test.Migration.Util
import Test.QuickCheck
import Test.Search
import Testlib.MockIntegrationService (MockServerSettings (..), withMockServer)
import Testlib.Prelude
import Testlib.ResourcePool
import UnliftIO

testUserMigrationToPostgres :: App ()
testUserMigrationToPostgres = withMockServer botServiceSettings mkBotService $ \(botHost, botPort) _botChan -> do
  resourcePool <- asks (.resourcePool)

  runCodensity (acquireResources 1 resourcePool) $ \[migratingBackend] -> do
    let domainM = migratingBackend.berDomain
    (mel, pid, sid, seedUsers) <- runCodensity (startDynamicBackend migratingBackend phase1Overrides) $ \_ -> do
      -- mel exists to connect with all the personal users, so we can wait for a
      -- notification for their deletion
      mel <- randomUser domainM def

      pid <- setupProvider domainM def {newProviderPassword = Just defPassword} %. "id" & asString
      service <-
        newService domainM pid
          $ def
            { newServiceUrl = "https://" <> botHost <> ":" <> show botPort,
              newServiceKey = cs botServiceSettings.publicKey
            }
      sid <- service %. "id" & asString
      updateServiceConn domainM pid sid (object ["password" .= defPassword, "enabled" .= True])

      seedUsers <- seedTestUsers domainM mel pid sid
      pure (mel, pid, sid, seedUsers)

    newUsersRef <- newIORef mempty
    updatedUsersRef <- newIORef mempty
    updates <- fmap IntMap.fromList . for [1 .. 5] $ \phase -> do
      (phase,) <$> liftIO (generate (arbitraryPhaseUpdates nUpdates))

    addUsersToFailureContext [("mel", mel)]
      $ addJSONToFailureContext "updates" updates
      $ addJSONToFailureContext "seed users" seedUsers do
        let runPhase :: (HasCallStack) => Int -> App ()
            runPhase phase = do
              runCodensity (startDynamicBackend migratingBackend (phaseOverrides IntMap.! phase)) $ \_ -> do
                let toBeUpdated = seedUsers.updates IntMap.! phase
                    phaseUpdates = updates IntMap.! phase

                updatedScimUsersWithRichInfo <- updateScimUsers domainM toBeUpdated.scimUsersWithRichInfo phaseUpdates.scimUsersWithRichInfo
                updatedScimUsersWithoutRichInfo <- updateScimUsers domainM toBeUpdated.scimUsersWithoutRichInfo phaseUpdates.scimUsersWithoutRichInfo
                updatedPendingScimUsers <- updatePendingScimUsers domainM toBeUpdated.pendingScimUsers phaseUpdates.pendingScimUsers
                updatedSsoUsers <- checkUpdateUser toBeUpdated.ssoUsers.users phaseUpdates.ssoUsers
                updatedPasswordTeamUsers <- checkUpdateUser toBeUpdated.passwordTeamUsers.users phaseUpdates.passwordTeamUsers
                updatedPersonalUsersWithoutHandle <- checkUpdateUser toBeUpdated.personalUsersWithoutHandle phaseUpdates.personalUsersWithoutHandle
                updatedPersonalUsersWithHandle <- checkUpdateUser toBeUpdated.personalUsersWithHandle phaseUpdates.personalUsersWithHandle
                let updatedUsers =
                      TestUserList
                        { scimUsersWithRichInfo = updatedScimUsersWithRichInfo,
                          scimUsersWithoutRichInfo = updatedScimUsersWithoutRichInfo,
                          pendingScimUsers = updatedPendingScimUsers,
                          ssoUsers = toBeUpdated.ssoUsers {users = updatedSsoUsers} :: TestTeamUsers,
                          passwordTeamUsers = toBeUpdated.passwordTeamUsers {users = updatedPasswordTeamUsers} :: TestTeamUsers,
                          personalUsersWithoutHandle = updatedPersonalUsersWithoutHandle,
                          personalUsersWithHandle = updatedPersonalUsersWithHandle,
                          -- Bots don't have any updates
                          botsInTeamConvs = toBeUpdated.botsInTeamConvs,
                          botsInPersonalConvs = toBeUpdated.botsInPersonalConvs
                        }

                newUsers <- createTestUsers domainM mel pid sid nNew
                modifyIORef newUsersRef (IntMap.insert phase newUsers)
                modifyIORef updatedUsersRef (IntMap.insert phase updatedUsers)

                let toBeDeleted = seedUsers.deletes IntMap.! phase

                deleteScimUsers domainM False toBeDeleted.scimUsersWithRichInfo
                deleteScimUsers domainM False toBeDeleted.scimUsersWithoutRichInfo
                deleteScimUsers domainM True toBeDeleted.pendingScimUsers
                deleteTeamUsers toBeDeleted.ssoUsers
                deleteTeamUsers toBeDeleted.passwordTeamUsers
                deletePersonalUsers mel toBeDeleted.personalUsersWithoutHandle
                deletePersonalUsers mel toBeDeleted.personalUsersWithHandle
                deleteBotsTeam toBeDeleted.botsInTeamConvs pid sid
                deleteBotConvs mel toBeDeleted.botsInPersonalConvs

                checkAllDeletionsWorked domainM mel seedUsers.deletes phase
                checkUnaffectedUsers domainM seedUsers.deletes seedUsers.updates phase

                when (phase == 3) $ do
                  waitForMigration domainM userMigrationFinishedCounterName
        runPhase 1
        runPhase 2
        runPhase 3
        runPhase 4
        runPhase 5
  where
    parallelism = 64

    -- Number of users of each type
    nUpdates = 5
    nDeletes = 1
    nNew = 1

    botServiceSettings = def

    seedTestUsers :: (HasCallStack, MakesValue mel) => String -> mel -> String -> String -> App TestUsersByOperations
    seedTestUsers domain mel pid sid =
      fmap mconcat . for [(1 :: Int) .. 5] $ \phase -> do
        updates <- IntMap.singleton phase <$> createTestUsers domain mel pid sid nUpdates
        deletes <- IntMap.singleton phase <$> createTestUsers domain mel pid sid nDeletes
        pure TestUsersByOperations {..}

    tombstone :: String -> String -> Maybe String -> Value
    tombstone domain uid mTid =
      object
        $ [ "accent_id" .= (0 :: Int),
            "assets" .= (),
            "deleted" .= True,
            "id" .= uid,
            "legalhold_status" .= "no_consent",
            "name" .= "default",
            "picture" .= (),
            "qualified_id" .= object ["domain" .= domain, "id" .= uid],
            "searchable" .= True,
            "supported_protocols" .= ["proteus"],
            "type" .= "regular"
          ]
        <> (maybe [] (\tid -> ["team" .= tid]) mTid)

    scimUserIdsWithGetter :: (HasCallStack) => IntMap TestUserList -> [(String, String)]
    scimUserIdsWithGetter relevantSeedUsers =
      foldMap IntMap.elems . for relevantSeedUsers $ \usersInPhase -> do
        map (usersInPhase.scimUsersWithRichInfo.token,) (Map.keys usersInPhase.scimUsersWithRichInfo.users)
          <> map (usersInPhase.scimUsersWithoutRichInfo.token,) (Map.keys usersInPhase.scimUsersWithoutRichInfo.users)
          <> map (usersInPhase.pendingScimUsers.token,) (Map.keys usersInPhase.pendingScimUsers.users)

    nonScimUserIds :: (HasCallStack) => Value -> IntMap TestUserList -> [(Value, Value)]
    nonScimUserIds mel relevantSeedUsers = foldMap IntMap.elems . for relevantSeedUsers $ \usersInPhase -> do
      map (usersInPhase.scimUsersWithRichInfo.owner,) (thd3 <$> Map.elems usersInPhase.scimUsersWithRichInfo.users)
        <> map (usersInPhase.scimUsersWithoutRichInfo.owner,) (thd3 <$> Map.elems usersInPhase.scimUsersWithoutRichInfo.users)
        <> map (usersInPhase.passwordTeamUsers.owner,) (fst <$> Map.elems usersInPhase.passwordTeamUsers.users)
        <> map (mel,) (fst <$> Map.elems usersInPhase.personalUsersWithHandle)
        <> map (mel,) (fst <$> Map.elems usersInPhase.personalUsersWithoutHandle)

    checkAllDeletionsWorked :: (HasCallStack) => String -> Value -> IntMap TestUserList -> Int -> App ()
    checkAllDeletionsWorked domain mel seedUsers phase = do
      let deletedSoFar = IntMap.restrictKeys seedUsers (IntSet.fromList $ [1 .. phase])
      pooledForConcurrentlyN_ parallelism (scimUserIdsWithGetter deletedSoFar) $ \(token, uid) ->
        getScimUser domain token uid >>= assertStatus 404

      pooledForConcurrentlyN_ parallelism (nonScimUserIds mel deletedSoFar) $ \(getter, user) -> do
        uid <- user %. "qualified_id.id" & asString
        mTid <- lookupField user "team" & asStringM
        getUser getter (object ["domain" .= domain, "id" .= uid]) `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json `shouldMatch` tombstone domain uid mTid

      let bots =
            concatMap
              ( \testUsers ->
                  map fst (Map.elems testUsers.botsInTeamConvs.users)
                    <> map fst (Map.elems testUsers.botsInPersonalConvs)
              )
              (IntMap.elems deletedSoFar)
      pooledForConcurrentlyN_ parallelism bots $ \botUser -> do
        botTombstone <- setField "status" "deleted" =<< setField "deleted" True botUser
        getSelf botUser `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json `shouldMatch` botTombstone

    checkUnaffectedUsers :: (HasCallStack) => String -> IntMap TestUserList -> IntMap TestUserList -> Int -> App ()
    checkUnaffectedUsers domain seedUsersToBeUpdated seedUsersToBeDeleted phase = do
      let upcomingPhases = IntSet.fromList [(phase + 1) .. 5]
          _pastPhases = IntSet.fromList [1 .. phase]
          usersNotYetDeleted = IntMap.restrictKeys seedUsersToBeDeleted upcomingPhases
          usersNotYetUpdated = IntMap.restrictKeys seedUsersToBeUpdated upcomingPhases
          existingUserLists = IntMap.elems usersNotYetDeleted <> IntMap.elems usersNotYetUpdated
          existingScimUsers = concatMap extractScimUsers existingUserLists
          existingUsers = concatMap extractTestUsers existingUserLists

      pooledForConcurrentlyN_ parallelism existingScimUsers $ \(token, scimUser) ->
        addJSONToFailureContext "scimUser" scimUser $ do
          uid <- scimUser %. "id" & asString
          getScimUser domain token uid `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            resp.json `shouldMatch` scimUser
      pooledForConcurrentlyN_ parallelism existingUsers $ \user ->
        addJSONToFailureContext "user" user $ do
          getSelf user `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            resp.json `shouldMatch` user

    extractScimUsers :: TestUserList -> [(String, Value)]
    extractScimUsers testUserList =
      map (testUserList.scimUsersWithRichInfo.token,) (fst3 <$> Map.elems testUserList.scimUsersWithRichInfo.users)
        <> map (testUserList.scimUsersWithoutRichInfo.token,) (fst3 <$> Map.elems testUserList.scimUsersWithoutRichInfo.users)
        <> map (testUserList.pendingScimUsers.token,) (fst3 <$> Map.elems testUserList.pendingScimUsers.users)

    extractTestUsers :: TestUserList -> [Value]
    extractTestUsers testUserList =
      map thd3 (Map.elems testUserList.scimUsersWithRichInfo.users)
        <> map thd3 (Map.elems testUserList.scimUsersWithoutRichInfo.users)
        <> map fst (Map.elems testUserList.ssoUsers.users)
        <> map fst (Map.elems testUserList.passwordTeamUsers.users)
        <> map fst (Map.elems testUserList.personalUsersWithHandle)
        <> map fst (Map.elems testUserList.personalUsersWithoutHandle)

    createTestUsers :: (HasCallStack, MakesValue mel) => String -> mel -> String -> String -> Int -> App TestUserList
    createTestUsers domain mel pid sid n = runConcurrently $ do
      scimUsersWithRichInfo <- Concurrently $ createScimUsers domain n True True
      scimUsersWithoutRichInfo <- Concurrently $ createScimUsers domain n False True
      pendingScimUsers <- Concurrently $ createScimUsers domain n False False
      ssoUsers <- Concurrently $ createSsoUsers domain n
      passwordTeamUsers <- Concurrently $ createPasswordTeamUsers domain n
      personalUsersWithoutHandle <- Concurrently $ createPersonalUsers domain mel n False
      personalUsersWithHandle <- Concurrently $ createPersonalUsers domain mel n True
      botsInTeamConvs <- Concurrently $ createTeamBots domain pid sid n
      botsInPersonalConvs <- Concurrently $ createConvsAndAddBot domain mel Nothing pid sid n
      pure TestUserList {..}

    getUnqualifiedUser :: String -> String -> App (Map String Value)
    getUnqualifiedUser domain uid = do
      let quid = object ["domain" .= domain, "id" .= uid]
      Map.singleton uid <$> (getSelf quid >>= getJSON 200)

    createScimUsers :: (HasCallStack) => String -> Int -> Bool -> Bool -> App TestScimUsers
    createScimUsers domain n shouldCreateRichInfo shouldAcceptInvite = do
      (owner, tid, _) <- createTeam domain 1
      tok <- createScimToken owner def >>= \resp -> resp.json %. "token" >>= asString
      users <- fmap Map.unions . pooledReplicateConcurrentlyN 16 n $ do
        newScimUser0 <- randomScimUser
        newScimUser <-
          if shouldCreateRichInfo
            then do
              richInfoKey <- randomAlphaString 10
              richInfoValue <- randomString 10
              modifyObject (KeyMap.insert (fromString "urn:ietf:params:scim:schemas:extension:wire:1.0:User") (object [richInfoKey .= richInfoValue]))
                =<< setField "schemas" ["urn:ietf:params:scim:schemas:core:2.0:User", "urn:ietf:params:scim:schemas:extension:wire:1.0:User"] newScimUser0
            else pure newScimUser0
        email <- asString $ newScimUser %. "emails.0.value"
        inactiveScimUser <- createScimUser domain tok newScimUser >>= getJSON 201
        uid <- inactiveScimUser %. "id" & asString
        (scimUser, userOrInv) <-
          if shouldAcceptInvite
            then do
              registerInvitedUser domain tid email
              scimUser <- getScimUser owner tok uid >>= getJSON 200
              (scimUser,) <$> getUnqualifiedUser domain uid
            else fmap (inactiveScimUser,) . fmap (Map.singleton uid) . getJSON 200 =<< I.getInvitationByEmail domain email
        pure $ (scimUser,defPassword,) <$> userOrInv
      pure $ TestScimUsers owner tok users

    deleteScimUsers :: (HasCallStack) => String -> Bool -> TestScimUsers -> App ()
    deleteScimUsers domain arePendingUsers testScimUsers = do
      withWebSocket testScimUsers.owner $ \wsOwner -> do
        pooledForConcurrentlyN_ parallelism testScimUsers.users $ \(scimUser, _, _) -> do
          uid <- scimUser %. "id" & asString
          deleteScimUser domain testScimUsers.token uid >>= assertSuccess
          getScimUser domain testScimUsers.token uid >>= assertStatus 404

        unless arePendingUsers $ do
          void $ awaitNMatches (Map.size testScimUsers.users) isTeamMemberLeaveNotif wsOwner

    updatePendingScimUserAndCheck :: (HasCallStack) => String -> String -> (Value, String, Value) -> UserUpdate -> App (Value, String, Value)
    updatePendingScimUserAndCheck domain token (scimUser, pw, inv) update = do
      addJSONToFailureContext "update" update . addJSONToFailureContext "scimUser" scimUser $ do
        uid <- scimUser %. "id" & asString
        updatedScimUser <- case update of
          UpdatePassword _ -> do
            pure scimUser
          _ -> do
            let updateScimRecord = case update of
                  UpdateName newName -> setField "displayName" newName
                  UpdateEmail newEmail -> setField "emails" (Array (Vector.singleton (object ["value" .= newEmail])))
                  UpdateHandle newHandle -> setField "userName" newHandle
            updateScimReq <- setField "active" True =<< updateScimRecord scimUser
            updateScimUser domain token uid updateScimReq `bindResponse` \resp -> do
              resp.status `shouldMatchInt` 200
              case update of
                UpdateName newName -> resp.json %. "displayName" `shouldMatch` newName
                UpdateEmail newEmail -> resp.json %. "emails.0.value" `shouldMatch` newEmail
                UpdateHandle newHandle -> resp.json %. "userName" `shouldMatch` newHandle
              assertJust "expected a updated scim user" resp.json
        (updatedUserOrInv, newPassword) <- do
          case update of
            UpdatePassword newPassword -> pure (inv, newPassword)
            _ ->
              -- Changing email of a pending user doesn't generate a new
              -- invitation, perhaps this is a bug?
              -- Changing other things ofc doesn't generate a new invitation.
              pure (inv, pw)
        pure (updatedScimUser, newPassword, updatedUserOrInv)

    updateScimUserAndCheck :: (HasCallStack) => String -> String -> (Value, String, Value) -> UserUpdate -> App (Value, String, Value)
    updateScimUserAndCheck domain token (scimUser, pw, user) update = do
      uid <- scimUser %. "id" & asString
      updatedScimUser <- case update of
        UpdatePassword newPassword -> do
          putPassword user pw newPassword >>= assertSuccess
          pure scimUser
        _ -> do
          updateScimReq <- case update of
            UpdateName newName -> setField "displayName" newName scimUser
            UpdateEmail newEmail -> setField "emails" (Array (Vector.singleton (object ["value" .= newEmail]))) scimUser
            UpdateHandle newHandle -> setField "userName" newHandle scimUser
          updateScimUser domain token uid updateScimReq `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            case update of
              UpdateName newName -> resp.json %. "displayName" `shouldMatch` newName
              UpdateEmail newEmail -> resp.json %. "emails.0.value" `shouldMatch` newEmail
              UpdateHandle newHandle -> resp.json %. "userName" `shouldMatch` newHandle
            assertJust "expected a updated scim user" resp.json
      (updatedUserOrInv, newPassword) <- case update of
        UpdatePassword newPassword -> do
          email <- scimUser %. "emails.0.value" & asString
          login domain email newPassword >>= assertSuccess
          pure (user, newPassword)
        UpdateEmail newEmail -> do
          activateEmail domain newEmail
          getSelf user `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            resp.json %. "email" `shouldMatch` newEmail
            (,pw) <$> assertJust "expected user data" resp.json
        _ -> do
          getSelf user `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 200
            case update of
              UpdateName newName -> resp.json %. "name" `shouldMatch` newName
              UpdateHandle newHandle -> resp.json %. "handle" `shouldMatch` newHandle
            (,pw) <$> assertJust "expected user data" resp.json
      pure (updatedScimUser, newPassword, updatedUserOrInv)

    updateScimUsers :: (HasCallStack) => String -> TestScimUsers -> [UserUpdate] -> App TestScimUsers
    updateScimUsers domain testScimUsers updates = do
      let usersWithUpdates = (zip (Map.elems testScimUsers.users) updates)
      updatedUsers <- fmap Map.unions . pooledForConcurrentlyN parallelism usersWithUpdates $ \((scimUser, pw, user), update) -> do
        uid <- scimUser %. "id" & asString
        Map.singleton uid <$> updateScimUserAndCheck domain testScimUsers.token (scimUser, pw, user) update

      pure $ (testScimUsers {users = updatedUsers} :: TestScimUsers)

    updatePendingScimUsers :: (HasCallStack) => String -> TestScimUsers -> [PendingScimUpdate] -> App TestScimUsers
    updatePendingScimUsers domain testScimUsers updates = do
      let usersWithUpdates = (zip (Map.elems testScimUsers.users) updates)
      updatedUsers <- fmap Map.unions . pooledForConcurrentlyN parallelism usersWithUpdates $ \((scimUser, pw, inv), update) -> do
        uid <- scimUser %. "id" & asString
        email <- scimUser %. "externalId" & asString
        tid <- testScimUsers.owner %. "team" & asString
        Map.singleton uid <$> case update of
          RegisterPendingScimUser -> do
            registerInvitedUser domain tid email
            let quid = object ["domain" .= domain, "id" .= uid]
            fmap (scimUser,pw,) . getJSON 200 =<< getSelf quid
          UpdatePendingScimUser updateUser -> do
            updatePendingScimUserAndCheck domain testScimUsers.token (scimUser, pw, inv) updateUser
      pure (testScimUsers {users = updatedUsers} :: TestScimUsers)

    createSsoUsers :: (HasCallStack) => String -> Int -> App TestTeamUsers
    createSsoUsers domain n = do
      (owner, tid, _) <- createTeam domain 1
      I.setTeamFeatureStatus owner tid "sso" "enabled" >>= assertSuccess
      (createIdpResp, (idpMeta, privcreds)) <- registerTestIdPWithMetaWithPrivateCreds owner
      assertSuccess createIdpResp
      idpId <- asString =<< (createIdpResp.json %. "id")

      users <- fmap Map.unions . pooledReplicateConcurrentlyN 16 n $ do
        subject <- nextSubject
        (mUid, _) <- loginWithSamlWithZHost Nothing domain True tid subject (idpId, (idpMeta, privcreds))
        uid <- assertJust "user id not created by logging in with SAML" mUid
        (,Nothing) <$$> getUnqualifiedUser domain uid
      pure $ TestTeamUsers {..}

    createPasswordTeamUsers :: (HasCallStack) => String -> Int -> App TestTeamUsers
    createPasswordTeamUsers domain n = do
      (owner, _tid, usersWithoutPassword) <- createTeam domain n

      users <- fmap Map.unions . pooledForConcurrentlyN parallelism usersWithoutPassword $ \user -> do
        p <- randomPassword
        putPassword user defPassword p >>= assertSuccess
        uid <- user %. "qualified_id.id" & asString
        pure $ Map.singleton uid (user, Just p)

      pure $ TestTeamUsers {..}

    deleteTeamUsers :: (HasCallStack) => TestTeamUsers -> App ()
    deleteTeamUsers team = do
      withWebSocket team.owner $ \wsOwner -> do
        tid <- team.owner %. "team" & asString
        pooledForConcurrentlyN_ parallelism team.users $ \(user, _) -> do
          uid <- user %. "qualified_id.id" & asString
          deleteTeamMember tid team.owner uid >>= assertSuccess

        void $ awaitNMatches (Map.size team.users) isTeamMemberLeaveNotif wsOwner

    getSelfWithAssertion :: (HasCallStack, MakesValue user) => user -> ((HasCallStack) => Response -> App ()) -> App (Map String Value)
    getSelfWithAssertion user assertion = do
      getSelf user `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        assertion resp
        Map.singleton <$> (resp.json %. "qualified_id.id" & asString) <*> (assertJust "expected GET /self to return a JSON" resp.json)

    checkUpdateUser :: (HasCallStack) => Map String (Value, Maybe String) -> [UserUpdate] -> App (Map String (Value, Maybe String))
    checkUpdateUser users updates = do
      fmap Map.unions . pooledForConcurrentlyN parallelism (zip (Map.elems users) updates) $ \((user, mPassword), update) ->
        addJSONToFailureContext "user" user . addJSONToFailureContext "update" update $ do
          updatedUser <- case (update, mPassword) of
            (UpdateName newName, _) -> do
              putSelf user def {name = Just newName} >>= assertSuccess
              getSelfWithAssertion user $ \resp -> resp.json %. "name" `shouldMatch` newName
            (UpdateEmail newEmail, Just pw) -> do
              oldEmail <- user %. "email" & asString
              (cookie, token) <- bindResponse (login user oldEmail pw) $ \resp -> do
                resp.status `shouldMatchInt` 200
                token <- resp.json %. "access_token" & asString
                let cookie = fromJust $ getCookie "zuid" resp
                pure ("zuid=" <> cookie, token)
              updateEmail user newEmail cookie token >>= assertSuccess
              activateEmail user newEmail
              getSelfWithAssertion user $ \resp -> resp.json %. "email" `shouldMatch` newEmail
            (UpdateEmail {}, Nothing) -> do
              uid <- user %. "qualified_id.id" & asString
              pure $ Map.singleton uid user
            (UpdateHandle newHandle, _) -> do
              putHandle user newHandle >>= assertSuccess
              getSelfWithAssertion user $ \resp -> resp.json %. "handle" `shouldMatch` newHandle
            (UpdatePassword newPassword, Just oldPassword) -> do
              email <- user %. "email" & asString
              putPassword user oldPassword newPassword >>= assertSuccess
              login user email oldPassword `bindResponse` \resp ->
                resp.status `shouldMatchInt` 403
              login user email newPassword >>= assertSuccess
              uid <- user %. "qualified_id.id" & asString
              pure $ Map.singleton uid user
            (UpdatePassword {}, Nothing) -> do
              uid <- user %. "qualified_id.id" & asString
              pure $ Map.singleton uid user
          pure $ (,mPassword) <$> updatedUser

    createPersonalUsers :: (HasCallStack, MakesValue mel) => String -> mel -> Int -> Bool -> App (Map String (Value, Maybe String))
    createPersonalUsers domain mel n claimHandle =
      fmap Map.unions . pooledReplicateConcurrentlyN parallelism n $ do
        user <- randomUser domain def
        connectTwoUsers mel user
        uid <- user %. "qualified_id.id" & asString
        if claimHandle
          then do
            hdl <- randomHandle
            putHandle user hdl >>= assertSuccess
            fmap (,Just defPassword) . Map.singleton uid <$> (setField "handle" hdl user)
          else pure $ Map.singleton uid (user, Just defPassword)

    deletePersonalUsers :: (HasCallStack, MakesValue mel, ToWSConnect mel) => mel -> Map String (Value, Maybe String) -> App ()
    deletePersonalUsers mel users =
      withWebSocket mel $ \wsMel -> do
        pooledForConcurrentlyN_ parallelism users $ uncurry deleteUserWithPassword
        void $ awaitNMatches (Map.size users) isDeleteUserNotif wsMel

    createConvsAndAddBot :: (HasCallStack, MakesValue user) => String -> user -> Maybe String -> String -> String -> Int -> App (Map String (Value, Value))
    createConvsAndAddBot domain user tid pid sid n = do
      fmap Map.unions . pooledReplicateConcurrentlyN parallelism n $ do
        conv <- postConversation user (defProteus {team = tid}) >>= getJSON 201
        convId <- conv %. "qualified_id" & objId
        addBotResp <- addBot user pid sid convId >>= getJSON 201
        botId <- addBotResp %. "id" & asString
        (,conv) <$$> getUnqualifiedUser domain botId

    createTeamBots :: (HasCallStack) => String -> String -> String -> Int -> App TestTeamUsers
    createTeamBots domain pid sid n = do
      (owner, tid, _) <- createTeam domain 1
      postServiceWhitelist owner tid (object ["id" .= sid, "provider" .= pid, "whitelisted" .= True])
        >>= assertSuccess
      TestTeamUsers owner . fmap (\(x, _) -> (x, Nothing)) <$> createConvsAndAddBot domain owner (Just tid) pid sid n

    deleteBotsTeam :: (HasCallStack) => TestTeamUsers -> String -> String -> App ()
    deleteBotsTeam testTeam pid sid = do
      tid <- testTeam.owner %. "team" & asString
      withWebSocket testTeam.owner $ \ws -> do
        postServiceWhitelist testTeam.owner tid (object ["id" .= sid, "provider" .= pid, "whitelisted" .= False]) >>= assertSuccess
        void $ awaitNMatches (Map.size testTeam.users) isConvLeaveNotif ws

    deleteBotConvs :: (HasCallStack) => Value -> Map String (Value, Value) -> App ()
    deleteBotConvs mel botConvs = do
      pooledForConcurrentlyN_ parallelism (Map.elems botConvs) $ \(bot, conv) -> do
        cid <- conv %. "qualified_id.id" & asString
        bid <- bot %. "qualified_id.id" & asString
        rmBotSelf mel bid cid >>= assertSuccess

-- | This test creates users in PG and Cassandra separately to simulate a
-- situation where there are users in both DBs. Then tries to index them into ES
-- to make sure the pagination over these users works.
testReindexingUsersDuringMigration :: (HasCallStack) => App ()
testReindexingUsersDuringMigration = do
  resourcePool <- asks (.resourcePool)

  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain
    -- Create users in cassandra using 'phase1Overrides'
    (casSearcher, casExistingUsers, casDeletedUsers) <-
      runCodensity (startDynamicBackend backend phase1Overrides)
        $ \_ -> setupUsers domain

    -- Create users in postgres using 'phase5Overrides'
    (pgSearcher, pgExistingUsers, pgDeletedUsers) <-
      runCodensity (startDynamicBackend backend phase5Overrides)
        $ \_ -> setupUsers domain

    -- Test that searching in the already existing index works with in
    -- 'phase2Overrides', which should work with data in cassandra and postgres
    runCodensity (startDynamicBackend backend phase2Overrides) $ \_ -> do
      I.refreshIndex domain
      checkSearchWorks domain casSearcher casExistingUsers casDeletedUsers
      checkSearchWorks domain pgSearcher pgExistingUsers pgDeletedUsers

    newIndex <- createNewIndex
    let backendWithNewIndex = backend {berElasticsearchIndex = newIndex}
    runCodensity (startDynamicBackend backendWithNewIndex phase2Overrides) $ \_ -> do
      reindexUsers backendWithNewIndex phase2Overrides 5
      I.refreshIndex domain
      checkSearchWorks domain casSearcher casExistingUsers casDeletedUsers
      checkSearchWorks domain pgSearcher pgExistingUsers pgDeletedUsers
  where
    n = 5
    parallelism = 16

    setupUsers :: (HasCallStack) => String -> App (Value, [Value], [Value])
    setupUsers domain = do
      searcher <- randomUser domain def
      existingUsers <- pooledReplicateConcurrentlyN parallelism n $ randomUser domain def
      deletedUsers <- pooledReplicateConcurrentlyN parallelism n $ do
        u <- randomUser domain def
        connectTwoUsers searcher u
        pure u
      withWebSocket searcher $ \ws -> do
        pooledForConcurrentlyN_ parallelism deletedUsers deleteUser
        awaitNMatches n isDeleteUserNotif ws
      pure (searcher, existingUsers, deletedUsers)

    checkSearchWorks :: (HasCallStack) => String -> Value -> [Value] -> [Value] -> App ()
    checkSearchWorks domain searcher existingUsers deletedUsers = do
      pooledForConcurrentlyN_ parallelism existingUsers $ \u ->
        assertCanFind searcher u (u %. "name") domain

      pooledForConcurrentlyN_ parallelism deletedUsers $ \u ->
        assertCannotFind searcher u (u %. "name") domain

-- * Test Helpers

data TestUsersByOperations = TestUsersByOperations
  { updates :: IntMap TestUserList,
    deletes :: IntMap TestUserList
  }
  deriving (Show, Eq, Generic)

instance Semigroup TestUsersByOperations where
  users1 <> users2 =
    TestUsersByOperations
      { updates = users1.updates <> users2.updates,
        deletes = users1.deletes <> users2.deletes
      }

instance Monoid TestUsersByOperations where
  mempty = TestUsersByOperations {updates = mempty, deletes = mempty}

instance ToJSON TestUsersByOperations

-- \|
-- TODO: Add Weird cases
-- - Users without a name
-- - Users without activated
-- - Users with unclaimed handles
data TestUserList = TestUserList
  { scimUsersWithRichInfo :: TestScimUsers,
    scimUsersWithoutRichInfo :: TestScimUsers,
    pendingScimUsers :: TestScimUsers,
    ssoUsers :: TestTeamUsers,
    passwordTeamUsers :: TestTeamUsers,
    personalUsersWithoutHandle :: Map String (Value, Maybe String),
    personalUsersWithHandle :: Map String (Value, Maybe String),
    botsInTeamConvs :: TestTeamUsers,
    -- UserId -> (User, Conv)
    botsInPersonalConvs :: Map String (Value, Value)
  }
  deriving (Show, Eq)

data TestScimUsers = TestScimUsers
  { owner :: Value,
    token :: String,
    -- | ScimUser, Password, UserOrInv
    users :: Map String (Value, String, Value)
  }
  deriving (Show, Eq)

data TestTeamUsers = TestTeamUsers
  { owner :: Value,
    -- | (user, maybe password)
    users :: Map String (Value, Maybe String)
  }
  deriving (Show, Eq)

instance ToJSON TestUserList where
  toJSON userList = do
    object
      [ fromString "scimUsersWithRichInfo" .= Map.keys userList.scimUsersWithRichInfo.users,
        fromString "scimUsersWithoutRichInfo" .= Map.keys userList.scimUsersWithoutRichInfo.users,
        fromString "pendingScimUsers" .= Map.keys userList.pendingScimUsers.users,
        fromString "ssoUsers" .= Map.keys userList.ssoUsers.users,
        fromString "passwordTeamUsers" .= Map.keys userList.passwordTeamUsers.users,
        fromString "personalUsersWithoutHandle" .= Map.keys userList.personalUsersWithoutHandle,
        fromString "personalUsersWithHandle" .= Map.keys userList.personalUsersWithHandle,
        fromString "botsInTeamConvs" .= Map.keys userList.botsInTeamConvs.users,
        fromString "botsInPersonalConvs" .= Map.keys userList.botsInPersonalConvs
      ]

data UserUpdate
  = UpdateName String
  | UpdateHandle String
  | UpdateEmail String
  | UpdatePassword String
  deriving (Show, Eq, Generic)

instance Arbitrary UserUpdate where
  arbitrary =
    oneof
      [ UpdateName <$> arbitraryName,
        UpdateHandle <$> arbitraryHandle,
        UpdateEmail <$> arbitraryEmail,
        UpdatePassword <$> arbitraryPassword
      ]

instance ToJSON UserUpdate

arbitraryNonPasswordUpdate :: Gen UserUpdate
arbitraryNonPasswordUpdate =
  oneof
    [ UpdateName <$> arbitraryName,
      UpdateHandle <$> arbitraryHandle,
      UpdateEmail <$> arbitraryEmail
    ]

data PendingScimUpdate
  = RegisterPendingScimUser
  | UpdatePendingScimUser UserUpdate
  deriving (Show, Eq, Generic)

instance Arbitrary PendingScimUpdate where
  arbitrary =
    oneof
      [ pure RegisterPendingScimUser,
        UpdatePendingScimUser <$> arbitraryNonPasswordUpdate
      ]

instance ToJSON PendingScimUpdate

data PhaseUpdates = PhaseUpdates
  { scimUsersWithRichInfo :: [UserUpdate],
    scimUsersWithoutRichInfo :: [UserUpdate],
    pendingScimUsers :: [PendingScimUpdate],
    ssoUsers :: [UserUpdate],
    passwordTeamUsers :: [UserUpdate],
    personalUsersWithoutHandle :: [UserUpdate],
    personalUsersWithHandle :: [UserUpdate]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PhaseUpdates

arbitraryPhaseUpdates :: Int -> Gen PhaseUpdates
arbitraryPhaseUpdates n = do
  scimUsersWithRichInfo <- replicateM n arbitrary
  scimUsersWithoutRichInfo <- replicateM n arbitrary
  pendingScimUsers <- replicateM n arbitrary
  ssoUsers <- replicateM n arbitraryNonPasswordUpdate
  passwordTeamUsers <- replicateM n arbitrary
  personalUsersWithoutHandle <- replicateM n arbitrary
  personalUsersWithHandle <- replicateM n arbitrary
  pure PhaseUpdates {..}

userMigrationFinishedCounterName :: String
userMigrationFinishedCounterName = "^wire_users_migration_finished"

commonOverrides, phase1Overrides, phase2Overrides, phase3Overrides, phase4Overrides, phase5Overrides :: ServiceOverrides
commonOverrides =
  def
    { brigCfg =
        setField @_ @Int "optSettings.setUserMaxConnections" 500
          >=> setField @_ @Int "optSettings.setActivationTimeout" 3600
          >=> setField @_ @Int "optSettings.setVerificationTimeout" 3600
          >=> setField @_ @Int "optSettings.setTeamInvitationTimeout" 3600
          >=> setField @_ @Int "optSettings.setUserCookieRenewAge" 1209600
          >=> setField @_ @Int "postgresqlPool.size" 200
          >=> removeField "optSettings.setSuspendInactiveUsers"
    }
phase1Overrides =
  commonOverrides
    <> def
      { brigCfg = setField "postgresMigration.user" "cassandra",
        galleyCfg = setField "postgresMigration.user" "cassandra",
        backgroundWorkerCfg =
          setField "postgresMigration.user" "cassandra"
            >=> setField "migrateUsers" False
      }
phase2Overrides =
  commonOverrides
    <> def
      { brigCfg = setField "postgresMigration.user" "migration-to-postgresql",
        galleyCfg = setField "postgresMigration.user" "migration-to-postgresql",
        backgroundWorkerCfg =
          setField "postgresMigration.user" "migration-to-postgresql"
            >=> setField "migrateUsers" False
      }
phase3Overrides =
  commonOverrides
    <> def
      { brigCfg = setField "postgresMigration.user" "migration-to-postgresql",
        galleyCfg = setField "postgresMigration.user" "migration-to-postgresql",
        backgroundWorkerCfg =
          setField "postgresMigration.user" "migration-to-postgresql"
            >=> setField "migrateUsers" True
      }
phase4Overrides =
  commonOverrides
    <> def
      { brigCfg = setField "postgresMigration.user" "migration-to-postgresql",
        galleyCfg = setField "postgresMigration.user" "migration-to-postgresql",
        backgroundWorkerCfg =
          setField "postgresMigration.user" "migration-to-postgresql"
            >=> setField "migrateUsers" False
      }
phase5Overrides =
  commonOverrides
    <> def
      { brigCfg = setField "postgresMigration.user" "postgresql",
        galleyCfg = setField "postgresMigration.user" "postgresql",
        backgroundWorkerCfg =
          setField "postgresMigration.user" "postgresql"
            >=> setField "migrateUsers" False
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
