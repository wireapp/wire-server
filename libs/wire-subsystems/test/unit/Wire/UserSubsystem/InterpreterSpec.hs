{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Control.Lens.At ()
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Handle
import Data.Id
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as S
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User hiding (DeleteUser)
import Wire.API.UserEvent
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist
import Wire.UserSubsystem.Interpreter (UserSubsystemConfig (..))

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfiles" do
    describe "[with federation]" do
      prop "gets all users on multiple federating backends" $
        \viewerTeam (localTargetUsersNotPending :: [NotPendingStoredUser]) targetUsers1 targetUsers2 visibility localDomain remoteDomain1 remoteDomain2 -> do
          let remoteBackend1 = def {users = targetUsers1}
              remoteBackend2 = def {users = targetUsers2}
              viewer = viewerTeam {teamId = Nothing}
              -- Having teams adds complications in email visibility,
              -- all that stuff is tested in [without federation] tests
              localTargetUsers = map (\user -> (coerce user) {teamId = Nothing}) localTargetUsersNotPending
              federation = [(remoteDomain1, remoteBackend1), (remoteDomain2, remoteBackend2)]
              mkUserIds domain = map (flip Qualified domain . (.id))
              localTargets = mkUserIds localDomain localTargetUsers
              target1 = mkUserIds remoteDomain1 targetUsers1
              target2 = mkUserIds remoteDomain2 targetUsers2
              localBackend = def {users = [viewer] <> localTargetUsers}
              retrievedProfiles =
                runFederationStack localBackend federation Nothing (UserSubsystemConfig visibility miniLocale) $
                  getUserProfiles
                    (toLocalUnsafe localDomain viewer.id)
                    (localTargets <> target1 <> target2)
              mkExpectedProfiles domain users =
                [ mkUserProfileWithEmail
                    Nothing
                    (mkUserFromStored domain miniLocale targetUser)
                    defUserLegalHoldStatus
                  | targetUser <- users
                ]
              expectedLocalProfiles = mkExpectedProfiles localDomain localTargetUsers
              expectedProfiles1 = mkExpectedProfiles remoteDomain1 targetUsers1
              expectedProfiles2 = mkExpectedProfiles remoteDomain2 targetUsers2
              expectedProfiles = expectedLocalProfiles <> expectedProfiles1 <> expectedProfiles2

          sortOn (.profileQualifiedId) retrievedProfiles
            === sortOn (.profileQualifiedId) expectedProfiles

    prop "fails when a backend is offline or returns an error" $
      \viewer onlineTargetUsers (offlineTargetUsers :: [StoredUser]) visibility localDomain onlineDomain (offlineDomain :: Domain) -> do
        let onlineRemoteBackend = def {users = onlineTargetUsers}
            online = [(onlineDomain, onlineRemoteBackend)]
            mkUserIds domain users = map (flip Qualified domain . (.id)) users
            onlineUsers = mkUserIds onlineDomain onlineTargetUsers
            offlineUsers = mkUserIds offlineDomain offlineTargetUsers
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            result =
              run
                . runErrorUnsafe @UserSubsystemError
                . runError @FederationError
                . interpretFederationStack localBackend online Nothing config
                $ getUserProfiles
                  (toLocalUnsafe localDomain viewer.id)
                  (onlineUsers <> offlineUsers)

        localDomain /= offlineDomain && not (null offlineTargetUsers) ==>
          -- The FederationError doesn't have an instance
          -- for Eq because of dependency on HTTP2Error
          first displayException result
            === Left (displayException (FederationUnexpectedError "RunFederatedEither"))

    describe "[without federation]" do
      prop "returns nothing when none of the users exist" $
        \viewer targetUserIds visibility domain locale ->
          let config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack def Nothing config $
                  getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
           in retrievedProfiles === []

      prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
        \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfiles =
                runNoFederationStack localBackend (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfiles
                === [ mkUserProfile
                        (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                        (mkUserFromStored domain locale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "gets a local user profile when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
        \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfile =
                runNoFederationStack localBackend (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfile
                === [ mkUserProfile
                        (fmap (const Nothing) visibility)
                        (mkUserFromStored domain locale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "returns Nothing if the target user has not accepted their invitation yet" $
        \viewer (PendingStoredUser targetUser) visibility domain locale ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfile =
                runNoFederationStack localBackend (Just teamMember) config $
                  getLocalUserProfiles (toLocalUnsafe domain [targetUser.id])
           in retrievedProfile === []

  describe "getUserProfilesWithErrors" $ do
    prop "If no errors, same behavior as getUserProfiles" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend federation Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (map (flip Qualified remoteDomain . (.id)) targetUsers)
            retrievedProfiles :: [UserProfile] =
              runFederationStack localBackend federation Nothing config $
                getUserProfiles
                  (toLocalUnsafe domain viewer.id)
                  (map (flip Qualified remoteDomain . (.id)) targetUsers)
        remoteDomain /= domain ==>
          counterexample ("Retrieved profiles with errors: " <> show retrievedProfilesWithErrors) do
            length (fst retrievedProfilesWithErrors) === 0
            .&&. snd retrievedProfilesWithErrors === retrievedProfiles
            .&&. length (snd retrievedProfilesWithErrors) === length targetUsers

    prop "Remote users on offline backend always fail to return" $
      \viewer (targetUsers :: Set StoredUser) visibility domain remoteDomain -> do
        let online = mempty
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  ( map (flip Qualified remoteDomain . (.id)) $
                      S.toList targetUsers
                  )
        remoteDomain /= domain ==>
          length (fst retrievedProfilesWithErrors) === length targetUsers
            .&&. length (snd retrievedProfilesWithErrors) === 0

    prop "Remote users with one offline and one online backend return errors for offline backend but successed with online backend" $
      \viewer targetUsers visibility domain remoteDomainA remoteDomainB -> do
        let remoteBackendA = def {users = targetUsers}
            online = [(remoteDomainA, remoteBackendA)]
            allDomains = [domain, remoteDomainA, remoteDomainB]
            remoteAUsers = map (flip Qualified remoteDomainA . (.id)) targetUsers
            remoteBUsers = map (flip Qualified remoteDomainB . (.id)) targetUsers
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (remoteAUsers <> remoteBUsers)
        nub allDomains == allDomains ==>
          length (fst retrievedProfilesWithErrors) === length remoteBUsers
            .&&. length (snd retrievedProfilesWithErrors) === length remoteAUsers

  describe "getSelfProfile" $ do
    prop "should retrieve a user which exists in the DB" \storedSelf otherStoredUsers domain config ->
      let localBackend = def {users = storedSelf : filter (\u -> u.id /= storedSelf.id) otherStoredUsers}
          retrievedProfile =
            runNoFederationStack localBackend Nothing config $
              getSelfProfile (toLocalUnsafe domain storedSelf.id)
       in retrievedProfile === Just (SelfProfile $ mkUserFromStored domain config.defaultLocale storedSelf)

    prop "should fail when the user does not exist in the DB" \selfId otherStoredUsers domain config ->
      let localBackend = def {users = filter (\u -> u.id /= selfId) otherStoredUsers}
          retrievedProfile =
            runNoFederationStack localBackend Nothing config $
              getSelfProfile (toLocalUnsafe domain selfId)
       in retrievedProfile === Nothing

    prop "should mark user as managed by scim if E2EId is enabled for the user and they have a handle" \storedSelf domain susbsystemConfig mlsE2EIdConfig ->
      let localBackend = def {users = [storedSelf]}
          allFeatureConfigs = def {afcMlsE2EId = withStatus FeatureStatusEnabled LockStatusUnlocked mlsE2EIdConfig FeatureTTLUnlimited}
          SelfProfile retrievedUser =
            fromJust
              . runAllErrorsUnsafe
              . interpretNoFederationStack localBackend Nothing allFeatureConfigs susbsystemConfig
              $ getSelfProfile (toLocalUnsafe domain storedSelf.id)
          expectedManagedBy = case storedSelf.handle of
            Nothing -> fromMaybe ManagedByWire storedSelf.managedBy
            Just _ -> ManagedByScim
       in retrievedUser.userManagedBy === expectedManagedBy

  describe "updateUserProfile" $ do
    prop "Update user" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            userBeforeUpdate = mkUserFromStored localDomain config.defaultLocale alice
            (SelfProfile userAfterUpdate) = fromJust $ runNoFederationStack localBackend Nothing config do
              updateUserProfile lusr Nothing UpdateOriginScim update
              getSelfProfile lusr
         in userAfterUpdate.userQualifiedId === tUntagged lusr
              .&&. userAfterUpdate.userDisplayName === fromMaybe userBeforeUpdate.userDisplayName update.name
              .&&. userAfterUpdate.userPict === fromMaybe userBeforeUpdate.userPict update.pict
              .&&. userAfterUpdate.userAssets === fromMaybe userBeforeUpdate.userAssets update.assets
              .&&. userAfterUpdate.userAccentId === fromMaybe userBeforeUpdate.userAccentId update.accentId
              .&&. userAfterUpdate.userLocale === fromMaybe userBeforeUpdate.userLocale update.locale

    prop "Update user events" $
      \(NotPendingStoredUser alice) connId localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            events = runNoFederationStack localBackend Nothing config do
              updateUserProfile lusr connId UpdateOriginScim update
              get @[MiniEvent]
         in events
              === [ MkMiniEvent
                      alice.id
                      connId
                      ( UserEvent . UserUpdated $
                          (emptyUserUpdatedData alice.id)
                            { eupName = update.name,
                              eupTextStatus = update.textStatus,
                              eupPict = update.pict,
                              eupAccentId = update.accentId,
                              eupAssets = update.assets,
                              eupLocale = update.locale,
                              eupSupportedProtocols = update.supportedProtocols
                            }
                      )
                  ]

    describe "user managed by scim doesn't allow certain update operations, but allows others" $ do
      prop "happy" $
        \(NotPendingStoredUser alice) localDomain update config ->
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def config do
                    updateUserProfile lusr Nothing UpdateOriginWireClient update {name = Nothing, locale = Nothing}
                    getUserProfile lusr (tUntagged lusr)
           in counterexample (show profileErr) $ isRight profileErr === True

      prop "name" $
        \(NotPendingStoredUser alice) localDomain name config ->
          alice.name /= name ==>
            let lusr = toLocalUnsafe localDomain alice.id
                localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
                profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                  run
                    . runErrorUnsafe
                    . runError
                    $ interpretNoFederationStack localBackend Nothing def config do
                      updateUserProfile lusr Nothing UpdateOriginWireClient def {name = Just name}
                      getUserProfile lusr (tUntagged lusr)
             in profileErr === Left UserSubsystemDisplayNameManagedByScim

      prop "locale" $
        \(NotPendingStoredUser alice) localDomain locale config ->
          alice.locale /= Just locale ==>
            let lusr = toLocalUnsafe localDomain alice.id
                localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
                profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                  run
                    . runErrorUnsafe
                    . runError
                    $ interpretNoFederationStack localBackend Nothing def config do
                      updateUserProfile lusr Nothing UpdateOriginWireClient def {locale = Just locale}
                      getUserProfile lusr (tUntagged lusr)
             in profileErr === Left UserSubsystemLocaleManagedByScim

    prop
      "if e2e identity is activated, the user name cannot be updated"
      \(NotPendingStoredUser alice) localDomain (newName :: Name) config ->
        (alice.name /= newName) ==>
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def {afcMlsE2EId = setStatus FeatureStatusEnabled defFeatureStatus} config do
                    updateUserProfile lusr Nothing UpdateOriginScim (def {name = Just newName})
                    getUserProfile lusr (tUntagged lusr)
           in profileErr === Left UserSubsystemDisplayNameManagedByScim

    prop
      "CheckHandle succeeds if there is a user with that handle"
      \((NotPendingStoredUser alice, handle :: Handle), config) ->
        not (isBlacklistedHandle handle) ==>
          let localBackend = def {users = [alice {managedBy = Just ManagedByWire, handle = Just handle}]}
              checkHandleResp =
                runNoFederationStack localBackend Nothing config $ checkHandle (fromHandle handle)
           in checkHandleResp === CheckHandleFound

    prop
      "CheckHandle fails if there is no user with that handle"
      \(handle :: Handle, config) ->
        not (isBlacklistedHandle handle) ==>
          let localBackend = def {users = []}
              checkHandleResp =
                runNoFederationStack localBackend Nothing config $ checkHandle (fromHandle handle)
           in checkHandleResp === CheckHandleNotFound

    prop
      "CheckHandles returns available handles from a list of handles, up to X"
      \((storedUsersAndHandles :: [(StoredUser, Handle)], randomHandles :: Set Handle), maxCount :: Word, config) ->
        not (any isBlacklistedHandle ((snd <$> storedUsersAndHandles) <> (S.toList randomHandles))) ==>
          let users = (\(u, h) -> u {handle = Just h, managedBy = Just ManagedByWire}) <$> storedUsersAndHandles
              localBackend = def {users = users}

              runCheckHandles :: [Handle] -> [Handle]
              runCheckHandles handles = runNoFederationStack localBackend Nothing config do
                checkHandles handles maxCount

              takenHandles = snd <$> storedUsersAndHandles
              freeHandles = runCheckHandles (S.toList randomHandles)
           in runCheckHandles takenHandles === []
                .&&. freeHandles `intersect` takenHandles === mempty
                .&&. counterexample (show (freeHandles, maxCount)) (length freeHandles <= fromIntegral maxCount)
                .&&. counterexample (show (freeHandles, randomHandles)) ((S.fromList freeHandles) `S.isSubsetOf` randomHandles)

    describe "Scim+UpdateProfileUpdate" do
      prop
        "Updating handles fails when UpdateOriginWireClient"
        \(alice, newHandle :: Handle, domain, config) ->
          not (isBlacklistedHandle newHandle) ==>
            let res :: Either UserSubsystemError ()
                res = run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def config do
                    updateHandle (toLocalUnsafe domain alice.id) Nothing UpdateOriginWireClient (fromHandle newHandle)

                localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
             in res === Left UserSubsystemHandleManagedByScim

      prop
        "Updating handles succeeds when UpdateOriginScim"
        \(alice, ssoId, email :: Maybe Email, fromHandle -> newHandle, domain, config) ->
          not (isBlacklistedHandle (fromJust (parseHandle newHandle))) ==>
            let res :: Either UserSubsystemError () = run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def config do
                    updateHandle (toLocalUnsafe domain alice.id) Nothing UpdateOriginScim newHandle
                localBackend =
                  def
                    { users =
                        [ alice
                            { managedBy = Just ManagedByScim,
                              email = email,
                              ssoId = Just ssoId,
                              activated = True
                            }
                        ]
                    }
             in res === Right ()

    prop
      "update valid handles succeeds"
      \(storedUser :: StoredUser, newHandle@(fromHandle -> rawNewHandle), config) ->
        (isJust storedUser.identity && not (isBlacklistedHandle newHandle)) ==>
          let updateResult :: Either UserSubsystemError () = run
                . runErrorUnsafe
                . runError
                $ interpretNoFederationStack (def {users = [storedUser]}) Nothing def config do
                  let luid = toLocalUnsafe dom storedUser.id
                      dom = Domain "localdomain"
                  updateHandle luid Nothing UpdateOriginScim rawNewHandle
           in updateResult === Right ()

    prop
      "update invalid handles fails"
      \(storedUser :: StoredUser, BadHandle badHandle, config) ->
        isJust storedUser.identity ==>
          let updateResult :: Either UserSubsystemError () = run
                . runErrorUnsafe
                . runError
                $ interpretNoFederationStack localBackend Nothing def config do
                  let luid = toLocalUnsafe dom storedUser.id
                      dom = Domain "localdomain"
                  updateHandle luid Nothing UpdateOriginScim badHandle
              localBackend = def {users = [storedUser]}
           in updateResult === Left UserSubsystemInvalidHandle

    prop "update / read supported-protocols" \(storedUser, config, newSupportedProtocols) ->
      not (hasPendingInvitation storedUser) ==>
        let luid :: Local UserId
            luid = toLocalUnsafe dom storedUser.id
              where
                dom = Domain "localdomain"

            operation :: (Monad m) => Sem (MiniBackendEffects `Append` AllErrors) a -> m a
            operation op = result `seq` pure result
              where
                result = runNoFederationStack localBackend Nothing config op
                localBackend = def {users = [storedUser]}

            actualSupportedProtocols = runIdentity $ operation do
              () <- updateUserProfile luid Nothing UpdateOriginWireClient (def {supportedProtocols = Just newSupportedProtocols})
              profileSupportedProtocols . fromJust <$> getUserProfile luid (tUntagged luid)

            expectedSupportedProtocols =
              if S.null newSupportedProtocols
                then defSupportedProtocols
                else newSupportedProtocols
         in actualSupportedProtocols === expectedSupportedProtocols

  describe "getLocalUserAccountByUserKey" $ do
    prop "gets users iff they are indexed by the UserKeyStore" $
      \(config :: UserSubsystemConfig) (localDomain :: Domain) (storedUser :: StoredUser) (userKey :: EmailKey) ->
        let localBackend =
              def
                { users = [storedUser],
                  userKeys = Map.singleton userKey storedUser.id
                }
            retrievedUser =
              run
                . runErrorUnsafe
                . runErrorUnsafe @UserSubsystemError
                . interpretNoFederationStack localBackend Nothing def config
                $ getLocalUserAccountByUserKey (toLocalUnsafe localDomain userKey)
         in retrievedUser === Just (mkAccountFromStored localDomain config.defaultLocale storedUser)

    prop "doesn't get users if they are not indexed by the UserKeyStore" $
      \(config :: UserSubsystemConfig) (localDomain :: Domain) (storedUserNoEmail :: StoredUser) (email :: Email) ->
        let localBackend =
              def
                { users = [storedUser],
                  userKeys = mempty
                }
            storedUser = storedUserNoEmail {email = Just email}
            retrievedUser =
              run
                . runErrorUnsafe
                . runErrorUnsafe @UserSubsystemError
                . interpretNoFederationStack localBackend Nothing def config
                $ getLocalUserAccountByUserKey (toLocalUnsafe localDomain (mkEmailKey email))
         in retrievedUser === Nothing

    prop "doesn't get users if they are not present in the UserStore but somehow are still indexed in UserKeyStore" $
      \(config :: UserSubsystemConfig) (localDomain :: Domain) (nonExistentUserId :: UserId) (userKey :: EmailKey) ->
        let localBackend =
              def
                { users = [],
                  userKeys = Map.singleton userKey nonExistentUserId
                }
            retrievedUser =
              run
                . runErrorUnsafe
                . runErrorUnsafe @UserSubsystemError
                . interpretNoFederationStack localBackend Nothing def config
                $ getLocalUserAccountByUserKey (toLocalUnsafe localDomain userKey)
         in retrievedUser === Nothing
