{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~))
import Control.Lens.At ()
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Handle
import Data.Id
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map qualified as Map
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set (insert, member, notMember)
import Data.Set qualified as S
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.State
import SAML2.WebSSO qualified as SAML
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.EnterpriseLogin
import Wire.API.Federation.Error
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User hiding (DeleteUser)
import Wire.API.User.IdentityProvider (IdPList (..), team)
import Wire.API.User.Search
import Wire.API.UserEvent
import Wire.AuthenticationSubsystem.Error
import Wire.DomainRegistrationStore qualified as DRS
import Wire.InvitationStore (InsertInvitation, StoredInvitation)
import Wire.InvitationStore qualified as InvitationStore
import Wire.MiniBackend
import Wire.MockInterpreters
import Wire.RateLimit
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
              viewer = viewerTeam {teamId = Nothing} :: StoredUser
              -- Having teams adds complications in email visibility,
              -- all that stuff is tested in [without federation] tests
              localTargetUsers = map (\user -> (coerce user) {teamId = Nothing} :: StoredUser) localTargetUsersNotPending
              federation = [(remoteDomain1, remoteBackend1), (remoteDomain2, remoteBackend2)]
              mkUserIds domain = map (flip Qualified domain . (.id))
              localTargets = mkUserIds localDomain localTargetUsers
              target1 = mkUserIds remoteDomain1 targetUsers1
              target2 = mkUserIds remoteDomain2 targetUsers2
              localBackend = def {users = [viewer] <> localTargetUsers}
              config = UserSubsystemConfig visibility miniLocale False 100 undefined
              retrievedProfiles =
                runFederationStack localBackend federation mempty config $
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
            config = UserSubsystemConfig visibility miniLocale False 100 undefined
            localBackend = def {users = [viewer]}
            result =
              run
                . runErrorUnsafe @UserSubsystemError
                . runErrorUnsafe @AuthenticationSubsystemError
                . runErrorUnsafe @RateLimitExceeded
                . runErrorUnsafe @TeamCollaboratorsError
                . runError @FederationError
                . interpretFederationStack localBackend online mempty config
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
        \viewer targetUserIds config domain ->
          let retrievedProfiles =
                runNoFederationStack def mempty config $
                  getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
           in retrievedProfiles === []

      prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
        \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) config domain sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} :: StoredUser else targetUserNoTeam
              localBackend = def {users = [targetUser, viewer]}
              galleyState = foldMap (\tid -> Map.singleton tid [teamMember]) viewer.teamId
              retrievedProfiles =
                runNoFederationStack localBackend galleyState config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfiles
                === [ mkUserProfile
                        (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) config.emailVisibilityConfig)
                        (mkUserFromStored domain config.defaultLocale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "gets a local user profile when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
        \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) config domain sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} :: StoredUser else targetUserNoTeam
              localBackend = def {users = [targetUser, viewer]}
              galleyState = foldMap (\tid -> Map.singleton tid [teamMember]) viewer.teamId
              retrievedProfile =
                runNoFederationStack localBackend galleyState config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfile
                === [ mkUserProfile
                        (fmap (const Nothing) config.emailVisibilityConfig)
                        (mkUserFromStored domain config.defaultLocale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "returns Nothing if the target user has not accepted their invitation yet" $
        \viewer (PendingStoredUser targetUser) config domain ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              localBackend = def {users = [targetUser, viewer]}
              galleyState = foldMap (\tid -> Map.singleton tid [teamMember]) viewer.teamId
              retrievedProfile =
                runNoFederationStack localBackend galleyState config $
                  getLocalUserProfiles (toLocalUnsafe domain [targetUser.id])
           in retrievedProfile === []

  describe "getUserProfilesWithErrors" $ do
    prop "If no errors, same behavior as getUserProfiles" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            config = UserSubsystemConfig visibility miniLocale False 100 undefined
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend federation mempty config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (map (flip Qualified remoteDomain . (.id)) targetUsers)
            retrievedProfiles :: [UserProfile] =
              runFederationStack localBackend federation mempty config $
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
            config = UserSubsystemConfig visibility miniLocale False 100 undefined
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online mempty config $
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
            config = UserSubsystemConfig visibility miniLocale False 100 undefined
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online mempty config $
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
            runNoFederationStack localBackend mempty config $
              getSelfProfile (toLocalUnsafe domain storedSelf.id)
       in retrievedProfile === Just (SelfProfile $ mkUserFromStored domain config.defaultLocale storedSelf)

    prop "should fail when the user does not exist in the DB" \selfId otherStoredUsers domain config ->
      let localBackend = def {users = filter (\u -> u.id /= selfId) otherStoredUsers}
          retrievedProfile =
            runNoFederationStack localBackend mempty config $
              getSelfProfile (toLocalUnsafe domain selfId)
       in retrievedProfile === Nothing

    prop "should mark user as managed by scim if E2EId is enabled for the user and they have a handle" \storedSelf domain susbsystemConfig (mlsE2EIdConfig :: MlsE2EIdConfig) ->
      let localBackend = def {users = [storedSelf]}
          allFeatureConfigs =
            npUpdate
              (LockableFeature FeatureStatusEnabled LockStatusUnlocked mlsE2EIdConfig)
              def
          SelfProfile retrievedUser =
            fromJust
              . runAllErrorsUnsafe
              . interpretNoFederationStack localBackend mempty allFeatureConfigs susbsystemConfig
              $ getSelfProfile (toLocalUnsafe domain storedSelf.id)
          expectedManagedBy = case storedSelf.handle of
            Nothing -> fromMaybe ManagedByWire storedSelf.managedBy
            Just _ -> ManagedByScim
       in retrievedUser.userManagedBy === expectedManagedBy

  describe "updateUserProfile" $ do
    prop "Update user" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire} :: StoredUser]}
            userBeforeUpdate = mkUserFromStored localDomain config.defaultLocale alice
            result = runNoFederationStackUserSubsystemErrorEither localBackend mempty config do
              updateUserProfile lusr Nothing UpdateOriginScim update
              getSelfProfile lusr
            mlsRemoval =
              BaseProtocolMLSTag `member` (fromMaybe mempty alice.supportedProtocols)
                && BaseProtocolMLSTag `notMember` (fromMaybe mempty update.supportedProtocols)
         in case result of
              Right (Just (SelfProfile userAfterUpdate)) ->
                userAfterUpdate.userQualifiedId === tUntagged lusr
                  .&&. userAfterUpdate.userDisplayName === fromMaybe userBeforeUpdate.userDisplayName update.name
                  .&&. userAfterUpdate.userPict === fromMaybe userBeforeUpdate.userPict update.pict
                  .&&. userAfterUpdate.userAssets === fromMaybe userBeforeUpdate.userAssets update.assets
                  .&&. userAfterUpdate.userAccentId === fromMaybe userBeforeUpdate.userAccentId update.accentId
                  .&&. userAfterUpdate.userLocale === fromMaybe userBeforeUpdate.userLocale update.locale
              Left UserSubsystemMlsRemovalNotAllowed -> mlsRemoval === True
              Right Nothing -> property False
              Left _ -> property False

    prop "Update user events" $
      \(NotPendingStoredUser alice) connId localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire} :: StoredUser]}
            -- MLS must not be removed from supported protocols, if exists
            mProtocolUpdates =
              update.supportedProtocols <&> \protocols ->
                if BaseProtocolMLSTag `member` (fromMaybe mempty alice.supportedProtocols)
                  then BaseProtocolMLSTag `insert` protocols
                  else protocols
            events = runNoFederationStack localBackend mempty config do
              updateUserProfile lusr connId UpdateOriginScim update {supportedProtocols = mProtocolUpdates}
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
                              eupSupportedProtocols = mProtocolUpdates
                            }
                      )
                  ]

    describe "getAccountsBy" do
      prop "GetBy userId when pending fails if not explicitly allowed" $
        \(PendingNotEmptyIdentityStoredUser alice') email teamId invitationInfo localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale False 100 undefined
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId
                    -- For simplicity, so we don't have to match the email with invitation
                  } ::
                  StoredUser
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = NoPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    -- We need valid invitations or the user gets deleted by
                    -- our drive-by cleanup job in the interprter.
                    -- FUTUREWORK: Remove this if we remove the enqueueDeletion from getAccountsByImpl
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

      prop "GetBy userId works for pending if explicitly queried" $
        \(PendingNotEmptyIdentityStoredUser alice') email teamId invitationInfo localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId
                    -- For simplicity, so we don't have to match the email with invitation
                  } ::
                  StoredUser
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    -- We need valid invitations or the user gets deleted by
                    -- our drive-by cleanup job in the interprter.
                    -- FUTUREWORK: Remove this if we remove the enqueueDeletion from getAccountsByImpl
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === [mkUserFromStored localDomain locale alice]
      prop "GetBy handle when pending fails if not explicitly allowed" $
        \(PendingNotEmptyIdentityStoredUser alice') handl email teamId invitationInfo localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId,
                    handle = Just handl
                    -- For simplicity, so we don't have to match the email with invitation
                  } ::
                  StoredUser
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByHandle = [handl],
                      includePendingInvitations = NoPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    -- We need valid invitations or the user gets deleted by
                    -- our drive-by cleanup job in the interprter.
                    -- FUTUREWORK: Remove this if we remove the enqueueDeletion from getAccountsByImpl
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

      prop "GetBy handle works for pending if explicitly queried" $
        \(PendingNotEmptyIdentityStoredUser alice') handl email teamId invitationInfo localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId,
                    handle = Just handl
                    -- For simplicity, so we don't have to match the email with invitation
                  } ::
                  StoredUser
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByHandle = [handl],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    -- We need valid invitations or the user gets deleted by
                    -- our drive-by cleanup job in the interprter.
                    -- FUTUREWORK: Remove this if we remove the enqueueDeletion from getAccountsByImpl
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === [mkUserFromStored localDomain locale alice]

      prop "GetBy email does not filter by pending, missing identity or expired invitations" $
        \(alice' :: StoredUser) email localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              alice = alice' {email = Just email} :: StoredUser
              localBackend =
                def
                  { users = [alice],
                    userKeys = Map.singleton (mkEmailKey email) alice.id
                  }
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsByEmailNoFilter (toLocalUnsafe localDomain [email])
           in result === [mkUserFromStored localDomain locale alice]

      prop "GetBy userId does not return missing identity users, pending invitation off" $
        \(NotPendingEmptyIdentityStoredUser alice) localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = NoPendingInvitations
                    }
              localBackend = def {users = [alice]}
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

      prop "GetBy userId does not return missing identity users, pending invtation on" $
        \(NotPendingEmptyIdentityStoredUser alice) localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend = def {users = [alice]}
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

      prop "GetBy pending user by id works if there is a valid invitation" $
        \(PendingNotEmptyIdentityStoredUser alice') (email :: EmailAddress) teamId (invitationInfo :: StoredInvitation) localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              emailKey = mkEmailKey email
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    userKeys = Map.singleton emailKey alice.id,
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              alice = alice' {email = Just email, teamId = Just teamId} :: StoredUser
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === [mkUserFromStored localDomain locale alice]

      prop "GetBy pending user by id fails if there is no valid invitation" $
        \(PendingNotEmptyIdentityStoredUser alice') (email :: EmailAddress) teamId localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              emailKey = mkEmailKey email
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByUserId = [alice.id],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    userKeys = Map.singleton emailKey alice.id
                  }
              alice = alice' {email = Just email, teamId = Just teamId} :: StoredUser
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

      prop "GetBy pending user handle id works if there is a valid invitation" $
        \(PendingNotEmptyIdentityStoredUser alice') (email :: EmailAddress) handl teamId (invitationInfo :: StoredInvitation) localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              emailKey = mkEmailKey email
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByHandle = [handl],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    userKeys = Map.singleton emailKey alice.id,
                    invitations =
                      Map.singleton
                        (teamId, invitationInfo.invitationId)
                        ( invitationInfo
                            { InvitationStore.email = email,
                              InvitationStore.teamId = teamId
                            }
                        )
                  }
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId,
                    handle = Just handl
                  } ::
                  StoredUser
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === [mkUserFromStored localDomain locale alice]

      prop "GetBy pending user by handle fails if there is no valid invitation" $
        \(PendingNotEmptyIdentityStoredUser alice') (email :: EmailAddress) handl teamId localDomain visibility locale ->
          let config = UserSubsystemConfig visibility locale True 100 undefined
              emailKey = mkEmailKey email
              getBy =
                toLocalUnsafe localDomain $
                  def
                    { getByHandle = [handl],
                      includePendingInvitations = WithPendingInvitations
                    }
              localBackend =
                def
                  { users = [alice],
                    userKeys = Map.singleton emailKey alice.id
                  }
              alice =
                alice'
                  { email = Just email,
                    teamId = Just teamId,
                    handle = Just handl
                  } ::
                  StoredUser
              result =
                runNoFederationStack localBackend mempty config $
                  getAccountsBy getBy
           in result === []

    describe "user managed by scim doesn't allow certain update operations, but allows others" $ do
      prop "happy" $
        \(NotPendingStoredUser alice) localDomain update config ->
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice {managedBy = Just ManagedByScim} :: StoredUser]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . userSubsystemErrorEitherUnsafe
                  $ interpretNoFederationStack localBackend mempty def config do
                    updateUserProfile lusr Nothing UpdateOriginWireClient update {name = Nothing, locale = Nothing, supportedProtocols = Nothing}
                    getUserProfile lusr (tUntagged lusr)
           in counterexample (show profileErr) $ isRight profileErr === True

      prop "name" $
        \(NotPendingStoredUser alice) localDomain name config ->
          alice.name /= name ==>
            let lusr = toLocalUnsafe localDomain alice.id
                localBackend = def {users = [alice {managedBy = Just ManagedByScim} :: StoredUser]}
                profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                  run
                    . userSubsystemErrorEitherUnsafe
                    $ interpretNoFederationStack localBackend mempty def config do
                      updateUserProfile lusr Nothing UpdateOriginWireClient def {name = Just name}
                      getUserProfile lusr (tUntagged lusr)
             in profileErr === Left UserSubsystemDisplayNameManagedByScim

      prop "locale" $
        \(NotPendingStoredUser alice) localDomain locale config ->
          alice.locale /= Just locale ==>
            let lusr = toLocalUnsafe localDomain alice.id
                localBackend = def {users = [alice {managedBy = Just ManagedByScim} :: StoredUser]}
                profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                  run
                    . userSubsystemErrorEitherUnsafe
                    $ interpretNoFederationStack localBackend mempty def config do
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
                  . userSubsystemErrorEitherUnsafe
                  $ interpretNoFederationStack
                    localBackend
                    mempty
                    ( npUpdate
                        ( def
                            { status = FeatureStatusEnabled
                            } ::
                            LockableFeature MlsE2EIdConfig
                        )
                        def
                    )
                    config
                    do
                      updateUserProfile lusr Nothing UpdateOriginScim (def {name = Just newName, supportedProtocols = Nothing})
                      getUserProfile lusr (tUntagged lusr)
           in profileErr === Left UserSubsystemDisplayNameManagedByScim

    prop
      "CheckHandle succeeds if there is a user with that handle"
      \((NotPendingStoredUser alice, handle :: Handle), config) ->
        not (isBlacklistedHandle handle) ==>
          let localBackend = def {users = [alice {managedBy = Just ManagedByWire, handle = Just handle} :: StoredUser]}
              checkHandleResp =
                runNoFederationStack localBackend mempty config $ checkHandle (fromHandle handle)
           in checkHandleResp === CheckHandleFound

    prop
      "CheckHandle fails if there is no user with that handle"
      \(handle :: Handle, config) ->
        not (isBlacklistedHandle handle) ==>
          let localBackend = def {users = []}
              checkHandleResp =
                runNoFederationStack localBackend mempty config $ checkHandle (fromHandle handle)
           in checkHandleResp === CheckHandleNotFound

    prop
      "CheckHandles returns available handles from a list of handles, up to X"
      \((storedUsersAndHandles :: [(StoredUser, Handle)], randomHandles :: Set Handle), maxCount :: Word, config) ->
        not (any isBlacklistedHandle ((snd <$> storedUsersAndHandles) <> (S.toList randomHandles))) ==>
          let users = (\(u, h) -> u {handle = Just h, managedBy = Just ManagedByWire} :: StoredUser) <$> storedUsersAndHandles
              localBackend = def {users = users}

              runCheckHandles :: [Handle] -> [Handle]
              runCheckHandles handles = runNoFederationStack localBackend mempty config do
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
                  . userSubsystemErrorEitherUnsafe
                  $ interpretNoFederationStack localBackend mempty def config do
                    updateHandle (toLocalUnsafe domain alice.id) Nothing UpdateOriginWireClient (fromHandle newHandle)

                localBackend = def {users = [alice {managedBy = Just ManagedByScim} :: StoredUser]}
             in res === Left UserSubsystemHandleManagedByScim

      prop
        "Updating handles succeeds when UpdateOriginScim"
        \(alice, ssoId, email :: Maybe EmailAddress, fromHandle -> newHandle, domain, config) ->
          not (isBlacklistedHandle (fromJust (parseHandle newHandle))) ==>
            let res :: Either UserSubsystemError () = run
                  . userSubsystemErrorEitherUnsafe
                  $ interpretNoFederationStack localBackend mempty def config do
                    updateHandle (toLocalUnsafe domain alice.id) Nothing UpdateOriginScim newHandle
                localBackend =
                  def
                    { users =
                        [ alice
                            { managedBy = Just ManagedByScim,
                              email = email,
                              ssoId = Just ssoId,
                              activated = True
                            } ::
                            StoredUser
                        ]
                    }
             in res === Right ()

    prop
      "update valid handles succeeds"
      \(storedUser :: StoredUser, newHandle@(fromHandle -> rawNewHandle), config) ->
        (isJust storedUser.identity && not (isBlacklistedHandle newHandle)) ==>
          let updateResult :: Either UserSubsystemError () = run
                . userSubsystemErrorEitherUnsafe
                $ interpretNoFederationStack (def {users = [storedUser]}) mempty def config do
                  let luid = toLocalUnsafe dom storedUser.id
                      dom = Domain "localdomain"
                  updateHandle luid Nothing UpdateOriginScim rawNewHandle
           in updateResult === Right ()

    prop
      "update invalid handles fails"
      \(storedUser :: StoredUser, BadHandle badHandle, config) ->
        isJust storedUser.identity ==>
          let updateResult :: Either UserSubsystemError () = run
                . userSubsystemErrorEitherUnsafe
                $ interpretNoFederationStack localBackend mempty def config do
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

            operation :: (Monad m) => Sem (MiniBackendEffects `Append` AllErrors) a -> m (Either UserSubsystemError a)
            operation op = result `seq` pure result
              where
                result = runNoFederationStackUserSubsystemErrorEither localBackend mempty config op
                localBackend = def {users = [storedUser]}

            actual = runIdentity $ operation do
              () <- updateUserProfile luid Nothing UpdateOriginWireClient (def {supportedProtocols = Just newSupportedProtocols})
              profileSupportedProtocols . fromJust <$> getUserProfile luid (tUntagged luid)

            mlsRemoval =
              BaseProtocolMLSTag `member` (fromMaybe mempty storedUser.supportedProtocols)
                && BaseProtocolMLSTag `notMember` newSupportedProtocols
            expected
              | mlsRemoval = Left UserSubsystemMlsRemovalNotAllowed
              | S.null newSupportedProtocols = Right defSupportedProtocols
              | otherwise = Right newSupportedProtocols
         in actual === expected

  describe "getLocalUserAccountByUserKey" $ do
    prop "gets users iff they are indexed by the UserKeyStore" $
      \(config :: UserSubsystemConfig) (localDomain :: Domain) (storedUser :: StoredUser) (userKey :: EmailKey) ->
        let localBackend =
              def
                { users = [storedUser],
                  userKeys = Map.singleton userKey storedUser.id
                }
            retrievedUser =
              runAllErrorsUnsafe
                . interpretNoFederationStack localBackend mempty def config
                $ getLocalUserAccountByUserKey (toLocalUnsafe localDomain userKey)
         in retrievedUser === Just (mkUserFromStored localDomain config.defaultLocale storedUser)

    prop "doesn't get users if they are not indexed by the UserKeyStore" $
      \(config :: UserSubsystemConfig) (localDomain :: Domain) (storedUserNoEmail :: StoredUser) (email :: EmailAddress) ->
        let localBackend =
              def
                { users = [storedUser],
                  userKeys = mempty
                }
            storedUser = storedUserNoEmail {email = Just email} :: StoredUser
            retrievedUser =
              runAllErrorsUnsafe
                . interpretNoFederationStack localBackend mempty def config
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
              runAllErrorsUnsafe
                . interpretNoFederationStack localBackend mempty def config
                $ getLocalUserAccountByUserKey (toLocalUnsafe localDomain userKey)
         in retrievedUser === Nothing
  describe "Removing an email address" do
    prop "Cannot remove an email of a non-existing user" $ \lusr config ->
      let localBackend = def
          result =
            runNoFederationStack localBackend mempty config $
              removeEmailEither lusr
       in result === Left UserSubsystemProfileNotFound
    prop "Cannot remove an email of a no-identity user" $
      \(locx :: Local ()) (NotPendingEmptyIdentityStoredUser user) config ->
        let localBackend = def {users = [user]}
            lusr = qualifyAs locx user.id
            result =
              runNoFederationStack localBackend mempty config $
                removeEmailEither lusr
         in result === Left UserSubsystemNoIdentity
    prop "Cannot remove an email of a last-identity user" $
      \(locx :: Local ()) user' email sso config ->
        let user =
              user'
                { activated = True,
                  email = email,
                  ssoId = if isNothing email then Just sso else Nothing
                } ::
                StoredUser
            localBackend = def {users = [user]}
            lusr = qualifyAs locx user.id
            result =
              runNoFederationStack localBackend mempty config $
                removeEmailEither lusr
         in result === Left UserSubsystemLastIdentity
    prop "Successfully remove an email from an SSOId user" $
      \(locx :: Local ()) (NotPendingSSOIdWithEmailStoredUser user) config ->
        let localBackend = def {users = [user]}
            lusr = qualifyAs locx user.id
            result =
              runNoFederationStack localBackend mempty config $ do
                remRes <- removeEmailEither lusr
                (remRes,) <$> gets users
         in result === (Right (), [user {email = Nothing} :: StoredUser])
  describe "Changing an email address" $ do
    prop "Idempotent email change" $
      \(locx :: Local ()) (NotPendingStoredUser user') email config ->
        let user = user' {email = Just email} :: StoredUser
            localBackend = def {users = [user]}
            lusr = qualifyAs locx user.id
            result =
              runNoFederationStack localBackend mempty config $ do
                c <- requestEmailChange lusr email UpdateOriginWireClient
                (c,) <$> gets users
         in result === (ChangeEmailResponseIdempotent, [user])
    prop "Email change needing activation" $
      \(locx :: Local ()) (NotPendingStoredUser user') config ->
        let email = unsafeEmailAddress "me" "example.com"
            updatedEmail = unsafeEmailAddress "you" "example.com"
            user = user' {email = Just email, managedBy = Nothing} :: StoredUser
            localBackend = def {users = [user]}
            lusr = qualifyAs locx user.id
            result =
              runNoFederationStack localBackend mempty config $ do
                c <- requestEmailChange lusr updatedEmail UpdateOriginWireClient
                (c,) <$> gets users
         in result
              === ( ChangeEmailResponseNeedsActivation,
                    [user {emailUnvalidated = Just updatedEmail} :: StoredUser]
                  )
    prop "Email change is not allowed if the email domain is taken by another backend or team" $
      \(preDomreg :: DomainRegistration) (locx :: Local ()) (NotPendingStoredUser user') (preEmail :: EmailAddress) (domainTakenBySameTeam :: Bool) preIdp config ->
        let email :: EmailAddress
            email = unsafeEmailAddress l (cs d)
              where
                l :: ByteString = localPart preEmail
                d :: Text = domainText domreg.domain
            user = user' {managedBy = Nothing} :: StoredUser
            lusr = qualifyAs locx user.id
            localBackend =
              def
                { users = [user],
                  teamIdps = case (preDomreg.domainRedirect, user.teamId, domainTakenBySameTeam) of
                    (SSO ssoId, Just tid, True) ->
                      Map.singleton tid $
                        IdPList
                          [ preIdp
                              & SAML.idpId .~ ssoId
                              & SAML.idpExtraInfo . team .~ tid
                          ]
                    (SSO _, Just tid, False) ->
                      Map.singleton tid $ IdPList [preIdp & SAML.idpExtraInfo . team .~ tid]
                    _ -> mempty
                }
            domreg =
              if domainTakenBySameTeam
                then
                  preDomreg
                    { teamInvite = case (preDomreg.teamInvite, user.teamId) of
                        (Team _, Just tid) -> Team tid
                        (x, _) -> x
                    } ::
                    DomainRegistration
                else preDomreg
            outcome = run
              . userSubsystemErrorEitherUnsafe
              $ interpretNoFederationStack localBackend mempty def config do
                DRS.upsert domreg
                void $ requestEmailChange lusr email UpdateOriginWireClient

            expected = case (domreg.domainRedirect, domreg.teamInvite) of
              (NoRegistration, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToNoRegistration
              (Backend {}, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToBackend
              (SSO {}, _) | domainTakenBySameTeam && isJust user.teamId -> Right ()
              (SSO {}, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToSSO
              (_, NotAllowed) -> Left $ UserSubsystemGuardFailed TeamInviteSetToNotAllowed
              (_, Team _) | not domainTakenBySameTeam && isJust user.teamId -> Left $ UserSubsystemGuardFailed TeamInviteRestrictedToOtherTeam
              (_, Team _) | isNothing user.teamId -> Left $ UserSubsystemGuardFailed TeamInviteRestrictedToOtherTeam
              _ -> Right ()
         in outcome === expected

  describe "GuardRegisterActivateUserEmailDomain" $ do
    prop "throws the appropriate errors" $
      \(domreg :: DomainRegistration) (preEmail :: EmailAddress) config ->
        let email :: EmailAddress
            email = unsafeEmailAddress l (cs d)
              where
                l :: ByteString = localPart preEmail
                d :: Text = domainText domreg.domain

            outcome = run
              . userSubsystemErrorEitherUnsafe
              $ interpretNoFederationStack def mempty def config do
                DRS.upsert domreg
                guardRegisterActivateUserEmailDomain email

            expected = case domreg.domainRedirect of
              None -> Right ()
              Locked -> Right ()
              SSO _ -> Left $ UserSubsystemGuardFailed DomRedirSetToSSO
              Backend _ _ -> Left $ UserSubsystemGuardFailed DomRedirSetToBackend
              NoRegistration -> Left $ UserSubsystemGuardFailed DomRedirSetToNoRegistration
              PreAuthorized -> Right ()
         in outcome === expected

  describe "GuardUpgradePersonalUserEmailDomain" $ do
    prop "throws the appropriate errors" $
      \(domreg :: DomainRegistration) (preEmail :: EmailAddress) config ->
        let email :: EmailAddress
            email = unsafeEmailAddress l (cs d)
              where
                l :: ByteString = localPart preEmail
                d :: Text = domainText domreg.domain

            outcome = run
              . userSubsystemErrorEitherUnsafe
              $ interpretNoFederationStack def mempty def config do
                DRS.upsert domreg
                guardUpgradePersonalUserToTeamEmailDomain email

            expected = case (domreg.domainRedirect, domreg.teamInvite) of
              (NoRegistration, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToNoRegistration
              (Backend {}, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToBackend
              (SSO {}, _) -> Left $ UserSubsystemGuardFailed DomRedirSetToSSO
              (_, NotAllowed) -> Left $ UserSubsystemGuardFailed TeamInviteSetToNotAllowed
              (_, Team _) -> Left $ UserSubsystemGuardFailed TeamInviteRestrictedToOtherTeam
              _ -> Right ()
         in outcome === expected

  describe "InternalFindTeamInvitation" $ do
    prop "throws error if email" $
      \(preDomreg :: DomainRegistration) (preEmail :: EmailAddress) (preInv :: InsertInvitation) (invIntoSameTeamAsDomain :: Bool) timeout config ->
        let email :: EmailAddress
            email = unsafeEmailAddress l (cs d)
              where
                l :: ByteString = localPart preEmail
                d :: Text = domainText preDomreg.domain

            inv :: InsertInvitation = preInv {InvitationStore.inviteeEmail = email}
            domreg =
              if invIntoSameTeamAsDomain
                then
                  preDomreg
                    { teamInvite = case preDomreg.teamInvite of
                        Team _ -> Team inv.teamId
                        x -> x
                    } ::
                    DomainRegistration
                else preDomreg

            storedInv = InvitationStore.insertInvToStoredInv inv

            outcome = run
              . userSubsystemErrorEitherUnsafe
              $ interpretNoFederationStack def mempty def config {maxTeamSize = 100} do
                void $ InvitationStore.insertInvitation inv timeout
                DRS.upsert domreg
                internalFindTeamInvitation (Just $ mkEmailKey email) storedInv.code

            expected = case domreg.teamInvite of
              NotAllowed -> Left $ UserSubsystemGuardFailed TeamInviteSetToNotAllowed
              Allowed -> Right storedInv
              Team tid | tid == storedInv.teamId -> Right storedInv
              Team _ -> Left $ UserSubsystemGuardFailed TeamInviteRestrictedToOtherTeam
         in outcome === expected

  describe "SearchUsers" $ do
    prop "exact handle matches are not duplicate" $
      \(ActiveStoredUser searcheeNoHandle) (searcheeHandle :: Handle) (ActiveStoredUser searcher) localDomain configBase ->
        let teamMember = mkTeamMember searcher.id fullPermissions Nothing defUserLegalHoldStatus
            searchee = searcheeNoHandle {handle = Just searcheeHandle} :: StoredUser
            localBackend =
              def
                { users = [searchee, searcher],
                  userIndex = indexFromStoredUsers [searchee, searcher]
                }
            galleyState = foldMap (\tid -> Map.singleton tid [teamMember]) searcher.teamId
            config = configBase {searchSameTeamOnly = False}
         in runNoFederationStack localBackend galleyState config $ do
              result <-
                searchUsers
                  (toLocalUnsafe localDomain searcher.id)
                  (fromHandle searcheeHandle)
                  Nothing
                  (Just $ toRange (Proxy @2))
              let expectedContact =
                    Contact
                      { contactTeam = searchee.teamId,
                        contactQualifiedId = Qualified searchee.id localDomain,
                        contactName = fromName searchee.name,
                        contactHandle = fromHandle <$> searchee.handle,
                        contactColorId = Just . fromIntegral $ searchee.accentId.fromColourId
                      }
              pure $
                result.searchResults === [expectedContact]
