{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~))
import Control.Lens.At ()
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Handle
import Data.HashSet qualified as HashSet
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map qualified as Map
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set (insert, member, notMember)
import Data.Set qualified as S
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day (ModifiedJulianDay), UTCTime (..))
import Data.UUID qualified as UUID
import Database.Bloodhound.Internal.Client qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import SAML2.WebSSO qualified as SAML
import Servant.Client.Core (RunClient (..))
import Servant.Client.Core qualified as SC
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Type.Reflection (eqTypeRep, typeRep, (:~~:) (HRefl))
import Wire.API.EnterpriseLogin
import Wire.API.Federation.API (FederationMonad (..))
import Wire.API.Federation.API.Brig qualified as FedBrig
import Wire.API.Federation.Component (Component (Brig))
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite (CipherSuiteTag, csSignatureScheme)
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature (FeatureStatus (..), LockStatus (..), LockableFeature (..), MlsE2EIdConfig, npUpdate)
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User hiding (DeleteUser)
import Wire.API.User.Client (Client (..), ClientType (..))
import Wire.API.User.Client.Prekey (PrekeyId (..), UncheckedPrekeyBundle (..))
import Wire.API.User.IdentityProvider (IdPList (..), team)
import Wire.API.User.Search
import Wire.API.UserEvent
import Wire.API.VersionInfo (VersionedMonad (..))
import Wire.AppStore qualified as AppStore
import Wire.AppSubsystem
import Wire.Arbitrary (GenericUniform (..))
import Wire.AuthenticationSubsystem.Error
import Wire.ClientStore (ClientStore)
import Wire.ClientSubsystem.Error (ClientError)
import Wire.DomainRegistrationStore qualified as DRS
import Wire.FederationAPIAccess (FederationAPIAccess (..))
import Wire.IndexedUserStore qualified as IU
import Wire.InvitationStore (InsertInvitation, StoredInvitation)
import Wire.InvitationStore qualified as InvitationStore
import Wire.MiniBackend
import Wire.MlsKeyPackageSubsystem (MlsKeyPackageSubsystem (..))
import Wire.MockInterpreters
import Wire.RateLimit
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserStore (UserStore)
import Wire.UserStore.IndexUser
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist
import Wire.UserSubsystem.Interpreter (UserSubsystemConfig (..), getContactStatusesImpl, getLocalContactStatusesImpl)
import Wire.Util
import Wire.VerificationCodeSubsystem

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
              config = def {emailVisibilityConfig = visibility}
              retrievedProfiles =
                runFederationStack localBackend federation mempty config $
                  getUserProfiles
                    (toLocalUnsafe localDomain viewer.id)
                    (localTargets <> target1 <> target2)
              mkExpectedProfiles domain users =
                [ mkUserProfileWithEmail
                    Nothing
                    (mkUserFromStored domain miniLocale targetUser)
                    Nothing
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
            config = def {emailVisibilityConfig = visibility}
            localBackend = def {users = [viewer]}
            result =
              run
                . runErrorUnsafe @ClientError
                . runErrorUnsafe @UserSubsystemError
                . runErrorUnsafe @AppSubsystemError
                . runErrorUnsafe @AuthenticationSubsystemError
                . runErrorUnsafe @VerificationCodeSubsystemError
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
          let teamMember = mkTeamMember viewer.id (rolePermissions RoleMember) Nothing defUserLegalHoldStatus
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
                        Nothing
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
                        Nothing
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
            config = def {emailVisibilityConfig = visibility}
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
            config = def {emailVisibilityConfig = visibility}
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
            config = def {emailVisibilityConfig = visibility}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
          let config = def {emailVisibilityConfig = visibility, defaultLocale = locale}
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
      "if e2e identity is activated, the user name cannot be updated by a wire client"
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
                      updateUserProfile lusr Nothing UpdateOriginWireClient (def {name = Just newName, supportedProtocols = Nothing})
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

    prop "update / read supported-protocols" \(storedUser_, config, newSupportedProtocols) ->
      not (hasPendingInvitation storedUser_) ==>
        let storedUser :: StoredUser
            storedUser = storedUser_ {userType = Just UserTypeRegular}

            luid :: Local UserId
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
    prop "Email change not allowed for blocked domains" $ do
      \(NotPendingStoredUser user)
       (emailUsername :: EmailUsername)
       (blockedDomains :: NonEmptyList Domain)
       config -> do
          blockedEmailDomain <- anyElementOf blockedDomains

          let blockedEmailAddress :: EmailAddress =
                unsafeEmailAddress
                  ((fromString . getEmailUsername) emailUsername)
                  ((encodeUtf8 . domainText) blockedEmailDomain)
              localBackend = def {users = [user]}
              lusr = toLocalUnsafe (fromRight' (mkDomain "local.example.com")) user.id
              result =
                runNoFederationStack
                  localBackend
                  mempty
                  ( config
                      { blockedDomains = (HashSet.fromList . getNonEmpty) blockedDomains
                      }
                  )
                  $ try (requestEmailChange lusr blockedEmailAddress UpdateOriginWireClient)
           in pure $ result === (Left UserSubsystemBlockedDomain)

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

            storedUserToDoc :: StoredUser -> UserDoc
            storedUserToDoc user = indexUserToDoc defaultSearchVisibilityInbound Nothing (storedUserToIndexUser user)

            indexFromStoredUsers :: [StoredUser] -> UserIndex
            indexFromStoredUsers storedUsers = do
              run . execState emptyIndex . inMemoryIndexedUserStoreInterpreter $ do
                for_ storedUsers $ \storedUser ->
                  IU.upsert (userIdToDocId storedUser.id) (storedUserToDoc storedUser) ES.NoVersionControl

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
                  Nothing
              let expectedContact =
                    Contact
                      { contactTeam = searchee.teamId,
                        contactQualifiedId = Qualified searchee.id localDomain,
                        contactName = fromName searchee.name,
                        contactHandle = fromHandle <$> searchee.handle,
                        contactColorId = Just . fromIntegral $ searchee.accentId.fromColourId,
                        contactType = fromMaybe UserTypeRegular searchee.userType
                      }
              pure $ result.searchResults === [expectedContact | fromMaybe True searchee.searchable]

  describe "getLocalAppProfiles" $ do
    prop "does *not* include apps that are collaborators from other teams" $
      \(NotPendingStoredUser appUser_)
       (NotPendingStoredUser ownerA_)
       (NotPendingStoredUser ownerB_)
       (teamAId :: TeamId)
       (teamBId :: TeamId)
       config ->
          teamAId /= teamBId ==>
            let localDomain = Domain "localdomain"
                ownerA = ownerA_ {teamId = Just teamAId} :: StoredUser
                ownerB = ownerB_ {teamId = Just teamBId} :: StoredUser
                teamAOwnerId = toLocalUnsafe localDomain ownerA.id
                teamBOwnerId = toLocalUnsafe localDomain ownerB.id
                appUser =
                  appUser_ {userType = Just UserTypeApp, teamId = Just teamBId} :: StoredUser
                storedApp =
                  AppStore.StoredApp
                    { id = appUser.id,
                      teamId = teamBId,
                      meta = mempty,
                      category = Category "other",
                      description = unsafeRange "test app",
                      creator = appUser.id
                    }
                collab =
                  TeamCollaborator
                    { gUser = appUser.id,
                      gTeam = teamAId,
                      gPermissions = mempty
                    }
                localBackend =
                  def
                    { users = [appUser, ownerA, ownerB],
                      apps = [storedApp],
                      teamCollaborators = Map.fromList [(teamAId, [collab])]
                    }
                teams =
                  Map.fromList
                    [ (teamAId, [mkTeamMember ownerA.id fullPermissions Nothing defUserLegalHoldStatus]),
                      ( teamBId,
                        [ mkTeamMember ownerB.id fullPermissions Nothing defUserLegalHoldStatus,
                          mkTeamMember appUser.id fullPermissions Nothing defUserLegalHoldStatus
                        ]
                      )
                    ]
                result :: ([UserId], [UserId]) =
                  runNoFederationStack localBackend teams config $ do
                    let getAppId tid caller =
                          qUnqualified . (.profileQualifiedId)
                            <$$> getLocalAppProfiles caller tid
                     in (,)
                          <$> getAppId teamAId teamAOwnerId
                          <*> getAppId teamBId teamBOwnerId
             in result === ([], [appUser.id])

    prop "denies access when the caller's own team is not the requested team" . withNumTests 1 $
      \(NotPendingStoredUser caller_)
       (NotPendingStoredUser appUser_)
       (callerTeamId :: TeamId)
       (targetTeamId :: TeamId)
       config ->
          callerTeamId /= targetTeamId ==>
            let localDomain = Domain "localdomain"
                caller = caller_ {teamId = Just callerTeamId} :: StoredUser
                callerId = toLocalUnsafe localDomain caller.id
                appUser = appUser_ {userType = Just UserTypeApp, teamId = Just targetTeamId} :: StoredUser
                storedApp =
                  AppStore.StoredApp
                    { id = appUser.id,
                      teamId = targetTeamId,
                      meta = mempty,
                      category = Category "other",
                      description = unsafeRange "test app",
                      creator = appUser.id
                    }
                localBackend =
                  def
                    { users = [caller, appUser],
                      apps = [storedApp]
                    }
                teams =
                  Map.fromList
                    [ ( callerTeamId,
                        [mkTeamMember caller.id fullPermissions Nothing defUserLegalHoldStatus]
                      ),
                      ( targetTeamId,
                        [mkTeamMember appUser.id fullPermissions Nothing defUserLegalHoldStatus]
                      )
                    ]
                result :: Either UserSubsystemError [UserProfile] =
                  runNoFederationStackUserSubsystemErrorEither localBackend teams config $
                    getLocalAppProfiles callerId targetTeamId
             in result === Left UserSubsystemProfileNotFound

  describe "getContactStatuses" do
    prop "one federation call per remote domain, none without remote users" $
      \(remoteUsers :: Map Domain [UserId]) (localUserIds :: [UserId]) ctx -> do
        let remote = remoteOnly remoteUsers
            handlers = Map.map (const allContactable) remote
            qids = qualify remote <> qualify (Map.singleton localDomain' localUserIds)
            (calls, _) = runContactRequester emptyBackend handlers (getContactStatusesImpl lself ctx qids)
        sort calls === sort (Map.keys remote)

    prop "no federation call when there are no remote users" $
      \(localUserIds :: [UserId]) ctx -> do
        let (calls, _) = runContactRequester emptyBackend mempty (getContactStatusesImpl lself ctx (qualify (Map.singleton localDomain' localUserIds)))
        calls === []

    prop "a failing remote domain has no entries, other domains are unaffected" $
      \(upUsers :: Map Domain [UserId]) (downUsers :: Map Domain [UserId]) ctx -> do
        let up = remoteOnly upUsers
            down = remoteOnly (Map.difference downUsers up)
            handlers = Map.map (const allContactable) up
            (calls, result) = runContactRequester emptyBackend handlers (getContactStatusesImpl lself ctx (qualify up <> qualify down))
        conjoin
          [ result === Map.fromList [(q, ContactStatus Contactable) | q <- qualify up],
            sort calls === sort (Map.keys up <> Map.keys down)
          ]

    prop "pending-invitation users are left out, deleted users are non-contactable" $
      \ctx targetMls (facts :: [(StoredUser, UserFacts)]) -> do
        let b0 = mkContactBackend targetMls facts
            -- every second user has a pending invitation
            backend =
              b0
                { storedUsers =
                    [ if odd i then u {status = Just PendingInvitation} else u
                    | (i, u) <- zip [0 :: Int ..] b0.storedUsers
                    ] ::
                      [StoredUser]
                }
            expected =
              Map.fromList
                [ (u.id, ContactStatus (if oracle ctx targetMls fs then Contactable else NonContactable))
                | (i, (u, fs)) <- zip [0 :: Int ..] (uniqueFacts facts),
                  even i
                ]
        runContactBackend backend (getLocalContactStatusesImpl ctx (map (.id) b0.storedUsers)) === expected

    prop "local route and remote route evaluate the same facts identically" $
      \ctx targetMls (facts :: [(StoredUser, UserFacts)]) (unknownIds :: [UserId]) -> do
        let backend = mkContactBackend targetMls facts
            known = [u.id | u <- backend.storedUsers]
            qids = [Qualified u targetDomain | u <- known <> unknownIds]
            targetSelf = toLocalUnsafe targetDomain (Id UUID.nil)
            -- requester backend: unrelated server MLS flag, must have no influence
            requesterBackend = emptyBackend {serverMls = not targetMls}
            (_, viaLocal) =
              runContactRequester backend mempty (getContactStatusesImpl targetSelf ctx qids)
            (calls, viaRemote) =
              runContactRequester requesterBackend (Map.singleton targetDomain (loopback backend)) (getContactStatusesImpl lself ctx qids)
            expected =
              Map.fromList
                [ (Qualified u.id targetDomain, ContactStatus (if oracle ctx targetMls fs then Contactable else NonContactable))
                | (u, fs) <- uniqueFacts facts,
                  u.id `elem` known
                ]
        conjoin
          [ viaLocal === expected,
            viaRemote === expected,
            length calls === (if null qids then 0 else 1)
          ]
  where
    localDomain' = Domain "local.example.com"
    targetDomain = Domain "target.example.com"
    lself = toLocalUnsafe localDomain' (Id UUID.nil)
    qualify m = [Qualified u d | (d, us) <- Map.toList m, u <- us]
    remoteOnly = Map.filter (not . null) . Map.delete localDomain'
    allContactable req = FedBrig.GetContactStatusResponse (Map.fromList [(u, ContactStatus Contactable) | u <- fromRange req.users])

----------------------------------------------------------------------
-- contact status test harness

data ClientFacts = ClientFacts
  { hasPrekey :: Bool,
    signatureSuites :: S.Set CipherSuiteTag,
    keyPackageSuites :: S.Set CipherSuiteTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientFacts)

data UserFacts = UserFacts
  { protocols :: S.Set BaseProtocolTag,
    clients :: [ClientFacts],
    deleted :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserFacts)

data ContactBackend = ContactBackend
  { serverMls :: Bool,
    storedUsers :: [StoredUser],
    clientState :: ClientStoreState,
    keyPackages :: S.Set (UserId, ClientId, CipherSuiteTag)
  }

emptyBackend :: ContactBackend
emptyBackend = ContactBackend False [] emptyClientStoreState mempty

-- | Keep the first occurrence of each user id.
uniqueFacts :: [(StoredUser, UserFacts)] -> [(StoredUser, UserFacts)]
uniqueFacts = nubBy (\a b -> (fst a).id == (fst b).id)

mkContactBackend :: Bool -> [(StoredUser, UserFacts)] -> ContactBackend
mkContactBackend mlsFlag facts =
  ContactBackend
    { serverMls = mlsFlag,
      storedUsers = [u {status = Just (if fs.deleted then Deleted else Active), supportedProtocols = Just fs.protocols} | (u, fs) <- ufs] :: [StoredUser],
      clientState =
        ClientStoreState . Map.fromList $
          [ (u.id, Map.fromList [(cid, StoredClient (mkClient cid cf) [UncheckedPrekeyBundle (PrekeyId 1) "k" | cf.hasPrekey]) | (cid, cf) <- numbered fs])
          | (u, fs) <- ufs
          ],
      keyPackages =
        S.fromList
          [(u.id, cid, suite) | (u, fs) <- ufs, (cid, cf) <- numbered fs, suite <- S.toList cf.keyPackageSuites]
    }
  where
    ufs = uniqueFacts facts
    numbered fs = zip (ClientId <$> [1 ..]) fs.clients
    mkClient cid cf =
      Client
        { clientId = cid,
          clientType = PermanentClientType,
          clientTime = toUTCTimeMillis (UTCTime (ModifiedJulianDay 0) 0),
          clientClass = Nothing,
          clientLabel = Nothing,
          clientCookie = Nothing,
          clientModel = Nothing,
          clientCapabilities = mempty,
          clientMLSPublicKeys = Map.fromList [(csSignatureScheme suite, "key") | suite <- S.toList cf.signatureSuites],
          clientLastActive = Nothing
        }

-- | The contactability rule, written out independently of the interpreter.
oracle :: ContactStatusContext -> Bool -> UserFacts -> Bool
oracle ctx targetMls fs
  | fs.deleted = False
  | otherwise = proteus || mlsReady
  where
    protos = if S.null fs.protocols then defSupportedProtocols else fs.protocols
    proteus = BaseProtocolProteusTag `S.member` protos && any (.hasPrekey) fs.clients
    mlsReady =
      ctx.mlsAvailable
        && targetMls
        && BaseProtocolMLSTag `S.member` protos
        && any
          ( \c ->
              any
                (\suite -> csSignatureScheme suite `elem` S.map csSignatureScheme c.signatureSuites && suite `S.member` c.keyPackageSuites)
                ctx.allowedCipherSuites
          )
          fs.clients

type ContactHandler = FedBrig.GetContactStatusRequest -> FedBrig.GetContactStatusResponse

-- | Federation monad whose only endpoint is @get-users-contact-status@, answered by a pure handler.
newtype CFed (comp :: Component) a = CFed (ContactHandler -> a)
  deriving (Functor, Applicative, Monad) via ((->) ContactHandler)

instance RunClient (CFed comp) where
  runRequestAcceptStatus _ _ = error "CFed does not support servant client"
  throwClientError _ = error "CFed does not support servant client"

instance VersionedMonad v (CFed comp) where
  guardVersion _ = pure ()

instance FederationMonad CFed where
  fedClientWithProxy (Proxy @_) (Proxy @api) (_ :: Proxy (CFed comp)) =
    case eqTypeRep
      (typeRep @(SC.Client (CFed comp) api))
      (typeRep @(FedBrig.GetContactStatusRequest -> CFed 'Brig FedBrig.GetContactStatusResponse)) of
      Just HRefl -> \req -> CFed ($ req)
      Nothing -> error "CFed: only get-users-contact-status is supported"

mockMlsKeyPackages :: ContactBackend -> InterpreterFor MlsKeyPackageSubsystem r
mockMlsKeyPackages b = interpret \case
  HasMlsKeyPackagesBulk reqs -> pure (S.fromList [r | r <- reqs, r `S.member` b.keyPackages])
  _ -> error "unimplemented MlsKeyPackageSubsystem operation"

-- | Interprets the store/config layers shared by backend and requester.
runContactStores ::
  ContactBackend ->
  Sem (UserStore : ClientStore : MlsKeyPackageSubsystem : Input UserSubsystemConfig : r) a ->
  Sem r a
runContactStores b =
  runInputConst (def {mlsEnabled = b.serverMls})
    . mockMlsKeyPackages b
    . evalState b.clientState
    . runInMemoryClientStoreInterpreterWithState
    . raiseUnder
    . runInMemoryUserStoreInterpreter b.storedUsers mempty

runContactBackend ::
  ContactBackend ->
  Sem '[UserStore, ClientStore, MlsKeyPackageSubsystem, Input UserSubsystemConfig] a ->
  a
runContactBackend b = run . runContactStores b

loopback :: ContactBackend -> ContactHandler
loopback b req =
  FedBrig.GetContactStatusResponse . runContactBackend b $
    getLocalContactStatusesImpl req.context (fromRange req.users)

-- | Records each call; domains without a handler are unreachable.
runCountingFed ::
  (Member (State [Domain]) r) =>
  Map Domain ContactHandler ->
  InterpreterFor (FederationAPIAccess CFed) r
runCountingFed handlers = interpret \case
  RunFederatedEither remote (CFed f) -> do
    modify (tDomain remote :)
    pure $ case Map.lookup (tDomain remote) handlers of
      Nothing -> Left (FederationUnexpectedError "unreachable")
      Just h -> Right (f h)
  RunFederatedConcurrentlyEither remotes rpc ->
    for (bucketRemote remotes) \ruids@(tDomain -> d) -> case rpc ruids of
      CFed f -> do
        modify (d :)
        pure $ case Map.lookup d handlers of
          Nothing -> Left (ruids, FederationUnexpectedError "unreachable")
          Just h -> Right (qualifyAs ruids (f h))
  RunFederatedConcurrentlyBucketsEither {} -> error "unimplemented: RunFederatedConcurrentlyBucketsEither"
  IsFederationConfigured -> pure True

-- | Runs on the requester's backend. Returns the domains that were called (with
-- multiplicity) and the result.
runContactRequester ::
  ContactBackend ->
  Map Domain ContactHandler ->
  Sem
    '[ UserStore,
       ClientStore,
       MlsKeyPackageSubsystem,
       Input UserSubsystemConfig,
       FederationAPIAccess CFed,
       TinyLog,
       State [Domain]
     ]
    a ->
  ([Domain], a)
runContactRequester b handlers =
  run
    . runState []
    . noopLogger
    . runCountingFed handlers
    . runContactStores b
