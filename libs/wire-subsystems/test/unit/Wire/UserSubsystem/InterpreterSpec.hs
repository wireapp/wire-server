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
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map qualified as Map
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set (insert, member, notMember)
import Data.Set qualified as S
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID qualified as UUID
import Database.Bloodhound.Internal.Client qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal
import Polysemy.Resource (Resource, runResource)
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import SAML2.WebSSO qualified as SAML
import Servant.Client.Core (RunClient (..))
import Servant.Client.Core qualified as SC
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Conversation.Protocol (ProtocolTag (ProtocolMLSTag))
import Wire.API.EnterpriseLogin
import Wire.API.Federation.API (FederationMonad (..), nameVal)
import Wire.API.Federation.API.Brig qualified as FedBrig
import Wire.API.Federation.Component (Component (Brig), componentVal)
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite (CipherSuiteTag, csSignatureScheme)
import Wire.API.MLS.KeyPackage
  ( KeyPackage (..),
    KeyPackageData (..),
    KeyPackageRef,
    KeyPackageTBS (..),
    kpRef,
  )
import Wire.API.MLS.LeafNode (LeafNode (..), LeafNodeCore (..), LeafNodeSource (..))
import Wire.API.MLS.Lifetime (Lifetime (..), Timestamp (..))
import Wire.API.MLS.Serialisation (RawMLS (..), mkRawMLS)
import Wire.API.Pagination (OffsetPage (..))
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature
  ( AllTeamFeatures,
    FeatureStatus (..),
    LockStatus (..),
    LockableFeature (..),
    MLSConfig,
    MLSConfigB (..),
    MlsE2EIdConfig,
    npUpdate,
  )
import Wire.API.Team.Invitation (ResendVerificationEmailResult (..))
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User hiding (DeleteUser)
import Wire.API.User.Client (Client (..))
import Wire.API.User.Client.Prekey (PrekeyId (..), UncheckedPrekeyBundle (..))
import Wire.API.User.IdentityProvider (IdPList (..), team)
import Wire.API.User.Search
import Wire.API.UserEvent
import Wire.API.VersionInfo (VersionedMonad (..))
import Wire.ActivationCodeStore qualified as ActivationCodeStore
import Wire.AppStore qualified as AppStore
import Wire.AppSubsystem
import Wire.Arbitrary (GenericUniform (..), generateExample)
import Wire.AuthenticationSubsystem.Error
import Wire.ClientStore (ClientStore)
import Wire.ClientSubsystem.Error (ClientError)
import Wire.DomainRegistrationStore qualified as DRS
import Wire.FederationAPIAccess (FederationAPIAccess (..))
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.IndexedUserStore qualified as IU
import Wire.InvitationStore (InsertInvitation, StoredInvitation)
import Wire.InvitationStore qualified as InvitationStore
import Wire.MiniBackend
import Wire.MlsKeyPackageSubsystem (MlsKeyPackageSubsystem)
import Wire.MlsKeyPackageSubsystem.Interpreter (interpretMlsKeyPackageSubsystem)
import Wire.MockInterpreters
import Wire.MockInterpreters.MlsKeyPackageStore (runInMemoryMlsKeyPackageStore)
import Wire.RateLimit
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserStore (UserStore)
import Wire.UserStore.IndexUser
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist
import Wire.UserSubsystem.Interpreter
  ( UserSubsystemConfig (..),
    getContactStatusesImpl,
    getLocalContactStatusesImpl,
  )
import Wire.Util
import Wire.VerificationCodeSubsystem

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "resendTeamMemberVerificationEmail" do
    it "rejects a missing account" $ do
      domain <- generate arbitrary
      uid <- generate arbitrary
      tid <- generate arbitrary
      let result =
            runNoFederationStackUserSubsystemErrorEither def mempty def $
              void $
                resendTeamMemberVerificationEmail tid (toLocalUnsafe domain uid)
      result `shouldBe` Left UserSubsystemProfileNotFound

    it "rejects a target in another team even when verification is pending" $ do
      NotPendingStoredUser stored <- generate arbitrary
      domain <- generate arbitrary
      tid <- generate arbitrary
      email <- generate arbitrary
      let user = stored {teamId = Nothing, status = Just Active, emailUnvalidated = Just email} :: StoredUser
          backend = def {users = [user]}
          result =
            runNoFederationStackUserSubsystemErrorEither backend mempty def $
              void $
                resendTeamMemberVerificationEmail tid (toLocalUnsafe domain user.id)
      result `shouldBe` Left UserSubsystemProfileNotFound

    it "returns nothing pending for a team member without an unvalidated email" $ do
      NotPendingStoredUser stored <- generate arbitrary
      domain <- generate arbitrary
      tid <- generate arbitrary
      let user = stored {teamId = Just tid, status = Just Active, emailUnvalidated = Nothing} :: StoredUser
          backend = def {users = [user]}
          result =
            runNoFederationStack backend mempty def $
              resendTeamMemberVerificationEmail tid (toLocalUnsafe domain user.id)
      case result of
        NoVerificationEmailPending -> pure ()
        VerificationEmailSent -> expectationFailure "sent verification without a pending email"

    it "allows resending a SCIM member's pending email without an existing activation code" $ do
      NotPendingStoredUser stored <- generate arbitrary
      domain <- generate arbitrary
      tid <- generate arbitrary
      email <- generate arbitrary
      oldEmail <- generate $ arbitrary `suchThat` (/= email)
      let user = stored {teamId = Just tid, status = Just Active, managedBy = Just ManagedByScim, activated = True, email = Just oldEmail, emailUnvalidated = Just email} :: StoredUser
          backend = def {users = [user]}
          (result, activation) =
            runNoFederationStack backend mempty def do
              resendResult <- resendTeamMemberVerificationEmail tid (toLocalUnsafe domain user.id)
              storedActivation <- ActivationCodeStore.lookupActivationCode (mkEmailKey email)
              pure (resendResult, storedActivation)
      case result of
        VerificationEmailSent -> pure ()
        NoVerificationEmailPending -> expectationFailure "did not resend the pending SCIM email"
      fmap fst activation `shouldBe` Just (Just user.id)

  describe "internalGetScimUsersPaginated" do
    prop "excludes expired pending accounts before counting and pagination" $
      \(accepted :: [NotPendingStoredUser]) (pendingAccounts :: [PendingNotEmptyIdentityStoredUser]) email teamId ->
        let acceptedUsers =
              [ user {teamId = Just teamId, managedBy = Just ManagedByScim, status = Just Active} :: StoredUser
              | wrapped <- accepted,
                let user = coerce wrapped :: StoredUser
              ]
            expiredUsers =
              [ user {teamId = Just teamId, managedBy = Just ManagedByScim, status = Just PendingInvitation, activated = True, email = Just email} :: StoredUser
              | wrapped <- pendingAccounts,
                let user = coerce wrapped :: StoredUser
              ]
            backend = def {users = acceptedUsers <> expiredUsers}
            expectedIds = sort $ map (.id) acceptedUsers
         in conjoin
              [ let page =
                      runNoFederationStack backend mempty def $
                        internalGetScimUsersPaginated teamId (unsafeRange count) (Just offset)
                 in conjoin
                      [ page.totalResults === fromIntegral (length acceptedUsers),
                        map (qUnqualified . userQualifiedId) page.results === take (fromIntegral count) (drop (fromIntegral offset) expectedIds)
                      ]
              | count <- [0, 1, 5],
                offset <- [0, 1, 5]
              ]

    prop "keeps pending accounts whose invitations still exist" $
      \(PendingNotEmptyIdentityStoredUser user) email teamId invitationInfo ->
        let pendingUser = user {teamId = Just teamId, managedBy = Just ManagedByScim, activated = True, email = Just email} :: StoredUser
            backend =
              def
                { users = [pendingUser],
                  invitations =
                    Map.singleton
                      (teamId, invitationInfo.invitationId)
                      (invitationInfo {InvitationStore.email = email, InvitationStore.teamId = teamId})
                }
            page =
              runNoFederationStack backend mempty def $
                internalGetScimUsersPaginated teamId (toRange (Proxy @5)) Nothing
         in conjoin
              [ page.totalResults === 1,
                map (qUnqualified . userQualifiedId) page.results === [pendingUser.id]
              ]

    it "finds live invitations beyond the first team invitation batch" $ do
      PendingNotEmptyIdentityStoredUser user <- generate arbitrary
      teamId <- generate arbitrary
      email <- generate arbitrary
      generated <- generate $ vectorOf 501 (arbitrary @StoredInvitation)
      let ordered = sortOn (.invitationId) generated
          invitations =
            [ inv {InvitationStore.teamId = teamId, InvitationStore.email = if i == (500 :: Int) then email else inv.email}
            | (i, inv) <- zip [0 ..] ordered
            ]
          pendingUser = user {teamId = Just teamId, managedBy = Just ManagedByScim, activated = True, email = Just email} :: StoredUser
          backend = def {users = [pendingUser], invitations = Map.fromList [((teamId, inv.invitationId), inv) | inv <- invitations]}
          page =
            runNoFederationStack backend mempty def $
              internalGetScimUsersPaginated teamId (toRange (Proxy @5)) Nothing
      page.totalResults `shouldBe` 1
      map (qUnqualified . userQualifiedId) page.results `shouldBe` [pendingUser.id]

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
      \(remoteUsers :: Map Domain [UserId]) (localUserIds :: [UserId]) ctx -> ioProperty do
        let remote = remoteDomainUsers remoteUsers
            handlers = allContactableHandler <$ remote
            qids =
              toQualifiedUids remote <> toQualifiedUids (Map.singleton localDomain' localUserIds)
        (calls, _) <-
          runOnRequesterBackend
            (requesterBackendFor ctx)
            handlers
            (getContactStatusesImpl lself qids)
        pure $ sort calls === sort (Map.keys remote)

    prop "no federation call when there are no remote users" $
      \(localUserIds :: [UserId]) ctx -> ioProperty do
        (calls, _) <-
          runOnRequesterBackend
            (requesterBackendFor ctx)
            mempty
            ( getContactStatusesImpl
                lself
                (toQualifiedUids (Map.singleton localDomain' localUserIds))
            )
        pure $ calls === []

    prop "a failing remote domain has no entries, other domains are unaffected" $
      \(upUsers :: Map Domain [UserId]) (downUsers :: Map Domain [UserId]) ctx -> ioProperty do
        let up = remoteDomainUsers upUsers
            down = remoteDomainUsers (Map.difference downUsers up)
            handlers = allContactableHandler <$ up
        (calls, result) <-
          runOnRequesterBackend
            (requesterBackendFor ctx)
            handlers
            (getContactStatusesImpl lself (toQualifiedUids up <> toQualifiedUids down))
        pure $
          conjoin
            [ result
                === Map.fromList [(qid, ContactStatus Contactable) | qid <- toQualifiedUids up],
              sort calls === sort (Map.keys up <> Map.keys down)
            ]

    -- The contactability rule is tested on users that are contactable by
    -- construction. Negative cases remove exactly one ingredient.
    prop "proteus user is contactable iff a client has a prekey" $
      \base ctx targetMls -> ioProperty do
        let user = activeWith (Just (S.singleton BaseProtocolProteusTag)) base
        conjoin
          <$> sequence
            [ statusOf ctx targetMls user [proteusClient]
                `shouldBeStatus` Just (ContactStatus Contactable),
              statusOf ctx targetMls user [proteusClient {hasPrekey = False}]
                `shouldBeStatus` Just (ContactStatus NonContactable),
              statusOf ctx targetMls user [] `shouldBeStatus` Just (ContactStatus NonContactable)
            ]

    prop "missing or empty supported protocols default to proteus" $
      \base ctx targetMls -> ioProperty do
        let status protos = statusOf ctx targetMls (activeWith protos base) [proteusClient]
        conjoin
          <$> sequence
            [ status Nothing `shouldBeStatus` Just (ContactStatus Contactable),
              status (Just mempty) `shouldBeStatus` Just (ContactStatus Contactable)
            ]

    prop "MLS user is contactable, each missing ingredient makes it non-contactable" $
      \base ctx suite -> ioProperty do
        let ctx' = ctx {mlsAvailable = True, allowedCipherSuites = S.singleton suite}
            user = activeWith (Just (S.singleton BaseProtocolMLSTag)) base
            client = mlsClient suite
            nonContactable = Just (ContactStatus NonContactable)
        conjoin
          <$> sequence
            [ statusOf ctx' True user [client] `shouldBeStatus` Just (ContactStatus Contactable),
              counterexample "target server MLS off"
                <$> (statusOf ctx' False user [client] `shouldBeStatus` nonContactable),
              counterexample "requester MLS unavailable"
                <$> ( statusOf ctx' {mlsAvailable = False} True user [client]
                        `shouldBeStatus` nonContactable
                    ),
              counterexample "no allowed cipher suite"
                <$> ( statusOf ctx' {allowedCipherSuites = mempty} True user [client]
                        `shouldBeStatus` nonContactable
                    ),
              counterexample "no signature key"
                <$> ( statusOf ctx' True user [client {signatureSuites = mempty}]
                        `shouldBeStatus` nonContactable
                    ),
              counterexample "no key package"
                <$> ( statusOf ctx' True user [client {keyPackageSuites = mempty}]
                        `shouldBeStatus` nonContactable
                    ),
              counterexample "expired key package"
                <$> ( statusOfBackend
                        ctx'
                        (mkFakeBackend True [(user, [client])])
                          { keyPackageLifetime = expiredLifetime
                          }
                        user
                        `shouldBeStatus` nonContactable
                    ),
              counterexample "user does not support MLS"
                <$> ( statusOf
                        ctx'
                        True
                        (activeWith (Just (S.singleton BaseProtocolProteusTag)) base)
                        [client]
                        `shouldBeStatus` nonContactable
                    )
            ]

    prop "one capable client among incapable ones makes a user contactable, wherever it is" $
      \base ctx suite -> ioProperty do
        let ctx' = ctx {mlsAvailable = True, allowedCipherSuites = S.singleton suite}
            incapable =
              FakeClient {hasPrekey = False, signatureSuites = mempty, keyPackageSuites = mempty}
            user = activeWith (Just (S.fromList [BaseProtocolProteusTag, BaseProtocolMLSTag])) base
            statusWith clients = statusOf ctx' True user clients
        conjoin
          <$> sequence
            [ statusWith [incapable, incapable]
                `shouldBeStatus` Just (ContactStatus NonContactable),
              statusWith [incapable, proteusClient]
                `shouldBeStatus` Just (ContactStatus Contactable),
              statusWith [proteusClient, incapable]
                `shouldBeStatus` Just (ContactStatus Contactable),
              statusWith [incapable, mlsClient suite]
                `shouldBeStatus` Just (ContactStatus Contactable),
              statusWith [mlsClient suite, incapable]
                `shouldBeStatus` Just (ContactStatus Contactable)
            ]

    prop "deleted users are non-contactable, pending-invitation users are left out" $
      \base ctx targetMls -> ioProperty do
        let status s =
              statusOf
                ctx
                targetMls
                (base {status = Just s, supportedProtocols = Nothing} :: StoredUser)
                [proteusClient]
        conjoin
          <$> sequence
            [ status Active `shouldBeStatus` Just (ContactStatus Contactable),
              status Deleted `shouldBeStatus` Just (ContactStatus NonContactable),
              status PendingInvitation `shouldBeStatus` Nothing
            ]

    prop "local route and remote route evaluate the same users identically" $
      \ctx targetMls (users :: [(StoredUser, [FakeClient])]) (unknownIds :: [UserId]) ->
        ioProperty do
          let backend = mkFakeBackend targetMls users
              known = [user.id | user <- backend.storedUsers]
              qids = [Qualified uid targetDomain | uid <- known <> unknownIds]
              targetSelf = toLocalUnsafe targetDomain (Id UUID.nil)
          -- both requesters derive the same effective MLS availability: the
          -- target is its own requester, so its server flag applies on both sides
          (_, viaLocal) <-
            runOnRequesterBackend
              backend {features = featuresFor ctx}
              mempty
              (getContactStatusesImpl targetSelf qids)
          (calls, viaRemote) <-
            runOnRequesterBackend
              (requesterBackendFor ctx)
              (Map.singleton targetDomain (remoteBackendHandler backend))
              (getContactStatusesImpl lself qids)
          pure $
            conjoin
              [ viaRemote === viaLocal,
                counterexample
                  "only requested users are answered"
                  (all (`elem` qids) (Map.keys viaLocal)),
                length calls === (if null qids then 0 else 1)
              ]
  where
    activeWith :: Maybe (S.Set BaseProtocolTag) -> StoredUser -> StoredUser
    activeWith protos user = user {status = Just Active, supportedProtocols = protos}

    -- Status of a single user on a backend with the given server MLS flag.
    statusOf ::
      ContactStatusContext -> Bool -> StoredUser -> [FakeClient] -> IO (Maybe ContactStatus)
    statusOf ctx serverMls user clients =
      statusOfBackend ctx (mkFakeBackend serverMls [(user, clients)]) user

    statusOfBackend :: ContactStatusContext -> FakeBackend -> StoredUser -> IO (Maybe ContactStatus)
    statusOfBackend ctx backend user =
      Map.lookup user.id <$> runFakeBackend backend (getLocalContactStatusesImpl ctx [user.id])

    -- QuickCheck has no monadic '==='. This keeps results as 'Property', so
    -- 'conjoin' still reports all failing checks.
    shouldBeStatus :: IO (Maybe ContactStatus) -> Maybe ContactStatus -> IO Property
    shouldBeStatus actual expected = (=== expected) <$> actual

    -- Requester whose derived 'ContactStatusContext' is the given one.
    requesterBackendFor :: ContactStatusContext -> FakeBackend
    requesterBackendFor ctx = emptyBackend {serverMls = True, features = featuresFor ctx}

    localDomain' = Domain "local.example.com"
    targetDomain = Domain "target.example.com"
    lself = toLocalUnsafe localDomain' (Id UUID.nil)

    toQualifiedUids :: Map Domain [UserId] -> [Qualified UserId]
    toQualifiedUids domainsToUids =
      [Qualified uid domain | (domain, uids) <- Map.toList domainsToUids, uid <- uids]

    remoteDomainUsers :: Map Domain [UserId] -> Map Domain [UserId]
    remoteDomainUsers = Map.filter (not . null) . Map.delete localDomain'

    allContactableHandler :: GetContactStatusHandler
    allContactableHandler req =
      pure $
        FedBrig.GetContactStatusResponse
          (Map.fromList [(uid, ContactStatus Contactable) | uid <- fromRange req.users])

-- | What a backend stores about a client: a prekey, MLS signature keys and key
-- packages (the latter two per cipher suite).
data FakeClient = FakeClient
  { hasPrekey :: Bool,
    signatureSuites :: S.Set CipherSuiteTag,
    keyPackageSuites :: S.Set CipherSuiteTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FakeClient)

-- | Contactable via proteus only.
proteusClient :: FakeClient
proteusClient = FakeClient {hasPrekey = True, signatureSuites = mempty, keyPackageSuites = mempty}

-- | Contactable via MLS with the given cipher suite only.
mlsClient :: CipherSuiteTag -> FakeClient
mlsClient suite =
  FakeClient
    { hasPrekey = False,
      signatureSuites = S.singleton suite,
      keyPackageSuites = S.singleton suite
    }

-- | Data required to simulate a fake backend
data FakeBackend = FakeBackend
  { serverMls :: Bool,
    storedUsers :: [StoredUser],
    clientState :: ClientStoreState,
    keyPackages :: S.Set (UserId, ClientId, CipherSuiteTag),
    -- | lifetime of every key package in 'keyPackages'
    keyPackageLifetime :: Lifetime,
    -- | team features of the requester, from which the 'ContactStatusContext' is derived
    features :: AllTeamFeatures
  }

emptyBackend :: FakeBackend
emptyBackend =
  FakeBackend
    { serverMls = False,
      storedUsers = [],
      clientState = emptyClientStoreState,
      keyPackages = mempty,
      keyPackageLifetime = usableLifetime,
      features = def
    }

-- | Team features that make the requester's derived 'ContactStatusContext' equal
-- to the given one (as long as the server has MLS enabled).
featuresFor :: ContactStatusContext -> AllTeamFeatures
featuresFor ctx = npUpdate @MLSConfig mls def
  where
    defMls = def :: LockableFeature MLSConfig
    mls =
      defMls
        { status = if ctx.mlsAvailable then FeatureStatusEnabled else FeatureStatusDisabled,
          config =
            defMls.config
              { mlsSupportedProtocols = [ProtocolMLSTag],
                mlsAllowedCipherSuites = S.toList ctx.allowedCipherSuites
              }
        }

-- | Users are stored as given (the first occurrence of each user id wins).
mkFakeBackend :: Bool -> [(StoredUser, [FakeClient])] -> FakeBackend
mkFakeBackend mlsFlag users =
  FakeBackend
    { serverMls = mlsFlag,
      storedUsers = fst <$> uniqueUsers,
      clientState =
        ClientStoreState
          (Map.fromList [(user.id, storedClients clients) | (user, clients) <- uniqueUsers]),
      keyPackages =
        S.fromList
          [ (user.id, cid, suite)
          | (user, clients) <- uniqueUsers,
            (cid, client) <- numbered clients,
            suite <- S.toList client.keyPackageSuites
          ],
      keyPackageLifetime = usableLifetime,
      features = def
    }
  where
    -- the first occurrence of each user id wins
    uniqueUsers = nubBy (\(user1, _) (user2, _) -> user1.id == user2.id) users
    -- the backend assigns each client of a user its id: 1, 2, ...
    numbered clients = zip (ClientId <$> [1 ..]) clients

    storedClients :: [FakeClient] -> Map ClientId StoredClient
    storedClients clients =
      Map.fromList [(cid, storedClient cid client) | (cid, client) <- numbered clients]

    storedClient :: ClientId -> FakeClient -> StoredClient
    storedClient cid client = StoredClient (mkClient cid client) prekeys
      where
        prekeys = if client.hasPrekey then [UncheckedPrekeyBundle (PrekeyId 1) "k"] else []

    mkClient cid client =
      def
        { clientId = cid,
          clientMLSPublicKeys =
            Map.fromList
              [(csSignatureScheme suite, "key") | suite <- S.toList client.signatureSuites]
        }

type GetContactStatusHandler =
  FedBrig.GetContactStatusRequest -> IO FedBrig.GetContactStatusResponse

-- | Federation monad whose only endpoint is @get-users-contact-status@, answered by a handler.
newtype CFed (comp :: Component) a = CFed (GetContactStatusHandler -> IO a)
  deriving (Functor, Applicative, Monad) via (ReaderT GetContactStatusHandler IO)

instance RunClient (CFed comp) where
  runRequestAcceptStatus _ _ = error "CFed does not support servant client"
  throwClientError _ = error "CFed does not support servant client"

-- | Version negotiation is FederatorClient's concern; like other test monads,
-- `CFed` models a remote supporting the endpoint.
instance VersionedMonad v (CFed comp) where
  guardVersion _ = pure ()

instance FederationMonad CFed where
  fedClientWithProxy (Proxy @name) (Proxy @api) (_ :: Proxy (CFed comp)) =
    lookupSubsystemOperation
      (componentVal @comp, nameVal @name, Proxy @(SC.Client (CFed comp) api))
      (error "CFed: only get-users-contact-status is supported")
      ((Brig, "get-users-contact-status", contactStatusClient) ::: TNil)

contactStatusClient ::
  FedBrig.GetContactStatusRequest -> CFed 'Brig FedBrig.GetContactStatusResponse
contactStatusClient req = CFed ($ req)

-- | A key package the real 'MlsKeyPackageSubsystem' decodes, valid exactly for the given lifetime.
mkKeyPackage :: CipherSuiteTag -> Lifetime -> (KeyPackageRef, KeyPackageData)
mkKeyPackage suite lifetime = (kpRef suite packageData, packageData)
  where
    base :: KeyPackage
    base = generateExample
    leafCore = base.leafNode.core.value {source = LeafNodeSourceKeyPackage lifetime}
    leaf = base.leafNode {core = mkRawMLS leafCore}
    tbs = base.tbs.value {leafNode = leaf}
    packageData = KeyPackageData . (.raw) . mkRawMLS $ KeyPackage (mkRawMLS tbs) base.signature_

usableLifetime, expiredLifetime :: Lifetime
usableLifetime = Lifetime (Timestamp 0) (Timestamp maxBound)
expiredLifetime = Lifetime (Timestamp 0) (Timestamp 1)

-- | Interprets the store/config layers shared by backend and requester.
runFakeBackendStores ::
  (Members '[Embed IO, Resource] r) =>
  FakeBackend ->
  Sem (UserStore : ClientStore : MlsKeyPackageSubsystem : Input UserSubsystemConfig : r) a ->
  Sem r a
runFakeBackendStores backend action = do
  lock <- embed (newMVar ())
  ( runInputConst (def {mlsEnabled = backend.serverMls})
      . runInMemoryMlsKeyPackageStore keyPackageStore
      . interpretMlsKeyPackageSubsystem Nothing lock
      . raiseUnder
      . evalState backend.clientState
      . runInMemoryClientStoreInterpreterWithState
      . raiseUnder
      . runInMemoryUserStoreInterpreter backend.storedUsers mempty
    )
    action
  where
    keyPackageStore =
      Map.fromSet
        (\(_, _, suite) -> [mkKeyPackage suite backend.keyPackageLifetime])
        backend.keyPackages

runFakeBackend ::
  FakeBackend ->
  Sem
    '[ UserStore,
       ClientStore,
       MlsKeyPackageSubsystem,
       Input UserSubsystemConfig,
       Resource,
       Embed IO
     ]
    a ->
  IO a
runFakeBackend backend = runM . runResource . runFakeBackendStores backend

remoteBackendHandler :: FakeBackend -> GetContactStatusHandler
remoteBackendHandler backend req =
  FedBrig.GetContactStatusResponse
    <$> runFakeBackend backend (getLocalContactStatusesImpl req.context (fromRange req.users))

-- | Records each call; domains without a handler are unreachable.
runRecordingFed ::
  (Members '[State [Domain], Embed IO] r) =>
  Map Domain GetContactStatusHandler ->
  InterpreterFor (FederationAPIAccess CFed) r
runRecordingFed handlers = interpret \case
  RunFederatedEither remote (CFed f) -> do
    modify (tDomain remote :)
    case Map.lookup (tDomain remote) handlers of
      Nothing -> pure $ Left (FederationUnexpectedError "unreachable")
      Just handler -> Right <$> embed (f handler)
  RunFederatedConcurrentlyEither remotes rpc ->
    for (bucketRemote remotes) \ruids@(tDomain -> domain) -> case rpc ruids of
      CFed f -> do
        modify (domain :)
        case Map.lookup domain handlers of
          Nothing -> pure $ Left (ruids, FederationUnexpectedError "unreachable")
          Just handler -> Right . qualifyAs ruids <$> embed (f handler)
  RunFederatedConcurrentlyBucketsEither {} ->
    error "unimplemented: RunFederatedConcurrentlyBucketsEither"
  IsFederationConfigured -> pure True

-- | Runs on the requester's backend. Returns the domains that were called (with
-- multiplicity) and the result.
runOnRequesterBackend ::
  FakeBackend ->
  Map Domain GetContactStatusHandler ->
  Sem
    '[ UserStore,
       ClientStore,
       MlsKeyPackageSubsystem,
       Input UserSubsystemConfig,
       GalleyAPIAccess,
       FederationAPIAccess CFed,
       TinyLog,
       State [Domain],
       Resource,
       Embed IO
     ]
    a ->
  IO ([Domain], a)
runOnRequesterBackend backend handlers =
  runM
    . runResource
    . runState emptyFedCallsAcc
    . noopLogger
    . runRecordingFed handlers
    . miniGalleyAPIAccess mempty backend.features
    . runFakeBackendStores backend
  where
    emptyFedCallsAcc :: [Domain]
    emptyFedCallsAcc = []
