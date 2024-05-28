{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Control.Lens (ix, (.~))
import Control.Lens.At ()
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Handle (Handle (Handle))
import Data.Handle qualified as Handle
import Data.Id
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Qualified
import Data.Set qualified as S
import Data.String.Conversions (cs)
import Data.Text qualified as Text
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Federation.Error
import Wire.API.Team.Feature (AllFeatureConfigs (afcMlsE2EId), FeatureStatus (..), defFeatureStatus, setStatus)
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User hiding (DeleteUser)
import Wire.API.UserEvent
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserSubsystem
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

    prop "Update user" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            profile = fromJust $ runNoFederationStack localBackend Nothing config do
              updateUserProfile lusr Nothing AllowSCIMUpdates update
              getUserProfile lusr (tUntagged lusr)
         in -- TODO: check locale update?
            -- TODO: more assertions
            profile.profileQualifiedId === tUntagged lusr
              -- if the name / pict / etc... is not set, the original
              -- value should be preserved
              .&&. profile.profileName === fromMaybe profile.profileName update.name
              .&&. profile.profilePict === fromMaybe profile.profilePict update.pict
              .&&. profile.profileAssets === fromMaybe profile.profileAssets update.assets
              .&&. profile.profileAccentId === fromMaybe profile.profileAccentId update.accentId

    prop "Update user events" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            events = runNoFederationStack localBackend Nothing config do
              updateUserProfile lusr Nothing AllowSCIMUpdates update
              get @[MiniEvent]
         in events
              === [ MkMiniEvent
                      alice.id
                      ( UserUpdated $
                          (emptyUserUpdatedData alice.id)
                            { eupName = update.name,
                              eupPict = update.pict,
                              eupAccentId = update.accentId,
                              eupAssets = update.assets
                            }
                      )
                  ]

    prop
      "user managed by scim doesn't allow update"
      \(NotPendingStoredUser alice) localDomain (update :: UserProfileUpdate) name config ->
        alice.name /= name ==>
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def config do
                    updateUserProfile lusr Nothing ForbidSCIMUpdates update {name = Just name}
                    getUserProfile lusr (tUntagged lusr)
           in Left UserSubsystemDisplayNameManagedByScim === profileErr

    prop
      "if e2e identity is activated, the user name cannot be updated"
      \(NotPendingStoredUser alice) localDomain update name config ->
        alice.name /= name ==>
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def {afcMlsE2EId = setStatus FeatureStatusEnabled defFeatureStatus} config do
                    updateUserProfile lusr Nothing AllowSCIMUpdates (update {name = Just name})
                    getUserProfile lusr (tUntagged lusr)
           in Left UserSubsystemDisplayNameManagedByScim === profileErr

    prop
      "CheckHandle succeeds if there is a user with that handle"
      \((NotPendingStoredUser alice, fallbackHandle :: Handle), config) ->
        let isHandle :: CheckHandleResp = runNoFederationStack localBackend Nothing config do
              let handle = Handle.fromHandle . fromMaybe fallbackHandle $ alice.handle
              checkHandle handle
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
         in if isJust alice.handle
              then isHandle === CheckHandleFound
              else isHandle === CheckHandleNotFound

    prop
      "CheckHandles returns available handles from a list of handles, up to X"
      \((storedUsersAndHandles :: [(StoredUser, Handle)], handles :: Set Handle), maxCount :: Word, config) ->
        let availables = runNoFederationStack localBackend Nothing config do
              checkHandles (S.toList handles) maxCount
            notAvailables = runNoFederationStack localBackend Nothing config do
              checkHandles (mapMaybe ((.handle)) users) maxCount
            localBackend = def {users = users}
            users = (\(u, h) -> u {handle = Just h, managedBy = Just ManagedByWire}) <$> storedUsersAndHandles
         in notAvailables === []
              .&&. (length availables <= fromIntegral maxCount) === True -- TODO: add a better assertion here
              .&&. ((S.fromList availables) `S.isSubsetOf` handles) === True

    describe "Scim+UpdateProfileUpdate" do
      prop
        "Updating handles fails when ForbidSCIMUpdates"
        \(alice, Handle newHandle, domain, config) ->
          let res :: Either UserSubsystemError ()
              res = run
                . runErrorUnsafe
                . runError
                $ interpretNoFederationStack localBackend Nothing def config do
                  updateHandle (toLocalUnsafe domain alice.id) Nothing ForbidSCIMUpdates newHandle

              localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
           in res === Left UserSubsystemHandleManagedByScim

      prop
        "Updating handles succeeds when AllowSCIMUpdates"
        \(alice, ssoId, email :: Maybe Email, Handle newHandle, domain, config) ->
          let res :: Either UserSubsystemError () = run
                . runErrorUnsafe
                . runError
                $ interpretNoFederationStack localBackend Nothing def config do
                  updateHandle (toLocalUnsafe domain alice.id) Nothing AllowSCIMUpdates newHandle
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
      \(storedUser :: StoredUser, Handle rawHandle, config) ->
        isJust storedUser.identity ==>
          let updateResult :: Either UserSubsystemError () = run
                . runErrorUnsafe
                . runError
                $ interpretNoFederationStack (def {users = [storedUser]}) Nothing def config do
                  let luid = toLocalUnsafe dom storedUser.id
                      dom = Domain "localdomain"
                  updateHandle luid Nothing AllowSCIMUpdates rawHandle
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
                  updateHandle luid Nothing AllowSCIMUpdates badHandle
              localBackend = def {users = [storedUser]}
           in updateResult === Left UserSubsystemInvalidHandle

    it "read / update supported-protocols" $ do
      -- TODO: use prop after all and scale down the reruns?
      storedUser <- generate arbitrary
      config <- generate arbitrary

      let luid :: Local UserId
          luid = toLocalUnsafe dom storedUser.id
            where
              dom = Domain "localdomain"

          beforeProtocols :: Maybe (Set BaseProtocolTag)
          beforeProtocols = storedUser.supportedProtocols

          afterProtocols :: Set BaseProtocolTag
          afterProtocols = (S.fromList . ([minBound ..] \\) . S.toList) $ fromMaybe [minBound ..] beforeProtocols

          operation :: Sem (GetUserProfileEffects `Append` AllErrors) a -> a
          operation op = runNoFederationStack localBackend Nothing config op
            where
              localBackend = def {users = [storedUser]}

      beforeProtocols `shouldNotBe` Just afterProtocols

      let [before] = operation $ getUserProfiles luid [tUntagged luid]
          () = operation $ updateUserProfile luid Nothing ForbidSCIMUpdates (def {supportedProtocols = Just afterProtocols})
          [after] = operation $ getUserProfiles luid [tUntagged luid]

      before.supportedProtocols `shouldBe` beforeProtocols
      after.supportedProtocols `shouldBe` afterProtocols

newtype BadHandle = BadHandle {_unBadHandle :: Text}
  deriving newtype (Eq, Show)

instance Arbitrary BadHandle where
  arbitrary = oneof [tooShort, tooLong, badBytes]
    where
      tooShort = (BadHandle . Text.pack . (: [])) <$> elements validChar
      tooLong = (BadHandle . Text.pack) <$> replicateM 258 (elements validChar)
      badBytes =
        BadHandle <$> do
          totalLen :: Int <- choose (2, 256)
          invalidCharPos :: Int <- choose (0, totalLen - 1)
          invalidCharContent <- elements invalidChar
          good :: Text <- cs <$> replicateM totalLen (elements validChar)
          let bad :: Text = good & ix invalidCharPos .~ invalidCharContent
          pure bad

      validChar :: [Char] = ['a' .. 'z'] <> ['0' .. '9'] <> "_-."
      invalidChar :: [Char] = [minBound ..] \\ validChar
