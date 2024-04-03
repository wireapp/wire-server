module Wire.UserSubsystem.InterpreterSpec (spec) where

import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Imports
import Polysemy
import Polysemy.Input
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User
import Wire.GalleyAPIAccess
import Wire.UserStore
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfile" do
    prop "returns Nothing when the user doesn't exist" $
      \viewer targetUserId visibility domain locale ->
        let retrievedProfile =
              run
                . runInputConst (UserSubsystemConfig visibility domain locale)
                . staticUserStoreInterpreter []
                . emptyGalleyAPIAccess
                $ getLocalUserProfile viewer targetUserId
         in retrievedProfile === Nothing

    prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
      \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id_ fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            retrievedProfile =
              run
                . runInputConst (UserSubsystemConfig visibility domain locale)
                . staticUserStoreInterpreter [targetUser, viewer]
                . fakeGalleyAPIAccess teamMember
                $ getLocalUserProfile viewer.id_ targetUser.id_
         in retrievedProfile
              === Just
                ( mkUserProfile
                    (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                    (mkUserFromStored domain locale targetUser)
                    UserLegalHoldDisabled
                )
    prop "gets Nothing when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
      \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id_ fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            retrievedProfile =
              run
                . runInputConst (UserSubsystemConfig visibility domain locale)
                . staticUserStoreInterpreter [targetUser, viewer]
                . fakeGalleyAPIAccess teamMember
                $ getLocalUserProfile viewer.id_ targetUser.id_
         in retrievedProfile
              === Just
                ( mkUserProfile
                    (fmap (const Nothing) visibility)
                    (mkUserFromStored domain locale targetUser)
                    UserLegalHoldDisabled
                )

    prop "returns Nothing if the target user has not accepted their invitation yet" $
      \viewerId (PendingStoredUser targetUser) visibility domain locale ->
        let teamMember = mkTeamMember viewerId fullPermissions Nothing UserLegalHoldDisabled
            retrievedProfile =
              run
                . runInputConst (UserSubsystemConfig visibility domain locale)
                . staticUserStoreInterpreter [targetUser]
                . fakeGalleyAPIAccess teamMember
                $ getLocalUserProfile viewerId targetUser.id_
         in retrievedProfile === Nothing
  describe "mkUserFromStored" $ do
    prop "user identity" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if storedUser.activated == False
            then user.userIdentity === Nothing
            else
              (emailIdentity =<< user.userIdentity) === storedUser.email
                .&&. (phoneIdentity =<< user.userIdentity) === storedUser.phone
                .&&. (ssoIdentity =<< user.userIdentity) === storedUser.ssoId

    prop "user deleted" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in user.userDeleted === (storedUser.status == Just Deleted)

    prop "user expires" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if storedUser.status == Just Ephemeral
            then user.userExpire === storedUser.expires
            else user.userExpire === Nothing

    prop "user expires" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if storedUser.status == Just Ephemeral
            then user.userExpire === storedUser.expires
            else user.userExpire === Nothing

    prop "user locale" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if (isJust storedUser.language)
            then user.userLocale === Locale (fromJust storedUser.language) storedUser.country
            else user.userLocale === defaultLocale

newtype PendingStoredUser = PendingStoredUser StoredUser
  deriving (Show, Eq)

instance Arbitrary PendingStoredUser where
  arbitrary = do
    user <- arbitrary
    pure $ PendingStoredUser (user {status = Just PendingInvitation})

newtype NotPendingStoredUser = NotPendingStoredUser StoredUser
  deriving (Show, Eq)

instance Arbitrary NotPendingStoredUser where
  arbitrary = do
    user <- arbitrary
    notPendingStatus <- elements (Nothing : map Just [Active, Suspended, Deleted, Ephemeral])
    pure $ NotPendingStoredUser (user {status = notPendingStatus})

staticUserStoreInterpreter :: [StoredUser] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> user.id_ == uid) allUsers

emptyGalleyAPIAccess :: InterpreterFor GalleyAPIAccess r
emptyGalleyAPIAccess = interpret $ \case
  _ -> error "uninterpreted effect"

fakeGalleyAPIAccess :: TeamMember -> InterpreterFor GalleyAPIAccess r
fakeGalleyAPIAccess member = interpret $ \case
  GetTeamMember _ _ -> pure (Just member)
  _ -> error "uninterpreted effect"
