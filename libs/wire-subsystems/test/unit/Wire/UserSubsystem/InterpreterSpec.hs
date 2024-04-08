{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Data.ByteString.Builder qualified as Builder
import Data.Domain
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant.Client.Core
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Federation.API
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.StoredUser
import Wire.UserStore
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfile" do
    prop "returns nothing when the none of the users exist" $
      \viewer targetUserIds visibility domain locale ->
        let config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [] Nothing config $
                getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
         in retrievedProfile === []

    prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
      \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id_ fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getUserProfiles (toLocalUnsafe domain viewer.id_) [Qualified targetUser.id_ domain]
         in retrievedProfile
              === [ mkUserProfile
                      (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                      (mkUserFromStored domain locale targetUser)
                      UserLegalHoldDisabled
                  ]
    prop "gets Nothing when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
      \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id_ fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getUserProfiles (toLocalUnsafe domain viewer.id_) [Qualified targetUser.id_ domain]
         in retrievedProfile
              === [ mkUserProfile
                      (fmap (const Nothing) visibility)
                      (mkUserFromStored domain locale targetUser)
                      UserLegalHoldDisabled
                  ]

    prop "returns Nothing if the target user has not accepted their invitation yet" $
      \viewer (PendingStoredUser targetUser) visibility domain locale ->
        let teamMember = mkTeamMember viewer.id_ fullPermissions Nothing UserLegalHoldDisabled
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getLocalUserProfiles viewer.id_ [targetUser.id_]
         in retrievedProfile === []

-- prop "returns the profiles returned by remote backends" $
--   \viewerId targetUser visibility domain locale ->
--     let config = UserSubsystemConfig visibility domain locale
--         retrievedProfile =

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

type GetUserProfileEffects =
  [ GalleyAPIAccess,
    UserStore,
    (Input UserSubsystemConfig),
    (Error FederationError),
    (FederationAPIAccess FakeFederation),
    (Concurrency 'Unsafe)
  ]

newtype FakeFederation (c :: Component) a = FakeFederation (ReaderT (Map Domain [UserProfile]) Identity a)
  deriving newtype (Functor, Applicative, Monad)

instance KnownComponent c => RunClient (FakeFederation c) where
  runRequestAcceptStatus _acceptableStatuses req = do
    case (componentVal @c, Builder.toLazyByteString req.requestPath) of
      -- TODO: Create a fake servant server and call that
      (Brig, "/get-users-by-ids") -> undefined
      (component, rpc) -> error $ "Unexpected federated call: " <> show (component, rpc)

  throwClientError err = error $ "Unexpected federation client error: " <> displayException err

instance FederationMonad FakeFederation where
  fedClientWithProxy _ _ = some_pattern_matching_and_unsafe_coerce_magic

data SomeEndpoint where
  SomeEndpoint :: forall (comp :: Component) api name. HasFedEndpoint comp api name => SomeEndpoint

class FakeFederationImpl comp name where
  fakeFederationImpl :: HasFedEndpoint comp name api => Client (FakeFederation comp) api

runFakeFederation :: Map Domain [UserProfile] -> FakeFederation c a -> a
runFakeFederation users (FakeFederation action) = runIdentity $ runReaderT action users

-- fakeGetUsersById :: Request -> FakeFederation c Response
-- fakeGetUsersById req = do

runFederationStack ::
  Map Domain [UserProfile] ->
  [StoredUser] ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem GetUserProfileEffects a ->
  a
runFederationStack fedUsers localUsers teamMember config =
  run
    . sequentiallyPerformConcurrency
    . fakeFederationAPIAccess fedUsers
    . runErrorUnsafe
    . runInputConst config
    . staticUserStoreInterpreter localUsers
    . fakeGalleyAPIAccess teamMember

runNoFederationStack ::
  [StoredUser] ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem GetUserProfileEffects a ->
  a
runNoFederationStack allUsers teamMember config =
  run
    . sequentiallyPerformConcurrency
    . emptyFederationAPIAcesss
    . runErrorUnsafe
    . runInputConst config
    . staticUserStoreInterpreter allUsers
    . fakeGalleyAPIAccess teamMember

runErrorUnsafe :: Exception e => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

emptyFederationAPIAcesss :: InterpreterFor (FederationAPIAccess FakeFederation) r
emptyFederationAPIAcesss = interpret $ \case
  _ -> error "uninterpreted effect: FederationAPIAccess"

fakeFederationAPIAccess :: forall r. Member (Concurrency 'Unsafe) r => Map Domain [UserProfile] -> InterpreterFor (FederationAPIAccess FakeFederation) r
fakeFederationAPIAccess userProfiles =
  let runner :: FederatedActionRunner FakeFederation r
      runner _domain rpc = pure . Right $ runFakeFederation userProfiles rpc
   in interpretFederationAPIAccessGeneral runner (pure True)

staticUserStoreInterpreter :: [StoredUser] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> user.id_ == uid) allUsers

fakeGalleyAPIAccess :: Maybe TeamMember -> InterpreterFor GalleyAPIAccess r
fakeGalleyAPIAccess member = interpret $ \case
  GetTeamMember _ _ -> pure member
  _ -> error "uninterpreted effect: GalleyAPIAccess"
