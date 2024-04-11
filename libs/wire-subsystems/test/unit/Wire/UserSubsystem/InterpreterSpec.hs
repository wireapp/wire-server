{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Data.Default (Default (def))
import Data.Domain
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Data.Map.Lazy qualified as LM
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Qualified
import Data.Set qualified as S
import Data.Type.Equality
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Servant.Client.Core
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Type.Reflection
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
    prop
      "all users on federating backends"
      \viewer targetUsers visibility domain remoteDomain -> do
        let localBackend = def {users = [viewer]}
            remoteBackend = def {users = targetUsers}
            federation = [(domain, localBackend), (remoteDomain, remoteBackend)]
            retrievedProfiles =
              runFederationStack federation Nothing (UserSubsystemConfig visibility domain miniLocale) $
                getUserProfiles
                  (toLocalUnsafe domain viewer.id)
                  ( map (flip Qualified remoteDomain . (.id)) $
                      S.toList targetUsers
                  )
        remoteDomain /= domain ==>
          retrievedProfiles
            === [ mkUserProfileWithEmail
                    Nothing
                    UserLegalHoldDisabled
                    (mkUserFromStored remoteDomain miniLocale targetUser)
                  | targetUser <- S.toList targetUsers
                ]
    prop "returns nothing when the none of the users exist" $
      \viewer targetUserIds visibility domain locale ->
        let config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [] Nothing config $
                getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
         in retrievedProfile === []

    prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
      \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
         in retrievedProfile
              === [ mkUserProfile
                      (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                      UserLegalHoldDisabled
                      (mkUserFromStored domain locale targetUser)
                  ]
    prop "gets Nothing when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
      \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
        let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
            targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
         in retrievedProfile
              === [ mkUserProfile
                      (fmap (const Nothing) visibility)
                      UserLegalHoldDisabled
                      (mkUserFromStored domain locale targetUser)
                  ]

    prop "returns Nothing if the target user has not accepted their invitation yet" $
      \viewer (PendingStoredUser targetUser) visibility domain locale ->
        let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
            config = UserSubsystemConfig visibility domain locale
            retrievedProfile =
              runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                getLocalUserProfiles viewer.id [targetUser.id]
         in retrievedProfile === []

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
    (FederationAPIAccess MiniFederationMonad),
    (Concurrency 'Unsafe)
  ]

-- | a type representing the state of a single backend
data MiniBackend = MkMiniBackend
  { -- | this is morally the same as the users stored in the actual backend
    --   invariant: for each key, the user.id and the key are the same
    users :: Set StoredUser
  }

instance Default MiniBackend where
  def = MkMiniBackend {users = mempty}

-- | represents an entire federated, stateful world of backends
newtype MiniFederation = MkMiniFederation
  { -- | represens the state of the backends, mapped from their domains
    backends :: Map Domain MiniBackend
  }

data MiniContext = MkMiniContext
  { -- | the domain that is receiving the request
    ownDomain :: Domain
  }

newtype MiniFederationMonad comp a = MkMiniFederationMonad
  {unMiniFederation :: Sem [Input MiniContext, State MiniFederation] a}
  deriving newtype (Functor, Applicative, Monad)

instance RunClient (MiniFederationMonad comp) where
  runRequestAcceptStatus _acceptableStatuses _req = error "MiniFederation does not support servant client"
  throwClientError _err = error "MiniFederation does not support servant client"

data TypeableTypes where
  TNil :: TypeableTypes
  (:::) :: Typeable a => (Component, Text, a) -> TypeableTypes -> TypeableTypes

infixr 5 :::

tryAll ::
  Typeable a =>
  -- | The type to compare to
  (Component, Text, Proxy a) ->
  -- | what to return when none of the types match
  a ->
  -- | the types to try
  TypeableTypes ->
  a
tryAll goal@(goalComp, goalRoute, Proxy @goalType) a = \case
  TNil -> a
  (comp, route, client) ::: xs -> case eqTypeRep (typeRep @goalType) (typeOf client) of
    Just HRefl | comp == goalComp && route == goalRoute -> client
    _ -> tryAll goal a xs

instance FederationMonad MiniFederationMonad where
  fedClientWithProxy (Proxy @name) (Proxy @api) (_ :: Proxy (MiniFederationMonad comp)) =
    -- TODO(mangoiv): checking the type of the Client alone is not enough; we also want to check that
    -- the route is correct; this has yet to be implemented by storing an existential of the route
    -- or, more easily a `Text` that represents it
    tryAll
      (componentVal @comp, nameVal @name, Proxy @(Client (MiniFederationMonad comp) api))
      do
        error
          "The testsuite has evaluated a tuple of component, route and client that is\
          \ not covered by the MiniFederation implementation of FederationMonad"
      do
        (Brig, "get-users-by-ids", miniGetUsersByIds)
          ::: (Brig, "get-user-by-id", miniGetUserById)
          ::: TNil

miniLocale :: Locale
miniLocale =
  Locale
    { lLanguage = Language EN,
      lCountry = Nothing
    }

-- | runs a stateful backend, returns the state and puts it back into the
--   federated global state
runOnOwnBackend :: Sem '[Input Domain, State MiniBackend] a -> MiniFederationMonad comp a
runOnOwnBackend = MkMiniFederationMonad . runOnOwnBackend' . subsume_

-- | Runs action in the context of a single backend, without access to others.
runOnOwnBackend' ::
  (Member (Input MiniContext) r, Member (State MiniFederation) r) =>
  Sem (Input Domain ': State MiniBackend ': r) a ->
  Sem r a
runOnOwnBackend' act = do
  ownDomain <- inputs (.ownDomain)
  ownBackend <-
    fromMaybe (error "tried to lookup domain that is not part of the backends' state")
      <$> gets (M.lookup ownDomain . backends)
  (newBackend, res) <- runState ownBackend $ runInputConst ownDomain act
  modify (\minifed -> minifed {backends = M.insert ownDomain newBackend (minifed.backends)})
  pure res

miniGetAllProfiles ::
  (Member (Input Domain) r, Member (State MiniBackend) r) =>
  Sem r [UserProfile]
miniGetAllProfiles = do
  users <- gets (.users)
  dom <- input
  pure $
    map
      (mkUserProfileWithEmail Nothing UserLegalHoldDisabled . mkUserFromStored dom miniLocale)
      (S.toList users)

miniGetUsersByIds :: [UserId] -> MiniFederationMonad 'Brig [UserProfile]
miniGetUsersByIds userIds = runOnOwnBackend do
  usersById :: LM.Map UserId UserProfile <-
    M.fromList . map (\user -> (user.profileQualifiedId.qUnqualified, user)) <$> miniGetAllProfiles
  pure $ mapMaybe (flip M.lookup usersById) userIds

miniGetUserById :: UserId -> MiniFederationMonad 'Brig (Maybe UserProfile)
miniGetUserById uid =
  runOnOwnBackend $
    find (\u -> u.profileQualifiedId.qUnqualified == uid) <$> miniGetAllProfiles

runMiniFederation :: Domain -> Map Domain MiniBackend -> MiniFederationMonad c a -> a
runMiniFederation ownDomain backends =
  run
    . evalState MkMiniFederation {backends = backends}
    . runInputConst MkMiniContext {ownDomain = ownDomain}
    . unMiniFederation

runFederationStack ::
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem GetUserProfileEffects a ->
  a
runFederationStack fedBackends teamMember config =
  run
    . sequentiallyPerformConcurrency
    . miniFederationAPIAccess fedBackends
    . runErrorUnsafe
    . runInputConst config
    . staticUserStoreInterpreter do
      maybe [] (S.toList . (.users)) $ M.lookup (config.localDomain) fedBackends
    . miniGalleyAPIAccess teamMember

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
    . miniGalleyAPIAccess teamMember

runErrorUnsafe :: Exception e => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

emptyFederationAPIAcesss :: InterpreterFor (FederationAPIAccess MiniFederationMonad) r
emptyFederationAPIAcesss = interpret $ \case
  _ -> error "uninterpreted effect: FederationAPIAccess"

miniFederationAPIAccess :: forall r. Member (Concurrency 'Unsafe) r => Map Domain MiniBackend -> InterpreterFor (FederationAPIAccess MiniFederationMonad) r
miniFederationAPIAccess userProfiles =
  let runner :: FederatedActionRunner MiniFederationMonad r
      runner domain rpc = pure . Right $ runMiniFederation domain userProfiles rpc
   in interpretFederationAPIAccessGeneral runner (pure True)

staticUserStoreInterpreter :: [StoredUser] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> user.id == uid) allUsers

miniGalleyAPIAccess :: Maybe TeamMember -> InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess member = interpret $ \case
  GetTeamMember _ _ -> pure member
  _ -> error "uninterpreted effect: GalleyAPIAccess"
