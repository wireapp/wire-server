{-# OPTIONS -Wwarn #-}

module Wire.MiniBackend
  ( -- * Mini backends
    MiniBackend (..),
    interpretFederationStack,
    runFederationStack,
    interpretNoFederationStack,
    runNoFederationStack,
    runErrorUnsafe,
    miniLocale,

    -- * Mini events
    MiniEvent (..),

    -- * Quickcheck helpers
    NotPendingStoredUser (..),
    PendingStoredUser (..),
  )
where

import Data.Default (Default (def))
import Data.Domain
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map.Lazy qualified as LM
import Data.Map.Strict qualified as M
import Data.Proxy
import Data.Qualified
import Data.Time
import Data.Type.Equality
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State
import Servant.Client.Core
import Test.QuickCheck
import Type.Reflection
import Wire.API.Federation.API
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.User hiding (DeleteUser)
import Wire.API.UserEvent
import Wire.DeleteQueue
import Wire.DeleteQueue.InMemory
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter as FI
import Wire.GalleyAPIAccess
import Wire.InternalEvent
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Now hiding (get)
import Wire.StoredUser
import Wire.UserEvents
import Wire.UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Interpreter

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

type AllErrors =
  [ Error UserSubsystemError,
    Error FederationError
  ]

type GetUserProfileEffects =
  [ UserSubsystem,
    GalleyAPIAccess,
    UserStore,
    DeleteQueue,
    UserEvents,
    State [InternalNotification],
    State MiniBackend,
    State [MiniEvent],
    Now,
    Input UserSubsystemConfig,
    FederationAPIAccess MiniFederationMonad,
    Concurrency 'Unsafe
  ]

data MiniEvent = MkMiniEvent
  { userId :: UserId,
    event :: UserEvent
  }
  deriving stock (Eq, Show)

-- | a type representing the state of a single backend
data MiniBackend = MkMiniBackend
  { -- | this is morally the same as the users stored in the actual backend
    --   invariant: for each key, the user.id and the key are the same
    users :: [StoredUser]
  }

instance Default MiniBackend where
  def = MkMiniBackend {users = mempty}

-- | represents an entire federated, stateful world of backends
newtype MiniFederation = MkMiniFederation
  { -- | represents the state of the backends, mapped from their domains
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

data SubsystemOperationList where
  TNil :: SubsystemOperationList
  (:::) :: Typeable a => (Component, Text, a) -> SubsystemOperationList -> SubsystemOperationList

infixr 5 :::

lookupSubsystemOperation ::
  Typeable a =>
  -- | The type to compare to
  (Component, Text, Proxy a) ->
  -- | what to return when none of the types match
  a ->
  -- | the types to try
  SubsystemOperationList ->
  a
lookupSubsystemOperation goal@(goalComp, goalRoute, Proxy @goalType) a = \case
  TNil -> a
  (comp, route, client) ::: xs -> case eqTypeRep (typeRep @goalType) (typeOf client) of
    Just HRefl | comp == goalComp && route == goalRoute -> client
    _ -> lookupSubsystemOperation goal a xs

instance FederationMonad MiniFederationMonad where
  fedClientWithProxy (Proxy @name) (Proxy @api) (_ :: Proxy (MiniFederationMonad comp)) =
    lookupSubsystemOperation
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
      (\u -> mkUserProfileWithEmail Nothing (mkUserFromStored dom miniLocale u) defUserLegalHoldStatus)
      users

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

interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

runFederationStack ::
  MiniBackend ->
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (GetUserProfileEffects `Append` AllErrors) a ->
  a
runFederationStack localBackend fedBackends teamMember cfg =
  runAllErrorsUnsafe
    . interpretFederationStack
      localBackend
      fedBackends
      teamMember
      cfg

interpretFederationStack ::
  (Members AllErrors r) =>
  -- | the local backend
  MiniBackend ->
  -- | the available backends
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (GetUserProfileEffects `Append` r) a ->
  Sem r a
interpretFederationStack localBackend backends teamMember cfg =
  sequentiallyPerformConcurrency
    . miniFederationAPIAccess backends
    . runInputConst cfg
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . evalState localBackend
    . evalState []
    . miniEventInterpreter
    . inMemoryDeleteQueueInterpreter
    . staticUserStoreInterpreter
    . miniGalleyAPIAccess teamMember def
    . runUserSubsystem cfg

runNoFederationStack ::
  MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (GetUserProfileEffects `Append` AllErrors) a ->
  a
runNoFederationStack localBackend teamMember cfg =
  runAllErrorsUnsafe . interpretNoFederationStack localBackend teamMember def cfg

interpretNoFederationStack ::
  (Members AllErrors r) =>
  MiniBackend ->
  Maybe TeamMember ->
  AllFeatureConfigs ->
  UserSubsystemConfig ->
  Sem (GetUserProfileEffects `Append` r) a ->
  Sem r a
interpretNoFederationStack localBackend teamMember galleyConfigs cfg =
  sequentiallyPerformConcurrency
    . emptyFederationAPIAcesss
    . runInputConst cfg
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . evalState localBackend
    . evalState []
    . miniEventInterpreter
    . inMemoryDeleteQueueInterpreter
    . staticUserStoreInterpreter
    . miniGalleyAPIAccess teamMember galleyConfigs
    . runUserSubsystem cfg

runErrorUnsafe :: Exception e => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

runAllErrorsUnsafe :: forall a. Sem AllErrors a -> a
runAllErrorsUnsafe = run . runErrorUnsafe . runErrorUnsafe

emptyFederationAPIAcesss :: InterpreterFor (FederationAPIAccess MiniFederationMonad) r
emptyFederationAPIAcesss = interpret $ \case
  _ -> error "uninterpreted effect: FederationAPIAccess"

miniFederationAPIAccess ::
  forall a r.
  Map Domain MiniBackend ->
  Sem (FederationAPIAccess MiniFederationMonad : r) a ->
  Sem r a
miniFederationAPIAccess online = do
  let runner :: FederatedActionRunner MiniFederationMonad r
      runner domain rpc = pure . Right $ runMiniFederation domain online rpc
  interpret \case
    RunFederatedEither remote rpc ->
      if isJust (M.lookup (qDomain $ tUntagged remote) online)
        then FI.runFederatedEither runner remote rpc
        else pure $ Left do FederationUnexpectedError "RunFederatedEither"
    RunFederatedConcurrently _remotes _rpc -> error "unimplemented: RunFederatedConcurrently"
    RunFederatedBucketed _domain _rpc -> error "unimplemented: RunFederatedBucketed"
    IsFederationConfigured -> pure True

getLocalUsers :: Member (State MiniBackend) r => Sem r [StoredUser]
getLocalUsers = gets (.users)

modifyLocalUsers ::
  Member (State MiniBackend) r =>
  ([StoredUser] -> [StoredUser]) ->
  Sem r ()
modifyLocalUsers f = modify $ \b -> b {users = f b.users}

staticUserStoreInterpreter ::
  Member (State MiniBackend) r =>
  InterpreterFor UserStore r
staticUserStoreInterpreter = interpret $ \case
  GetUser uid -> find (\user -> user.id == uid) <$> getLocalUsers
  UpdateUser uid update -> modifyLocalUsers (map doUpdate)
    where
      doUpdate :: StoredUser -> StoredUser
      doUpdate u
        | u.id == uid =
            maybe Imports.id setStoredUserAccentId update.accentId
              . maybe Imports.id setStoredUserAssets update.assets
              . maybe Imports.id setStoredUserPict update.pict
              . maybe Imports.id (setStoredUserName . (.value)) update.name
              $ u
      doUpdate u = u

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Maybe TeamMember ->
  -- | what to return when calling GetAllFeatureConfigsForUser
  AllFeatureConfigs ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess member configs = interpret $ \case
  GetTeamMember _ _ -> pure member
  GetAllFeatureConfigsForUser _ -> pure configs
  _ -> error "uninterpreted effect: GalleyAPIAccess"

miniEventInterpreter ::
  Member (State [MiniEvent]) r =>
  InterpreterFor UserEvents r
miniEventInterpreter = interpret \case
  GenerateUserEvent uid _mconn e -> modify (MkMiniEvent uid e :)
