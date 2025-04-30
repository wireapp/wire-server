{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.MiniBackend
  ( -- * Mini backends
    MiniBackend (..),
    AllErrors,
    MiniBackendEffects,
    interpretFederationStack,
    runFederationStack,
    interpretNoFederationStack,
    interpretNoFederationStackState,
    runNoFederationStack,
    runAllErrorsUnsafe,
    runNoFederationStackUserSubsystemErrorEither,
    runErrorUnsafe,
    miniLocale,
    defaultAuthenticationSubsystemConfig,
    defaultZAuthSettings,

    -- * Mini events
    MiniEvent (..),

    -- * Quickcheck helpers
    NotPendingStoredUser (..),
    NotPendingEmptyIdentityStoredUser (..),
    PendingNotEmptyIdentityStoredUser (..),
    NotPendingSSOIdWithEmailStoredUser (..),
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
import Data.Vector qualified as Vector
import Data.ZAuth.Creation
import Data.ZAuth.CryptoSign
import GHC.Generics
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal
import Polysemy.State
import Polysemy.TinyLog
import Servant.Client.Core
import System.Logger qualified as Log
import Test.QuickCheck
import Type.Reflection
import Wire.API.Allowlists (AllowlistEmailDomains)
import Wire.API.Federation.API
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member hiding (userId)
import Wire.API.User as User hiding (DeleteUser)
import Wire.API.User.Activation (ActivationCode)
import Wire.API.User.IdentityProvider
import Wire.API.User.Password
import Wire.ActivationCodeStore
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Cookie.Limit
import Wire.AuthenticationSubsystem.Interpreter
import Wire.BlockListStore
import Wire.DeleteQueue
import Wire.DeleteQueue.InMemory
import Wire.DomainRegistrationStore qualified as DRS
import Wire.EmailSubsystem (EmailSubsystem)
import Wire.Events
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter as FI
import Wire.FederationConfigStore
import Wire.GalleyAPIAccess
import Wire.HashPassword (HashPassword)
import Wire.IndexedUserStore
import Wire.InternalEvent hiding (DeleteUser)
import Wire.InvitationStore
import Wire.MockInterpreters
import Wire.PasswordResetCodeStore
import Wire.PasswordStore
import Wire.RateLimit
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Metrics
import Wire.Sem.Metrics.IO (ignoreMetrics)
import Wire.Sem.Now hiding (get)
import Wire.Sem.Random (Random)
import Wire.SessionStore (SessionStore)
import Wire.SparAPIAccess
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.Interpreter

newtype PendingNotEmptyIdentityStoredUser = PendingNotEmptyIdentityStoredUser StoredUser
  deriving (Show, Eq)

instance Arbitrary PendingNotEmptyIdentityStoredUser where
  arbitrary = do
    user <- arbitrary `suchThat` \user -> isJust user.identity
    pure $ PendingNotEmptyIdentityStoredUser (user {status = Just PendingInvitation})

newtype NotPendingEmptyIdentityStoredUser = NotPendingEmptyIdentityStoredUser StoredUser
  deriving (Show, Eq)

-- TODO: make sure this is a valid state
instance Arbitrary NotPendingEmptyIdentityStoredUser where
  arbitrary = do
    user <- arbitrary `suchThat` \user -> isNothing user.identity
    notPendingStatus <- elements (Nothing : map Just [Active, Suspended, Ephemeral])
    pure $ NotPendingEmptyIdentityStoredUser (user {status = notPendingStatus})

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
    user <- arbitrary `suchThat` \user -> isJust user.identity
    notPendingStatus <- elements (Nothing : map Just [Active, Suspended, Ephemeral])
    pure $ NotPendingStoredUser (user {status = notPendingStatus})

newtype NotPendingSSOIdWithEmailStoredUser = NotPendingSSOIdWithEmailStoredUser StoredUser
  deriving (Show, Eq)

instance Arbitrary NotPendingSSOIdWithEmailStoredUser where
  arbitrary = do
    user <- arbitrary `suchThat` \user -> fmap isUserSSOId user.ssoId == Just True
    notPendingStatus <- elements (Nothing : map Just [Active, Suspended, Ephemeral])
    e <- arbitrary
    pure $
      NotPendingSSOIdWithEmailStoredUser
        ( user
            { activated = True,
              status = notPendingStatus,
              email = Just e
            }
        )

type AllErrors =
  [ Error UserSubsystemError,
    Error FederationError,
    Error AuthenticationSubsystemError,
    Error RateLimitExceeded
  ]

type MiniBackendEffects = UserSubsystem ': MiniBackendLowerEffects

----------------------------------------------------------------------
-- lower effect interpreters (hierarchically)

data MiniBackendParams r = MiniBackendParams
  { maybeFederationAPIAccess ::
      InterpreterFor
        (FederationAPIAccess MiniFederationMonad)
        (Logger (Log.Msg -> Log.Msg) : Concurrency 'Unsafe : r),
    localBackend :: MiniBackend,
    teams :: Map TeamId [TeamMember],
    galleyConfigs :: AllTeamFeatures,
    cfg :: UserSubsystemConfig
  }

-- | `MiniBackendLowerEffects` is not a long, flat list, but a tree of effects.  This way we
-- can work on a subtree without having to debug the entire vast list.  Makes for much better
-- error messages.
--
-- FUTUREWORK: it'd be nice to have a steeper hierarchy of effects here, and maybe not
-- organize along effect types ("all `State`s"), but the domain ("everything about block
-- lists").
type MiniBackendLowerEffects =
  '[ EmailSubsystem,
     GalleyAPIAccess,
     SparAPIAccess,
     InvitationStore,
     PasswordStore,
     ActivationCodeStore,
     BlockListStore,
     UserStore,
     UserKeyStore,
     IndexedUserStore,
     FederationConfigStore,
     DRS.DomainRegistrationStore,
     PasswordResetCodeStore,
     SessionStore,
     RateLimit,
     HashPassword,
     DeleteQueue,
     Events,
     CryptoSign,
     Random,
     Now
   ]
    `Append` InputEffects
    `Append` '[ Metrics
              ]
    `Append` StateEffects
    `Append` '[ FederationAPIAccess MiniFederationMonad,
                TinyLog,
                Concurrency 'Unsafe
              ]

miniBackendLowerEffectsInterpreters ::
  forall r a.
  MiniBackendParams r ->
  Sem (MiniBackendLowerEffects `Append` r) a ->
  Sem r (MiniBackend, a)
miniBackendLowerEffectsInterpreters mb@(MiniBackendParams {..}) =
  sequentiallyPerformConcurrency
    . noopLogger
    . maybeFederationAPIAccess
    . stateEffectsInterpreters mb
    . ignoreMetrics
    . inputEffectsInterpreters cfg localBackend.teamIdps
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . runRandomPure
    . runCryptoSignUnsafe
    . miniEventInterpreter
    . inMemoryDeleteQueueInterpreter
    . staticHashPasswordInterpreter
    . noRateLimit
    . runInMemorySessionStore
    . runInMemoryPasswordResetCodeStore
    . inMemoryDomainRegistrationStoreInterpreter
    . runFederationConfigStoreInMemory
    . inMemoryIndexedUserStoreInterpreter
    . inMemoryUserKeyStoreInterpreter
    . inMemoryUserStoreInterpreter
    . inMemoryBlockListStoreInterpreter
    . inMemoryActivationCodeStoreInterpreter
    . runInMemoryPasswordStoreInterpreter
    . inMemoryInvitationStoreInterpreter
    . miniSparAPIAccess
    . miniGalleyAPIAccess teams galleyConfigs
    . noopEmailSubsystemInterpreter

type StateEffects =
  '[ State (Map (TeamId, InvitationId) StoredInvitation),
     State (Map InvitationCode StoredInvitation),
     State (Map EmailKey (Maybe UserId, ActivationCode)),
     State [EmailKey],
     State [StoredUser],
     State (Map EmailKey UserId),
     State [DRS.StoredDomainRegistration],
     State [InternalNotification],
     State MiniBackend,
     State [MiniEvent]
   ]

stateEffectsInterpreters :: forall r r' a. MiniBackendParams r' -> Sem (StateEffects `Append` r) a -> Sem r (MiniBackend, a)
stateEffectsInterpreters MiniBackendParams {..} =
  evalState []
    . runState localBackend
    . evalState []
    . evalState []
    . liftUserKeyStoreState
    . liftUserStoreState
    . liftBlockListStoreState
    . liftActivationCodeStoreState
    . liftInvitationInfoStoreState
    . liftInvitationStoreState

type InputEffects =
  '[ Input UserSubsystemConfig,
     Input (Maybe AllowlistEmailDomains),
     Input (Map TeamId IdPList),
     Input AuthenticationSubsystemConfig
   ]

defaultZAuthSettings :: ZAuthSettings
defaultZAuthSettings =
  ZAuthSettings
    1
    (UserTokenTimeout (60 * 60 * 24 * 28)) -- 28 days
    (SessionTokenTimeout (60 * 60 * 24)) -- 1 day
    (AccessTokenTimeout 900) -- 15 minutes
    (ProviderTokenTimeout (60 * 60 * 24 * 7)) -- 7 days
    (LegalHoldUserTokenTimeout (60 * 60 * 24 * 56)) -- 56 days
    (LegalHoldAccessTokenTimeout (60 * 15)) -- 15 minutes

defaultZAuthEnv :: ZAuthEnv
defaultZAuthEnv =
  ZAuthEnv
    { private =
        SigningKey
          { keyIdx = 1,
            key = read "Z-x7AIRMxXYbY2BBan0dFUH0WR_hUqoNF_EJzQ7cSdBrLBirXOBCsdTEKibIJ1WGgeshXkGdYMWh7EMsJ_X9UA=="
          },
      publicKeys = Vector.singleton $ read "aywYq1zgQrHUxComyCdVhoHrIV5BnWDFoexDLCf1_VA=",
      settings = defaultZAuthSettings
    }

defaultAuthenticationSubsystemConfig :: AuthenticationSubsystemConfig
defaultAuthenticationSubsystemConfig =
  AuthenticationSubsystemConfig
    { zauthEnv = defaultZAuthEnv,
      allowlistEmailDomains = Nothing,
      local = defaultLocalDomain,
      userCookieRenewAge = 2,
      userCookieLimit = 5,
      userCookieThrottle = StdDevThrottle 5 3
    }

defaultLocalDomain :: Local ()
defaultLocalDomain = (toLocalUnsafe (Domain "localdomain") ())

inputEffectsInterpreters :: forall r a. UserSubsystemConfig -> Map TeamId IdPList -> Sem (InputEffects `Append` r) a -> Sem r a
inputEffectsInterpreters cfg teamIdps =
  runInputConst defaultAuthenticationSubsystemConfig
    . runInputConst teamIdps
    . runInputConst Nothing
    . runInputConst cfg

----------------------------------------------------------------------

-- | a type representing the state of a single backend
data MiniBackend = MkMiniBackend
  { -- | this is morally the same as the users stored in the actual backend
    --   invariant: for each key, the user.id and the key are the same
    users :: [StoredUser],
    userKeys :: Map EmailKey UserId,
    passwordResetCodes :: Map PasswordResetKey (PRQueryData Identity),
    blockList :: [EmailKey],
    activationCodes :: Map EmailKey (Maybe UserId, ActivationCode),
    invitationInfos :: Map InvitationCode StoredInvitation,
    invitations :: Map (TeamId, InvitationId) StoredInvitation,
    teamIdps :: Map TeamId IdPList
  }
  deriving stock (Eq, Show, Generic)

instance Default MiniBackend where
  def =
    MkMiniBackend
      { users = mempty,
        userKeys = mempty,
        passwordResetCodes = mempty,
        blockList = mempty,
        activationCodes = mempty,
        invitationInfos = mempty,
        invitations = mempty,
        teamIdps = mempty
      }

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
  (:::) :: (Typeable a) => (Component, Text, a) -> SubsystemOperationList -> SubsystemOperationList

infixr 5 :::

lookupSubsystemOperation ::
  (Typeable a) =>
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

runFederationStack ::
  (HasCallStack) =>
  MiniBackend ->
  Map Domain MiniBackend ->
  Map TeamId [TeamMember] ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` AllErrors) a ->
  a
runFederationStack localBackend fedBackends teamMember cfg =
  runAllErrorsUnsafe
    . interpretFederationStack
      localBackend
      fedBackends
      teamMember
      cfg

interpretFederationStack ::
  (HasCallStack, Members AllErrors r) =>
  -- | the local backend
  MiniBackend ->
  -- | the available backends
  Map Domain MiniBackend ->
  Map TeamId [TeamMember] ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r a
interpretFederationStack localBackend remoteBackends teams cfg =
  snd <$$> interpretFederationStackState localBackend remoteBackends teams cfg

interpretFederationStackState ::
  (HasCallStack, Members AllErrors r) =>
  -- | the local backend
  MiniBackend ->
  -- | the available backends
  Map Domain MiniBackend ->
  Map TeamId [TeamMember] ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretFederationStackState localBackend backends teams cfg =
  interpretMaybeFederationStackState
    MiniBackendParams
      { maybeFederationAPIAccess = (miniFederationAPIAccess backends),
        localBackend = localBackend,
        teams = teams,
        galleyConfigs = def,
        cfg = cfg
      }

runNoFederationStack ::
  MiniBackend ->
  Map TeamId [TeamMember] ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` AllErrors) a ->
  a
runNoFederationStack localBackend teams cfg =
  -- (A 'runNoFederationStackEither' variant of this that returns 'AllErrors' in an 'Either'
  -- would be nice, but is complicated by the fact that we not only have 'UserSubsystemErrors',
  -- but other errors as well.  Maybe just wait with this until we have a better idea how we
  -- want to do errors?)
  runAllErrorsUnsafe . interpretNoFederationStack localBackend teams def cfg

runNoFederationStackUserSubsystemErrorEither ::
  MiniBackend ->
  Map TeamId [TeamMember] ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` AllErrors) a ->
  Either UserSubsystemError a
runNoFederationStackUserSubsystemErrorEither localBackend teams cfg =
  run . userSubsystemErrorEitherUnsafe . interpretNoFederationStack localBackend teams def cfg

userSubsystemErrorEitherUnsafe :: Sem AllErrors a -> Sem '[] (Either UserSubsystemError a)
userSubsystemErrorEitherUnsafe = runErrorUnsafe . runErrorUnsafe . runErrorUnsafe . runError

interpretNoFederationStack ::
  (Members AllErrors r) =>
  MiniBackend ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r a
interpretNoFederationStack localBackend teams galleyConfigs cfg =
  snd <$$> interpretNoFederationStackState localBackend teams galleyConfigs cfg

interpretNoFederationStackState ::
  (Members AllErrors r) =>
  MiniBackend ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretNoFederationStackState localBackend teams galleyConfigs cfg =
  interpretMaybeFederationStackState
    MiniBackendParams
      { maybeFederationAPIAccess = emptyFederationAPIAcesss,
        localBackend = localBackend,
        teams = teams,
        galleyConfigs = galleyConfigs,
        cfg = cfg
      }

interpretMaybeFederationStackState ::
  forall r a.
  (Members AllErrors r) =>
  MiniBackendParams r ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretMaybeFederationStackState mb =
  let authSubsystemInterpreter :: InterpreterFor AuthenticationSubsystem (MiniBackendLowerEffects `Append` r)
      authSubsystemInterpreter = interpretAuthenticationSubsystem userSubsystemInterpreter

      userSubsystemInterpreter :: InterpreterFor UserSubsystem (MiniBackendLowerEffects `Append` r)
      userSubsystemInterpreter = runUserSubsystem authSubsystemInterpreter
   in miniBackendLowerEffectsInterpreters mb . userSubsystemInterpreter

liftInvitationInfoStoreState :: (Member (State MiniBackend) r) => Sem (State (Map InvitationCode StoredInvitation) : r) a -> Sem r a
liftInvitationInfoStoreState = interpret \case
  Polysemy.State.Get -> gets (.invitationInfos)
  Put newAcs -> modify $ \b -> b {invitationInfos = newAcs}

liftInvitationStoreState :: (Member (State MiniBackend) r) => Sem (State (Map (TeamId, InvitationId) StoredInvitation) : r) a -> Sem r a
liftInvitationStoreState = interpret \case
  Polysemy.State.Get -> gets (.invitations)
  Put newInvs -> modify $ \b -> b {invitations = newInvs}

liftActivationCodeStoreState :: (Member (State MiniBackend) r) => Sem (State (Map EmailKey (Maybe UserId, ActivationCode)) : r) a -> Sem r a
liftActivationCodeStoreState = interpret \case
  Polysemy.State.Get -> gets (.activationCodes)
  Put newAcs -> modify $ \b -> b {activationCodes = newAcs}

liftBlockListStoreState :: (Member (State MiniBackend) r) => Sem (State [EmailKey] : r) a -> Sem r a
liftBlockListStoreState = interpret $ \case
  Polysemy.State.Get -> gets (.blockList)
  Put newBlockList -> modify $ \b -> b {blockList = newBlockList}

liftUserKeyStoreState :: (Member (State MiniBackend) r) => Sem (State (Map EmailKey UserId) : r) a -> Sem r a
liftUserKeyStoreState = interpret $ \case
  Polysemy.State.Get -> gets (.userKeys)
  Put newUserKeys -> modify $ \b -> b {userKeys = newUserKeys}

liftUserStoreState :: (Member (State MiniBackend) r) => Sem (State [StoredUser] : r) a -> Sem r a
liftUserStoreState = interpret $ \case
  Polysemy.State.Get -> gets (.users)
  Put newUsers -> modify $ \b -> b {users = newUsers}

runAllErrorsUnsafe :: forall a. (HasCallStack) => Sem AllErrors a -> a
runAllErrorsUnsafe = run . runErrorUnsafe . runErrorUnsafe . runErrorUnsafe . runErrorUnsafe

emptyFederationAPIAcesss :: InterpreterFor (FederationAPIAccess MiniFederationMonad) r
emptyFederationAPIAcesss = interpret $ \case
  _ -> error "uninterpreted effect: FederationAPIAccess"

miniFederationAPIAccess ::
  forall a r.
  (HasCallStack) =>
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
