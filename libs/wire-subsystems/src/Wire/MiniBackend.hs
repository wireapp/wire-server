module Wire.MiniBackend
  ( -- * Mini backends
    MiniBackend (..),
    AllErrors,
    MiniBackendEffects,
    interpretFederationStack,
    runFederationStack,
    interpretNoFederationStack,
    runNoFederationStackState,
    interpretNoFederationStackState,
    runNoFederationStack,
    runAllErrorsUnsafe,
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
import Data.Handle (Handle)
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Map.Lazy qualified as LM
import Data.Map.Strict qualified as M
import Data.Misc
import Data.Proxy
import Data.Qualified
import Data.Text.Ascii
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Type.Equality
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
import Wire.API.Federation.API
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.Password
import Wire.API.Team.Feature
import Wire.API.Team.Member hiding (userId)
import Wire.API.User as User hiding (DeleteUser)
import Wire.API.User.Password
import Wire.API.UserEvent
import Wire.AuthenticationSubsystem.Interpreter
import Wire.DeleteQueue
import Wire.DeleteQueue.InMemory
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter as FI
import Wire.GalleyAPIAccess
import Wire.HashPassword
import Wire.InternalEvent hiding (DeleteUser)
import Wire.PasswordResetCodeStore
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Now hiding (get)
import Wire.SessionStore
import Wire.StoredUser
import Wire.UserEvents
import Wire.UserKeyStore
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
    user <- arbitrary `suchThat` \user -> isJust user.identity
    notPendingStatus <- elements (Nothing : map Just [Active, Suspended, Deleted, Ephemeral])
    pure $ NotPendingStoredUser (user {status = notPendingStatus})

type AllErrors =
  [ Error UserSubsystemError,
    Error AuthenticationSubsystemError,
    Error FederationError
  ]

-- TODO: This probably doesn't need password reset stuff
type MiniBackendEffects =
  [ UserSubsystem,
    GalleyAPIAccess,
    UserStore,
    UserKeyStore,
    PasswordResetCodeStore,
    HashPassword,
    SessionStore,
    DeleteQueue,
    UserEvents,
    State [InternalNotification],
    State MiniBackend,
    State [MiniEvent],
    Now,
    Input UserSubsystemConfig,
    Input (Local ()),
    FederationAPIAccess MiniFederationMonad,
    TinyLog,
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
    users :: [StoredUser],
    userKeys :: Map UserKey UserId,
    passwordResetCodes :: Map PasswordResetKey (PRQueryData Identity)
  }

instance Default MiniBackend where
  def =
    MkMiniBackend
      { users = mempty,
        userKeys = mempty,
        passwordResetCodes = mempty
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

interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

noOpLogger ::
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
noOpLogger = interpret $ \case
  Log _lvl _msg -> pure ()

runFederationStack ::
  (HasCallStack) =>
  MiniBackend ->
  Map Domain MiniBackend ->
  Maybe TeamMember ->
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
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r a
interpretFederationStack localBackend remoteBackends teamMember cfg =
  snd <$$> interpretFederationStackState localBackend remoteBackends teamMember cfg

interpretFederationStackState ::
  (HasCallStack, Members AllErrors r) =>
  -- | the local backend
  MiniBackend ->
  -- | the available backends
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretFederationStackState localBackend backends teamMember =
  interpretMaybeFederationStackState (miniFederationAPIAccess backends) localBackend teamMember def

runNoFederationStack ::
  MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` AllErrors) a ->
  a
runNoFederationStack localBackend teamMember cfg =
  -- (A 'runNoFederationStackEither' variant of this that returns 'AllErrors' in an 'Either'
  -- would be nice, but is complicated by the fact that we not only have 'UserSubsystemErrors',
  -- but other errors as well.  Maybe just wait with this until we have a better idea how we
  -- want to do errors?)
  runAllErrorsUnsafe . interpretNoFederationStack localBackend teamMember def cfg

runNoFederationStackState ::
  (HasCallStack) =>
  MiniBackend ->
  Maybe TeamMember ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` AllErrors) a ->
  (MiniBackend, a)
runNoFederationStackState localBackend teamMember cfg =
  runAllErrorsUnsafe . interpretNoFederationStackState localBackend teamMember def cfg

interpretNoFederationStack ::
  (Members AllErrors r) =>
  MiniBackend ->
  Maybe TeamMember ->
  AllFeatureConfigs ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r a
interpretNoFederationStack localBackend teamMember galleyConfigs cfg =
  snd <$$> interpretNoFederationStackState localBackend teamMember galleyConfigs cfg

interpretNoFederationStackState ::
  (Members AllErrors r) =>
  MiniBackend ->
  Maybe TeamMember ->
  AllFeatureConfigs ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretNoFederationStackState = interpretMaybeFederationStackState emptyFederationAPIAcesss

interpretMaybeFederationStackState ::
  (Members AllErrors r) =>
  InterpreterFor (FederationAPIAccess MiniFederationMonad) (Logger (Log.Msg -> Log.Msg) : Concurrency 'Unsafe : r) ->
  MiniBackend ->
  Maybe TeamMember ->
  AllFeatureConfigs ->
  UserSubsystemConfig ->
  Sem (MiniBackendEffects `Append` r) a ->
  Sem r (MiniBackend, a)
interpretMaybeFederationStackState maybeFederationAPIAccess localBackend teamMember galleyConfigs cfg =
  sequentiallyPerformConcurrency
    . noOpLogger
    . maybeFederationAPIAccess
    . runInputConst (toLocalUnsafe (Domain "localdomain") ())
    . runInputConst cfg
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . runState localBackend
    . evalState []
    . miniEventInterpreter
    . inMemoryDeleteQueueInterpreter
    . staticSessionStoreInterpreter
    . staticHashPasswordInterpreter
    . staticPasswordResetCodeStore
    . staticUserKeyStoreInterpreter
    . staticUserStoreInterpreter
    . miniGalleyAPIAccess teamMember galleyConfigs
    . runUserSubsystem cfg

runErrorUnsafe :: (HasCallStack, Exception e) => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

runAllErrorsUnsafe :: forall a. (HasCallStack) => Sem AllErrors a -> a
runAllErrorsUnsafe = run . runErrorUnsafe . runErrorUnsafe . runErrorUnsafe

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

getLocalUsers :: (Member (State MiniBackend) r) => Sem r [StoredUser]
getLocalUsers = gets (.users)

modifyLocalUsers ::
  (Member (State MiniBackend) r) =>
  ([StoredUser] -> Sem r [StoredUser]) ->
  Sem r ()
modifyLocalUsers f = do
  us <- gets (.users)
  us' <- f us
  modify $ \b -> b {users = us'}

staticPasswordResetCodeStore ::
  forall r.
  (Member (State MiniBackend) r) =>
  InterpreterFor PasswordResetCodeStore r
staticPasswordResetCodeStore = interpret \case
  GenerateEmailCode ->
    pure . PasswordResetCode . encodeBase64Url $ "email-code"
  GeneratePhoneCode -> (error "deprecated")
  CodeSelect resetKey -> do
    codes <- gets (.passwordResetCodes)
    pure $ mapPRQueryData (Just . runIdentity) <$> M.lookup resetKey codes
  CodeInsert resetKey queryData _ttl ->
    modify $ \b -> b {passwordResetCodes = M.insert resetKey queryData (passwordResetCodes b)}
  CodeDelete resetKey ->
    modify $ \b -> b {passwordResetCodes = M.delete resetKey (passwordResetCodes b)}

staticUserStoreInterpreter ::
  forall r.
  (Member (State MiniBackend) r) =>
  InterpreterFor UserStore r
staticUserStoreInterpreter = interpret $ \case
  GetUser uid -> find (\user -> user.id == uid) <$> getLocalUsers
  UpdateUser uid update -> modifyLocalUsers (pure . fmap doUpdate)
    where
      doUpdate :: StoredUser -> StoredUser
      doUpdate u =
        if u.id == uid
          then
            maybe Imports.id setStoredUserAccentId update.accentId
              . maybe Imports.id setStoredUserAssets update.assets
              . maybe Imports.id setStoredUserPict update.pict
              . maybe Imports.id setStoredUserName update.name
              . maybe Imports.id setStoredUserLocale update.locale
              . maybe Imports.id setStoredUserSupportedProtocols update.supportedProtocols
              $ u
          else u
  UpdateUserHandleEither uid hUpdate -> runError $ modifyLocalUsers (traverse doUpdate)
    where
      doUpdate :: StoredUser -> Sem (Error StoredUserUpdateError : r) StoredUser
      doUpdate u
        | u.id == uid = do
            handles <- mapMaybe (.handle) <$> gets (.users)
            when
              ( hUpdate.old /= Just hUpdate.new
                  && elem hUpdate.new handles
              )
              $ throw StoredUserUpdateHandleExists
            pure $ setStoredUserHandle hUpdate.new u
      doUpdate u = pure u
  DeleteUser user -> modifyLocalUsers $ \us ->
    pure $ filter (\u -> u.id /= User.userId user) us
  LookupHandle h -> miniBackendLookupHandle h
  GlimpseHandle h -> miniBackendLookupHandle h
  LookupStatus uid -> miniBackendLookupStatus uid
  IsActivated uid -> miniBackendIsActivated uid

miniBackendIsActivated :: (Member (State MiniBackend) r) => UserId -> Sem r Bool
miniBackendIsActivated uid = do
  users <- gets (.users)
  pure $ maybe False (.activated) (find ((== uid) . (.id)) users)

miniBackendLookupStatus :: (Member (State MiniBackend) r) => UserId -> Sem r (Maybe AccountStatus)
miniBackendLookupStatus uid = do
  users <- gets (.users)
  pure $ (.status) =<< (find ((== uid) . (.id)) users)

miniBackendLookupHandle ::
  (Member (State MiniBackend) r) =>
  Handle ->
  Sem r (Maybe UserId)
miniBackendLookupHandle h = do
  users <- gets (.users)
  pure $ fmap (.id) (find ((== Just h) . (.handle)) users)

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
  (Member (State [MiniEvent]) r) =>
  InterpreterFor UserEvents r
miniEventInterpreter = interpret \case
  GenerateUserEvent uid _mconn e -> modify (MkMiniEvent uid e :)

staticUserKeyStoreInterpreter ::
  (Member (State MiniBackend) r) =>
  InterpreterFor UserKeyStore r
staticUserKeyStoreInterpreter = interpret $ \case
  LookupKey key -> do
    keys <- gets (.userKeys)
    pure $ M.lookup key keys
  InsertKey uid key ->
    modify $ \b -> b {userKeys = M.insert key uid (userKeys b)}
  DeleteKey key ->
    modify $ \b -> b {userKeys = M.delete key (userKeys b)}
  DeleteKeyForUser uid key ->
    modify $ \b -> b {userKeys = M.filterWithKey (\k u -> k /= key && u /= uid) (userKeys b)}
  ClaimKey key uid -> do
    keys <- gets (.userKeys)
    let free = M.notMember key keys || M.lookup key keys == (Just uid)
    when free $
      modify $
        \b -> b {userKeys = M.insert key uid (userKeys b)}
    pure free
  KeyAvailable key uid -> do
    keys <- gets (.userKeys)
    pure $ M.notMember key keys || M.lookup key keys == uid

staticSessionStoreInterpreter ::
  InterpreterFor SessionStore r
staticSessionStoreInterpreter = interpret $ \case
  InsertCookie uid cookie ttl -> (error "implement on demand") uid cookie ttl
  LookupCookie uid time cid -> (error "implement on demand") uid time cid
  ListCookies uid -> (error "implement on demand") uid
  DeleteAllCookies _ -> pure ()
  DeleteCookies uid cc -> (error "implement on demand") uid cc

staticHashPasswordInterpreter :: InterpreterFor HashPassword r
staticHashPasswordInterpreter = interpret $ \case
  HashPasswordScrypt password -> go hashPasswordScryptWithSalt password
  HashPasswordArgon2id password -> go hashPasswordScryptWithSalt password
  where
    go alg password = do
      let passwordBS = Text.encodeUtf8 (fromPlainTextPassword password)
      pure $ unsafeMkPassword $ alg "salt" passwordBS
