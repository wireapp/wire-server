{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Control.Exception (throw)
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
import Data.Time
import Data.Type.Equality
import Imports
import Polysemy
import Polysemy.Error hiding (throw)
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
import Wire.API.User hiding (DeleteUser)
import Wire.DeleteQueue
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter
import Wire.GalleyAPIAccess
import Wire.InternalEvent
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.Sequential
import Wire.Sem.Now
import Wire.StoredUser
import Wire.UserStore
import Wire.UserSubsystem qualified as QS
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfiles" do
    describe "[with federation]" do
      prop "all users on federating backends" $
        \viewer targetUsers visibility domain remoteDomain -> do
          let remoteBackend = def {users = targetUsers}
              federation = [(remoteDomain, remoteBackend)]
              retrievedProfiles =
                runFederationStack [viewer] federation Nothing $
                  runUserSubsystem (UserSubsystemConfig visibility miniLocale) $
                    QS.getUserProfiles
                      (toLocalUnsafe domain viewer.id)
                      ( map (flip Qualified remoteDomain . (.id)) $
                          S.toList targetUsers
                      )
          remoteDomain /= domain ==>
            retrievedProfiles
              === [ mkUserProfileWithEmail
                      Nothing
                      (mkUserFromStored remoteDomain miniLocale targetUser)
                      UserLegalHoldDisabled
                    | targetUser <- S.toList targetUsers
                  ]

    describe "[without federation]" do
      prop "returns nothing when none of the users exist" $
        \viewer targetUserIds visibility domain locale ->
          let config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack [] Nothing $
                  runUserSubsystem config $
                    QS.getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
           in retrievedProfiles === []

      prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
        \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack [targetUser, viewer] (Just teamMember) $
                  runUserSubsystem config $
                    QS.getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfiles
                === [ mkUserProfile
                        (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                        (mkUserFromStored domain locale targetUser)
                        UserLegalHoldDisabled
                    ]

      prop "gets a local user profile when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
        \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              retrievedProfile =
                runNoFederationStack [targetUser, viewer] (Just teamMember) $
                  runUserSubsystem config $
                    QS.getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfile
                === [ mkUserProfile
                        (fmap (const Nothing) visibility)
                        (mkUserFromStored domain locale targetUser)
                        UserLegalHoldDisabled
                    ]

      prop "returns Nothing if the target user has not accepted their invitation yet" $
        \viewer (PendingStoredUser targetUser) visibility domain locale ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              config = UserSubsystemConfig visibility locale
              retrievedProfile =
                runNoFederationStack [targetUser, viewer] (Just teamMember) $
                  runUserSubsystem config $
                    QS.getLocalUserProfiles (toLocalUnsafe domain [targetUser.id])
           in retrievedProfile === []

      prop "gets a remote user profile when the user exists and both user and viewer have accepted their invitations" $
        pending

  describe "getUserProfilesWithErrors" $ do
    prop "If no errors, same behavior as getUserProfiles" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack [viewer] federation Nothing $
                runErrorUnsafe $
                  runUserSubsystem (UserSubsystemConfig visibility miniLocale) $
                    QS.getUserProfilesWithErrors
                      (toLocalUnsafe domain viewer.id)
                      ( map (flip Qualified remoteDomain . (.id)) $
                          S.toList targetUsers
                      )
            retrievedProfiles :: [UserProfile] =
              runFederationStack [viewer] federation Nothing $
                runUserSubsystem (UserSubsystemConfig visibility miniLocale) $
                  QS.getUserProfiles
                    (toLocalUnsafe domain viewer.id)
                    ( map (flip Qualified remoteDomain . (.id)) $
                        S.toList targetUsers
                    )
        remoteDomain /= domain ==>
          length (fst retrievedProfilesWithErrors) === 0
            .&&. snd retrievedProfilesWithErrors === retrievedProfiles
            .&&. length (snd retrievedProfilesWithErrors) === length targetUsers

    prop "If we get errors, the non-failing user profiles are returned together with errors for failing ones" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStackFails [viewer] federation Nothing $
                runUserSubsystem (UserSubsystemConfig visibility miniLocale) $
                  QS.getUserProfilesWithErrors
                    (toLocalUnsafe domain viewer.id)
                    ( map (flip Qualified remoteDomain . (.id)) $
                        S.toList targetUsers
                    )
            retrievedProfiles :: [UserProfile] =
              runFederationStack [viewer] federation Nothing $
                runErrorUnsafe $
                  runUserSubsystem (UserSubsystemConfig visibility miniLocale) $
                    QS.getUserProfiles
                      (toLocalUnsafe domain viewer.id)
                      ( map (flip Qualified remoteDomain . (.id)) $
                          S.toList targetUsers
                      )
        remoteDomain /= domain ==>
          length (fst retrievedProfilesWithErrors) === length targetUsers
            .&&. snd retrievedProfilesWithErrors === retrievedProfiles
            .&&. length (snd retrievedProfilesWithErrors) === 0

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
    DeleteQueue,
    State [InternalNotification],
    Now,
    FederationAPIAccess MiniFederationMonad,
    Concurrency 'Unsafe,
    Error FederationError
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

data SubsystemOperationList where
  TNil :: SubsystemOperationList
  (:::) :: Typeable a => (Component, Text, a) -> SubsystemOperationList -> SubsystemOperationList

infixr 5 :::

{- ORMOLU_DISABLE -}
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
    -- TODO(mangoiv): checking the type of the Client alone is not enough; we also want to check that
    -- the route is correct; this has yet to be implemented by storing an existential of the route
    -- or, more easily a `Text` that represents it
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
{- ORMOLU_ENABLE -}

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
      (\u -> mkUserProfileWithEmail Nothing (mkUserFromStored dom miniLocale u) UserLegalHoldDisabled)
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

interpretNowConst ::
  UTCTime ->
  Sem (Now : r) a ->
  Sem r a
interpretNowConst time = interpret \case
  Wire.Sem.Now.Get -> pure time

runFederationStack ::
  [StoredUser] ->
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  Sem GetUserProfileEffects a ->
  a
runFederationStack allLocalUsers fedBackends teamMember =
  run
    . runErrorUnsafe
    . sequentiallyPerformConcurrency
    . miniFederationAPIAccess fedBackends
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . inMemoryDeleteQueueInterpreter
    . staticUserStoreInterpreter allLocalUsers
    . miniGalleyAPIAccess teamMember

runFederationStackFails ::
  [StoredUser] ->
  Map Domain MiniBackend ->
  Maybe TeamMember ->
  Sem GetUserProfileEffects a ->
  a
runFederationStackFails allLocalUsers fedBackends teamMember =
  run
    . runErrorRethrow
    . sequentiallyPerformConcurrency
    . miniFederationAPIAccessFails fedBackends
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . inMemoryDeleteQueueInterpreter
    . staticUserStoreInterpreter allLocalUsers
    . miniGalleyAPIAccess teamMember

runNoFederationStack ::
  [StoredUser] ->
  Maybe TeamMember ->
  Sem GetUserProfileEffects a ->
  a
runNoFederationStack allUsers teamMember =
  run
    . runErrorUnsafe
    . sequentiallyPerformConcurrency
    . emptyFederationAPIAcesss
    . interpretNowConst (UTCTime (ModifiedJulianDay 0) 0)
    . evalState []
    . inMemoryDeleteQueueInterpreter
    . staticUserStoreInterpreter allUsers
    . miniGalleyAPIAccess teamMember

inMemoryDeleteQueueInterpreter :: Member (State [InternalNotification]) r => InterpreterFor DeleteQueue r
inMemoryDeleteQueueInterpreter = interpret $ \case
  EnqueueUserDeletion uid -> modify (\l -> DeleteUser uid : l)
  EnqueueClientDeletion cid uid mConnId -> modify (\l -> DeleteClient cid uid mConnId : l)
  EnqueueServiceDeletion pid sid -> modify (\l -> DeleteService pid sid : l)

runErrorRethrow :: Exception e => InterpreterFor (Error e) r
runErrorRethrow action = do
  res <- runError action
  case res of
    Left e -> throw e
    Right x -> pure x

runErrorUnsafe :: Exception e => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

emptyFederationAPIAcesss :: InterpreterFor (FederationAPIAccess MiniFederationMonad) r
emptyFederationAPIAcesss = interpret $ \case
  _ -> error "uninterpreted effect: FederationAPIAccess"

miniFederationAPIAccess ::
  forall r.
  ( Member (Concurrency 'Unsafe) r
  ) =>
  Map Domain MiniBackend ->
  InterpreterFor (FederationAPIAccess MiniFederationMonad) r
miniFederationAPIAccess userProfiles =
  let runner :: FederatedActionRunner MiniFederationMonad r
      runner domain rpc = pure . Right $ runMiniFederation domain userProfiles rpc
   in interpretFederationAPIAccessGeneral runner (pure True)

miniFederationAPIAccessFails ::
  forall r.
  (Member (Error FederationError) r) =>
  Map Domain MiniBackend ->
  InterpreterFor (FederationAPIAccess MiniFederationMonad) r
miniFederationAPIAccessFails userProfiles =
  let runner :: FederatedActionRunner MiniFederationMonad r
      runner domain rpc = pure . Right $ runMiniFederation domain userProfiles rpc
   in interpretFederationAPIAccessFails runner (pure True)

staticUserStoreInterpreter :: [StoredUser] -> InterpreterFor UserStore r
staticUserStoreInterpreter allUsers = interpret $ \case
  GetUser uid -> pure $ find (\user -> user.id == uid) allUsers

miniGalleyAPIAccess :: Maybe TeamMember -> InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess member = interpret $ \case
  GetTeamMember _ _ -> pure member
  _ -> error "uninterpreted effect: GalleyAPIAccess"
