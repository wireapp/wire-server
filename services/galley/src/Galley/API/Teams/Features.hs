-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Teams.Features
  ( getFeatureStatus,
    getFeatureStatusNoPermissionCheck,
    getFeatureStatusMulti,
    setFeatureStatus,
<<<<<<< HEAD
    setFeatureStatusNoTTL,
    getFeatureConfig,
    getAllFeatureConfigsForUser,
=======
    getFeatureStatusForUser,
>>>>>>> 447bf419f (Refactor features)
    getAllFeatureConfigsForTeam,
    getAllFeatureConfigsForUser,
    setLockStatus,
    -- Don't export methods of this typeclass
    GetFeatureConfig,
    -- Don't export methods of this typeclass
    SetFeatureConfig,
    guardSecondFactorDisabled,
    DoAuth (..),
  )
where

import Control.Lens
import Data.Bifunctor (second)
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Qualified (Local, tUnqualified)
import Data.Schema
import Data.String.Conversions (cs)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Galley.API.Error (InternalError)
import Galley.API.LegalHold (isLegalHoldEnabledForTeam)
import qualified Galley.API.LegalHold as LegalHold
import Galley.API.Teams (ensureNotTooLargeToActivateLegalHold)
import Galley.API.Util (assertTeamExists, getTeamMembersForFanout, membersToRecipients, permissionCheck)
import Galley.Cassandra.Paging
import Galley.Effects
import Galley.Effects.BrigAccess (getAccountFeatureConfigClient, updateSearchVisibilityInbound)
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.GundeckAccess
import Galley.Effects.Paging
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import Galley.Effects.TeamFeatureStore
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore (getLegalHoldFlag, getOneUserTeam, getTeam, getTeamMember)
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush)
import Galley.Options
import Galley.Types.Conversations.Roles (Action (RemoveConversationMember))
import Galley.Types.Teams hiding (newTeam)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Error (ErrorS, throwS)
import Wire.API.Error.Galley
import qualified Wire.API.Event.FeatureConfig as Event
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature

data DoAuth = DoAuth UserId | DontDoAuth

<<<<<<< HEAD
data FeatureScope
  = FeatureScopeServer
  | FeatureScopeTeam TeamId
  | FeatureScopeUser UserId

type FeatureGetter l f r = Tagged '(l, f) (FeatureScope -> Sem r (TeamFeatureStatus l f))

type FeatureSetter f r =
  Tagged
    f
    ( TeamId ->
      TeamFeatureStatus 'WithoutLockStatus f ->
      Maybe TeamFeatureTTLValue ->
      Sem r (TeamFeatureStatus 'WithoutLockStatus f)
    )
=======
-- | Don't export methods of this typeclass
class GetFeatureConfig (db :: *) cfg where
  type GetConfigForTeamConstraints db cfg (r :: EffectRow) :: Constraint
  type GetConfigForTeamConstraints db cfg (r :: EffectRow) = (FeaturePersistentConstraint db cfg, Members '[Input Opts, TeamFeatureStore db] r)

  type GetConfigForUserConstraints db cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForUserConstraints db cfg (r :: EffectRow) =
      ( FeaturePersistentConstraint db cfg,
        Members
          '[ Input Opts,
             ErrorS OperationDenied,
             ErrorS 'NotATeamMember,
             ErrorS 'TeamNotFound,
             TeamStore,
             TeamFeatureStore db
           ]
          r
      )

  getConfigForServer ::
    Members '[Input Opts] r =>
    Sem r (WithStatus cfg)

  getConfigForTeam ::
    GetConfigForTeamConstraints db cfg r =>
    TeamId ->
    Sem r (WithStatus cfg)
  default getConfigForTeam ::
    GetConfigForTeamConstraints db cfg r =>
    (FeaturePersistentConstraint db cfg, Members '[Input Opts, TeamFeatureStore db] r) =>
    TeamId ->
    Sem r (WithStatus cfg)
  getConfigForTeam = genericGetConfigForTeam @db

  getConfigForUser ::
    GetConfigForUserConstraints db cfg r =>
    UserId ->
    Sem r (WithStatus cfg)
  default getConfigForUser ::
    GetConfigForUserConstraints db cfg r =>
    GetConfigForTeamConstraints db cfg r =>
    ( FeaturePersistentConstraint db cfg,
      Members
        '[ Input Opts,
           ErrorS OperationDenied,
           ErrorS 'NotATeamMember,
           ErrorS 'TeamNotFound,
           TeamStore,
           TeamFeatureStore db
         ]
        r
    ) =>
    UserId ->
    Sem r (WithStatus cfg)
  getConfigForUser = genericGetConfigForUser @db

-- | Don't export methods of this typeclass
class GetFeatureConfig (db :: *) cfg => SetFeatureConfig (db :: *) cfg where
  type SetConfigForTeamConstraints db cfg (r :: EffectRow) :: Constraint
  type SetConfigForTeamConstraints db cfg (r :: EffectRow) = ()

  setConfigForTeam ::
    ( SetConfigForTeamConstraints db cfg r,
      GetConfigForTeamConstraints db cfg r,
      FeaturePersistentConstraint db cfg,
      Members
        '[ TeamFeatureStore db,
           P.Logger (Log.Msg -> Log.Msg),
           GundeckAccess,
           TeamStore
         ]
        r
    ) =>
    TeamId ->
    WithStatusNoLock cfg ->
    Sem r (WithStatus cfg)

getFeatureStatusNoPermissionCheck ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    GetConfigForUserConstraints db cfg r,
    Members
      '[ ErrorS OperationDenied,
         ErrorS 'NotATeamMember,
         ErrorS 'TeamNotFound,
         TeamStore,
         Input Opts
       ]
      r
  ) =>
  Maybe UserId ->
  Sem r (WithStatus cfg)
getFeatureStatusNoPermissionCheck = \case
  Just uid -> do
    mbTeam <- getOneUserTeam uid
    case mbTeam of
      Nothing -> getConfigForUser @db @cfg uid
      Just tid -> do
        teamExists <- isJust <$> getTeam tid
        if teamExists
          then getConfigForTeam @db @cfg tid
          else getConfigForUser @db @cfg uid
  Nothing -> getConfigForServer @db @cfg
>>>>>>> 447bf419f (Refactor features)

getFeatureStatus ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    Members
      '[ ErrorS OperationDenied,
         ErrorS 'NotATeamMember,
         ErrorS 'TeamNotFound,
         TeamStore
       ]
      r
  ) =>
  DoAuth ->
  TeamId ->
  Sem r (WithStatus cfg)
getFeatureStatus doauth tid = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ViewTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getConfigForTeam @db @cfg tid

getFeatureStatusMulti ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    FeaturePersistentConstraint db cfg,
    Members
      '[ Input Opts,
         TeamFeatureStore db
       ]
      r
  ) =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  Sem r (Multi.TeamFeatureNoConfigMultiResponse cfg)
getFeatureStatusMulti (Multi.TeamFeatureNoConfigMultiRequest tids) = do
  cfgs <- genericGetConfigForMultiTeam @db @cfg tids
  let xs = uncurry toTeamStatus . second forgetLock <$> cfgs
  pure $ Multi.TeamFeatureNoConfigMultiResponse xs

toTeamStatus :: TeamId -> WithStatusNoLock cfg -> Multi.TeamStatus cfg
toTeamStatus tid ws = Multi.TeamStatus tid (wssStatus ws)

setFeatureStatus ::
  forall db cfg r.
  ( SetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    SetConfigForTeamConstraints db cfg r,
    FeaturePersistentConstraint db cfg,
    Members
      '[ ErrorS 'NotATeamMember,
         ErrorS OperationDenied,
         ErrorS 'TeamNotFound,
         Error TeamFeatureError,
         TeamStore,
         TeamFeatureStore db,
         P.Logger (Log.Msg -> Log.Msg),
         GundeckAccess
       ]
      r
  ) =>
  DoAuth ->
  TeamId ->
<<<<<<< HEAD
  TeamFeatureStatus 'WithoutLockStatus a ->
  Maybe TeamFeatureTTLValue ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
setFeatureStatus (Tagged setter) doauth tid status ttl = do
=======
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
setFeatureStatus doauth tid wsnl = do
>>>>>>> 447bf419f (Refactor features)
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ChangeTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
<<<<<<< HEAD
  setter tid status ttl

-- |  Does not support TTL.
setFeatureStatusNoTTL ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    MaybeHasLockStatusCol a,
    Members
      '[ ErrorS 'NotATeamMember,
         ErrorS OperationDenied,
         ErrorS 'TeamNotFound,
         TeamStore,
         TeamFeatureStore
       ]
      r
  ) =>
  FeatureSetter a r ->
  DoAuth ->
  TeamId ->
  TeamFeatureStatus 'WithoutLockStatus a ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
setFeatureStatusNoTTL setter doauth tid status = setFeatureStatus setter doauth tid status Nothing
=======
  guardLockStatus . wsLockStatus =<< getConfigForTeam @db @cfg tid
  setConfigForTeam @db @cfg tid wsnl
>>>>>>> 447bf419f (Refactor features)

setLockStatus ::
  forall db cfg r.
  ( FeaturePersistentConstraint db cfg,
    Member (TeamFeatureStore db) r,
    Member TeamStore r,
    Member (ErrorS 'TeamNotFound) r
  ) =>
  TeamId ->
  LockStatus ->
  Sem r LockStatusResponse
setLockStatus tid lockStatus = do
  assertTeamExists tid
  TeamFeatures.setFeatureLockStatus @db (Proxy @cfg) tid lockStatus
  pure $ LockStatusResponse lockStatus

-- | For individual users to get feature config for their account (personal or team).
getFeatureStatusForUser ::
  forall (db :: *) cfg r.
  ( Members
      '[ ErrorS 'NotATeamMember,
         ErrorS OperationDenied,
         ErrorS 'TeamNotFound,
         TeamStore
       ]
      r,
    GetConfigForTeamConstraints db cfg r,
    GetConfigForUserConstraints db cfg r,
    GetFeatureConfig db cfg
  ) =>
  UserId ->
  Sem r (WithStatus cfg)
getFeatureStatusForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  case mbTeam of
    Nothing ->
      getConfigForUser @db @cfg zusr
    Just tid -> do
      zusrMembership <- getTeamMember tid zusr
      void $ permissionCheck ViewTeamFeature zusrMembership
      assertTeamExists tid
      getConfigForTeam @db @cfg tid

getAllFeatureConfigsForUser ::
  forall db r.
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       ErrorS 'TeamNotFound,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore db,
       TeamStore
     ]
    r =>
  ( FeaturePersistentConstraint db LegalholdConfig,
    FeaturePersistentConstraint db SSOConfig,
    FeaturePersistentConstraint db SearchVisibilityAvailableConfig,
    FeaturePersistentConstraint db ValidateSAMLEmailsConfig,
    FeaturePersistentConstraint db DigitalSignaturesConfig,
    FeaturePersistentConstraint db AppLockConfig,
    FeaturePersistentConstraint db FileSharingConfig,
    FeaturePersistentConstraint db ClassifiedDomainsConfig,
    FeaturePersistentConstraint db ConferenceCallingConfig,
    FeaturePersistentConstraint db SelfDeletingMessagesConfig,
    FeaturePersistentConstraint db GuestLinksConfig,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  when (isJust mbTeam) $ do
    zusrMembership <- maybe (pure Nothing) (`getTeamMember` zusr) mbTeam
    void $ permissionCheck ViewTeamFeature zusrMembership
  case mbTeam of
    Just tid ->
      getAllFeatureConfigsTeam @db tid
    Nothing ->
      getAllFeatureConfigsUser @db zusr

getAllFeatureConfigsForTeam ::
  forall db r.
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore db,
       TeamStore
     ]
    r =>
  ( FeaturePersistentConstraint db LegalholdConfig,
    FeaturePersistentConstraint db SSOConfig,
    FeaturePersistentConstraint db SearchVisibilityAvailableConfig,
    FeaturePersistentConstraint db ValidateSAMLEmailsConfig,
    FeaturePersistentConstraint db DigitalSignaturesConfig,
    FeaturePersistentConstraint db AppLockConfig,
    FeaturePersistentConstraint db FileSharingConfig,
    FeaturePersistentConstraint db ClassifiedDomainsConfig,
    FeaturePersistentConstraint db ConferenceCallingConfig,
    FeaturePersistentConstraint db SelfDeletingMessagesConfig,
    FeaturePersistentConstraint db GuestLinksConfig,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  Local UserId ->
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForTeam luid tid = do
  zusrMembership <- getTeamMember tid (tUnqualified luid)
  void $ permissionCheck ViewTeamFeature zusrMembership
  getAllFeatureConfigsTeam @db tid

getAllFeatureConfigsUser ::
  forall db r.
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS 'TeamNotFound,
       ErrorS OperationDenied,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore db,
       TeamStore
     ]
    r =>
  ( FeaturePersistentConstraint db LegalholdConfig,
    FeaturePersistentConstraint db SSOConfig,
    FeaturePersistentConstraint db SearchVisibilityAvailableConfig,
    FeaturePersistentConstraint db ValidateSAMLEmailsConfig,
    FeaturePersistentConstraint db DigitalSignaturesConfig,
    FeaturePersistentConstraint db AppLockConfig,
    FeaturePersistentConstraint db FileSharingConfig,
    FeaturePersistentConstraint db ClassifiedDomainsConfig,
    FeaturePersistentConstraint db ConferenceCallingConfig,
    FeaturePersistentConstraint db SelfDeletingMessagesConfig,
    FeaturePersistentConstraint db GuestLinksConfig,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsUser uid =
  AllFeatureConfigs
    <$> getConfigForUser @db @LegalholdConfig uid
    <*> getConfigForUser @db @SSOConfig uid
    <*> getConfigForUser @db @SearchVisibilityAvailableConfig uid
    <*> getConfigForUser @db @ValidateSAMLEmailsConfig uid
    <*> getConfigForUser @db @DigitalSignaturesConfig uid
    <*> getConfigForUser @db @AppLockConfig uid
    <*> getConfigForUser @db @FileSharingConfig uid
    <*> getConfigForUser @db @ClassifiedDomainsConfig uid
    <*> getConfigForUser @db @ConferenceCallingConfig uid
    <*> getConfigForUser @db @SelfDeletingMessagesConfig uid
    <*> getConfigForUser @db @GuestLinksConfig uid
    <*> getConfigForUser @db @SndFactorPasswordChallengeConfig uid

getAllFeatureConfigsTeam ::
  forall db r.
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore db,
       TeamStore
     ]
    r =>
  ( FeaturePersistentConstraint db LegalholdConfig,
    FeaturePersistentConstraint db SSOConfig,
    FeaturePersistentConstraint db SearchVisibilityAvailableConfig,
    FeaturePersistentConstraint db ValidateSAMLEmailsConfig,
    FeaturePersistentConstraint db DigitalSignaturesConfig,
    FeaturePersistentConstraint db AppLockConfig,
    FeaturePersistentConstraint db FileSharingConfig,
    FeaturePersistentConstraint db ClassifiedDomainsConfig,
    FeaturePersistentConstraint db ConferenceCallingConfig,
    FeaturePersistentConstraint db SelfDeletingMessagesConfig,
    FeaturePersistentConstraint db GuestLinksConfig,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  TeamId ->
<<<<<<< HEAD
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a,
    Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r
  ) =>
  (TeamFeatureStatusValue -> TeamId -> Maybe TeamFeatureTTLValue -> Sem r ()) ->
  FeatureSetter a r
setFeatureStatusNoConfig applyState = Tagged $ \tid status ttl -> do
  applyState (tfwoStatus status) tid ttl
  newStatus <- TeamFeatures.setFeatureStatusNoConfig @a tid status ttl
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

getSSOStatusInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSSO r
getSSOStatusInternal =
  Tagged $ \case
    FeatureScopeTeam tid ->
      getFeatureStatusNoConfig @'TeamFeatureSSO getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    getDef :: Member (Input Opts) r => Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> TeamFeatureEnabled
        FeatureSSODisabledByDefault -> TeamFeatureDisabled

setSSOStatusInternal ::
  Members '[Error TeamFeatureError, GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSSO r
setSSOStatusInternal = setFeatureStatusNoConfig @'TeamFeatureSSO $ \case
  TeamFeatureDisabled -> const . const (throw DisableSsoNotImplemented)
  TeamFeatureEnabled -> const . const (pure ())

getTeamSearchVisibilityAvailableInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSearchVisibility r
getTeamSearchVisibilityAvailableInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusNoConfig @'TeamFeatureSearchVisibility getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    getDef = do
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal ::
  Members '[GundeckAccess, SearchVisibilityStore, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSearchVisibility r
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'TeamFeatureSearchVisibility $ \case
  TeamFeatureDisabled -> const . SearchVisibilityData.resetSearchVisibility
  TeamFeatureEnabled -> const . const (pure ())

getValidateSAMLEmailsInternal ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureValidateSAMLEmails r
getValidateSAMLEmailsInternal =
  Tagged $ getFeatureStatusWithDefaultConfig @'TeamFeatureValidateSAMLEmails flagsTeamFeatureValidateSAMLEmailsStatus . mbTeam
  where
    mbTeam = \case
      FeatureScopeTeam tid -> Just tid
      FeatureScopeUser _ -> Nothing
      FeatureScopeServer -> Nothing

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureValidateSAMLEmails r
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'TeamFeatureValidateSAMLEmails $ \_ _ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureDigitalSignatures r
getDigitalSignaturesInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusNoConfig @'TeamFeatureDigitalSignatures getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure TeamFeatureDisabled

setDigitalSignaturesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureDigitalSignatures r
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'TeamFeatureDigitalSignatures $ \_ _ _ -> pure ()

getLegalholdStatusInternal ::
  Members '[LegalHoldStore, TeamFeatureStore, TeamStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureLegalHold r
getLegalholdStatusInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    isLegalHoldEnabledForTeam tid <&> \case
      True -> TeamFeatureStatusNoConfig TeamFeatureEnabled
      False -> TeamFeatureStatusNoConfig TeamFeatureDisabled
  FeatureScopeUser _ -> pure $ TeamFeatureStatusNoConfig TeamFeatureDisabled
  FeatureScopeServer -> pure $ TeamFeatureStatusNoConfig TeamFeatureDisabled

setLegalholdStatusInternal ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
=======
  Sem r AllFeatureConfigs
getAllFeatureConfigsTeam tid =
  AllFeatureConfigs
    <$> getConfigForTeam @db @LegalholdConfig tid
    <*> getConfigForTeam @db @SSOConfig tid
    <*> getConfigForTeam @db @SearchVisibilityAvailableConfig tid
    <*> getConfigForTeam @db @ValidateSAMLEmailsConfig tid
    <*> getConfigForTeam @db @DigitalSignaturesConfig tid
    <*> getConfigForTeam @db @AppLockConfig tid
    <*> getConfigForTeam @db @FileSharingConfig tid
    <*> getConfigForTeam @db @ClassifiedDomainsConfig tid
    <*> getConfigForTeam @db @ConferenceCallingConfig tid
    <*> getConfigForTeam @db @SelfDeletingMessagesConfig tid
    <*> getConfigForTeam @db @GuestLinksConfig tid
    <*> getConfigForTeam @db @SndFactorPasswordChallengeConfig tid

-- | Note: this is an internal function which doesn't cover all features, e.g. LegalholdConfig
genericGetConfigForTeam ::
  forall db cfg r.
  GetFeatureConfig db cfg =>
  FeaturePersistentConstraint db cfg =>
  Members '[TeamFeatureStore db] r =>
  GetConfigForTeamConstraints db cfg r =>
  Members '[Input Opts] r =>
  TeamId ->
  Sem r (WithStatus cfg)
genericGetConfigForTeam tid = do
  computeFeatureConfigForTeamUser
    <$> TeamFeatures.getFeatureConfig @db (Proxy @cfg) tid
    <*> TeamFeatures.getFeatureLockStatus @db (Proxy @cfg) tid
    <*> getConfigForServer @db

-- Note: this function assumes the feature cannot be locked
genericGetConfigForMultiTeam ::
  forall db cfg r.
  GetFeatureConfig db cfg =>
  FeaturePersistentConstraint db cfg =>
  Members '[TeamFeatureStore db] r =>
  GetConfigForTeamConstraints db cfg r =>
  Members '[Input Opts] r =>
  [TeamId] ->
  Sem r [(TeamId, WithStatus cfg)]
genericGetConfigForMultiTeam tids = do
  def <- getConfigForServer @db
  (\(tid, mwsnl) -> (tid, computeFeatureConfigForTeamUser mwsnl (Just LockStatusUnlocked) def))
    <$$> TeamFeatures.getFeatureConfigMulti @db (Proxy @cfg) tids

-- | Note: this is an internal function which doesn't cover all features, e.g. conference calling
genericGetConfigForUser ::
  forall db cfg r.
  FeaturePersistentConstraint db cfg =>
  GetConfigForTeamConstraints db cfg r =>
  ( Members
      '[ Input Opts,
         TeamFeatureStore db,
         ErrorS OperationDenied,
         ErrorS 'NotATeamMember,
         ErrorS 'TeamNotFound,
         TeamStore
       ]
      r,
    GetFeatureConfig db cfg
  ) =>
  UserId ->
  Sem r (WithStatus cfg)
genericGetConfigForUser uid = do
  mbTeam <- getOneUserTeam uid
  case mbTeam of
    Nothing -> do
      getConfigForServer @db
    Just tid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ViewTeamFeature zusrMembership
      assertTeamExists tid
      genericGetConfigForTeam @db tid

persistAndPushEvent ::
  forall (db :: *) cfg r.
  ( IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    GetFeatureConfig db cfg,
    FeaturePersistentConstraint db cfg,
    GetConfigForTeamConstraints db cfg r,
>>>>>>> 447bf419f (Refactor features)
    Members
      '[ TeamFeatureStore db,
         P.Logger (Log.Msg -> Log.Msg),
         GundeckAccess,
         TeamStore
       ]
      r
  ) =>
<<<<<<< HEAD
  FeatureSetter 'TeamFeatureLegalHold r
setLegalholdStatusInternal = Tagged $ \tid status@(tfwoStatus -> statusValue) ttl -> do
  do
=======
  TeamId ->
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
persistAndPushEvent tid wsnl = do
  setFeatureConfig @db (Proxy @cfg) tid wsnl
  fs <- getConfigForTeam @db @cfg tid
  pushFeatureConfigEvent tid (Event.mkUpdateEvent fs)
  pure fs

pushFeatureConfigEvent ::
  Members '[GundeckAccess, TeamStore, P.TinyLog] r =>
  TeamId ->
  Event.Event ->
  Sem r ()
pushFeatureConfigEvent tid event = do
  memList <- getTeamMembersForFanout tid
  when ((memList ^. teamMemberListType) == ListTruncated) $ do
    P.warn $
      Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
        . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
        . Log.field "team" (Log.val (cs . show $ tid))
        . Log.msg @Text "Fanout limit exceeded. Some events will not be sent."
  let recipients = membersToRecipients Nothing (memList ^. teamMembers)
  for_
    (newPush (memList ^. teamMemberListType) Nothing (FeatureConfigEvent event) recipients)
    push1

guardLockStatus ::
  forall r.
  (Member (Error TeamFeatureError) r) =>
  LockStatus ->
  Sem r ()
guardLockStatus = \case
  LockStatusUnlocked -> pure ()
  LockStatusLocked -> throw FeatureLocked

-------------------------------------------------------------------------------
-- GetFeatureConfig and SetFeatureConfig instances

instance GetFeatureConfig db SSOConfig where
  getConfigForServer = do
    status <-
      inputs (view (optSettings . setFeatureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> FeatureStatusEnabled
        FeatureSSODisabledByDefault -> FeatureStatusDisabled
    pure $ defFeatureStatus {wsStatus = status}

  getConfigForUser = genericGetConfigForUser @db

instance SetFeatureConfig db SSOConfig where
  type SetConfigForTeamConstraints db SSOConfig (r :: EffectRow) = (Members '[Error TeamFeatureError] r)

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> throw DisableSsoNotImplemented
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SearchVisibilityAvailableConfig where
  getConfigForServer = do
    status <-
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> FeatureStatusEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> FeatureStatusDisabled
    pure $ defFeatureStatus {wsStatus = status}

instance SetFeatureConfig db SearchVisibilityAvailableConfig where
  type SetConfigForTeamConstraints db SearchVisibilityAvailableConfig (r :: EffectRow) = (Members '[SearchVisibilityStore] r)

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> SearchVisibilityData.resetSearchVisibility tid
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db ValidateSAMLEmailsConfig where
  getConfigForServer =
    inputs (view (optSettings . setFeatureFlags . flagsTeamFeatureValidateSAMLEmailsStatus . unDefaults))

instance SetFeatureConfig db ValidateSAMLEmailsConfig where
  setConfigForTeam tid wsnl = persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db DigitalSignaturesConfig where
  -- FUTUREWORK: we may also want to get a default from the server config file here, like for
  -- sso, and team search visibility.
  getConfigForServer = pure defFeatureStatus

instance SetFeatureConfig db DigitalSignaturesConfig where
  setConfigForTeam tid wsnl = persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db LegalholdConfig where
  type
    GetConfigForTeamConstraints db LegalholdConfig (r :: EffectRow) =
      ( FeaturePersistentConstraint db LegalholdConfig,
        Members '[Input Opts, TeamFeatureStore db, LegalHoldStore, TeamStore] r
      )
  type
    GetConfigForUserConstraints db LegalholdConfig (r :: EffectRow) =
      ( FeaturePersistentConstraint db LegalholdConfig,
        Members
          '[ Input Opts,
             TeamFeatureStore db,
             LegalHoldStore,
             TeamStore,
             ErrorS OperationDenied,
             ErrorS 'NotATeamMember,
             ErrorS 'TeamNotFound
           ]
          r
      )

  getConfigForServer = pure defFeatureStatus

  getConfigForTeam tid = do
    status <-
      isLegalHoldEnabledForTeam @db tid <&> \case
        True -> FeatureStatusEnabled
        False -> FeatureStatusDisabled
    pure $ defFeatureStatus {wsStatus = status}

  getConfigForUser uid = do
    mbTeam <- getOneUserTeam uid
    case mbTeam of
      Nothing -> do
        getConfigForServer @db
      Just tid -> do
        zusrMembership <- getTeamMember tid uid
        void $ permissionCheck ViewTeamFeature zusrMembership
        assertTeamExists tid
        getConfigForTeam @db tid

instance SetFeatureConfig db LegalholdConfig where
  type
    SetConfigForTeamConstraints db LegalholdConfig (r :: EffectRow) =
      ( Bounded (PagingBounds InternalPaging TeamMember),
        Members
          '[ BotAccess,
             BrigAccess,
             CodeStore,
             ConversationStore,
             Error AuthenticationError,
             Error InternalError,
             ErrorS ('ActionDenied 'RemoveConversationMember),
             ErrorS 'CannotEnableLegalHoldServiceLargeTeam,
             ErrorS 'NotATeamMember,
             Error TeamFeatureError,
             ErrorS 'LegalHoldNotEnabled,
             ErrorS 'LegalHoldDisableUnimplemented,
             ErrorS 'LegalHoldServiceNotRegistered,
             ErrorS 'UserLegalHoldIllegalOperation,
             ErrorS 'LegalHoldCouldNotBlockConnections,
             ExternalAccess,
             FederatorAccess,
             FireAndForget,
             GundeckAccess,
             Input (Local ()),
             Input UTCTime,
             LegalHoldStore,
             ListItems LegacyPaging ConvId,
             MemberStore,
             TeamFeatureStore db,
             TeamStore,
             TeamMemberStore InternalPaging,
             P.TinyLog
           ]
          r,
        FeaturePersistentConstraint db LegalholdConfig
      )

  setConfigForTeam tid wsnl = do
>>>>>>> 447bf419f (Refactor features)
    -- this extra do is to encapsulate the assertions running before the actual operation.
    -- enabling LH for teams is only allowed in normal operation; disabled-permanently and
    -- whitelist-teams have no or their own way to do that, resp.
    featureLegalHold <- getLegalHoldFlag
    case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throw LegalHoldFeatureFlagNotEnabled
      FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
        throw LegalHoldWhitelistedOnly

<<<<<<< HEAD
  -- we're good to update the status now.
  case statusValue of
    TeamFeatureDisabled -> removeSettings' @p tid
    TeamFeatureEnabled -> ensureNotTooLargeToActivateLegalHold tid
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureLegalHold tid status ttl

getFileSharingInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureFileSharing r
getFileSharingInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureFileSharing tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureFileSharing)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

determineFeatureStatus ::
  TeamFeatureStatusNoConfigAndLockStatus ->
  LockStatusValue ->
  Maybe TeamFeatureStatusNoConfig ->
  TeamFeatureStatusNoConfigAndLockStatus
determineFeatureStatus cfgDefault lockStatus mbFeatureStatus = case (lockStatus, mbFeatureStatus) of
  (Unlocked, Just featureStatus) ->
    TeamFeatureStatusNoConfigAndLockStatus
      (tfwoStatus featureStatus)
      lockStatus
  (Unlocked, Nothing) -> cfgDefault {tfwoapsLockStatus = lockStatus}
  (Locked, _) -> cfgDefault {tfwoapsLockStatus = lockStatus}

getFeatureStatusWithDefaultConfig ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    HasStatusCol a,
    FeatureHasNoConfig 'WithoutLockStatus a,
    Members '[Input Opts, TeamFeatureStore] r
  ) =>
  Lens' FeatureFlags (Defaults (TeamFeatureStatus 'WithoutLockStatus a)) ->
  Maybe TeamId ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @a getDef)
  where
    getDef :: Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> tfwoStatus . view unDefaults

setFileSharingInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureFileSharing r
setFileSharingInternal = Tagged $ \tid status ttl -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureFileSharing tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureFileSharing
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureFileSharing tid status ttl <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults . to tfwoapsLockStatus)

getAppLockInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureAppLock r
getAppLockInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    mStatus <- TeamFeatures.getApplockFeatureStatus tid
    pure $ fromMaybe cfgDefault mStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault = do
      inputs (view (optSettings . setFeatureFlags . flagAppLockDefaults . unDefaults))

setAppLockInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, Error TeamFeatureError, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureAppLock r
setAppLockInternal = Tagged $ \tid status _ttl -> do
  when (applockInactivityTimeoutSecs (tfwcConfig status) < 30) $
    throw AppLockInactivityTimeoutTooLow
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update TeamFeatureAppLock (EdFeatureApplockChanged status)
  TeamFeatures.setApplockFeatureStatus tid status <* pushEvent

getClassifiedDomainsInternal ::
  Member (Input Opts) r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureClassifiedDomains r
getClassifiedDomainsInternal = Tagged . const $ do
  globalConfig <- inputs (view (optSettings . setFeatureFlags . flagClassifiedDomains))
  let config = globalConfig
  pure $ case tfwcStatus config of
    TeamFeatureDisabled -> defTeamFeatureStatus @'TeamFeatureClassifiedDomains
    TeamFeatureEnabled -> config

getConferenceCallingInternal ::
  Members '[BrigAccess, Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureConferenceCalling r
getConferenceCallingInternal = Tagged $ \case
  FeatureScopeTeam tid -> getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling (Just tid)
  FeatureScopeUser uid -> getFeatureConfigViaAccount @'TeamFeatureConferenceCalling uid
  FeatureScopeServer -> getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling Nothing

setConferenceCallingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureConferenceCalling r
setConferenceCallingInternal =
  setFeatureStatusNoConfig @'TeamFeatureConferenceCalling $ \_status _tid _ttl -> pure ()

getSelfDeletingMessagesInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureSelfDeletingMessages r
getSelfDeletingMessagesInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    let defLockStatus = tfwcapsLockStatus cfgDefault
    (mbFeatureStatus, fromMaybe defLockStatus -> lockStatus) <- TeamFeatures.getSelfDeletingMessagesStatus tid
    pure $ case (lockStatus, mbFeatureStatus) of
      (Unlocked, Just featureStatus) ->
        TeamFeatureStatusWithConfigAndLockStatus
          (tfwcStatus featureStatus)
          (tfwcConfig featureStatus)
          Unlocked
      (Unlocked, Nothing) -> cfgDefault {tfwcapsLockStatus = Unlocked}
      (Locked, _) -> cfgDefault {tfwcapsLockStatus = Locked}
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatusWithConfigAndLockStatus TeamFeatureSelfDeletingMessagesConfig)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults)

setSelfDeletingMessagesInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureSelfDeletingMessages r
setSelfDeletingMessagesInternal = Tagged $ \tid st _ -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureSelfDeletingMessages tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update TeamFeatureSelfDeletingMessages (EdFeatureSelfDeletingMessagesChanged st)
  TeamFeatures.setSelfDeletingMessagesStatus tid st <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults . to tfwcapsLockStatus)

getGuestLinkInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureGuestLinks r
getGuestLinkInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureGuestLinks tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureGuestLinks)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)

setGuestLinkInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureGuestLinks r
setGuestLinkInternal = Tagged $ \tid status ttl -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureGuestLinks tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureGuestLinks
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureGuestLinks tid status ttl <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults . to tfwoapsLockStatus)

getSndFactorPasswordChallengeInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge r
getSndFactorPasswordChallengeInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureSndFactorPasswordChallenge tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

getSndFactorPasswordChallengeNoAuth ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  Maybe UserId ->
  Sem r TeamFeatureStatusNoConfig
getSndFactorPasswordChallengeNoAuth mbUserId = do
  scope <- getScope mbUserId
  TeamFeatureStatusNoConfig . tfwoapsStatus <$> unTagged getSndFactorPasswordChallengeInternal scope
  where
    getScope :: Maybe UserId -> Sem r FeatureScope
    getScope = \case
      Just uid -> do
        mbTeam <- getOneUserTeam uid
        case mbTeam of
          Nothing -> pure $ FeatureScopeUser uid
          Just tid -> do
            teamExists <- isJust <$> getTeam tid
            if teamExists
              then pure $ FeatureScopeTeam tid
              else pure $ FeatureScopeUser uid
      Nothing -> pure FeatureScopeServer

-- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.  (This is a workaround until we have 2FA for those end-points as well.)
--
=======
    -- we're good to update the status now.

    case wssStatus wsnl of
      FeatureStatusDisabled -> LegalHold.removeSettings' @InternalPaging tid
      FeatureStatusEnabled -> ensureNotTooLargeToActivateLegalHold tid

    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db FileSharingConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

instance SetFeatureConfig db FileSharingConfig where
  setConfigForTeam tid wsnl =
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db AppLockConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagAppLockDefaults . unDefaults)

instance SetFeatureConfig db AppLockConfig where
  type SetConfigForTeamConstraints db AppLockConfig r = Members '[Error TeamFeatureError] r

  setConfigForTeam tid wsnl = do
    when ((applockInactivityTimeoutSecs . wssConfig $ wsnl) < 30) $
      throw AppLockInactivityTimeoutTooLow
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db ClassifiedDomainsConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagClassifiedDomains)

instance GetFeatureConfig db ConferenceCallingConfig where
  type
    GetConfigForUserConstraints db ConferenceCallingConfig r =
      ( FeaturePersistentConstraint db ConferenceCallingConfig,
        Members
          '[ Input Opts,
             ErrorS OperationDenied,
             ErrorS 'NotATeamMember,
             ErrorS 'TeamNotFound,
             TeamStore,
             TeamFeatureStore db,
             BrigAccess
           ]
          r
      )

  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagConferenceCalling . unDefaults)

  getConfigForUser uid = do
    wsnl <- getAccountFeatureConfigClient uid
    pure $ withLockStatus (wsLockStatus (defFeatureStatus @ConferenceCallingConfig)) wsnl

instance SetFeatureConfig db ConferenceCallingConfig where
  setConfigForTeam tid wsnl =
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SelfDeletingMessagesConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults)

instance SetFeatureConfig db SelfDeletingMessagesConfig where
  setConfigForTeam tid wsnl =
    persistAndPushEvent @db tid wsnl

instance SetFeatureConfig db GuestLinksConfig where
  setConfigForTeam tid wsnl = persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db GuestLinksConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)

instance SetFeatureConfig db SndFactorPasswordChallengeConfig where
  setConfigForTeam tid wsnl = persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SndFactorPasswordChallengeConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

instance SetFeatureConfig db SearchVisibilityInboundConfig where
  type SetConfigForTeamConstraints db SearchVisibilityInboundConfig (r :: EffectRow) = (Members '[BrigAccess] r)
  setConfigForTeam tid wsnl = do
    updateSearchVisibilityInbound $ toTeamStatus tid wsnl
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SearchVisibilityInboundConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSearchVisibilityInbound . unDefaults)

-- -- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.  (This is a workaround until we have 2FA for those end-points as well.)
-- --
>>>>>>> 447bf419f (Refactor features)
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabled ::
  forall db r a.
  ( Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member ConversationStore r,
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig
  ) =>
  UserId ->
  ConvId ->
  Sem r a ->
  Sem r a
guardSecondFactorDisabled uid cid action = do
  mbCnvData <- ConversationStore.getConversationMetadata cid
  tf <- case mbCnvData >>= cnvmTeam of
    Nothing -> getConfigForUser @db @SndFactorPasswordChallengeConfig uid
    Just tid -> do
      teamExists <- isJust <$> getTeam tid
      if teamExists
<<<<<<< HEAD
        then TeamFeatureStatusNoConfig . tfwoapsStatus <$> unTagged getSndFactorPasswordChallengeInternal (FeatureScopeTeam tid)
        else getSndFactorPasswordChallengeNoAuth (Just uid)
  case tfwoStatus teamFeature of
    TeamFeatureDisabled -> action
    TeamFeatureEnabled -> throwS @'AccessDenied

setSndFactorPasswordChallengeInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureSndFactorPasswordChallenge r
setSndFactorPasswordChallengeInternal = Tagged $ \tid status ttl -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureSndFactorPasswordChallenge tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureSndFactorPasswordChallenge
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureSndFactorPasswordChallenge tid status ttl <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults . to tfwoapsLockStatus)

getTeamSearchVisibilityInboundInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSearchVisibilityInbound r
getTeamSearchVisibilityInboundInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound (Just tid)
    FeatureScopeUser _ -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound Nothing
    FeatureScopeServer -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound Nothing

setTeamSearchVisibilityInboundInternal ::
  Members '[Error InternalError, GundeckAccess, TeamStore, TeamFeatureStore, BrigAccess, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSearchVisibilityInbound r
setTeamSearchVisibilityInboundInternal = Tagged $ \tid status ttl -> do
  updatedStatus <- unTagged (setFeatureStatusNoConfig @'TeamFeatureSearchVisibilityInbound $ \_ _ _ -> pure ()) tid status ttl
  mPersistedStatus <- listToMaybe <$> TeamFeatures.getFeatureStatusNoConfigMulti (Proxy @'TeamFeatureSearchVisibilityInbound) [tid]
  case mPersistedStatus of
    Just (persistedTid, persistedStatus, persistedWriteTime) ->
      updateSearchVisibilityInbound $
        Multi.TeamStatusUpdate persistedTid persistedStatus persistedWriteTime
    Nothing -> throw (InternalErrorWithDescription "Failed to retrieve search-visibility-inbound status after persisting it")
  pure updatedStatus

getFeatureStatusMulti ::
  forall f r.
  ( KnownTeamFeatureName f,
    FeatureHasNoConfig 'WithoutLockStatus f,
    HasStatusCol f,
    Members
      '[ TeamStore,
         TeamFeatureStore,
         Input Opts
       ]
      r
  ) =>
  Lens' FeatureFlags (Defaults (TeamFeatureStatus 'WithoutLockStatus f)) ->
  (Multi.TeamFeatureNoConfigMultiRequest -> (Sem r) (Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound))
getFeatureStatusMulti lens' (Multi.TeamFeatureNoConfigMultiRequest teams) = do
  triples <- TeamFeatures.getFeatureStatusNoConfigMulti (Proxy @f) teams
  let tsExplicit = map (\(tid, sv, t) -> Multi.TeamStatus tid sv (Just t)) triples
  let teamsDefault = Set.toList (Set.fromList teams `Set.difference` Set.fromList (Multi.team <$> tsExplicit))
  defaultStatus <- getDef
  let tsImplicit = [Multi.TeamStatus tid defaultStatus Nothing | tid <- teamsDefault]
  pure $ Multi.TeamFeatureNoConfigMultiResponse $ tsExplicit <> tsImplicit
  where
    getDef :: Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> tfwoStatus . view unDefaults

getTeamSearchVisibilityInboundInternalMulti ::
  Members
    '[ TeamStore,
       TeamFeatureStore,
       Input Opts
     ]
    r =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  (Sem r) (Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound)
getTeamSearchVisibilityInboundInternalMulti =
  getFeatureStatusMulti @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound

-- TODO(fisx): move this function to a more suitable place / module.
guardLockStatus ::
  forall (a :: TeamFeatureName) r.
  ( MaybeHasLockStatusCol a,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureError) r
  ) =>
  TeamId ->
  LockStatusValue -> -- FUTUREWORK(fisx): move this into its own type class and infer from `a`?
  Sem r ()
guardLockStatus tid defLockStatus = do
  (TeamFeatures.getLockStatus @a tid <&> fromMaybe defLockStatus) >>= \case
    Unlocked -> pure ()
    Locked -> throw FeatureLocked

pushFeatureConfigEvent ::
  Members '[GundeckAccess, TeamStore, P.TinyLog] r =>
  TeamId ->
  Event.Event ->
  Sem r ()
pushFeatureConfigEvent tid event = do
  memList <- getTeamMembersForFanout tid
  when ((memList ^. teamMemberListType) == ListTruncated) $ do
    P.warn $
      Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
        . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
        . Log.field "team" (Log.val (cs . show $ tid))
        . Log.msg @Text "Fanout limit exceeded. Some events will not be sent."
  let recipients = membersToRecipients Nothing (memList ^. teamMembers)
  for_
    (newPush (memList ^. teamMemberListType) Nothing (FeatureConfigEvent event) recipients)
    push1

-- | (Currently, we only have 'TeamFeatureConferenceCalling' here, but we may have to
-- extend this in the future.)
getFeatureConfigViaAccount ::
  ( flag ~ 'TeamFeatureConferenceCalling,
    Member BrigAccess r
  ) =>
  UserId ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus flag)
getFeatureConfigViaAccount = getAccountFeatureConfigClient
=======
        then getConfigForTeam @db @SndFactorPasswordChallengeConfig tid
        else getConfigForUser @db @SndFactorPasswordChallengeConfig uid
  case wsStatus tf of
    FeatureStatusDisabled -> action
    FeatureStatusEnabled -> throwS @'AccessDenied
>>>>>>> 447bf419f (Refactor features)
