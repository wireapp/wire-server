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
    -- setFeatureStatusNoTTL,
    getFeatureStatusForUser,
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
import Galley.Effects.BrigAccess (getAccountConferenceCallingConfigClient, updateSearchVisibilityInbound)
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.GundeckAccess
import Galley.Effects.Paging
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import Galley.Effects.TeamFeatureStore
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore (getLegalHoldFlag, getOneUserTeam, getTeam, getTeamMember)
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush)
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Conversation.Role (Action (RemoveConversationMember))
import Wire.API.Error (ErrorS, throwS)
import Wire.API.Error.Galley
import qualified Wire.API.Event.FeatureConfig as Event
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature
import Wire.API.Team.Member

data DoAuth = DoAuth UserId | DontDoAuth

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
    Maybe FeatureTTL ->
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
  Maybe FeatureTTL ->
  DoAuth ->
  TeamId ->
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
setFeatureStatus mTtl doauth tid wsnl = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ChangeTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  guardLockStatus . wsLockStatus =<< getConfigForTeam @db @cfg tid
  setConfigForTeam @db @cfg tid wsnl mTtl

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
  Maybe FeatureTTL ->
  Sem r (WithStatus cfg)
persistAndPushEvent tid wsnl mTtl = do
  setFeatureConfig @db (Proxy @cfg) tid wsnl mTtl
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

  setConfigForTeam tid wsnl _ = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> throw DisableSsoNotImplemented
    persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db SearchVisibilityAvailableConfig where
  getConfigForServer = do
    status <-
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> FeatureStatusEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> FeatureStatusDisabled
    pure $ defFeatureStatus {wsStatus = status}

instance SetFeatureConfig db SearchVisibilityAvailableConfig where
  type SetConfigForTeamConstraints db SearchVisibilityAvailableConfig (r :: EffectRow) = (Members '[SearchVisibilityStore] r)

  setConfigForTeam tid wsnl _ = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> SearchVisibilityData.resetSearchVisibility tid
    persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db ValidateSAMLEmailsConfig where
  getConfigForServer =
    inputs (view (optSettings . setFeatureFlags . flagsTeamFeatureValidateSAMLEmailsStatus . unDefaults . unImplicitLockStatus))

instance SetFeatureConfig db ValidateSAMLEmailsConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db DigitalSignaturesConfig where
  -- FUTUREWORK: we may also want to get a default from the server config file here, like for
  -- sso, and team search visibility.
  getConfigForServer = pure defFeatureStatus

instance SetFeatureConfig db DigitalSignaturesConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

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

  -- we're good to update the status now.
  setConfigForTeam tid wsnl _ = do
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

    case wssStatus wsnl of
      FeatureStatusDisabled -> LegalHold.removeSettings' @InternalPaging tid
      FeatureStatusEnabled -> ensureNotTooLargeToActivateLegalHold tid
    persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db FileSharingConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

instance SetFeatureConfig db FileSharingConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db AppLockConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagAppLockDefaults . unDefaults . unImplicitLockStatus)

instance SetFeatureConfig db AppLockConfig where
  type SetConfigForTeamConstraints db AppLockConfig r = Members '[Error TeamFeatureError] r

  setConfigForTeam tid wsnl _ = do
    when ((applockInactivityTimeoutSecs . wssConfig $ wsnl) < 30) $
      throw AppLockInactivityTimeoutTooLow
    persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db ClassifiedDomainsConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagClassifiedDomains . unImplicitLockStatus)

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
    input <&> view (optSettings . setFeatureFlags . flagConferenceCalling . unDefaults . unImplicitLockStatus)

  getConfigForUser uid = do
    wsnl <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (wsLockStatus (defFeatureStatus @ConferenceCallingConfig)) wsnl

instance SetFeatureConfig db ConferenceCallingConfig where
  setConfigForTeam tid wsnl mTtl = persistAndPushEvent @db tid wsnl mTtl

instance GetFeatureConfig db SelfDeletingMessagesConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults)

instance SetFeatureConfig db SelfDeletingMessagesConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

instance SetFeatureConfig db GuestLinksConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db GuestLinksConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)

instance SetFeatureConfig db SndFactorPasswordChallengeConfig where
  setConfigForTeam tid wsnl _ = persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db SndFactorPasswordChallengeConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

instance SetFeatureConfig db SearchVisibilityInboundConfig where
  type SetConfigForTeamConstraints db SearchVisibilityInboundConfig (r :: EffectRow) = (Members '[BrigAccess] r)
  setConfigForTeam tid wsnl _ = do
    updateSearchVisibilityInbound $ toTeamStatus tid wsnl
    persistAndPushEvent @db tid wsnl Nothing

instance GetFeatureConfig db SearchVisibilityInboundConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSearchVisibilityInbound . unDefaults . unImplicitLockStatus)

-- -- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.  (This is a workaround until we have 2FA for those end-points as well.)
-- --
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
        then getConfigForTeam @db @SndFactorPasswordChallengeConfig tid
        else getConfigForUser @db @SndFactorPasswordChallengeConfig uid
  case wsStatus tf of
    FeatureStatusDisabled -> action
    FeatureStatusEnabled -> throwS @'AccessDenied
