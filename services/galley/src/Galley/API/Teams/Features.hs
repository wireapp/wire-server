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
    getFeatureStatusMulti,
    setFeatureStatus,
    setFeatureStatusInternal,
    patchFeatureStatusInternal,
    getFeatureStatusForUser,
    getAllFeatureConfigsForServer,
    getAllFeatureConfigsForTeam,
    getAllFeatureConfigsForUser,
    updateLockStatus,
    -- Don't export methods of this typeclass
    GetFeatureConfig,
    -- Don't export methods of this typeclass
    SetFeatureConfig,
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
  )
where

import Control.Lens
import Data.Bifunctor (second)
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Kind
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
import Galley.App
import Galley.Effects
import Galley.Effects.BrigAccess (getAccountConferenceCallingConfigClient, updateSearchVisibilityInbound)
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.GundeckAccess
import Galley.Effects.ProposalStore
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
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

data DoAuth = DoAuth UserId | DontDoAuth

-- | Don't export methods of this typeclass
class GetFeatureConfig (db :: Type) cfg where
  type GetConfigForTeamConstraints db cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForTeamConstraints db cfg (r :: EffectRow) =
      ( FeaturePersistentConstraint db cfg,
        ( Member (Input Opts) r,
          Member (TeamFeatureStore db) r
        )
      )

  type GetConfigForUserConstraints db cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForUserConstraints db cfg (r :: EffectRow) =
      ( FeaturePersistentConstraint db cfg,
        ( Member (Input Opts) r,
          Member (ErrorS OperationDenied) r,
          Member (ErrorS 'NotATeamMember) r,
          Member (ErrorS 'TeamNotFound) r,
          Member TeamStore r,
          Member (TeamFeatureStore db) r
        )
      )

  getConfigForServer ::
    Member (Input Opts) r =>
    Sem r (WithStatus cfg)
  -- only override if there is additional business logic for getting the feature config
  -- and/or if the feature flag is configured for the backend in 'FeatureFlags' for galley in 'Galley.Types.Teams'
  -- otherwise this will return the default config from wire-api
  default getConfigForServer :: (IsFeatureConfig cfg) => Sem r (WithStatus cfg)
  getConfigForServer = pure defFeatureStatus

  getConfigForTeam ::
    GetConfigForTeamConstraints db cfg r =>
    TeamId ->
    Sem r (WithStatus cfg)
  default getConfigForTeam ::
    ( FeaturePersistentConstraint db cfg,
      ( Member (Input Opts) r,
        Member (TeamFeatureStore db) r
      )
    ) =>
    TeamId ->
    Sem r (WithStatus cfg)
  getConfigForTeam = genericGetConfigForTeam @db

  getConfigForUser ::
    GetConfigForUserConstraints db cfg r =>
    UserId ->
    Sem r (WithStatus cfg)
  default getConfigForUser ::
    ( FeaturePersistentConstraint db cfg,
      ( Member (Input Opts) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r,
        Member (TeamFeatureStore db) r
      )
    ) =>
    UserId ->
    Sem r (WithStatus cfg)
  getConfigForUser = genericGetConfigForUser @db

-- | Don't export methods of this typeclass
class GetFeatureConfig (db :: Type) cfg => SetFeatureConfig (db :: Type) cfg where
  type SetConfigForTeamConstraints db cfg (r :: EffectRow) :: Constraint
  type SetConfigForTeamConstraints db cfg (r :: EffectRow) = ()

  -- | This method should generate the side-effects of changing the feature and
  -- also (depending on the feature) persist the new setting to the database and
  -- push a event to clients (see 'persistAndPushEvent').
  setConfigForTeam ::
    ( SetConfigForTeamConstraints db cfg r,
      GetConfigForTeamConstraints db cfg r,
      FeaturePersistentConstraint db cfg,
      ( Member (TeamFeatureStore db) r,
        Member (P.Logger (Log.Msg -> Log.Msg)) r,
        Member GundeckAccess r,
        Member TeamStore r
      )
    ) =>
    TeamId ->
    WithStatusNoLock cfg ->
    Sem r (WithStatus cfg)
  default setConfigForTeam ::
    ( GetConfigForTeamConstraints db cfg r,
      FeaturePersistentConstraint db cfg,
      IsFeatureConfig cfg,
      KnownSymbol (FeatureSymbol cfg),
      ToSchema cfg,
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
  setConfigForTeam tid wsnl = persistAndPushEvent @db tid wsnl

type FeaturePersistentAllFeatures db =
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
    FeaturePersistentConstraint db SndFactorPasswordChallengeConfig,
    FeaturePersistentConstraint db MLSConfig,
    FeaturePersistentConstraint db SearchVisibilityInboundConfig,
    FeaturePersistentConstraint db ExposeInvitationURLsToTeamAdminConfig,
    FeaturePersistentConstraint db OutlookCalIntegrationConfig,
    FeaturePersistentConstraint db MlsE2EIdConfig
  )

getFeatureStatus ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'TeamNotFound) r,
      Member TeamStore r
    )
  ) =>
  DoAuth ->
  TeamId ->
  Sem r (WithStatus cfg)
getFeatureStatus doauth tid = do
  case doauth of
    DoAuth uid ->
      getTeamMember tid uid >>= maybe (throwS @'NotATeamMember) (const $ pure ())
    DontDoAuth ->
      assertTeamExists tid
  getConfigForTeam @db @cfg tid

getFeatureStatusMulti ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    FeaturePersistentConstraint db cfg,
    ( Member (Input Opts) r,
      Member (TeamFeatureStore db) r
    )
  ) =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  Sem r (Multi.TeamFeatureNoConfigMultiResponse cfg)
getFeatureStatusMulti (Multi.TeamFeatureNoConfigMultiRequest tids) = do
  cfgs <- genericGetConfigForMultiTeam @db @cfg tids
  let xs = uncurry toTeamStatus . second forgetLock <$> cfgs
  pure $ Multi.TeamFeatureNoConfigMultiResponse xs

toTeamStatus :: TeamId -> WithStatusNoLock cfg -> Multi.TeamStatus cfg
toTeamStatus tid ws = Multi.TeamStatus tid (wssStatus ws)

patchFeatureStatusInternal ::
  forall db cfg r.
  ( SetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    SetConfigForTeamConstraints db cfg r,
    FeaturePersistentConstraint db cfg,
    ( Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS OperationDenied) r,
      Member (ErrorS 'TeamNotFound) r,
      Member (Error TeamFeatureError) r,
      Member TeamStore r,
      Member (TeamFeatureStore db) r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member GundeckAccess r
    )
  ) =>
  TeamId ->
  WithStatusPatch cfg ->
  Sem r (WithStatus cfg)
patchFeatureStatusInternal tid patch = do
  currentFeatureStatus <- getFeatureStatus @db @cfg DontDoAuth tid
  let newFeatureStatus = applyPatch currentFeatureStatus
  when (isJust $ wspLockStatus patch) $ void $ updateLockStatus @db @cfg tid (wsLockStatus newFeatureStatus)
  setConfigForTeam @db @cfg tid (forgetLock newFeatureStatus)
  where
    applyPatch :: WithStatus cfg -> WithStatus cfg
    applyPatch current =
      current
        & setStatus (fromMaybe (wsStatus current) (wspStatus patch))
        & setLockStatus (fromMaybe (wsLockStatus current) (wspLockStatus patch))
        & setConfig (fromMaybe (wsConfig current) (wspConfig patch))
        & setWsTTL (fromMaybe (wsTTL current) (wspTTL patch))

setFeatureStatus ::
  forall db cfg r.
  ( SetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    SetConfigForTeamConstraints db cfg r,
    FeaturePersistentConstraint db cfg,
    ( Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS OperationDenied) r,
      Member (ErrorS 'TeamNotFound) r,
      Member (Error TeamFeatureError) r,
      Member TeamStore r,
      Member (TeamFeatureStore db) r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member GundeckAccess r
    )
  ) =>
  DoAuth ->
  TeamId ->
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
setFeatureStatus doauth tid wsnl = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ChangeTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  guardLockStatus . wsLockStatus =<< getConfigForTeam @db @cfg tid
  setConfigForTeam @db @cfg tid wsnl

setFeatureStatusInternal ::
  forall db cfg r.
  ( SetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    SetConfigForTeamConstraints db cfg r,
    FeaturePersistentConstraint db cfg,
    ( Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS OperationDenied) r,
      Member (ErrorS 'TeamNotFound) r,
      Member (Error TeamFeatureError) r,
      Member TeamStore r,
      Member (TeamFeatureStore db) r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member GundeckAccess r
    )
  ) =>
  TeamId ->
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
setFeatureStatusInternal = setFeatureStatus @db @cfg DontDoAuth

updateLockStatus ::
  forall db cfg r.
  ( FeaturePersistentConstraint db cfg,
    Member (TeamFeatureStore db) r,
    Member TeamStore r,
    Member (ErrorS 'TeamNotFound) r
  ) =>
  TeamId ->
  LockStatus ->
  Sem r LockStatusResponse
updateLockStatus tid lockStatus = do
  assertTeamExists tid
  TeamFeatures.setFeatureLockStatus @db (Proxy @cfg) tid lockStatus
  pure $ LockStatusResponse lockStatus

-- | For individual users to get feature config for their account (personal or team).
-- This looks supposedly redundant to the implementations of `getConfigForUser` but it's not.
-- Here we explicitly return the team setting if the user is a team member.
-- In `getConfigForUser` this is mostly also the case. But there are exceptions, e.g. `ConferenceCallingConfig`
getFeatureStatusForUser ::
  forall (db :: Type) cfg r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
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
      void $ maybe (throwS @'NotATeamMember) pure zusrMembership
      assertTeamExists tid
      getConfigForTeam @db @cfg tid

getAllFeatureConfigsForUser ::
  forall db r.
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member (TeamFeatureStore db) r,
    Member TeamStore r
  ) =>
  FeaturePersistentAllFeatures db =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  when (isJust mbTeam) $ do
    zusrMembership <- maybe (pure Nothing) (`getTeamMember` zusr) mbTeam
    maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
  case mbTeam of
    Just tid ->
      getAllFeatureConfigsTeam @db tid
    Nothing ->
      getAllFeatureConfigsUser @db zusr

getAllFeatureConfigsForTeam ::
  forall db r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member (TeamFeatureStore db) r,
    Member TeamStore r
  ) =>
  FeaturePersistentAllFeatures db =>
  Local UserId ->
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForTeam luid tid = do
  zusrMembership <- getTeamMember tid (tUnqualified luid)
  maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
  getAllFeatureConfigsTeam @db tid

getAllFeatureConfigsForServer ::
  forall r.
  Member (Input Opts) r =>
  Sem r AllFeatureConfigs
getAllFeatureConfigsForServer =
  AllFeatureConfigs
    <$> getConfigForServer @LegalholdConfig
    <*> getConfigForServer @SSOConfig
    <*> getConfigForServer @SearchVisibilityAvailableConfig
    <*> getConfigForServer @SearchVisibilityInboundConfig
    <*> getConfigForServer @ValidateSAMLEmailsConfig
    <*> getConfigForServer @DigitalSignaturesConfig
    <*> getConfigForServer @AppLockConfig
    <*> getConfigForServer @FileSharingConfig
    <*> getConfigForServer @ClassifiedDomainsConfig
    <*> getConfigForServer @ConferenceCallingConfig
    <*> getConfigForServer @SelfDeletingMessagesConfig
    <*> getConfigForServer @GuestLinksConfig
    <*> getConfigForServer @SndFactorPasswordChallengeConfig
    <*> getConfigForServer @MLSConfig
    <*> getConfigForServer @ExposeInvitationURLsToTeamAdminConfig
    <*> getConfigForServer @OutlookCalIntegrationConfig
    <*> getConfigForServer @MlsE2EIdConfig

getAllFeatureConfigsUser ::
  forall db r.
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member (TeamFeatureStore db) r,
    Member TeamStore r
  ) =>
  FeaturePersistentAllFeatures db =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsUser uid =
  AllFeatureConfigs
    <$> getConfigForUser @db @LegalholdConfig uid
    <*> getConfigForUser @db @SSOConfig uid
    <*> getConfigForUser @db @SearchVisibilityAvailableConfig uid
    <*> getConfigForUser @db @SearchVisibilityInboundConfig uid
    <*> getConfigForUser @db @ValidateSAMLEmailsConfig uid
    <*> getConfigForUser @db @DigitalSignaturesConfig uid
    <*> getConfigForUser @db @AppLockConfig uid
    <*> getConfigForUser @db @FileSharingConfig uid
    <*> getConfigForUser @db @ClassifiedDomainsConfig uid
    <*> getConfigForUser @db @ConferenceCallingConfig uid
    <*> getConfigForUser @db @SelfDeletingMessagesConfig uid
    <*> getConfigForUser @db @GuestLinksConfig uid
    <*> getConfigForUser @db @SndFactorPasswordChallengeConfig uid
    <*> getConfigForUser @db @MLSConfig uid
    <*> getConfigForUser @db @ExposeInvitationURLsToTeamAdminConfig uid
    <*> getConfigForUser @db @OutlookCalIntegrationConfig uid
    <*> getConfigForUser @db @MlsE2EIdConfig uid

getAllFeatureConfigsTeam ::
  forall db r.
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member (TeamFeatureStore db) r,
    Member TeamStore r
  ) =>
  FeaturePersistentAllFeatures db =>
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsTeam tid =
  AllFeatureConfigs
    <$> getConfigForTeam @db @LegalholdConfig tid
    <*> getConfigForTeam @db @SSOConfig tid
    <*> getConfigForTeam @db @SearchVisibilityAvailableConfig tid
    <*> getConfigForTeam @db @SearchVisibilityInboundConfig tid
    <*> getConfigForTeam @db @ValidateSAMLEmailsConfig tid
    <*> getConfigForTeam @db @DigitalSignaturesConfig tid
    <*> getConfigForTeam @db @AppLockConfig tid
    <*> getConfigForTeam @db @FileSharingConfig tid
    <*> getConfigForTeam @db @ClassifiedDomainsConfig tid
    <*> getConfigForTeam @db @ConferenceCallingConfig tid
    <*> getConfigForTeam @db @SelfDeletingMessagesConfig tid
    <*> getConfigForTeam @db @GuestLinksConfig tid
    <*> getConfigForTeam @db @SndFactorPasswordChallengeConfig tid
    <*> getConfigForTeam @db @MLSConfig tid
    <*> getConfigForTeam @db @ExposeInvitationURLsToTeamAdminConfig tid
    <*> getConfigForTeam @db @OutlookCalIntegrationConfig tid
    <*> getConfigForTeam @db @MlsE2EIdConfig tid

-- | Note: this is an internal function which doesn't cover all features, e.g. LegalholdConfig
genericGetConfigForTeam ::
  forall db cfg r.
  GetFeatureConfig db cfg =>
  FeaturePersistentConstraint db cfg =>
  Member (TeamFeatureStore db) r =>
  Member (Input Opts) r =>
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
  Member (TeamFeatureStore db) r =>
  Member (Input Opts) r =>
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
  ( Member (Input Opts) r,
    Member (TeamFeatureStore db) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
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
      maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
      assertTeamExists tid
      genericGetConfigForTeam @db tid

persistAndPushEvent ::
  forall (db :: Type) cfg r.
  ( IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    GetFeatureConfig db cfg,
    FeaturePersistentConstraint db cfg,
    GetConfigForTeamConstraints db cfg r,
    ( Member (TeamFeatureStore db) r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member GundeckAccess r,
      Member TeamStore r
    )
  ) =>
  TeamId ->
  WithStatusNoLock cfg ->
  Sem r (WithStatus cfg)
persistAndPushEvent tid wsnl = do
  setFeatureConfig @db (Proxy @cfg) tid wsnl
  fs <- getConfigForTeam @db @cfg tid
  pushFeatureConfigEvent tid (Event.mkUpdateEvent fs)
  pure fs

pushFeatureConfigEvent ::
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
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
    pure $ setStatus status defFeatureStatus

  getConfigForUser = genericGetConfigForUser @db

instance SetFeatureConfig db SSOConfig where
  type SetConfigForTeamConstraints db SSOConfig (r :: EffectRow) = (Member (Error TeamFeatureError) r)

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> throw DisableSsoNotImplemented
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SearchVisibilityAvailableConfig where
  getConfigForServer = do
    status <-
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityAvailableByDefault -> FeatureStatusEnabled
        FeatureTeamSearchVisibilityUnavailableByDefault -> FeatureStatusDisabled
    pure $ setStatus status defFeatureStatus

instance SetFeatureConfig db SearchVisibilityAvailableConfig where
  type SetConfigForTeamConstraints db SearchVisibilityAvailableConfig (r :: EffectRow) = (Member SearchVisibilityStore r)

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> SearchVisibilityData.resetSearchVisibility tid
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db ValidateSAMLEmailsConfig where
  getConfigForServer =
    inputs (view (optSettings . setFeatureFlags . flagsTeamFeatureValidateSAMLEmailsStatus . unDefaults . unImplicitLockStatus))

instance SetFeatureConfig db ValidateSAMLEmailsConfig

instance GetFeatureConfig db DigitalSignaturesConfig

instance SetFeatureConfig db DigitalSignaturesConfig

instance GetFeatureConfig db LegalholdConfig where
  type
    GetConfigForTeamConstraints db LegalholdConfig (r :: EffectRow) =
      ( FeaturePersistentConstraint db LegalholdConfig,
        ( Member (Input Opts) r,
          Member (TeamFeatureStore db) r,
          Member LegalHoldStore r,
          Member TeamStore r
        )
      )
  type
    GetConfigForUserConstraints db LegalholdConfig (r :: EffectRow) =
      ( FeaturePersistentConstraint db LegalholdConfig,
        ( Member (Input Opts) r,
          Member (TeamFeatureStore db) r,
          Member LegalHoldStore r,
          Member TeamStore r,
          Member (ErrorS OperationDenied) r,
          Member (ErrorS 'NotATeamMember) r,
          Member (ErrorS 'TeamNotFound) r
        )
      )

  getConfigForTeam tid = do
    status <-
      isLegalHoldEnabledForTeam @db tid <&> \case
        True -> FeatureStatusEnabled
        False -> FeatureStatusDisabled
    pure $ setStatus status defFeatureStatus

instance SetFeatureConfig db LegalholdConfig where
  type
    SetConfigForTeamConstraints db LegalholdConfig (r :: EffectRow) =
      ( Bounded (PagingBounds InternalPaging TeamMember),
        ( Member BotAccess r,
          Member BrigAccess r,
          Member CodeStore r,
          Member ConversationStore r,
          Member (Error AuthenticationError) r,
          Member (Error InternalError) r,
          Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
          Member (ErrorS 'CannotEnableLegalHoldServiceLargeTeam) r,
          Member (ErrorS 'NotATeamMember) r,
          Member (Error TeamFeatureError) r,
          Member (ErrorS 'LegalHoldNotEnabled) r,
          Member (ErrorS 'LegalHoldDisableUnimplemented) r,
          Member (ErrorS 'LegalHoldServiceNotRegistered) r,
          Member (ErrorS 'UserLegalHoldIllegalOperation) r,
          Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
          Member ExternalAccess r,
          Member FederatorAccess r,
          Member FireAndForget r,
          Member GundeckAccess r,
          Member (Input (Local ())) r,
          Member (Input Env) r,
          Member (Input UTCTime) r,
          Member LegalHoldStore r,
          Member (ListItems LegacyPaging ConvId) r,
          Member MemberStore r,
          Member ProposalStore r,
          Member (TeamFeatureStore db) r,
          Member TeamStore r,
          Member (TeamMemberStore InternalPaging) r,
          Member P.TinyLog r
        ),
        FeaturePersistentConstraint db LegalholdConfig
      )

  -- we're good to update the status now.
  setConfigForTeam tid wsnl = do
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
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db FileSharingConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

instance SetFeatureConfig db FileSharingConfig

instance GetFeatureConfig db AppLockConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagAppLockDefaults . unDefaults . unImplicitLockStatus)

instance SetFeatureConfig db AppLockConfig where
  type SetConfigForTeamConstraints db AppLockConfig r = Member (Error TeamFeatureError) r

  setConfigForTeam tid wsnl = do
    when ((applockInactivityTimeoutSecs . wssConfig $ wsnl) < 30) $
      throw AppLockInactivityTimeoutTooLow
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db ClassifiedDomainsConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagClassifiedDomains . unImplicitLockStatus)

instance GetFeatureConfig db ConferenceCallingConfig where
  type
    GetConfigForUserConstraints db ConferenceCallingConfig r =
      ( FeaturePersistentConstraint db ConferenceCallingConfig,
        ( Member (Input Opts) r,
          Member (ErrorS OperationDenied) r,
          Member (ErrorS 'NotATeamMember) r,
          Member (ErrorS 'TeamNotFound) r,
          Member TeamStore r,
          Member (TeamFeatureStore db) r,
          Member BrigAccess r
        )
      )

  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagConferenceCalling . unDefaults . unImplicitLockStatus)

  getConfigForUser uid = do
    wsnl <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (wsLockStatus (defFeatureStatus @ConferenceCallingConfig)) wsnl

instance SetFeatureConfig db ConferenceCallingConfig

instance GetFeatureConfig db SelfDeletingMessagesConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults)

instance SetFeatureConfig db SelfDeletingMessagesConfig

instance SetFeatureConfig db GuestLinksConfig

instance GetFeatureConfig db GuestLinksConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)

instance SetFeatureConfig db SndFactorPasswordChallengeConfig

instance GetFeatureConfig db SndFactorPasswordChallengeConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

instance SetFeatureConfig db SearchVisibilityInboundConfig where
  type SetConfigForTeamConstraints db SearchVisibilityInboundConfig (r :: EffectRow) = (Member BrigAccess r)
  setConfigForTeam tid wsnl = do
    updateSearchVisibilityInbound $ toTeamStatus tid wsnl
    persistAndPushEvent @db tid wsnl

instance GetFeatureConfig db SearchVisibilityInboundConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSearchVisibilityInbound . unDefaults . unImplicitLockStatus)

instance GetFeatureConfig db MLSConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagMLS . unDefaults . unImplicitLockStatus)

instance SetFeatureConfig db MLSConfig

instance GetFeatureConfig db ExposeInvitationURLsToTeamAdminConfig where
  getConfigForTeam tid = do
    allowList <- input <&> view (optSettings . setExposeInvitationURLsTeamAllowlist . to (fromMaybe []))
    mbOldStatus <- TeamFeatures.getFeatureConfig @db (Proxy @ExposeInvitationURLsToTeamAdminConfig) tid <&> fmap wssStatus
    let teamAllowed = tid `elem` allowList
    pure $ computeConfigForTeam teamAllowed (fromMaybe FeatureStatusDisabled mbOldStatus)
    where
      computeConfigForTeam :: Bool -> FeatureStatus -> WithStatus ExposeInvitationURLsToTeamAdminConfig
      computeConfigForTeam teamAllowed teamDbStatus =
        if teamAllowed
          then makeConfig LockStatusUnlocked teamDbStatus
          else makeConfig LockStatusLocked FeatureStatusDisabled

      makeConfig :: LockStatus -> FeatureStatus -> WithStatus ExposeInvitationURLsToTeamAdminConfig
      makeConfig lockStatus status =
        withStatus
          status
          lockStatus
          ExposeInvitationURLsToTeamAdminConfig
          FeatureTTLUnlimited

instance SetFeatureConfig db ExposeInvitationURLsToTeamAdminConfig

instance SetFeatureConfig db OutlookCalIntegrationConfig

instance GetFeatureConfig db OutlookCalIntegrationConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagOutlookCalIntegration . unDefaults)

instance SetFeatureConfig db MlsE2EIdConfig

instance GetFeatureConfig db MlsE2EIdConfig where
  getConfigForServer =
    input <&> view (optSettings . setFeatureFlags . flagMlsE2EId . unDefaults)

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

featureEnabledForTeam ::
  forall db cfg r.
  ( GetFeatureConfig db cfg,
    GetConfigForTeamConstraints db cfg r,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'TeamNotFound) r,
      Member TeamStore r
    )
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeam tid = (==) FeatureStatusEnabled . wsStatus <$> getFeatureStatus @db @cfg DontDoAuth tid
