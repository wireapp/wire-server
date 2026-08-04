{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamInvitationSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Domain
import Data.HashSet qualified as HashSet
import Data.Id
import Data.LegalHold
import Data.Map qualified as Map
import Data.Qualified
import Data.Tagged (Tagged)
import Data.Text.Encoding
import Data.Time
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.TinyLog
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.EnterpriseLogin
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley (GalleyError (TeamMemberNotFound, TeamNotFound))
import Wire.API.Password (Password)
import Wire.API.Team.Invitation
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role (defaultRole)
import Wire.API.User
import Wire.EmailSubsystem
import Wire.EnterpriseLoginSubsystem
import Wire.GalleyAPIAccess
import Wire.InvitationStore
import Wire.MockInterpreters
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Random
import Wire.StoredUser
import Wire.TeamInvitationSubsystem
import Wire.TeamInvitationSubsystem.Error
import Wire.TeamInvitationSubsystem.Interpreter
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI
import Wire.UserKeyStore
import Wire.UserStore (UserStore)
import Wire.UserStore qualified as UserStore
import Wire.UserSubsystem
import Wire.Util

type AllEffects =
  [ EnterpriseLoginSubsystem,
    TinyLog,
    TeamSubsystem,
    GalleyAPIAccess,
    Random,
    State StdGen,
    InvitationStore,
    UserKeyStore,
    State (Map (TeamId, InvitationId) StoredInvitation),
    State (Map (InvitationCode) StoredInvitation),
    State (Map (TeamId, EmailAddress) [UserId]),
    Now,
    State UTCTime,
    Error TeamInvitationSubsystemError,
    ErrorS 'TeamMemberNotFound,
    ErrorS 'TeamNotFound,
    EmailSubsystem,
    State (Map EmailAddress [SentMail]),
    UserSubsystem,
    UserStore,
    State [UserId],
    UserKeyStore
  ]

data RunAllEffectsArgs = RunAllEffectsArgs
  { teams :: Map TeamId [TeamMember],
    initialUsers :: [StoredUser],
    constGuardResult :: Maybe DomainRegistration
  }
  deriving (Eq, Show)

data InviteScenarioObservation = InviteScenarioObservation
  { -- 'Nothing' means the manual invitation was created successfully.
    invitationResult :: Maybe TeamInvitationSubsystemError,
    -- User IDs passed to 'UserStore.DeleteUser' during reconciliation.
    deletedUserIds :: [UserId],
    -- The candidate user's record after reconciliation, if it still exists.
    observedUser :: Maybe StoredUser,
    -- User IDs still present in the pending SCIM index after reconciliation.
    observedPendingScimUsers :: [UserId]
  }
  deriving (Eq, Show)

data InviteScenarioInput = InviteScenarioInput
  { invitationTeam :: TeamId,
    inviter :: StoredUser,
    otherUsers :: [StoredUser],
    pendingScimUsers :: [(TeamId, EmailAddress, UserId)],
    liveInvitations :: [InsertInvitation],
    inviteeEmail :: EmailAddress,
    observedUid :: UserId
  }
  deriving (Eq, Show)

runAllEffects :: RunAllEffectsArgs -> Sem AllEffects a -> Either LocalErrors a
runAllEffects args = runAllEffectsWithUserKeys args.initialUsers args

runAllEffectsWithUserKeys :: [StoredUser] -> RunAllEffectsArgs -> Sem AllEffects a -> Either LocalErrors a
runAllEffectsWithUserKeys initialUsers args =
  run
    . runInMemoryUserKeyStoreIntepreterWithStoredUsers initialUsers
    . evalState ([] :: [UserId])
    . evalState mempty
    . evalState args.initialUsers
    . inMemoryUserStoreInterpreterWithDeleteHook (\uid -> modify @[UserId] (uid :))
    . raiseUnder @(State [StoredUser])
    . raiseUnder @(State (Map UserId Password))
    . inMemoryUserSubsystemInterpreter
    . evalState mempty
    . noopEmailSubsystemInterpreter
    . runLocalErrors
    . evalState defaultTime
    . interpretNowAsState
    . evalState mempty
    . evalState mempty
    . evalState mempty
    . (evalState mempty . inMemoryUserKeyStoreInterpreter . raiseUnder)
    . inMemoryInvitationStoreInterpreter
    . evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . miniGalleyAPIAccess args.teams def
    . interpretTeamSubsystemToGalleyAPI
    . discardTinyLogs
    . enterpriseLoginSubsystemTestInterpreter args.constGuardResult

runInviteScenarioObserved ::
  InviteScenarioInput ->
  Either LocalErrors InviteScenarioObservation
runInviteScenarioObserved input =
  runAllEffectsWithUserKeys [input.inviter] args . runTeamInvitationSubsystem config $ do
    for_ input.liveInvitations $ \inv -> void $ insertInvitation inv 3_000_000
    for_ input.pendingScimUsers $ \(indexTeam, email, uid) ->
      deleteKey (mkEmailKey email) >> insertPendingScimUser indexTeam email uid
    result <- catch (inviteUser inviterLuid input.invitationTeam invitationRequest >> pure Nothing) (pure . Just)
    deletedUsers <- get @[UserId]
    observedUser <- UserStore.getUser input.observedUid
    observedIndex <- lookupPendingScimUsers input.invitationTeam input.inviteeEmail
    pure
      InviteScenarioObservation
        { invitationResult = result,
          deletedUserIds = deletedUsers,
          observedUser,
          observedPendingScimUsers = observedIndex
        }
  where
    inviterLuid = toLocalUnsafe testDomain input.inviter.id
    inviterMember = mkTeamMember input.inviter.id fullPermissions Nothing UserLegalHoldDisabled
    invitationRequest =
      InvitationRequest
        { locale = Nothing,
          role = Nothing,
          inviteeName = Nothing,
          inviteeEmail = input.inviteeEmail,
          allowExisting = False
        }
    config =
      TeamInvitationSubsystemConfig
        { maxTeamSize = 50,
          teamInvitationTimeout = 3_000_000,
          blockedDomains = HashSet.empty
        }
    args =
      RunAllEffectsArgs
        { teams = Map.singleton input.invitationTeam [inviterMember],
          initialUsers = input.inviter : input.otherUsers,
          constGuardResult = Nothing
        }

data LocalErrors
  = ETeamMemberNotFound
  | ETeamNotFound
  | ESubsystem TeamInvitationSubsystemError
  deriving stock (Eq, Show)

runLocalErrors ::
  Sem (Error TeamInvitationSubsystemError ': ErrorS 'TeamMemberNotFound ': ErrorS 'TeamNotFound ': r) a ->
  Sem r (Either LocalErrors a)
runLocalErrors = fmap toLocalErrors . runError . runError . runError
  where
    toLocalErrors ::
      Either (Tagged 'TeamNotFound ()) (Either (Tagged 'TeamMemberNotFound ()) (Either TeamInvitationSubsystemError a)) ->
      Either LocalErrors a
    toLocalErrors = \case
      Right (Right (Right a)) -> Right a
      Right (Right (Left e)) -> Left (ESubsystem e)
      Right (Left _) -> Left ETeamMemberNotFound
      Left _ -> Left ETeamNotFound

spec :: Spec
spec = do
  focus $ describe "InviteUser" $ do
    prop "rejects a manual invitation when a matching SCIM invitation is pending" $
      \(tid :: TeamId)
       (inviter0 :: StoredUser)
       (scimUser0 :: StoredUser)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress)
       (code :: InvitationCode) ->
          inviter0.id /= scimUser0.id ==>
            let inviter :: StoredUser
                inviter =
                  inviter0
                    { email = Just inviterEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just tid,
                      managedBy = Just ManagedByWire,
                      userType = Just UserTypeRegular
                    }

                scimUser :: StoredUser
                scimUser =
                  scimUser0
                    { email = Just inviteeEmail,
                      emailUnvalidated = Nothing,
                      activated = False,
                      status = Just PendingInvitation,
                      teamId = Just tid,
                      managedBy = Just ManagedByScim,
                      userType = Just UserTypeRegular
                    }

                storedInvitation =
                  MkInsertInvitation
                    { invitationId = Id (toUUID scimUser.id),
                      teamId = tid,
                      role = defaultRole,
                      createdAt = defaultTime,
                      createdBy = Just inviter.id,
                      inviteeEmail = inviteeEmail,
                      inviteeName = Nothing,
                      code = code
                    }

                outcome =
                  runInviteScenarioObserved
                    InviteScenarioInput
                      { invitationTeam = tid,
                        inviter,
                        otherUsers = [scimUser],
                        pendingScimUsers = [(tid, inviteeEmail, scimUser.id)],
                        liveInvitations = [storedInvitation],
                        inviteeEmail,
                        observedUid = scimUser.id
                      }
             in counterexample (show (inviter, scimUser, storedInvitation)) $
                  outcome
                    === Right
                      InviteScenarioObservation
                        { invitationResult = Just TeamInvitationEmailTaken,
                          deletedUserIds = [],
                          observedUser = Just scimUser,
                          observedPendingScimUsers = [scimUser.id]
                        }

    prop "allows a manual invitation after a matching SCIM invitation expired" $
      \(tid :: TeamId)
       (inviter0 :: StoredUser)
       (scimUser0 :: StoredUser)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress) ->
          inviter0.id /= scimUser0.id ==>
            let inviter =
                  inviter0
                    { email = Just inviterEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just tid,
                      managedBy = Just ManagedByWire,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                scimUser =
                  scimUser0
                    { email = Just inviteeEmail,
                      activated = False,
                      status = Just PendingInvitation,
                      teamId = Just tid,
                      managedBy = Just ManagedByScim,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                outcome =
                  runInviteScenarioObserved
                    InviteScenarioInput
                      { invitationTeam = tid,
                        inviter,
                        otherUsers = [scimUser],
                        pendingScimUsers = [(tid, inviteeEmail, scimUser.id)],
                        liveInvitations = [],
                        inviteeEmail,
                        observedUid = scimUser.id
                      }
             in outcome
                  === Right
                    InviteScenarioObservation
                      { invitationResult = Nothing,
                        deletedUserIds = [scimUser.id],
                        observedUser = Nothing,
                        observedPendingScimUsers = []
                      }

    prop "rejects a manual invitation for an active SCIM account" $
      \(tid :: TeamId)
       (inviter0 :: StoredUser)
       (scimUser0 :: StoredUser)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress) ->
          inviter0.id /= scimUser0.id ==>
            let inviter =
                  inviter0
                    { email = Just inviterEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just tid,
                      managedBy = Just ManagedByWire,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                scimUser =
                  scimUser0
                    { email = Just inviteeEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just tid,
                      managedBy = Just ManagedByScim,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                outcome =
                  runInviteScenarioObserved
                    InviteScenarioInput
                      { invitationTeam = tid,
                        inviter,
                        otherUsers = [scimUser],
                        pendingScimUsers = [(tid, inviteeEmail, scimUser.id)],
                        liveInvitations = [],
                        inviteeEmail,
                        observedUid = scimUser.id
                      }
             in outcome
                  === Right
                    InviteScenarioObservation
                      { invitationResult = Just TeamInvitationEmailTaken,
                        deletedUserIds = [],
                        observedUser = Just scimUser,
                        observedPendingScimUsers = [scimUser.id]
                      }

    prop "allows a manual invitation when the SCIM index entry is stale" $
      \(tid :: TeamId)
       (inviter :: StoredUser)
       (staleUid :: UserId)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress) ->
          inviter.id /= staleUid ==>
            let preparedInviter =
                  inviter
                    { email = Just inviterEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just tid,
                      managedBy = Just ManagedByWire,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                outcome =
                  runInviteScenarioObserved
                    InviteScenarioInput
                      { invitationTeam = tid,
                        inviter = preparedInviter,
                        otherUsers = [],
                        pendingScimUsers = [(tid, inviteeEmail, staleUid)],
                        liveInvitations = [],
                        inviteeEmail,
                        observedUid = staleUid
                      }
             in outcome
                  === Right
                    InviteScenarioObservation
                      { invitationResult = Nothing,
                        deletedUserIds = [],
                        observedUser = Nothing,
                        observedPendingScimUsers = []
                      }

    prop "allows a manual invitation in another team despite a pending SCIM invitation" $
      \(scimTeam :: TeamId)
       (manualTeam :: TeamId)
       (inviter0 :: StoredUser)
       (scimUser0 :: StoredUser)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress) ->
          scimTeam /= manualTeam && inviter0.id /= scimUser0.id ==>
            let inviter =
                  inviter0
                    { email = Just inviterEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Just manualTeam,
                      managedBy = Just ManagedByWire,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                scimUser =
                  scimUser0
                    { email = Just inviteeEmail,
                      activated = False,
                      status = Just PendingInvitation,
                      teamId = Just scimTeam,
                      managedBy = Just ManagedByScim,
                      userType = Just UserTypeRegular
                    } ::
                    StoredUser
                outcome =
                  runInviteScenarioObserved
                    InviteScenarioInput
                      { invitationTeam = manualTeam,
                        inviter,
                        otherUsers = [scimUser],
                        pendingScimUsers = [(scimTeam, inviteeEmail, scimUser.id)],
                        liveInvitations = [],
                        inviteeEmail,
                        observedUid = scimUser.id
                      }
             in outcome
                  === Right
                    InviteScenarioObservation
                      { invitationResult = Nothing,
                        deletedUserIds = [],
                        observedUser = Just scimUser,
                        observedPendingScimUsers = []
                      }

    prop "honors domain config from `brig.domain_registration`" $
      \(tid :: TeamId)
       (preDomRegUpd :: DomainRegistrationUpdate)
       (preInviter :: StoredUser)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress)
       (preExistingPersonalAccount :: Maybe StoredUser)
       (preRegisteredDomain {- if Nothing, use invitee's email domain -} :: Maybe Domain)
       (sameTeam {- team id matches the team id in the domain registration -} :: Bool) ->
          let -- prepare the pre* prop args
              --
              domRegUpd = preDomRegUpd & if sameTeam then setTeamId else Imports.id
                where
                  setTeamId upd = case upd.teamInvite of
                    Team _ -> DomainRegistrationUpdate upd.domainRedirect (Team tid)
                    _ -> upd

              inviter =
                preInviter
                  { email = Just inviterEmail,
                    activated = True,
                    status = Just Active
                  } ::
                  StoredUser

              existingPersonalAccount =
                preExistingPersonalAccount <&> \r ->
                  r
                    { email = Just inviteeEmail,
                      activated = True,
                      status = Just Active,
                      teamId = Nothing,
                      managedBy = Just ManagedByWire
                    } ::
                    StoredUser

              registeredDomain :: Domain
              registeredDomain = fromMaybe edom preRegisteredDomain
                where
                  edom = fromRight (error "test crashed") $ emailDomain inviteeEmail

              -- setup team, owner, interpreter
              --
              cfg =
                TeamInvitationSubsystemConfig
                  { maxTeamSize = 50,
                    teamInvitationTimeout = 3_000_000,
                    blockedDomains = HashSet.empty
                  }

              inviterUid = inviter.id
              inviterLuid = toLocalUnsafe testDomain inviterUid
              inviterMember = mkTeamMember inviterUid fullPermissions Nothing UserLegalHoldDisabled

              invReq =
                InvitationRequest
                  { locale = Nothing,
                    role = Nothing,
                    inviteeName = Nothing,
                    inviteeEmail = inviteeEmail,
                    allowExisting = isJust preExistingPersonalAccount
                  }

              args =
                RunAllEffectsArgs
                  { teams = Map.singleton tid [inviterMember],
                    initialUsers = [inviter] <> maybeToList existingPersonalAccount,
                    constGuardResult =
                      let domreg =
                            (mkDomainRegistration registeredDomain :: DomainRegistration)
                              { domainRedirect = domRegUpd.domainRedirect,
                                teamInvite = domRegUpd.teamInvite
                              }
                       in Just domreg
                  }

              -- run the test
              --
              outcome :: Either LocalErrors ()
              outcome = runAllEffects args . runTeamInvitationSubsystem cfg $ do
                void $ inviteUser inviterLuid tid invReq

              -- result invariants
              --
              teamNotAllowedOrWrongTeamIdFails =
                outcome === case domRegUpd.teamInvite of
                  Allowed -> Right ()
                  NotAllowed -> Left (ESubsystem TeamInvitationNotAllowedForEmail)
                  Team allowedTid ->
                    if allowedTid == tid
                      then Right ()
                      else Left (ESubsystem TeamInvitationNotAllowedForEmail)

              backendRedirectOrNoRegistrationFails = case domRegUpd.domainRedirect of
                Backend _ _ ->
                  -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
                  teamNotAllowedOrWrongTeamIdFails
                NoRegistration ->
                  if isJust preExistingPersonalAccount
                    then outcome === Left (ESubsystem TeamInvitationNotAllowedForEmail)
                    else teamNotAllowedOrWrongTeamIdFails
                _ -> teamNotAllowedOrWrongTeamIdFails

              counterexamples =
                counterexample (show domRegUpd)
                  . counterexample (show inviter)
                  . counterexample (show existingPersonalAccount)
           in counterexamples backendRedirectOrNoRegistrationFails

    prop "try to invite to blocked domain" $
      \(tid :: TeamId)
       (preExistingPersonalAccount :: Maybe StoredUser)
       (preExistingInviteeEmail :: EmailAddress)
       (inviterNoEmail :: StoredUser)
       (inviterEmail :: EmailAddress)
       (emailUsername :: EmailUsername)
       (blockedDomains :: NonEmptyList Domain) -> do
          let inviter =
                inviterNoEmail
                  { email = Just inviterEmail,
                    status = Just Active,
                    activated = True
                  } ::
                  StoredUser

          blockedEmailDomain <- anyElementOf blockedDomains

          let blockedEmailAddress :: EmailAddress =
                unsafeEmailAddress
                  ((fromString . getEmailUsername) emailUsername)
                  ((encodeUtf8 . domainText) blockedEmailDomain)

              invitationRequest =
                InvitationRequest
                  { locale = Nothing,
                    role = Nothing,
                    inviteeName = Nothing,
                    inviteeEmail = blockedEmailAddress,
                    allowExisting = False
                  }

              config =
                TeamInvitationSubsystemConfig
                  { maxTeamSize = 50,
                    teamInvitationTimeout = 3_000_000,
                    blockedDomains = (HashSet.fromList . getNonEmpty) blockedDomains
                  }

              inviterUid = inviter.id
              inviterLuid = toLocalUnsafe testDomain inviterUid
              inviterMember = mkTeamMember inviterUid fullPermissions Nothing UserLegalHoldDisabled

              existingPersonalAccount =
                preExistingPersonalAccount <&> \r ->
                  r
                    { email = Just preExistingInviteeEmail,
                      status = Just Active,
                      teamId = Nothing,
                      managedBy = Just ManagedByWire
                    } ::
                    StoredUser

              interpreterArgs =
                RunAllEffectsArgs
                  { teams = Map.singleton tid [inviterMember],
                    initialUsers = [inviter] <> maybeToList existingPersonalAccount,
                    constGuardResult = Nothing
                  }

              outcome :: Either LocalErrors ()
              outcome = runAllEffects interpreterArgs . runTeamInvitationSubsystem config $ do
                void $ inviteUser inviterLuid tid invitationRequest
           in pure $ outcome === Left (ESubsystem TeamInvitationBlockedDomain)
