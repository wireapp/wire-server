{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-ambiguous-fields #-}

module Wire.TeamInvitationSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Domain
import Data.HashSet qualified as HashSet
import Data.Id
import Data.LegalHold
import Data.Map qualified as Map
import Data.Qualified
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
import Wire.API.Team.Invitation
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User
import Wire.EmailSubsystem
import Wire.EnterpriseLoginSubsystem
import Wire.GalleyAPIAccess
import Wire.InvitationStore
import Wire.MockInterpreters
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Random
import Wire.TeamInvitationSubsystem
import Wire.TeamInvitationSubsystem.Error
import Wire.TeamInvitationSubsystem.Interpreter
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI
import Wire.UserSubsystem
import Wire.Util

type AllEffects =
  [ Error TeamInvitationSubsystemError,
    EnterpriseLoginSubsystem,
    TinyLog,
    TeamSubsystem,
    GalleyAPIAccess,
    Random,
    State StdGen,
    InvitationStore,
    State (Map (TeamId, InvitationId) StoredInvitation),
    State (Map (InvitationCode) StoredInvitation),
    Now,
    State UTCTime,
    EmailSubsystem,
    State (Map EmailAddress [SentMail]),
    UserSubsystem
  ]

data RunAllEffectsArgs = RunAllEffectsArgs
  { teams :: Map TeamId [TeamMember],
    initialUsers :: [User],
    constGuardResult :: Maybe DomainRegistration
  }
  deriving (Eq, Show)

runAllEffects :: RunAllEffectsArgs -> Sem AllEffects a -> Either TeamInvitationSubsystemError a
runAllEffects args =
  run
    . userSubsystemTestInterpreter args.initialUsers
    . evalState mempty
    . noopEmailSubsystemInterpreter
    . evalState defaultTime
    . interpretNowAsState
    . evalState mempty
    . evalState mempty
    . inMemoryInvitationStoreInterpreter
    . evalState (mkStdGen 3)
    . randomToStatefulStdGen
    . miniGalleyAPIAccess args.teams def
    . intepreterTeamSubsystemToGalleyAPI
    . discardTinyLogs
    . enterpriseLoginSubsystemTestInterpreter args.constGuardResult
    . runError

spec :: Spec
spec = do
  describe "InviteUser" $ do
    prop "honors dommain config from `brig.domain_registration`" $
      \(tid :: TeamId)
       (preDomRegUpd :: DomainRegistrationUpdate)
       (preInviter :: User)
       (inviterEmail :: EmailAddress)
       (inviteeEmail :: EmailAddress)
       (preExistingPersonalAccount :: Maybe User)
       (preRegisteredDomain {- if Nothing, use invitee's email domain -} :: Maybe Domain)
       (sameTeam {- team id matches the team id in the domain registration -} :: Bool) ->
          let -- prepare the pre* prop args
              --
              domRegUpd = preDomRegUpd & if sameTeam then setTeamId else id
                where
                  setTeamId upd = case upd.teamInvite of
                    Team _ -> DomainRegistrationUpdate upd.domainRedirect (Team tid)
                    _ -> upd

              inviter = preInviter {userIdentity = Just $ EmailIdentity inviterEmail}

              existingPersonalAccount =
                preExistingPersonalAccount <&> \r ->
                  r
                    { userIdentity = Just $ EmailIdentity inviteeEmail,
                      userStatus = Active,
                      userTeam = Nothing,
                      userManagedBy = ManagedByWire
                    }

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

              inviterUid = qUnqualified inviter.userQualifiedId
              inviterLuid = let domain = qDomain inviter.userQualifiedId in toLocalUnsafe domain inviterUid
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
              outcome :: Either TeamInvitationSubsystemError ()
              outcome = runAllEffects args . runTeamInvitationSubsystem cfg $ do
                void $ inviteUser inviterLuid tid invReq

              -- result invariants
              --
              teamNotAllowedOrWrongTeamIdFails =
                outcome === case domRegUpd.teamInvite of
                  Allowed -> Right ()
                  NotAllowed -> Left TeamInvitationNotAllowedForEmail
                  Team allowedTid ->
                    if allowedTid == tid
                      then Right ()
                      else Left TeamInvitationNotAllowedForEmail

              backendRedirectOrNoRegistrationFails = case domRegUpd.domainRedirect of
                Backend _ _ ->
                  -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
                  teamNotAllowedOrWrongTeamIdFails
                NoRegistration ->
                  if isJust preExistingPersonalAccount
                    then outcome === Left TeamInvitationNotAllowedForEmail
                    else teamNotAllowedOrWrongTeamIdFails
                _ -> teamNotAllowedOrWrongTeamIdFails

              counterexamples =
                counterexample (show domRegUpd)
                  . counterexample (show inviter)
                  . counterexample (show existingPersonalAccount)
           in counterexamples backendRedirectOrNoRegistrationFails

    prop "try to invite to blocked domain" $
      \(tid :: TeamId)
       (preExistingPersonalAccount :: Maybe User)
       (preExistingInviteeEmail :: EmailAddress)
       (emailUsername :: EmailUsername)
       (blockedDomains :: NonEmptyList Domain) -> do
          let hasEmailIdentity user = isJust $ emailIdentity =<< userIdentity user

          blockedEmailDomain <- anyElementOf blockedDomains
          inviter <- arbitrary @User `suchThat` hasEmailIdentity

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

              inviterUid = qUnqualified inviter.userQualifiedId
              inviterLuid = let domain = qDomain inviter.userQualifiedId in toLocalUnsafe domain inviterUid
              inviterMember = mkTeamMember inviterUid fullPermissions Nothing UserLegalHoldDisabled

              existingPersonalAccount =
                preExistingPersonalAccount <&> \r ->
                  r
                    { userIdentity = Just $ EmailIdentity preExistingInviteeEmail,
                      userStatus = Active,
                      userTeam = Nothing,
                      userManagedBy = ManagedByWire
                    }

              interpreterArgs =
                RunAllEffectsArgs
                  { teams = Map.singleton tid [inviterMember],
                    initialUsers = [inviter] <> maybeToList existingPersonalAccount,
                    constGuardResult = Nothing
                  }

              outcome :: Either TeamInvitationSubsystemError ()
              outcome = runAllEffects interpreterArgs . runTeamInvitationSubsystem config $ do
                void $ inviteUser inviterLuid tid invitationRequest
           in pure $ outcome === Left TeamInvitationBlockedDomain
