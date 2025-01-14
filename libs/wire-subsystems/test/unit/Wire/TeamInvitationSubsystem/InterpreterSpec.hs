{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.TeamInvitationSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Id
import Data.LegalHold
import Data.Qualified
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
import Wire.API.Team.Invitation
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User
import Wire.EmailSubsystem
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error (EnterpriseLoginSubsystemError (EnterpriseLoginSubsystemGuardFailed))
import Wire.GalleyAPIAccess
import Wire.InvitationStore
import Wire.MockInterpreters
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Random
import Wire.TeamInvitationSubsystem
import Wire.TeamInvitationSubsystem.Error
import Wire.TeamInvitationSubsystem.Interpreter
import Wire.UserSubsystem

type AllEffects =
  [ Error TeamInvitationSubsystemError,
    EnterpriseLoginSubsystem,
    Error EnterpriseLoginSubsystemError,
    TinyLog,
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
  { teamOwner :: TeamMember,
    initialUsers :: [User]
  }
  deriving (Eq, Show)

runAllEffects :: RunAllEffectsArgs -> Sem AllEffects a -> Either EnterpriseLoginSubsystemError a
runAllEffects args =
  run
    . userSubsystemTestInterpreter args.initialUsers
    . evalState mempty
    . emailSubsystemInterpreter
    . evalState defaultTime
    . interpretNowAsState
    . evalState mempty
    . evalState mempty
    . inMemoryInvitationStoreInterpreter
    . evalState (mkStdGen 3) -- deterministic randomness, good for tests. :)
    . randomToStatefulStdGen
    . miniGalleyAPIAccess (Just args.teamOwner) def
    . discardTinyLogs
    . runError
    . enterpriseLoginSubsystemTestInterpreter
    . runErrorUnsafe @TeamInvitationSubsystemError

spec :: Spec
spec = do
  describe "InviteUser" $ do
    prop "calls guardEmailDomainRegistrationState if appropriate" $
      \preInviter tid inviterEmail inviteeEmail ->
        let cfg =
              TeamInvitationSubsystemConfig
                { maxTeamSize = 50,
                  teamInvitationTimeout = 3_000_000
                }
            invReq =
              InvitationRequest
                { locale = Nothing,
                  role = Nothing,
                  inviteeName = Nothing,
                  inviteeEmail = inviteeEmail,
                  allowExisting = False
                }
            inviter = preInviter {userIdentity = Just $ EmailIdentity inviterEmail}
            uid = qUnqualified inviter.userQualifiedId
            domain = qDomain inviter.userQualifiedId
            luid = toLocalUnsafe domain uid
            teamMember = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled
            args = RunAllEffectsArgs teamMember [inviter]
            outcome = runAllEffects args . runTeamInvitationSubsystem cfg $ do
              void $ inviteUser luid tid invReq
         in outcome === Left (EnterpriseLoginSubsystemGuardFailed "error")
