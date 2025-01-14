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
    UserSubsystem,
    EnterpriseLoginSubsystem
  ]

data RunAllEffectsArgs = RunAllEffectsArgs
  { teamOwner :: TeamMember
  }
  deriving (Eq, Show)

runAllEffects :: RunAllEffectsArgs -> Sem AllEffects a -> Either TeamInvitationSubsystemError a
runAllEffects args =
  run
    . enterpriseLoginSubsystemTestInterpreter
    . userSubsystemTestInterpreter []
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

spec :: Spec
spec = do
  describe "InviteUser" $ do
    prop "calls guardEmailDomainRegistrationState if appropriate" $
      \uid tid email ->
        let cfg =
              TeamInvitationSubsystemConfig
                { maxTeamSize = 50,
                  teamInvitationTimeout = 3_000_000
                }
            invreq =
              InvitationRequest
                { locale = Nothing,
                  role = Nothing,
                  inviteeName = Nothing,
                  inviteeEmail = email,
                  allowExisting = False
                }
            teamMember = mkTeamMember (tUnqualified uid) fullPermissions Nothing UserLegalHoldDisabled
            args = RunAllEffectsArgs teamMember
            outcome = runAllEffects args . runTeamInvitationSubsystem cfg $ do
              void $ inviteUser uid tid invreq
         in outcome === Right () -- TODO: should be some Left.
