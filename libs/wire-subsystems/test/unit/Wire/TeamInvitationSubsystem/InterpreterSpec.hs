{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.TeamInvitationSubsystem.InterpreterSpec (spec) where

import Data.Id
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

runAllEffects :: Sem AllEffects a -> Either TeamInvitationSubsystemError a
runAllEffects =
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
    . miniGalleyAPIAccess Nothing undefined
    . discardTinyLogs
    . runError

spec :: Spec
spec = do
  describe "InviteUser" $ do
    focus . prop "works (TODO: better description pls)" $
      \() ->
        let cfg =
              TeamInvitationSubsystemConfig
                { maxTeamSize = 50,
                  teamInvitationTimeout = 3_000_000
                }
            outcome = runAllEffects . runTeamInvitationSubsystem cfg $ do
              pure ()
         in outcome === Right ()
