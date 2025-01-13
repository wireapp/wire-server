{-# LANGUAGE TemplateHaskell #-}

module Wire.EnterpriseLoginSubsystemSpec where

import Data.Domain
import Data.Id
import Polysemy
import Text.Email.Parser
import Wire.API.EnterpriseLogin
import Wire.API.EnterpriseLogin.Interpreter
import Wire.MockInterpreters.DomainRegistrationStore
import Wire.MockInterpreters.EmailSubsystem
import Wire.Sem.Logger.TinyLog


runDependencies :: Sem '[  DomainRegistrationStore,
       Error EnterpriseLoginSubsystemError,
       TinyLog,
       Input (Maybe EnterpriseLoginSubsystemConfig),
       EmailSending
    ] a -> Either e a
runDependencies =
  run
    . emailSubsystemInterpreter
    . runInput (pure Nothing)
    . discardTinyLogs
    . runError
    . inMemoryDomainRegistrationStoreInterpreter

spec :: Spec
spec = describe "EnterpriseLoginSubsystem" $ do
  describe "LockDomain" pending
  describe "UnlockDomain" pending
  describe "PreAuthorizeDomain" pending
  describe "UnAuthorizeDomain" pending
  describe "UpdateDomainRegistration" pending
  describe "DeleteDomain" pending

  describe "GuardEmailDomainRegistrationState" $ do
    let testTeamInvitation fun = it "team-invitation" $ do
          outcome <- runDependencies . runEnterpriseLoginSubsystem $ do
            upsert
            fun
          case (.teamInvite) <$> outcome of
            Allowed -> ok
            NotAllowed -> nope "`teamInvite` is set to `not-allowed`"
            Team allowedTid ->
              if allowedTid == tid
                then ok
                else nope $ "`teamInvite` is restricted to another team."

        anyTeam = Id "74d17a68-d196-11ef-934c-2b0e41bf5418"
        anyEmailAddress = unsafeEmailAddress "me" "example.com"

    context "invite new user" $ do
      let fun = guardEmailDomainRegistrationState NewUser         anyTeam         anyEmailAddress

      it "no entry in enterprise login table -> ok" $ do
        fun `shouldReturn ()

      it "domain-redirect" $ for [minBound ..] \_ -> do
        -- write to mock table or something.  (also team invite entry)
        fun `shouldReturn` ()
      testTeamInvitation fun

    context "invite existing user" $ do
      let fun = guardEmailDomainRegistrationState NewUser         anyTeam         anyEmailAddress
      it "no entry in enterprise login table -> ok" $ pending
      it "domain-redirect"
        $ for [minBound ..] \domRegTag -> do
          -- write to mock table or something.  (also team invite entry)
          case domRegTag of
            None -> ok
            Locked -> ok
            SSO _ -> ok
            Backend _ -> ok
            NoRegistration -> nope "`domain_redirect` is set to `no-registration`"
            PreAuthorized -> ok
      testTeamInvitation fun
