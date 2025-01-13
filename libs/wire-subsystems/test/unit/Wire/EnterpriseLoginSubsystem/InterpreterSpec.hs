module Wire.EnterpriseLoginSubsystem.InterpreterSpec where

import Data.Domain
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Wire.API.EnterpriseLogin
import Wire.API.User.EmailAddress
import Wire.DomainRegistrationStore
import Wire.EmailSending
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error
import Wire.EnterpriseLoginSubsystem.Interpreter
import Wire.MockInterpreters.DomainRegistrationStore
import Wire.Sem.Logger.TinyLog

runDependencies ::
  Sem
    '[ DomainRegistrationStore,
       Error EnterpriseLoginSubsystemError,
       TinyLog,
       Input (Maybe EnterpriseLoginSubsystemConfig),
       EmailSending
     ]
    a ->
  Either EnterpriseLoginSubsystemError a
runDependencies =
  run
    . emailSendingInterpreter
    . runInputConst Nothing
    . discardTinyLogs
    . runError
    . evalState mempty
    . inMemoryDomainRegistrationStoreInterpreter
    . raiseUnder

emailSendingInterpreter :: InterpreterFor EmailSending r
emailSendingInterpreter =
  interpret \case
    SendMail _ -> pure ()

spec :: Spec
spec = describe "EnterpriseLoginSubsystem" $ do
  it "LockDomain" pending
  it "UnlockDomain" pending
  it "PreAuthorizeDomain" pending
  it "UnAuthorizeDomain" pending
  it "UpdateDomainRegistration" pending
  it "DeleteDomain" pending

  describe "GuardEmailDomainRegistrationState" $ do
    let testTeamInvitation sut = it "team-invitation" $ do
          let teamInvites = [Allowed, NotAllowed, Team anyTeam, Team otherTeam]
          for_ teamInvites \teamInvite -> do
            let update = DomainRegistrationUpdate {teamInvite = teamInvite, domainRedirect = None}
                outcome = runDependencies . runEnterpriseLoginSubsystem $ do
                  updateDomainRegistration (Domain "example.com") update
                  sut
            case teamInvite of
              Allowed -> outcome `shouldBe` Right ()
              NotAllowed -> outcome `shouldBe` Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is set to `not-allowed`")
              Team allowedTid ->
                if allowedTid == anyTeam
                  then outcome `shouldBe` Right ()
                  else outcome `shouldBe` Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is restricted to another team.")

        anyTeam = Id (fromMaybe (error "invalid uuid") $ UUID.fromString "74d17a68-d196-11ef-934c-2b0e41bf5418")
        otherTeam = Id (fromMaybe (error "invalid uuid") $ UUID.fromString "74d17a68-d196-11ef-934c-2b0e41bf5419")
        anyEmailAddress = unsafeEmailAddress "me" "example.com"
        domainRedirects =
          [ None,
            Locked,
            SSO undefined,
            Backend undefined,
            NoRegistration,
            PreAuthorized
          ]
    context "invite new user" $ do
      let sut = guardEmailDomainRegistrationState NewUser anyTeam anyEmailAddress

      it "no entry in enterprise login table -> ok" $ do
        (runDependencies . runEnterpriseLoginSubsystem) sut `shouldBe` Right ()

      it "domain-redirect" $ do
        for_ domainRedirects \domReg -> do
          let teamInvite = case domReg of
                -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
                Backend _ -> NotAllowed
                _ -> Allowed
              update = DomainRegistrationUpdate {teamInvite = teamInvite, domainRedirect = domReg}
          let outcome = runDependencies . runEnterpriseLoginSubsystem $ do
                updateDomainRegistration (Domain "example.com") update
                sut
          case domReg of
            None -> outcome `shouldBe` Right ()
            Locked -> outcome `shouldBe` Right ()
            SSO _ -> outcome `shouldBe` Right ()
            Backend _ -> outcome `shouldBe` Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is set to `not-allowed`")
            NoRegistration -> outcome `shouldBe` Right ()
            PreAuthorized -> outcome `shouldBe` Right ()

      testTeamInvitation sut

    context "invite existing user" $ do
      let sut = guardEmailDomainRegistrationState ExistingUser anyTeam anyEmailAddress

      it "no entry in enterprise login table -> ok" $ do
        (runDependencies . runEnterpriseLoginSubsystem) sut `shouldBe` Right ()

      it "domain-redirect" $ do
        for_ domainRedirects \domReg -> do
          let teamInvite = case domReg of
                -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
                Backend _ -> NotAllowed
                _ -> Allowed
              update = DomainRegistrationUpdate {teamInvite = teamInvite, domainRedirect = domReg}
          let outcome = runDependencies . runEnterpriseLoginSubsystem $ do
                updateDomainRegistration (Domain "example.com") update
                sut
          case domReg of
            None -> outcome `shouldBe` Right ()
            Locked -> outcome `shouldBe` Right ()
            SSO _ -> outcome `shouldBe` Right ()
            Backend _ -> outcome `shouldBe` Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is set to `not-allowed`")
            NoRegistration -> outcome `shouldBe` Left (EnterpriseLoginSubsystemGuardFailed "`domain_redirect` is set to `no-registration`")
            PreAuthorized -> outcome `shouldBe` Right ()

      testTeamInvitation $ sut
