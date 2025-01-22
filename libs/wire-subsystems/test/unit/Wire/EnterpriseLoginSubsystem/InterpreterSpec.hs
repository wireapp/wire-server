module Wire.EnterpriseLoginSubsystem.InterpreterSpec where

import Data.Default
import Data.Domain
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wire.API.EnterpriseLogin
import Wire.API.User.EmailAddress (domainPart)
import Wire.DomainRegistrationStore
import Wire.DomainVerificationChallengeStore
import Wire.EmailSending
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error
import Wire.EnterpriseLoginSubsystem.Interpreter
import Wire.GalleyAPIAccess
import Wire.MockInterpreters.DomainRegistrationStore
import Wire.MockInterpreters.DomainVerificationChallengeStore
import Wire.MockInterpreters.EmailSending
import Wire.MockInterpreters.Error
import Wire.MockInterpreters.GalleyAPIAccess
import Wire.MockInterpreters.Random
import Wire.MockInterpreters.SparAPIAccess
import Wire.MockInterpreters.UserKeyStore
import Wire.MockInterpreters.UserSubsystem
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Random
import Wire.SparAPIAccess
import Wire.UserKeyStore
import Wire.UserSubsystem

runDependencies ::
  Sem
    '[ DomainRegistrationStore,
       DomainVerificationChallengeStore,
       (Error EnterpriseLoginSubsystemError),
       (Error ParseException),
       GalleyAPIAccess,
       SparAPIAccess,
       TinyLog,
       (Input EnterpriseLoginSubsystemConfig),
       EmailSending,
       Random,
       Rpc,
       UserKeyStore,
       UserSubsystem
     ]
    a ->
  Either EnterpriseLoginSubsystemError a
runDependencies =
  run
    . userSubsystemTestInterpreter []
    . (evalState mempty . inMemoryUserKeyStoreInterpreter . raiseUnder)
    . fakeRpc
    . runRandomPure
    . noopEmailSendingInterpreter
    . runInputConst
      ( EnterpriseLoginSubsystemConfig
          Nothing
          (error "undefined wire-server-enterprise endpoint")
      )
    . discardTinyLogs
    . miniSparAPIAccess
    . miniGalleyAPIAccess Nothing def
    . runErrorUnsafe
    . runError
    . (evalState mempty . inMemoryDomainVerificationChallengeStoreInterpreter . raiseUnder)
    . (evalState mempty . inMemoryDomainRegistrationStoreInterpreter . raiseUnder)

fakeRpc :: InterpreterFor Rpc r
fakeRpc = interpret $ \case
  Rpc {} -> error "Rpc not implemented"
  RpcWithRetries {} -> error "RpcWithRetries not implemented"

spec :: Spec
spec = describe "EnterpriseLoginSubsystem" $ do
  it "LockDomain" pending
  it "UnlockDomain" pending
  it "PreAuthorizeDomain" pending
  it "UnAuthorizeDomain" pending
  it "UpdateDomainRegistration" pending
  it "DeleteDomain" pending

  -- TODO: Migrate the tests below to TeamInvitationSubsystemSpec

  -- prop "GuardEmailDomainRegistrationTeamInvitation" $
  --   \flow sameTeam teamId email preDomRegEntry ->
  --     let setTeamId :: DomainRegistrationUpdate -> TeamId -> DomainRegistrationUpdate
  --         setTeamId update tid = case update.teamInvite of
  --           Team _ -> DomainRegistrationUpdate update.domainRedirect (Team tid)
  --           _ -> update

  --         domRegEntry = if sameTeam then setTeamId preDomRegEntry teamId else preDomRegEntry

  --         outcome = runDependencies . runEnterpriseLoginSubsystem $ do
  --           updateDomainRegistration (Domain . cs $ domainPart email) domRegEntry
  --           guardEmailDomainRegistrationTeamInvitation flow teamId email

  --         teamNotAllowedOrWrongTeamIdFails = case domRegEntry.teamInvite of
  --           Allowed -> outcome === Right ()
  --           NotAllowed -> outcome === Left (EnterpriseLoginSubsystemGuardFailed TeamInviteSetToNotAllowed)
  --           Team allowedTid ->
  --             if allowedTid == teamId
  --               then outcome === Right ()
  --               else outcome === Left (EnterpriseLoginSubsystemGuardFailed TeamInviteRestrictedToOtherTeam)

  --         backendRedirectOrNoRegistrationFails = case domRegEntry.domainRedirect of
  --           Backend _ ->
  --             -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
  --             teamNotAllowedOrWrongTeamIdFails
  --           NoRegistration ->
  --             case flow of
  --               ExistingUser -> outcome === Left (EnterpriseLoginSubsystemGuardFailed DomRedirSetToNoRegistration)
  --               NewUser -> teamNotAllowedOrWrongTeamIdFails
  --           _ -> teamNotAllowedOrWrongTeamIdFails
  --      in backendRedirectOrNoRegistrationFails

  -- TODO: Migrate somewhere else?
  prop "GuardEmailDomainRegistrationRegister" $
    \email domRegEntry ->
      let outcome = runDependencies . runEnterpriseLoginSubsystem $ do
            updateDomainRegistration (Domain . cs $ domainPart email) domRegEntry
            guardEmailDomainRegistrationRegister email
          expected = case domRegEntry.domainRedirect of
            None -> Right ()
            Locked -> Right ()
            SSO _ -> Left $ EnterpriseLoginSubsystemGuardFailed DomRedirSetToSSO
            Backend _ -> Left $ EnterpriseLoginSubsystemGuardFailed DomRedirSetToBackend
            NoRegistration -> Left $ EnterpriseLoginSubsystemGuardFailed DomRedirSetToNoRegistration
            PreAuthorized -> Right ()
       in outcome === expected
