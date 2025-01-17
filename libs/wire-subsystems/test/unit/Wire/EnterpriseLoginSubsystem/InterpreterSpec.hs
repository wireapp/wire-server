module Wire.EnterpriseLoginSubsystem.InterpreterSpec where

import Data.ByteString.Builder (toLazyByteString)
import Data.Domain
import Data.Id
import Data.Misc (httpsUrl)
import Data.String.Conversions (cs)
import Data.Text.Lazy.Encoding qualified as LT
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State
import Polysemy.TinyLog
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import URI.ByteString (serializeURIRef)
import Wire.API.EnterpriseLogin
import Wire.API.User.EmailAddress (domainPart)
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

  prop "GuardEmailDomainRegistrationTeamInvitation" $
    \flow sameTeam teamId email preDomRegEntry ->
      let setTeamId :: DomainRegistrationUpdate -> TeamId -> DomainRegistrationUpdate
          setTeamId update tid = case update.teamInvite of
            Team _ -> DomainRegistrationUpdate update.domainRedirect (Team tid)
            _ -> update

          domRegEntry = if sameTeam then setTeamId preDomRegEntry teamId else preDomRegEntry

          outcome = runDependencies . runEnterpriseLoginSubsystem $ do
            updateDomainRegistration (Domain . cs $ domainPart email) domRegEntry
            guardEmailDomainRegistrationTeamInvitation flow teamId email

          teamNotAllowedOrWrongTeamIdFails = case domRegEntry.teamInvite of
            Allowed -> outcome === Right ()
            NotAllowed -> outcome === Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is set to `not-allowed`")
            Team allowedTid ->
              if allowedTid == teamId
                then outcome === Right ()
                else outcome === Left (EnterpriseLoginSubsystemGuardFailed "`teamInvite` is restricted to another team.")

          backendRedirectOrNoRegistrationFails = case domRegEntry.domainRedirect of
            Backend _ ->
              -- if domain-redirect is set to `backend`, then team-invite must be set to `not-allowed`
              teamNotAllowedOrWrongTeamIdFails
            NoRegistration ->
              case flow of
                ExistingUser -> outcome === Left (EnterpriseLoginSubsystemGuardFailed "`domain_redirect` is set to `no-registration`")
                NewUser -> teamNotAllowedOrWrongTeamIdFails
            _ -> teamNotAllowedOrWrongTeamIdFails
       in backendRedirectOrNoRegistrationFails

  focus . prop "GuardEmailDomainRegistrationRegister" $
    \email domRegEntry ->
      let outcome = runDependencies . runEnterpriseLoginSubsystem $ do
            updateDomainRegistration (Domain . cs $ domainPart email) domRegEntry
            guardEmailDomainRegistrationRegister email
          expected = case domRegEntry.domainRedirect of
            None -> Right ()
            Locked -> Right ()
            SSO _ -> Left $ EnterpriseLoginSubsystemGuardFailed "`domain_redirect` is set to `sso:{code}`"
            Backend url -> Left $ EnterpriseLoginSubsystemGuardFailed $ "TODO: dummy text: url=" <> (LT.decodeUtf8 . toLazyByteString . serializeURIRef . httpsUrl) url
            NoRegistration -> Left $ EnterpriseLoginSubsystemGuardFailed "`domain_redirect` is set to `no_registration`"
            PreAuthorized -> Right ()
       in outcome === expected
