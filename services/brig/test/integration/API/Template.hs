module API.Template (tests) where

import Bilge
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.Map as Map
import Data.Text.Ascii (AsciiChars (validate))
import Data.UUID as UUID
import Imports
import Network.Mail.Mime
import Test.Tasty
import Test.Tasty.HUnit
import Text.Mustache
import Util
import Wire.API.Locale
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.EmailAddress
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template

tests :: FilePath -> Manager -> IO TestTree
tests templateDir manager = do
  pure $
    testGroup
      "templates"
      [ test manager "team invitation email" $ testTeamInvitation templateDir
      ]

testTeamInvitation :: (HasCallStack) => FilePath -> Http ()
testTeamInvitation templateDir = do
  let locale = defaultLocale
      emailSender = fromJust $ emailAddressText "inviter@example.com"
      invitationEmailUrl = "https://example.com/invite?teamId=${team}&code=${code}"
      invitationEmailExistingUserUrl = "https://example.com/invite/existing"
      creatorWelcomeEmailUrl = "https://example.com/welcome/creator"
      memberWelcomeEmailUrl = "https://example.com/welcome/member"
  result <-
    liftIO $
      loadTeamTemplates
        locale
        templateDir
        emailSender
        invitationEmailUrl
        invitationEmailExistingUserUrl
        creatorWelcomeEmailUrl
        memberWelcomeEmailUrl
  let allTemplates = uncurry Map.insert result.locDefault result.locOther
  for_ (Map.assocs allTemplates) $ \(_, templates) -> do
    let tpl = templates.invitationEmail
        input =
          InvitationEmailInput
            { branding = brandingOpts,
              invUrlTemplate = tpl.invitationEmailUrl,
              invTo = fromJust $ emailAddressText "test@example.com",
              invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
              invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
              invInviter = emailSender
            }
        (mail, url) = renderInvitationEmail input tpl
    checkInvitationEmailTemplate input tpl
    liftIO $ do
      mail.mailFrom.addressEmail @?= (fromEmail emailSender)
      url @?= "https://example.com/invite?teamId=123e4567-e89b-12d3-a456-426614174000&code=ZoMX0xs="

checkInvitationEmailTemplate :: InvitationEmailInput -> InvitationEmailTemplate -> Http ()
checkInvitationEmailTemplate input tpl = do
  let (textErrs, _) = checkedSubstitute tpl.invitationEmailBodyText input
      (htmlErrs, _) = checkedSubstitute tpl.invitationEmailBodyHtml input
      (subjErrs, _) = checkedSubstitute tpl.invitationEmailSubject input
  liftIO $ do
    case textErrs of
      [] -> pure ()
      errs -> assertFailure $ "Text substitution errors: " <> show errs
    case htmlErrs of
      [] -> pure ()
      errs -> assertFailure $ "HTML substitution errors: " <> show errs
    case subjErrs of
      [] -> pure ()
      errs -> assertFailure $ "Subject substitution errors: " <> show errs

defaultLocale :: Locale
defaultLocale =
  Locale
    { lLanguage = Language EN,
      lCountry = Nothing
    }

brandingOpts :: BrandingOpts
brandingOpts =
  BrandingOpts
    { brand = "Wire",
      brandUrl = "https://wire.com",
      brandLabelUrl = "wire.com",
      brandLogoUrl = "https://wire.com/p/img/email/logo-email-black.png",
      brandService = "Wire Service Provider",
      copyright = "© WIRE SWISS GmbH",
      misuse = "misuse@wire.com",
      legal = "https://wire.com/legal/",
      forgot = "https://wire.com/forgot/",
      support = "https://support.wire.com/"
    }
