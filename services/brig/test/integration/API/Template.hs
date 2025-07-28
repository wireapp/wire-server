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

testTeamInvitation :: FilePath -> Http ()
testTeamInvitation templateDir = do
  let locale = defaultLocale
      emailSender = fromJust $ emailAddressText "test@example.com"
      invitationEmailUrl = "https://example.com/invite"
      invitationEmailExistingUserUrl = "https://example.com/invite/existing"
      creatorWelcomeEmailUrl = "https://example.com/welcome/creator"
      memberWelcomeEmailUrl = "https://example.com/welcome/member"
      invitationEmailData =
        InvitationEmail
          { invTo = fromJust $ emailAddressText "test@example.com",
            invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
            invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
            invInviter = fromJust $ emailAddressText "inviter@example.com"
          }
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
    let (mail, _) = renderInvitationEmail invitationEmailData templates.invitationEmail id
    liftIO $ mail.mailFrom.addressEmail @?= (fromEmail emailSender)

defaultLocale :: Locale
defaultLocale =
  Locale
    { lLanguage = Language EN,
      lCountry = Nothing
    }
