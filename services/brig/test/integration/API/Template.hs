module API.Template (tests) where

import Bilge
import Brig.Options
import Brig.Team.Template (loadTeamTemplates)
import Brig.Template
import Data.Id
import Data.Map qualified as Map
import Data.Text.Ascii (AsciiChars (validate))
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.EmailAddress
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template

tests :: Opts -> Manager -> IO TestTree
tests opts manager = do
  localizedTemplates <- liftIO $ loadTeamTemplates opts
  let allTemplates = uncurry Map.insert localizedTemplates.locDefault localizedTemplates.locOther
  pure $
    testGroup
      "email templates"
      ( do
          (loc, templates) <- Map.assocs allTemplates
          let branding = genTemplateBrandingMap opts.emailSMS.general.templateBranding
              input =
                InvitationEmail
                  { invTo = fromJust $ emailAddressText "test@example.com",
                    invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
                    invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
                    invInviter = fromJust $ emailAddressText "inviter@example.com"
                  }
          [ test manager ("[" <> show loc <> "] team invitation") $ testTeamInvitationEmail branding input templates,
            test manager ("[" <> show loc <> "] team invitation existing user") $ testTeamInvitationEmailExistingUser branding input templates,
            test manager ("[" <> show loc <> "] member welcome") $ testMemberWelcomeEmail branding templates
            ]
      )

testTeamInvitationEmailExistingUser :: (HasCallStack) => Map Text Text -> InvitationEmail -> TeamTemplates -> Http ()
testTeamInvitationEmailExistingUser branding input templates = do
  liftIO $ do
    let tpl = templates.existingUserInvitationEmail
        (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
    mail.mailFrom.addressEmail @?= (fromEmail tpl.invitationEmailSender)
    url @?= "http://127.0.0.1:8080/accept-invitation?team-code=ZoMX0xs="
    assertBool ("The following variables were not replaced: " <> show errs) (null errs)

testTeamInvitationEmail :: (HasCallStack) => Map Text Text -> InvitationEmail -> TeamTemplates -> Http ()
testTeamInvitationEmail branding input templates = do
  liftIO $ do
    let tpl = templates.invitationEmail
        (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
    mail.mailFrom.addressEmail @?= (fromEmail tpl.invitationEmailSender)
    url @?= "http://127.0.0.1:8080/register?team=123e4567-e89b-12d3-a456-426614174000&team_code=ZoMX0xs="
    assertBool ("The following variables were not replaced: " <> show errs) (null errs)

testMemberWelcomeEmail :: (HasCallStack) => Map Text Text -> TeamTemplates -> Http ()
testMemberWelcomeEmail branding templates = do
  liftIO $ do
    let tpl = templates.memberWelcomeEmail
        to = fromJust $ emailAddressText "test@example.com"
        tid = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")
        tname = "funky team"
        (errs, _) = run $ runOutputList @Text $ renderMemberWelcomeMail to tid tname tpl branding
    assertBool ("The following variables were not replaced: " <> show errs) (null errs)
