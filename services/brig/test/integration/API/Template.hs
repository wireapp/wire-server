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
  pure $
    testGroup
      "templates"
      [ test manager "team invitation email" $ testTeamInvitationEmail opts
      ]

testTeamInvitationEmail :: (HasCallStack) => Opts -> Http ()
testTeamInvitationEmail opts = do
  localizedTemplates <- liftIO $ loadTeamTemplates opts
  let allTemplates = uncurry Map.insert localizedTemplates.locDefault localizedTemplates.locOther
  for_ (Map.assocs allTemplates) $ \(loc, templates) -> do
    let tpl = templates.invitationEmail
        input =
          InvitationEmail
            { invTo = fromJust $ emailAddressText "test@example.com",
              invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
              invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
              invInviter = fromJust $ emailAddressText "inviter@example.com"
            }
    let (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl (genTemplateBrandingMap opts.emailSMS.general.templateBranding)
    liftIO $ do
      mail.mailFrom.addressEmail @?= (fromEmail tpl.invitationEmailSender)
      url @?= "http://127.0.0.1:8080/register?team=123e4567-e89b-12d3-a456-426614174000&team_code=ZoMX0xs="
      assertBool ("InvitationEmailTemplate[" <> show loc <> "] the following variables were not replaced: " <> show errs) (null errs)
