module Wire.EmailSubsystem.TemplateSpec (spec) where

import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.Map as Map
import Data.Text.Ascii (AsciiChars (validate))
import Data.UUID as UUID
import Imports
import Network.Mail.Mime
import System.FilePath
import Test.Hspec
import Text.Mustache
import Text.Mustache.Render (SubstitutionError (VariableNotFound))
import Wire.API.Locale
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.EmailAddress
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template

data Hello = Hello
  { name :: Text
  }
  deriving (Show, Generic)

instance ToMustache Hello where
  toMustache (Hello {name}) =
    object
      [ "name" ~> name
      ]

spec :: Spec
spec = do
  describe "test mustache" $ do
    it "renders mustache template" $ do
      let templateText = "Hello, ${name}!${foo}"
          res = compileTemplate "example" ("{{= ${ } =}}" <> templateText)
      case res of
        Left err -> print err
        Right tpl ->
          let (errs, rendered) = checkedSubstitute tpl (Hello {name = "<b>World</b>"})
           in do
                rendered `shouldBe` "Hello, &lt;b&gt;World&lt;/b&gt;!"
                case errs of
                  [VariableNotFound ["foo"]] -> pure ()
                  _ -> expectationFailure $ "Unexpected errors: " <> show errs
  describe "loadTeamTemplates" $ do
    it "loads team templates" $ do
      currentDir <- getCurrentDirectory
      let locale = defaultLocale
          relativePath = "../../services/brig/deb/opt/brig/templates"
          templateDir = currentDir </> relativePath
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
        mail.mailFrom.addressEmail `shouldBe` (fromEmail emailSender)

defaultLocale :: Locale
defaultLocale =
  Locale
    { lLanguage = Language EN,
      lCountry = Nothing
    }
