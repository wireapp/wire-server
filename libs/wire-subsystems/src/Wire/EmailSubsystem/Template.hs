{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.EmailSubsystem.Template
  ( module Wire.EmailSubsystem.Template,
    module Wire.EmailSubsystem.Templates.Team,
    module Wire.EmailSubsystem.Templates.User,

    -- * Re-exports
    Template,
    template,
  )
where

import Control.Exception (catchJust)
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as Lazy
import Data.Text.Template
import HTMLEntities.Text qualified as HTML
import Imports hiding (readFile)
import System.IO.Error (isDoesNotExistError)
import Text.Mustache (compileTemplate)
import Text.Mustache qualified as Mustache
import Wire.API.Locale
import Wire.API.User
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

data BrandingOpts = BrandingOpts
  { brand :: !Text,
    brandUrl :: !Text,
    brandLabelUrl :: !Text,
    brandLogoUrl :: !Text,
    brandService :: !Text,
    copyright :: !Text,
    misuse :: !Text,
    legal :: !Text,
    forgot :: !Text,
    support :: !Text
  }
  deriving (Show, Generic)

instance FromJSON BrandingOpts

-- | Function to be applied everywhere where email/sms/call
-- templating is used (ensures that placeholders are replaced
-- by the appropriate branding, typically Wire)
genTemplateBranding :: BrandingOpts -> TemplateBranding
genTemplateBranding
  BrandingOpts
    { brand,
      brandUrl,
      brandLabelUrl,
      brandLogoUrl,
      brandService,
      copyright,
      misuse,
      legal,
      forgot,
      support
    } = fn
    where
      fn "brand" = brand
      fn "brand_url" = brandUrl
      fn "brand_label_url" = brandLabelUrl
      fn "brand_logo" = brandLogoUrl
      fn "brand_service" = brandService
      fn "copyright" = copyright
      fn "misuse" = misuse
      fn "legal" = legal
      fn "forgot" = forgot
      fn "support" = support
      fn other = other

-- | Lookup a localised item from a 'Localised' structure.
forLocale ::
  -- | 'Just' the preferred locale or 'Nothing' for
  -- the default locale.
  Maybe Locale ->
  -- | The 'Localised' structure.
  Localised a ->
  -- | Pair of the effectively chosen locale and the
  -- associated value.
  (Locale, a)
forLocale pref t = case pref of
  Just l -> fromMaybe (locDefault t) (select l)
  Nothing -> locDefault t
  where
    select l =
      let l' = l {lCountry = Nothing}
          loc = Map.lookup l (locOther t)
          lan = Map.lookup l' (locOther t)
       in (l,) <$> loc <|> (l',) <$> lan

-- | See 'genTemplateBranding'.
type TemplateBranding = Text -> Text

-- | Localised templates.
data Localised a = Localised
  { locDefault :: (Locale, a),
    locOther :: Map Locale a
  }

-- | Uses a replace and a branding function, to replaces all placeholders from the
-- given template to produce a Text. To be used on plain text templates
renderText :: Template -> (Text -> Text) -> Lazy.Text
renderText tpl replace = render tpl replace

-- | Uses a replace and a branding function, to replaces all placeholders from the
-- given template to produce a Text. To be used on plain text templates
renderTextWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderTextWithBranding tpl replace branding = render tpl (replace . branding)

-- | Uses a replace and a branding function to replace all placeholders from the
-- given template to produce a Text. To be used on HTML templates
renderHtmlWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderHtmlWithBranding tpl replace branding = render tpl (HTML.text . replace . branding)

readLocalesDir ::
  -- | Default locale.
  Locale ->
  -- | Base directory.
  FilePath ->
  -- | Template directory (user, provider, team)
  FilePath ->
  -- | Handler to load the templates for a locale.
  (FilePath -> IO a) ->
  IO (Localised a)
readLocalesDir defLocale base typ load = do
  def <- load (basePath defLocaleDir)
  Localised (defLocale, def) <$> do
    -- Ignore locales if no such directory exist for the locale
    ls <-
      filterM (doesDirectoryExist . basePath)
        . filter (/= defLocaleDir)
        =<< listDirectory base
    Map.fromList . zip (map readLocale ls) <$> mapM (load . basePath) ls
  where
    basePath :: FilePath -> FilePath
    basePath loc = base <> "/" <> loc <> "/" <> typ
    defLocaleDir :: FilePath
    defLocaleDir = unpack (locToText defLocale)
    readLocale :: FilePath -> Locale
    readLocale l =
      fromMaybe (error ("Invalid locale: " ++ show l)) $
        parseLocale (pack l)

readTemplateWithDefault ::
  FilePath ->
  Locale ->
  FilePath ->
  String ->
  FilePath ->
  IO Template
readTemplateWithDefault = readWithDefault readTemplate

readTemplate :: FilePath -> IO Template
readTemplate f = template <$> readText f

readFile :: FilePath -> IO Text
readFile f = T.decodeUtf8 <$> BS.readFile f

readTextWithDefault ::
  FilePath ->
  Locale ->
  FilePath ->
  String ->
  FilePath ->
  IO Text
readTextWithDefault = readWithDefault readText

readText :: FilePath -> IO Text
readText f =
  catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (readFile f)
    (\_ -> error $ "Missing file: '" ++ f)

readWithDefault ::
  (String -> IO a) ->
  FilePath ->
  Locale ->
  FilePath ->
  String ->
  FilePath ->
  IO a
readWithDefault readFn baseDir defLoc typ prefix name = do
  exists <- doesFileExist fileToLoad
  if exists
    then readFn fileToLoad
    else readFn fallback
  where
    fileToLoad = prefix <> "/" <> name
    fallback =
      baseDir
        <> "/"
        <> unpack (locToText defLoc)
        <> "/"
        <> typ
        <> "/"
        <> name

mustacheFromText :: String -> Text -> IO Mustache.Template
mustacheFromText tplName textTemplate = do
  let eTpl = compileTemplate tplName (setDelimiter textTemplate)
  case eTpl of
    Left err -> error $ "Invalid Mustache template: " ++ show err
    Right tpl -> pure tpl
  where
    setDelimiter :: Text -> Text
    setDelimiter t =
      "{{= ${ } =}}" <> t

readMustacheTemplateWithDefault ::
  String ->
  FilePath ->
  Locale ->
  FilePath ->
  String ->
  FilePath ->
  IO Mustache.Template
readMustacheTemplateWithDefault tplName baseDir defLoc typ prefix name = do
  readTextWithDefault
    baseDir
    defLoc
    typ
    prefix
    name
    >>= mustacheFromText tplName

loadTeamTemplates ::
  Locale ->
  FilePath ->
  EmailAddress ->
  Text ->
  Text ->
  Text ->
  Text ->
  IO (Localised TeamTemplates)
loadTeamTemplates
  defLocale
  templateDir
  emailSender
  invitationEmailUrl
  invitationEmailExistingUserUrl
  creatorWelcomeEmailUrl
  memberWelcomeEmailUrl =
    readLocalesDir defLocale templateDir "team" $ \fp ->
      TeamTemplates
        <$> ( InvitationEmailTemplate
                <$> mustacheFromText "invitationEmailUrl" invitationEmailUrl
                <*> readMTpl "invitation-subject" fp "email/invitation-subject.txt"
                <*> readMTpl "invitation-txt" fp "email/invitation.txt"
                <*> readMTpl "invitation-html" fp "email/invitation.html"
                <*> pure emailSender
                <*> readTxt fp "email/sender.txt"
            )
        <*> ( InvitationEmailTemplate
                <$> mustacheFromText "invitationEmailExistingUserUrl" invitationEmailExistingUserUrl
                <*> readMTpl "migration-subject" fp "email/migration-subject.txt"
                <*> readMTpl "migration-txt" fp "email/migration.txt"
                <*> readMTpl "migration-html" fp "email/migration.html"
                <*> pure emailSender
                <*> readTxt fp "email/sender.txt"
            )
        <*> ( CreatorWelcomeEmailTemplate creatorWelcomeEmailUrl
                <$> readTpl fp "email/new-creator-welcome-subject.txt"
                <*> readTpl fp "email/new-creator-welcome.txt"
                <*> readTpl fp "email/new-creator-welcome.html"
                <*> pure emailSender
                <*> readTxt fp "email/sender.txt"
            )
        <*> ( MemberWelcomeEmailTemplate memberWelcomeEmailUrl
                <$> readTpl fp "email/new-member-welcome-subject.txt"
                <*> readTpl fp "email/new-member-welcome.txt"
                <*> readTpl fp "email/new-member-welcome.html"
                <*> pure emailSender
                <*> readTxt fp "email/sender.txt"
            )
        <*> ( NewTeamOwnerWelcomeEmailTemplate creatorWelcomeEmailUrl
                <$> readTpl fp "email/new-team-owner-welcome-subject.txt"
                <*> readTpl fp "email/new-team-owner-welcome.txt"
                <*> readTpl fp "email/new-team-owner-welcome.html"
                <*> pure emailSender
                <*> readTxt fp "email/sender.txt"
            )
    where
      readTpl = readTemplateWithDefault templateDir defLocale "team"
      readTxt = readTextWithDefault templateDir defLocale "team"
      readMTpl tplName = readMustacheTemplateWithDefault tplName templateDir defLocale "team"
