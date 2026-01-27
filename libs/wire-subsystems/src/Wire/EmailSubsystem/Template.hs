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

module Wire.EmailSubsystem.Template where

import Control.Exception
import Data.Aeson (FromJSON)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as Lazy
import Data.Text.Template
import HTMLEntities.Text qualified as HTML
import Imports hiding (readFile)
import Polysemy
import Polysemy.Output
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.IO.Error (isDoesNotExistError)
import System.Logger (field, msg, val)
import Wire.API.Locale
import Wire.API.User.EmailAddress (EmailAddress)
import Wire.EmailSubsystem.Templates.Team

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
    locOther :: (Map Locale a)
  }

-- | Uses a replace and a branding function, to replaces all placeholders from the
-- given template to produce a Text. To be used on plain text templates
renderTextWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderTextWithBranding tpl replace branding = render tpl (replace . branding)

-- | Uses a replace and a branding function to replace all placeholders from the
-- given template to produce a Text. To be used on HTML templates
renderHtmlWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderHtmlWithBranding tpl replace branding = render tpl (HTML.text . replace . branding)

renderHtmlWithBrandingSem :: (Member (Output Text) r) => Template -> Map Text Text -> Sem r Lazy.Text
renderHtmlWithBrandingSem = renderWithBrandingSem HTML.text

renderTextWithBrandingSem :: (Member (Output Text) r) => Template -> Map Text Text -> Sem r Lazy.Text
renderTextWithBrandingSem = renderWithBrandingSem id

-- If a template field is not declared, do not replace it and drop key to `Output` effect.  This way we catch all errors, not just the first, and the caller gets to decide what to do with the error.
renderWithBrandingSem :: (Member (Output Text) r) => (Text -> Text) -> Template -> Map Text Text -> Sem r Lazy.Text
renderWithBrandingSem escapeHtml tpl replace = do
  let f x = case Map.lookup x replace of
        Just v -> pure v
        Nothing -> do
          output x
          pure x
  renderA tpl (escapeHtml <$$> f)

logEmailRenderErrors :: (Member TinyLog r) => Text -> Sem (Output Text : r) a -> Sem r a
logEmailRenderErrors tplName =
  runOutputSem $
    ( \warn ->
        do
          Log.warn $
            msg (val "Email template rendering failure")
              . field "template_name" (val (T.encodeUtf8 tplName))
              . field "unreplaced_variable" (val (T.encodeUtf8 warn))
    )

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

data TeamOpts = TeamOpts
  { -- | Team Invitation URL template
    tInvitationUrl :: !Text,
    -- | Existing User Invitation URL template
    tExistingUserInvitationUrl :: !Text,
    -- | Team Activation URL template
    tActivationUrl :: !Text,
    -- | Team Creator Welcome URL
    tCreatorWelcomeUrl :: !Text,
    -- | Team Member Welcome URL
    tMemberWelcomeUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON TeamOpts

loadTeamTemplates :: TeamOpts -> FilePath -> Locale -> EmailAddress -> IO (Localised TeamTemplates)
loadTeamTemplates tOptions templatesDir defLocale sender = readLocalesDir defLocale templatesDir "team" $ \fp ->
  TeamTemplates
    <$> ( InvitationEmailTemplate tUrl
            <$> readTemplate fp "email/invitation-subject.txt"
            <*> readTemplate fp "email/invitation.txt"
            <*> readTemplate fp "email/invitation.html"
            <*> pure sender
            <*> readText fp "email/sender.txt"
        )
    <*> ( InvitationEmailTemplate tExistingUrl
            <$> readTemplate fp "email/migration-subject.txt"
            <*> readTemplate fp "email/migration.txt"
            <*> readTemplate fp "email/migration.html"
            <*> pure sender
            <*> readText fp "email/sender.txt"
        )
    <*> ( MemberWelcomeEmailTemplate (tMemberWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-member-welcome-subject.txt"
            <*> readTemplate fp "email/new-member-welcome.txt"
            <*> readTemplate fp "email/new-member-welcome.html"
            <*> pure sender
            <*> readText fp "email/sender.txt"
        )
    <*> ( NewTeamOwnerWelcomeEmailTemplate (tCreatorWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-team-owner-welcome-subject.txt"
            <*> readTemplate fp "email/new-team-owner-welcome.txt"
            <*> readTemplate fp "email/new-team-owner-welcome.html"
            <*> pure sender
            <*> readText fp "email/sender.txt"
        )
    <*> ( IdPConfigChangeEmailTemplate
            <$> readTemplate fp "../partials/idp-certificate-added.html"
            <*> readTemplate fp "../partials/idp-certificate-added.txt"
            <*> readTemplate fp "../partials/idp-certificate-removed.html"
            <*> readTemplate fp "../partials/idp-certificate-removed.txt"
            <*> readTemplate fp "email/idp-config-change-subject.txt"
            <*> readTemplate fp "email/idp-config-change.txt"
            <*> readTemplate fp "email/idp-config-change.html"
            <*> pure sender
            <*> readText fp "email/sender.txt"
        )
  where
    tUrl = template tOptions.tInvitationUrl
    tExistingUrl = template tOptions.tExistingUserInvitationUrl
    readTemplate = readTemplateWithDefault templatesDir defLocale "team"
    readText = readTextWithDefault templatesDir defLocale "team"
