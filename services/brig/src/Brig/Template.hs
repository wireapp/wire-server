{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | Common templating utilities.
module Brig.Template
  ( -- * Reading templates
    Localised,
    forLocale,
    readLocalesDir,
    readTemplateWithDefault,
    readTextWithDefault,

    -- * Rendering templates
    renderText,
    renderHtml,
    renderTextWithBranding,
    renderHtmlWithBranding,
    genTemplateBranding,
    TemplateBranding,

    -- * Re-exports
    Template,
    template,
  )
where

import Brig.Options
import Brig.Types (Locale (..), locToText, parseLocale)
import Control.Exception (catchJust)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as Lazy
import Data.Text.Template (Template, template)
import qualified Data.Text.Template as Template
import qualified HTMLEntities.Text as HTML
import Imports hiding (readFile)
import System.IO.Error (isDoesNotExistError)

-- | See 'genTemplateBranding'.
type TemplateBranding = Text -> Text

-- | Localised templates.
data Localised a = Localised
  { locDefault :: !(Locale, a),
    locOther :: !(Map Locale a)
  }

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

-- | Uses a replace and a branding function, to replaces all placeholders from the
-- given template to produce a Text. To be used on plain text templates
renderTextWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderTextWithBranding tpl replace branding = renderText tpl (replace . branding)

-- | Uses a replace and a branding function to replace all placeholders from the
-- given template to produce a Text. To be used on HTML templates
renderHtmlWithBranding :: Template -> (Text -> Text) -> TemplateBranding -> Lazy.Text
renderHtmlWithBranding tpl replace branding = renderHtml tpl (replace . branding)

-- TODO: Do not export this function
renderText :: Template -> (Text -> Text) -> Lazy.Text
renderText = Template.render

-- TODO: Do not export this function
renderHtml :: Template -> (Text -> Text) -> Lazy.Text
renderHtml tpl replace = renderText tpl (HTML.text . replace)

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
      baseDir <> "/"
        <> unpack (locToText defLoc)
        <> "/"
        <> typ
        <> "/"
        <> name

-- | Function to be applied everywhere where email/sms/call
-- templating is used (ensures that placeholders are replaced
-- by the appropriate branding, typically Wire)
genTemplateBranding :: BrandingOpts -> TemplateBranding
genTemplateBranding BrandingOpts {..} = fn
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
