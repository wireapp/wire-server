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
    module Wire.EmailSubsystem.Templates.User,
    module Wire.EmailSubsystem.Templates.Team,

    -- * Re-exports
    Template,
  )
where

import Data.Map qualified as Map
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as Lazy
import Data.Text.Template
import HTMLEntities.Text qualified as HTML
import Imports
import Polysemy
import Polysemy.Output
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger (field, msg, val)
import Wire.API.Locale
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

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
