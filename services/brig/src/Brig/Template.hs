{-# LANGUAGE RecordWildCards #-}

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

-- | Common templating utilities.
module Brig.Template
  ( InvitationUrlTemplates (..),
    genTemplateBranding,
    genTemplateBrandingMap,
  )
where

import Brig.Options
import Data.Map.Strict qualified as Map
import Data.Text.Template (Template)
import Imports

data InvitationUrlTemplates = InvitationUrlTemplates
  { personalUser :: Template,
    newUser :: Template
  }

-- | See 'genTemplateBranding'.
type TemplateBranding = Text -> Text

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

genTemplateBrandingMap :: BrandingOpts -> Map Text Text
genTemplateBrandingMap opts =
  Map.fromList
    [ ("brand", opts.brand),
      ("brand_url", opts.brandUrl),
      ("brand_label_url", opts.brandLabelUrl),
      ("brand_logo", opts.brandLogoUrl),
      ("brand_service", opts.brandService),
      ("copyright", opts.copyright),
      ("misuse", opts.misuse),
      ("legal", opts.legal),
      ("forgot", opts.forgot),
      ("support", opts.support)
    ]
