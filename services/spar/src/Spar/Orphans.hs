{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

-- | Miscellaneous orphan instances needed for Spar.
module Spar.Orphans
  (
  )
where

import Data.Id
import Data.Time.Clock (getCurrentTime)
import Imports
import Polysemy
import qualified Polysemy.Reader as PS
import SAML2.WebSSO
  ( HasNow (..),
    Time (..),
  )
import Servant (FromHttpApiData (..), MimeRender (..), PlainText, ToHttpApiData (..))

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
  toUrlPiece = toUrlPiece . show

instance MimeRender PlainText Void where
  mimeRender _ = error "instance MimeRender HTML Void: impossible"

instance Member (Embed IO) r => HasNow (Sem r) where
  getNow =
    liftIO $ (Time <$> getCurrentTime)
