-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserSearch.Normalize
  ( normalized,
  )
where

import Data.Text (Text)
import Data.Text.ICU.Translit (trans, transliterate)

-- | Normalizes a name (or search term) for matching: transliterate to
-- Latin, strip diacritics, lowercase.  ("Björn" -> "bjorn")
--
-- This is the same function that used to be applied when writing the
-- ElasticSearch user documents (formerly 'Wire.UserStore.IndexUser.normalized');
-- it is now applied when writing @wire_user.name_normalized@ and when
-- building search queries.
normalized :: Text -> Text
normalized = transliterate (trans "Any-Latin; Latin-ASCII; Lower")
