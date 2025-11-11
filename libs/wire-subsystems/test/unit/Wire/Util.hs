-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Util where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Imports
import Test.QuickCheck
import Wire.API.User

-- | Quickcheck helper to generate the first part of an email address
-- (@<email-username>\@<some-domain>@)
--
-- See `Arbitrary EmailUsername`
newtype EmailUsername = EmailUsername {getEmailUsername :: String}
  deriving (Eq, Ord, Show, Read, Typeable)

instance Arbitrary EmailUsername where
  arbitrary =
    EmailUsername
      <$> ((arbitrary @EmailAddress) <&> (T.unpack . T.decodeUtf8 . domainPart))
  shrink (EmailUsername xs) = EmailUsername `fmap` shrink xs

-- | Generator to get any element from a NonEmpty list
anyElementOf :: NonEmptyList a -> Gen a
anyElementOf = elements . toList . getNonEmpty
