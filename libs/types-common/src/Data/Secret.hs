-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Data.Secret
  ( SecretText,
    secretText,
    revealSecretText,
  )
where

import Imports

-- | Text that may contain credentials or other sensitive material.
--
-- The constructor is intentionally opaque. 'revealSecretText' should only be
-- used at the narrow boundary where an external API requires the plaintext
-- representation.
newtype SecretText = SecretText Text

instance Show SecretText where
  show _ = "<redacted>"

-- | Wrap sensitive text without exposing it through the public constructor.
secretText :: Text -> SecretText
secretText = SecretText

-- | Reveal sensitive text for an API that explicitly requires plaintext.
revealSecretText :: SecretText -> Text
revealSecretText (SecretText value) = value
