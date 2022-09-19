{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- rename to Brig.Types.Account?
module Brig.Types.Common
  ( -- * PhoneBudgetTimeout
    PhoneBudgetTimeout (..),

    -- * PhonePrefix
    PhonePrefix (..),
    parsePhonePrefix,
    isValidPhonePrefix,
    allPrefixes,
    ExcludedPrefix (..),

    -- * misc
    foldKey,
    keyText,
    mkPhoneKey,
    mkEmailKey,
    EmailKey (..),
    PhoneKey (..),
    UserKey (..),
  )
where

import Control.Applicative (optional)
import Data.Aeson hiding ((<?>))
import Data.Attoparsec.Text
import Data.ByteString.Conversion
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Imports
import Wire.API.User.Identity

------------------------------------------------------------------------------
--- PhoneBudgetTimeout

-- | If the budget for SMS and voice calls for a phone number
-- has been exhausted within a certain time frame, this timeout
-- indicates in seconds when another attempt may be made.
newtype PhoneBudgetTimeout = PhoneBudgetTimeout
  {phoneBudgetTimeout :: NominalDiffTime}
  deriving (Eq, Show, Generic)

instance FromJSON PhoneBudgetTimeout where
  parseJSON = withObject "PhoneBudgetTimeout" $ \o ->
    PhoneBudgetTimeout <$> o .: "expires_in"

instance ToJSON PhoneBudgetTimeout where
  toJSON (PhoneBudgetTimeout t) = object ["expires_in" .= t]

-----------------------------------------------------------------------------
-- PhonePrefix (for excluding from SMS/calling)

newtype PhonePrefix = PhonePrefix {fromPhonePrefix :: Text}
  deriving (Eq, Show, ToJSON, Generic)

-- | Parses a phone number prefix with a mandatory leading '+'.
parsePhonePrefix :: Text -> Maybe PhonePrefix
parsePhonePrefix p
  | isValidPhonePrefix p = Just $ PhonePrefix p
  | otherwise = Nothing

-- | Checks whether a phone number prefix is valid,
-- i.e. it is like a E.164 format phone number, but shorter
-- (with a mandatory leading '+', followed by 1-15 digits.)
isValidPhonePrefix :: Text -> Bool
isValidPhonePrefix = isRight . parseOnly e164Prefix
  where
    e164Prefix = char '+' *> count 1 digit *> count 14 (optional digit) *> endOfInput

-- | get all valid prefixes of a phone number or phone number prefix
-- e.g. from +123456789 get prefixes ["+1", "+12", "+123", ..., "+123456789" ]
allPrefixes :: Text -> [PhonePrefix]
allPrefixes t = mapMaybe parsePhonePrefix (Text.inits t)

instance FromJSON PhonePrefix where
  parseJSON = withText "PhonePrefix" $ \s ->
    case parsePhonePrefix s of
      Just p -> pure p
      Nothing ->
        fail $
          "Invalid phone number prefix: [" ++ show s
            ++ "]. Expected format similar to E.164 (with 1-15 digits after the +)."

instance FromByteString PhonePrefix where
  parser = parser >>= maybe (fail "Invalid phone") pure . parsePhonePrefix

instance ToByteString PhonePrefix where
  builder = builder . fromPhonePrefix

data ExcludedPrefix = ExcludedPrefix
  { phonePrefix :: PhonePrefix,
    comment :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ExcludedPrefix where
  parseJSON = withObject "ExcludedPrefix" $ \o ->
    ExcludedPrefix
      <$> o .: "phone_prefix"
      <*> o .: "comment"

instance ToJSON ExcludedPrefix where
  toJSON (ExcludedPrefix p c) = object ["phone_prefix" .= p, "comment" .= c]

-------------------------------------------------------------------------------
-- Unique Keys

-- | An 'EmailKey' is an 'Email' in a form that serves as a unique lookup key.
data EmailKey = EmailKey
  { emailKeyUniq :: !Text,
    emailKeyOrig :: !Email
  }

instance Show EmailKey where
  showsPrec _ = shows . emailKeyUniq

instance Eq EmailKey where
  (EmailKey k _) == (EmailKey k' _) = k == k'

-- | Turn an 'Email' into an 'EmailKey'.
--
-- The following transformations are performed:
--
--   * Both local and domain parts are forced to lowercase to make
--     e-mail addresses fully case-insensitive.
--   * "+" suffixes on the local part are stripped unless the domain
--     part is contained in a trusted whitelist.
mkEmailKey :: Email -> EmailKey
mkEmailKey orig@(Email localPart domain) =
  let uniq = Text.toLower localPart' <> "@" <> Text.toLower domain
   in EmailKey uniq orig
  where
    localPart'
      | domain `notElem` trusted = Text.takeWhile (/= '+') localPart
      | otherwise = localPart
    trusted = ["wearezeta.com", "wire.com", "simulator.amazonses.com"]

data PhoneKey = PhoneKey
  { -- | canonical form of 'phoneKeyOrig', without whitespace.
    phoneKeyUniq :: !Text,
    -- | phone number with whitespace.
    phoneKeyOrig :: !Phone
  }

instance Show PhoneKey where
  showsPrec _ = shows . phoneKeyUniq

instance Eq PhoneKey where
  (PhoneKey k _) == (PhoneKey k' _) = k == k'

mkPhoneKey :: Phone -> PhoneKey
mkPhoneKey orig =
  let uniq = Text.filter (not . isSpace) (fromPhone orig)
   in PhoneKey uniq orig

-- | A natural identifier (i.e. unique key) of a user.
data UserKey
  = UserEmailKey !EmailKey
  | UserPhoneKey !PhoneKey

instance Eq UserKey where
  (UserEmailKey k) == (UserEmailKey k') = k == k'
  (UserPhoneKey k) == (UserPhoneKey k') = k == k'
  _ == _ = False

-- | Get the normalised text of a 'UserKey'.
keyText :: UserKey -> Text
keyText (UserEmailKey k) = emailKeyUniq k
keyText (UserPhoneKey k) = phoneKeyUniq k

foldKey :: (Email -> a) -> (Phone -> a) -> UserKey -> a
foldKey f g k = case k of
  UserEmailKey ek -> f (emailKeyOrig ek)
  UserPhoneKey pk -> g (phoneKeyOrig pk)
