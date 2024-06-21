{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.UserKeyStore where

import Data.Id
import Data.Text qualified as Text
import Imports
import Polysemy
import Test.QuickCheck
import Wire.API.User
import Wire.Arbitrary

data PhoneKey = PhoneKey
  { -- | canonical form of 'phoneKeyOrig', without whitespace.
    phoneKeyUniq :: !Text,
    -- | phone number with whitespace.
    phoneKeyOrig :: !Phone
  }
  deriving (Ord)

instance Show PhoneKey where
  showsPrec _ = shows . phoneKeyUniq

instance Eq PhoneKey where
  (PhoneKey k _) == (PhoneKey k' _) = k == k'

instance Arbitrary PhoneKey where
  arbitrary = mkPhoneKey <$> arbitrary

-- | An 'EmailKey' is an 'Email' in a form that serves as a unique lookup key.
data EmailKey = EmailKey
  { emailKeyUniq :: !Text,
    emailKeyOrig :: !Email
  }
  deriving (Ord)

instance Show EmailKey where
  showsPrec _ = shows . emailKeyUniq

instance Eq EmailKey where
  (EmailKey k _) == (EmailKey k' _) = k == k'

instance Arbitrary EmailKey where
  arbitrary = mkEmailKey <$> arbitrary

-- | A natural identifier (i.e. unique key) of a user.
data UserKey
  = UserEmailKey !EmailKey
  | UserPhoneKey !PhoneKey
  deriving stock (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform UserKey)

userEmailKey :: Email -> UserKey
userEmailKey = UserEmailKey . mkEmailKey

userPhoneKey :: Phone -> UserKey
userPhoneKey = UserPhoneKey . mkPhoneKey

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

mkPhoneKey :: Phone -> PhoneKey
mkPhoneKey orig =
  let uniq = Text.filter (not . isSpace) (fromPhone orig)
   in PhoneKey uniq orig

-- | Get the normalised text of a 'UserKey'.
keyText :: UserKey -> Text
keyText (UserEmailKey k) = emailKeyUniq k
keyText (UserPhoneKey k) = phoneKeyUniq k

-- | Get the original text of a 'UserKey', i.e. the original phone number
-- or email address.
keyTextOriginal :: UserKey -> Text
keyTextOriginal (UserEmailKey k) = fromEmail (emailKeyOrig k)
keyTextOriginal (UserPhoneKey k) = fromPhone (phoneKeyOrig k)

foldKey :: (Email -> a) -> (Phone -> a) -> UserKey -> a
foldKey f g k = case k of
  UserEmailKey ek -> f (emailKeyOrig ek)
  UserPhoneKey pk -> g (phoneKeyOrig pk)

forEmailKey :: (Applicative f) => UserKey -> (Email -> f a) -> f (Maybe a)
forEmailKey k f = foldKey (fmap Just . f) (const (pure Nothing)) k

forPhoneKey :: (Applicative f) => UserKey -> (Phone -> f a) -> f (Maybe a)
forPhoneKey k f = foldKey (const (pure Nothing)) (fmap Just . f) k

fromEither :: Either Email Phone -> UserKey
fromEither = either userEmailKey userPhoneKey

data UserKeyStore m a where
  LookupKey :: UserKey -> UserKeyStore m (Maybe UserId)
  InsertKey :: UserId -> UserKey -> UserKeyStore m ()
  DeleteKey :: UserKey -> UserKeyStore m ()
  DeleteKeyForUser :: UserId -> UserKey -> UserKeyStore m ()
  KeyAvailable :: UserKey -> Maybe UserId -> UserKeyStore m Bool
  ClaimKey :: UserKey -> UserId -> UserKeyStore m Bool

makeSem ''UserKeyStore
