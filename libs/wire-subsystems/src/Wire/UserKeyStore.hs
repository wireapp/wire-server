{-# LANGUAGE TemplateHaskell #-}

module Wire.UserKeyStore where

import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Imports hiding (local)
import Polysemy
import Test.QuickCheck
import Wire.API.User

-- | An 'EmailKey' is an 'Email' in a form that serves as a unique lookup key.
data EmailKey = EmailKey
  { emailKeyUniq :: !Text,
    emailKeyOrig :: !EmailAddress
  }
  deriving (Ord)

instance Show EmailKey where
  showsPrec _ = shows . emailKeyUniq

instance Eq EmailKey where
  (EmailKey k _) == (EmailKey k' _) = k == k'

instance Arbitrary EmailKey where
  arbitrary = mkEmailKey <$> arbitrary

-- | Turn an 'Email' into an 'EmailKey'.
--
-- The following transformations are performed:
--
--   * Both local and domain parts are forced to lowercase to make
--     e-mail addresses fully case-insensitive.
--   * "+" suffixes on the local part are stripped unless the domain
--     part is contained in a trusted whitelist.
mkEmailKey :: EmailAddress -> EmailKey
mkEmailKey orig =
  let uniq = Text.toLower localPart' <> "@" <> Text.toLower domain
   in EmailKey uniq orig
  where
    domain = decodeUtf8 . domainPart $ orig
    local = decodeUtf8 . localPart $ orig
    localPart'
      | (domainPart orig) `notElem` trusted = Text.takeWhile (/= '+') local
      | otherwise = decodeUtf8 (localPart orig)
    trusted = ["wearezeta.com", "wire.com", "simulator.amazonses.com"]

data UserKeyStore m a where
  LookupKey :: EmailKey -> UserKeyStore m (Maybe UserId)
  InsertKey :: UserId -> EmailKey -> UserKeyStore m ()
  DeleteKey :: EmailKey -> UserKeyStore m ()
  DeleteKeyForUser :: UserId -> EmailKey -> UserKeyStore m ()
  KeyAvailable :: EmailKey -> Maybe UserId -> UserKeyStore m Bool
  ClaimKey :: EmailKey -> UserId -> UserKeyStore m Bool

makeSem ''UserKeyStore
