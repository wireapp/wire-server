{-# LANGUAGE TemplateHaskell #-}

module Wire.UserKeyStore where

import Data.Id
import Data.Text qualified as Text
import Imports
import Polysemy
import Test.QuickCheck
import Wire.API.User

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

data UserKeyStore m a where
  LookupKey :: EmailKey -> UserKeyStore m (Maybe UserId)
  InsertKey :: UserId -> EmailKey -> UserKeyStore m ()
  DeleteKey :: EmailKey -> UserKeyStore m ()
  DeleteKeyForUser :: UserId -> EmailKey -> UserKeyStore m ()
  KeyAvailable :: EmailKey -> Maybe UserId -> UserKeyStore m Bool
  ClaimKey :: EmailKey -> UserId -> UserKeyStore m Bool

makeSem ''UserKeyStore
