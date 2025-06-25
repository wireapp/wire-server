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
anyElementOf ne = do
  let len = getList ne
  idx :: Int <- elements [0, length len - 1]
  pure ((getList ne) !! idx)
  where
    getList = toList . getNonEmpty
