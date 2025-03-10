{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Email type with case handling options.  This helps mitigate issues with users that get
-- casing wrong in their email addresses: 'render' is case-sensitive, but `validate' gets you
-- a case-insensitive value with case information intact, so you are forced to decide for
-- yourself which version of the email address you want.  Same assymetry for 'FromJSON',
-- 'ToJSON'.
--
-- (The legal situation is simple: [the local part is case
-- sensitive](https://tools.ietf.org/html/rfc5321), [the domain part is case
-- insensitive](https://tools.ietf.org/html/rfc1035).  However, in practice, the entire email,
-- both local and domain part, are treated as case insensitive almost everywhere.  So for
-- interoperability we want to do the same, but not destroy case information in the process.)
module SAML2.WebSSO.Types.Email (Email, render, validate) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions
import Data.Word8 (toLower)
import qualified Text.Email.Validate as Email

newtype Email = Email {fromEmail :: Email.EmailAddress}
  deriving (Eq, Ord, Show)

instance CI.FoldCase Email where
  foldCase (Email eml) =
    Email
      ( Email.unsafeEmailAddress
          (BS.map toLower . Email.localPart $ eml)
          (BS.map toLower . Email.domainPart $ eml)
      )

render :: (CI.FoldCase s, ConvertibleStrings BS.ByteString s) => Email -> s
render = cs . Email.toByteString . fromEmail

validate :: forall s. ConvertibleStrings s BS.ByteString => s -> Either String (CI.CI Email)
validate = fmap (CI.mk . Email) . Email.validate . cs

instance FromJSON (CI.CI Email) where
  parseJSON = withText "email address" $ either fail pure . validate

instance ToJSON Email where
  toJSON = String . render
