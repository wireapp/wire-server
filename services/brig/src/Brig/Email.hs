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

module Brig.Email
  ( -- * Validation
    validateEmail,

    -- * Unique Keys
    EmailKey,
    mkEmailKey,
    emailKeyUniq,
    emailKeyOrig,

    -- * Re-exports
    Email (..),

    -- * MIME Re-exports
    Mail (..),
    emptyMail,
    plainPart,
    htmlPart,
    Address (..),
    mkMimeAddress,
    sendMail,
  )
where

import qualified Brig.AWS as AWS
import Brig.App (AppIO, awsEnv, smtpEnv)
import qualified Brig.SMTP as SMTP
import Brig.Types
import Control.Applicative (optional)
import Control.Lens (view)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Network.Mail.Mime
import qualified Text.Email.Validate as Email

-------------------------------------------------------------------------------
sendMail :: Mail -> AppIO ()
sendMail m = view smtpEnv >>= \case
  Just smtp -> SMTP.sendMail smtp m
  Nothing -> view awsEnv >>= \e -> AWS.execute e $ AWS.sendMail m

-- Validation

-- | (Check out 'mkEmailKey' if you wonder about equality of emails with @+@ in their local part.)
validateEmail :: Email -> Either String Email
validateEmail (fromEmail -> e) =
  validateLength
    >>= Email.validate
    >>= validateDomain
    >>= pure . mkEmail
  where
    len = Text.length e
    validateLength
      | len <= 100 = Right $ encodeUtf8 e
      | otherwise = Left $ "length " <> show len <> " exceeds 100"
    -- cf. https://en.wikipedia.org/wiki/Email_address#Domain
    -- n.b. We do not allow IP address literals, comments or non-ASCII
    --      characters, mostly because SES (and probably many other mail
    --      systems) don't support that (yet?) either.
    validateDomain e' = parseOnly parser (Email.domainPart e')
      where
        parser = label *> many1 (char '.' *> label) *> endOfInput *> pure e'
        label =
          satisfy (inClass "a-zA-Z0-9")
            *> count 61 (optional (satisfy (inClass "-a-zA-Z0-9")))
            *> optional (satisfy (inClass "a-zA-Z0-9"))
    mkEmail v = Email (mkLocal v) (mkDomain v)
    mkLocal = decodeUtf8 . Email.localPart
    mkDomain = decodeUtf8 . Email.domainPart

-------------------------------------------------------------------------------
-- Unique Keys

-- | An 'EmailKey' is an 'Email' in a form that serves as a unique lookup key.
data EmailKey
  = EmailKey
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

-------------------------------------------------------------------------------
-- MIME Conversions

-- | Construct a MIME 'Address' from the given display 'Name' and 'Email'
-- address that does not exceed 320 bytes in length when rendered for use
-- in SMTP, which is a safe limit for most mail servers (including those of
-- Amazon SES). The display name is only included if it fits within that
-- limit, otherwise it is dropped.
mkMimeAddress :: Name -> Email -> Address
mkMimeAddress name email =
  let addr = Address (Just (fromName name)) (fromEmail email)
   in if Text.compareLength (renderAddress addr) 320 == GT
        then Address Nothing (fromEmail email)
        else addr
