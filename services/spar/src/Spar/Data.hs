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

-- | Manipulating Spar records in the database.
module Spar.Data
  ( schemaVersion,
    Env (..),
    mkEnv,
    mkTTLAssertions,
    nominalDiffToSeconds,
    mkTTLAuthnRequests,

    -- * SAML Users
    NormalizedUNameID (..),
    normalizeUnqualifiedNameId,
    normalizeQualifiedNameId,
  )
where

import Cassandra as Cas
import Control.Lens
import Control.Monad.Except
import Data.CaseInsensitive (foldCase)
import qualified Data.CaseInsensitive as CI
import Data.Time
import GHC.TypeLits (KnownSymbol)
import Imports
import SAML2.Util (renderURI)
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.Email as SAMLEmail
import Spar.Options
import qualified Spar.Schema.Run as Migrations
import Wire.API.User.Saml

-- | A lower bound: @schemaVersion <= whatWeFoundOnCassandra@, not @==@.
schemaVersion :: Int32
schemaVersion = Migrations.lastSchemaVersion

----------------------------------------------------------------------
-- helpers

-- | Carry some time constants we do not want to pull from Options, IO, respectively.  This way the
-- functions in this module need fewer effects.  See 'wrapMonadClientWithEnv' (as opposed to
-- 'wrapMonadClient' where we don't need an 'Env').
data Env = Env
  { dataEnvNow :: UTCTime,
    dataEnvMaxTTLAuthRequests :: TTL "authreq",
    dataEnvMaxTTLAssertions :: TTL "authresp"
  }
  deriving (Eq, Show)

mkEnv :: Opts -> UTCTime -> Env
mkEnv opts now =
  Env
    { dataEnvNow = now,
      dataEnvMaxTTLAuthRequests = maxttlAuthreq opts,
      dataEnvMaxTTLAssertions = maxttlAuthresp opts
    }

mkTTLAuthnRequests :: (MonadError TTLError m) => Env -> UTCTime -> m (TTL "authreq")
mkTTLAuthnRequests (Env now maxttl _) = mkTTL now maxttl

mkTTLAssertions :: (MonadError TTLError m) => Env -> UTCTime -> m (TTL "authresp")
mkTTLAssertions (Env now _ maxttl) = mkTTL now maxttl

mkTTL :: (MonadError TTLError m, KnownSymbol a) => UTCTime -> TTL a -> UTCTime -> m (TTL a)
mkTTL now maxttl endOfLife = mkTTLNDT maxttl $ endOfLife `diffUTCTime` now

mkTTLNDT :: (MonadError TTLError m, KnownSymbol a) => TTL a -> NominalDiffTime -> m (TTL a)
mkTTLNDT maxttl ttlNDT =
  if
    | actualttl > maxttl -> throwError $ TTLTooLong (showTTL actualttl) (showTTL maxttl)
    | actualttl <= 0 -> throwError $ TTLNegative (showTTL actualttl)
    | otherwise -> pure actualttl
  where
    actualttl = TTL . nominalDiffToSeconds $ ttlNDT

nominalDiffToSeconds :: NominalDiffTime -> Int32
nominalDiffToSeconds = round @Double . realToFrac

----------------------------------------------------------------------
-- user

-- | Used as a lookup key for 'UnqualifiedNameID' that only depends on the
-- lowercase version of the identifier. Use 'normalizeUnqualifiedNameId' or
-- 'normalizeQualifiedNameId' to create values.
newtype NormalizedUNameID = NormalizedUNameID {unNormalizedUNameID :: Text}
  deriving stock (Eq, Ord, Generic)

instance Cql NormalizedUNameID where
  ctype = Tagged TextColumn
  toCql = CqlText . unNormalizedUNameID
  fromCql (CqlText t) = pure $ NormalizedUNameID t
  fromCql _ = Left "NormalizedNameID: expected CqlText"

normalizeUnqualifiedNameId :: SAML.UnqualifiedNameID -> NormalizedUNameID
normalizeUnqualifiedNameId = NormalizedUNameID . foldCase . nameIdTxt
  where
    nameIdTxt :: SAML.UnqualifiedNameID -> Text
    nameIdTxt (SAML.UNameIDUnspecified txt) = txt
    nameIdTxt (SAML.UNameIDEmail email) = SAMLEmail.render $ CI.original email
    nameIdTxt (SAML.UNameIDX509 txt) = txt
    nameIdTxt (SAML.UNameIDWindows txt) = txt
    nameIdTxt (SAML.UNameIDKerberos txt) = txt
    nameIdTxt (SAML.UNameIDEntity uri) = renderURI uri
    nameIdTxt (SAML.UNameIDPersistent txt) = txt
    nameIdTxt (SAML.UNameIDTransient txt) = txt

-- | Qualifiers are ignored.
normalizeQualifiedNameId :: SAML.NameID -> NormalizedUNameID
normalizeQualifiedNameId = normalizeUnqualifiedNameId . view SAML.nameID
