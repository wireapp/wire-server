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
{-# OPTIONS_GHC -Wno-orphans #-}

module Brig.Sem.VerificationCodeStore.Cassandra (verificationCodeStoreToCassandra) where

import Brig.Data.Instances ()
import Brig.Sem.VerificationCodeStore
import Cassandra hiding (Value)
import Data.Code
import Data.UUID
import Imports
import Polysemy
import Wire.API.User.Identity

instance Cql Scope where
  ctype = Tagged IntColumn

  toCql AccountDeletion = CqlInt 1
  toCql IdentityVerification = CqlInt 2
  toCql PasswordReset = CqlInt 3
  toCql AccountLogin = CqlInt 4
  toCql AccountApproval = CqlInt 5
  toCql CreateScimToken = CqlInt 6
  toCql DeleteTeam = CqlInt 7

  fromCql (CqlInt 1) = pure AccountDeletion
  fromCql (CqlInt 2) = pure IdentityVerification
  fromCql (CqlInt 3) = pure PasswordReset
  fromCql (CqlInt 4) = pure AccountLogin
  fromCql (CqlInt 5) = pure AccountApproval
  fromCql (CqlInt 6) = pure CreateScimToken
  fromCql (CqlInt 7) = pure DeleteTeam
  fromCql _ = Left "fromCql: Scope: int expected"

instance Cql Retries where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . numRetries
  fromCql (CqlInt n) = pure (Retries (fromIntegral n))
  fromCql _ = Left "fromCql: Retries: int expected"

verificationCodeStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (VerificationCodeStore ': r) a ->
  Sem r a
verificationCodeStoreToCassandra =
  interpret $
    embed @m . \case
      x -> case x of
        GetPendingCode key scope -> lookupCode key scope
        InsertCode code -> insert code

-- | Lookup a pending code.
lookupCode :: MonadClient m => Key -> Scope -> m (Maybe Code)
lookupCode k s = fmap (toCode k s) <$> retry x1 (query1 cql (params LocalQuorum (k, s)))
  where
    cql :: PrepQuery R (Key, Scope) (Value, Int32, Retries, Maybe Email, Maybe Phone, Maybe UUID)
    cql =
      "SELECT value, ttl(value), retries, email, phone, account \
      \FROM vcodes WHERE key = ? AND scope = ?"

insert :: MonadClient m => Code -> m ()
insert c = do
  let k = codeKey c
  let s = codeScope c
  let v = codeValue c
  let r = fromIntegral (codeRetries c)
  let a = codeAccount c
  let e = codeForEmail c
  let p = codeForPhone c
  let t = round (codeTTL c)
  retry x5 (write cql (params LocalQuorum (k, s, v, r, e, p, a, t)))
  where
    cql :: PrepQuery W (Key, Scope, Value, Retries, Maybe Email, Maybe Phone, Maybe UUID, Int32) ()
    cql =
      "INSERT INTO vcodes (key, scope, value, retries, email, phone, account) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"

toCode :: Key -> Scope -> (Value, Int32, Retries, Maybe Email, Maybe Phone, Maybe UUID) -> Code
toCode k s (val, ttl, retries, email, phone, account) =
  let ek = ForEmail <$> email
      pk = ForPhone <$> phone
      to = Timeout (fromIntegral ttl)
   in case ek <|> pk of
        Nothing -> error "toCode: email or phone must be present"
        Just cf ->
          Code
            { codeKey = k,
              codeScope = s,
              codeValue = val,
              codeTTL = to,
              codeRetries = retries,
              codeFor = cf,
              codeAccount = account
            }
