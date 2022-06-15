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

module Brig.Sem.UniqueClaimsStore.Cassandra (uniqueClaimsStoreToCassandra) where

import Brig.Sem.UniqueClaimsStore
import Cassandra
import qualified Cassandra as C
import Data.Id
import Data.Timeout
import Imports
import Polysemy

minTtl :: Timeout
minTtl = 60 # Second

uniqueClaimsStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (UniqueClaimsStore ': r) a ->
  Sem r a
uniqueClaimsStoreToCassandra =
  interpret $
    embed @m . \case
      AddClaims u t txt -> addClaimsQuery u t txt
      GetClaims txt -> lookupClaims txt
      DeleteClaims u t txt -> deleteClaimsQuery u t txt

deleteClaimsQuery ::
  MonadClient m =>
  -- | The 'Id' associated with the claim.
  Id a ->
  -- | The minimum timeout (i.e. duration) of the rest of the claim.  (Each
  --   claim can have more than one claimer (even though this is a feature we
  --   never use), so removing a claim is an update operation on the database.
  --   Therefore, we reset the TTL the same way we reset it in 'withClaim'.)
  Timeout ->
  -- | The value on which to acquire the claim.
  Text ->
  m ()
deleteClaimsQuery u t v = do
  let ttl = max minTtl t
  retry x5 $ write cql $ params LocalQuorum (fromIntegral $ (ttl * 2) #> Second, C.Set [u], v)
  where
    cql :: PrepQuery W (Int32, C.Set (Id a), Text) ()
    cql = "UPDATE unique_claims USING TTL ? SET claims = claims - ? WHERE value = ?"

addClaimsQuery :: MonadClient m => Id a -> Timeout -> Text -> m ()
addClaimsQuery u t v = do
  let ttl = max minTtl t
  retry x5
    . write cql
    $ params LocalQuorum (fromIntegral $ (ttl * 2) #> Second, C.Set [u], v)
  where
    cql :: PrepQuery W (Int32, C.Set (Id a), Text) ()
    cql = "UPDATE unique_claims USING TTL ? SET claims = claims + ? WHERE value = ?"

-- | Lookup the current claims on a value.
lookupClaims :: MonadClient m => Text -> m [Id a]
lookupClaims v =
  fmap (maybe [] (fromSet . runIdentity)) $
    retry x1 $
      query1 cql $
        params LocalQuorum (Identity v)
  where
    cql :: PrepQuery R (Identity Text) (Identity (C.Set (Id a)))
    cql = "SELECT claims FROM unique_claims WHERE value = ?"
