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

-- | Temporary exclusive claims on 'Text'ual values which may be subject
-- to contention, i.e. where strong guarantees on uniqueness are desired.
module Wire.UserStore.Unique
  ( withClaim,
    deleteClaim,
    lookupClaims,

    -- * Re-exports
    Timeout,
    TimeoutUnit (..),
    (#),
  )
where

import Cassandra as C
import Control.Concurrent.Timeout
import Data.Id
import Data.Timeout
import Imports

-- | Obtain a (temporary) exclusive claim on a 'Text' value for some
-- 'Id'entifier. The claim expires after the provided timeout, whether
-- it was successful or not. Contention can thus render the value
-- unavailable until the timeout expires if no contender succeeds.
-- The given 'IO' computation is only run when the claim was successful
-- and is responsible for turning the temporary claim into permanent
-- ownership, if desired.
withClaim ::
  -- | The 'Id' associated with the claim.
  Id a ->
  -- | The value on which to acquire the claim.
  Text ->
  -- | The minimum timeout (i.e. duration) of the claim.
  Timeout ->
  -- | The computation to run with a successful claim.
  Client b ->
  -- | 'Just b' if the claim was successful and the 'IO'
  --   computation completed within the given timeout.
  Client (Maybe b)
withClaim u v t act = do
  claims <- lookupClaims v
  case claims of
    [] -> claim -- Free
    [u'] | u == u' -> claim -- Claimed by 'u' (retries are allowed).
    _ -> pure Nothing -- Conflicting claims, TTL must expire.
  where
    -- [Note: Guarantees]
    claim = do
      let ttl = max minTtl (fromIntegral (t #> Second))
      retry x5 $ write upsertQuery $ params LocalQuorum (ttl * 2, C.Set [u], v)
      claimed <- (== [u]) <$> lookupClaims v
      if claimed
        then do
          act' <- clientToIO act
          liftIO $ timeout (fromIntegral ttl # Second) act'
        else pure Nothing
    upsertQuery :: PrepQuery W (Int32, C.Set (Id a), Text) ()
    upsertQuery = "UPDATE unique_claims USING TTL ? SET claims = claims + ? WHERE value = ?"

deleteClaim ::
  -- | The 'Id' associated with the claim.
  Id a ->
  -- | The value on which to acquire the claim.
  Text ->
  -- | The minimum timeout (i.e. duration) of the rest of the claim.  (Each
  --   claim can have more than one claimer (even though this is a feature we
  --   never use), so removing a claim is an update operation on the database.
  --   Therefore, we reset the TTL the same way we reset it in 'withClaim'.)
  Timeout ->
  Client ()
deleteClaim u v t = do
  let ttl = max minTtl (fromIntegral (t #> Second))
  retry x5 $ write cql $ params LocalQuorum (ttl * 2, C.Set [u], v)
  where
    cql :: PrepQuery W (Int32, C.Set (Id a), Text) ()
    cql = "UPDATE unique_claims USING TTL ? SET claims = claims - ? WHERE value = ?"

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

clientToIO :: Client a -> Client (IO a)
clientToIO act = do
  s <- ask
  pure $ runClient s act

minTtl :: Int32
minTtl = 60 -- Seconds

-- [Note: Guarantees]
-- ~~~~~~~~~~~~~~~~~~
-- Correct operation (i.e. uniqueness of claims) rests on the following
-- properties of the implementation, which must have a negligible probability
-- of failure:
--
-- 1. CRDT properties of CQL Sets with the only operation of element addition,
--    in particular that all element additions are preserved in a concurrent
--    setting (cf. https://aphyr.com/posts/294-jepsen-cassandra).
--
-- 2. Strong read consistency (QUORUM write followed by QUORUM read)
--    combined with the conflict-free property of Set element insertions (1)
--    ensures that, of any two concurrent claims, at least one of them is
--    bound to see both inserted elements, hence failing the claim.
--
-- 3. The 'IO' computation that is run while holding a claim must complete
--    within the given timeout. The effective timeout (i.e. the row TTL)
--    is doubled, for an extra safety margin.
