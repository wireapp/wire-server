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
module Brig.Unique
  ( withClaim,
    getClaims,

    -- * Re-exports
    Timeout,
    TimeoutUnit (..),
    (#),
  )
where

import Brig.Data.Instances ()
import Brig.Sem.UniqueClaimsStore
import Data.Id
import Data.Timeout
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Resource
import Polysemy.Time.Data.TimeUnit

-- | Obtain a (temporary) exclusive claim on a 'Text' value for some
-- 'Id'entifier. The claim expires after the provided timeout, whether
-- it was successful or not. Contention can thus render the value
-- unavailable until the timeout expires if no contender succeeds.
-- The given 'IO' computation is only run when the claim was successful
-- and is responsible for turning the temporary claim into permanent
-- ownership, if desired.
withClaim ::
  Members '[Async, Race, Resource, UniqueClaimsStore] r =>
  -- | The 'Id' associated with the claim.
  Id a ->
  -- | The value on which to acquire the claim.
  Text ->
  -- | The minimum timeout (i.e. duration) of the claim.
  Timeout ->
  -- | The computation to run with a successful claim.
  Sem r b ->
  -- | 'Just b' if the claim was successful and the computation completed within
  --   the given timeout.
  Sem r (Maybe b)
withClaim u v t action = do
  claims <- getClaims v
  -- [Note: Guarantees]
  let claim = do
        let ttl = max minTtl t
        addClaims u t v
        if claims == [u]
          then withAsyncWait (toPolyTime ttl) action await
          else pure Nothing
  case claims of
    [] -> claim -- Free
    [u'] | u == u' -> claim -- Claimed by 'u' (retries are allowed).
    _ -> pure Nothing -- Conflicting claims, TTL must expire.

toPolyTime :: Timeout -> NanoSeconds
toPolyTime t = NanoSeconds . fromIntegral $ t #> NanoSecond

minTtl :: Timeout
minTtl = 60 # Second

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
