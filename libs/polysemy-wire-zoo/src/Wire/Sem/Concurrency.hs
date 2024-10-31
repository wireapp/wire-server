{-# LANGUAGE StandaloneKindSignatures #-}

module Wire.Sem.Concurrency where

import Data.Kind (Type)
import Imports
import Polysemy

data ConcurrencySafety = Safe | Unsafe

-- | Polysemy "effect" for hinting about concurrency. This comes with a host of
-- caveats, because concurrency fundamentally is not an effect we can ascribe
-- any semantics to.
--
-- For example, what should the result of the following program be?
--
-- @@
-- unsafePooledMapConcurrentlyN_ 8 put [0..10]
-- get
-- @@
--
-- There is no answer, and the actual behavior depends on unpredictable quirks
-- of the runtime. In general, we have no means of combining the resulting
-- state changes, so we have no option other than to arbitrarily pick one.
--
-- This is confusing behavior --- especially when the call to `Concurrency` is
-- far away from the observed bug.
--
-- Notice that almost everything in Polysemy is "stateful", even things that
-- don't invoke 'Polysemy.State.State'. The 'Polysemy.Error.Error' effect also
-- carries itself around as "state", and thus any interpretation composed of
-- these interpretations is subject to dropping observable state changes.
--
-- There is a "safe" usage of 'Concurrency', at least, no more unsafe than 'IO'
-- when the action you want to perform concurrently requires only @'Final'
-- 'IO'@. This use case is common in interpreters which can statically
-- guarantee their scoped effects do not have access to the full polysemy
-- stack.
type Concurrency :: ConcurrencySafety -> (Type -> Type) -> Type -> Type
data Concurrency (safe :: ConcurrencySafety) m a where
  UnsafePooledMapConcurrentlyN ::
    (Foldable t) =>
    Int ->
    (a -> m b) ->
    t a ->
    Concurrency safe m [b]
  UnsafePooledMapConcurrentlyN_ ::
    (Foldable t) =>
    Int ->
    (a -> m b) ->
    t a ->
    Concurrency safe m ()

unsafePooledMapConcurrentlyN ::
  forall r t a b.
  (Member (Concurrency 'Unsafe) r, Foldable t) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  (a -> Sem r b) ->
  t a ->
  Sem r [b]
unsafePooledMapConcurrentlyN n f as =
  send
    ( UnsafePooledMapConcurrentlyN n f as ::
        Concurrency 'Unsafe (Sem r) [b]
    )
{-# INLINEABLE unsafePooledMapConcurrentlyN #-}

unsafePooledMapConcurrentlyN_ ::
  forall r t a b.
  (Member (Concurrency 'Unsafe) r, Foldable t) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  (a -> Sem r b) ->
  t a ->
  Sem r ()
unsafePooledMapConcurrentlyN_ n f as =
  send
    (UnsafePooledMapConcurrentlyN_ n f as :: Concurrency 'Unsafe (Sem r) ())
{-# INLINEABLE unsafePooledMapConcurrentlyN_ #-}

unsafePooledForConcurrentlyN ::
  forall r t a b.
  (Member (Concurrency 'Unsafe) r, Foldable t) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  t a ->
  (a -> Sem r b) ->
  Sem r [b]
unsafePooledForConcurrentlyN n as f =
  send
    (UnsafePooledMapConcurrentlyN n f as :: Concurrency 'Unsafe (Sem r) [b])
{-# INLINEABLE unsafePooledForConcurrentlyN #-}

unsafePooledForConcurrentlyN_ ::
  forall r t a b.
  (Member (Concurrency 'Unsafe) r, Foldable t) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  t a ->
  (a -> Sem r b) ->
  Sem r ()
unsafePooledForConcurrentlyN_ n as f =
  send
    (UnsafePooledMapConcurrentlyN_ n f as :: Concurrency 'Unsafe (Sem r) ())
{-# INLINEABLE unsafePooledForConcurrentlyN_ #-}
