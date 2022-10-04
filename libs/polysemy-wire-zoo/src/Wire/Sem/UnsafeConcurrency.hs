{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Wire.Sem.UnsafeConcurrency where

import Data.Kind (Type)
import Imports
import Polysemy
import Polysemy.Internal

data ConcurrencySafety = Safe | Unsafe

-- | WARNING: VERY VERY UNSAFE
--
-- Polysemy "effect" for hinting about concurrency. This comes with a host of
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
-- Notice that almost everything in Polysemy is "stateful", even things that
-- don't invoke 'Polysemy.State.State'. The 'Polysemy.Error.Error' effect also
-- carries itself around as "state", and thus any interpretation composed of
-- these interpretations is subject to dropping observable state changes.
type Concurrency :: ConcurrencySafety -> (Type -> Type) -> Type -> Type
data Concurrency (safe :: ConcurrencySafety) m a where
  UnsafePooledMapConcurrentlyN ::
    Foldable t =>
    Int ->
    (a -> m b) ->
    t a ->
    Concurrency safe m [b]
  UnsafePooledMapConcurrentlyN_ ::
    Foldable t =>
    Int ->
    (a -> m b) ->
    t a ->
    Concurrency safe m ()

unsafePooledMapConcurrentlyN ::
  forall r t a b.
  (Member (Concurrency 'Unsafe) r, Foldable t) =>
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
  Int ->
  (a -> Sem r b) ->
  t a ->
  Sem r ()
unsafePooledMapConcurrentlyN_ n f as =
  send
    (UnsafePooledMapConcurrentlyN_ n f as :: Concurrency 'Unsafe (Sem r) ())
{-# INLINEABLE unsafePooledMapConcurrentlyN_ #-}

pooledMapConcurrentlyN ::
  forall r' r t a b.
  r' ~ '[Final IO] =>
  (Member (Concurrency 'Safe) r, Subsume r' r, Foldable t) =>
  Int ->
  (a -> Sem r' b) ->
  t a ->
  Sem r [b]
pooledMapConcurrentlyN n f as =
  send
    ( UnsafePooledMapConcurrentlyN n (subsume_ @r' @r . f) as ::
        Concurrency 'Safe (Sem r) [b]
    )
{-# INLINEABLE pooledMapConcurrentlyN #-}

pooledMapConcurrentlyN_ ::
  forall r' r t a b.
  r' ~ '[Final IO] =>
  (Member (Concurrency 'Safe) r, Subsume r' r, Foldable t) =>
  Int ->
  (a -> Sem r' b) ->
  t a ->
  Sem r ()
pooledMapConcurrentlyN_ n f as =
  send
    ( UnsafePooledMapConcurrentlyN_ n (subsume_ @r' @r . f) as ::
        Concurrency 'Safe (Sem r) ()
    )
{-# INLINEABLE pooledMapConcurrentlyN_ #-}
