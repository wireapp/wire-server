module Wire.Sem.UnsafeConcurrency where

import Imports

data UnsafeConcurrency m a where
  UnsafePooledMapConcurrentlyN ::
    Foldable t =>
    Int ->
    (a -> m b) ->
    t a ->
    UnsafeConcurrency m [b]
  UnsafePooledMapConcurrentlyN_ ::
    Foldable t =>
    Int ->
    (a -> m b) ->
    t a ->
    UnsafeConcurrency m ()
