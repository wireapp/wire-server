module Wire.Sem.UnsafeConcurrency.Sequential where

import Imports
import Wire.Sem.UnsafeConcurrency
import Polysemy


sequentiallyPerformConcurrency :: Sem (UnsafeConcurrency ': r) a -> Sem r a
sequentiallyPerformConcurrency = interpretH $ \case
  UnsafePooledMapConcurrentlyN _ f t -> do
    st <- getInitialStateT
    Inspector ins <- getInspectorT
    faction <- bindT f
    help ins faction st $ toList t
  UnsafePooledMapConcurrentlyN_ _ f t -> do
    st <- getInitialStateT
    faction <- bindT f
    help_ faction st $ toList t


help
    :: Functor f
    => (forall x. f x -> Maybe x)
    -> (f a1 -> Sem (UnsafeConcurrency : r) (f b))
    -> f ()
    -> [a1]
    -> Sem (WithTactics UnsafeConcurrency f (Sem rInitial) r) (f [b])
help _ _ st [] = pure $ [] <$ st
help ins f st (a : as) = do
  let run_it = raise . sequentiallyPerformConcurrency
  st' <- run_it $ f $ a <$ st
  fmap (maybe id (\b -> fmap (b:)) $ ins st') $ help ins f (() <$ st') as


help_
    :: Functor f
    => (f a1 -> Sem (UnsafeConcurrency : r) (f b))
    -> f ()
    -> [a1]
    -> Sem (WithTactics UnsafeConcurrency f (Sem rInitial) r) (f ())
help_ _ st [] = pure $ () <$ st
help_ f st (a : as) = do
  let run_it = raise . sequentiallyPerformConcurrency
  st' <- run_it $ f $ a <$ st
  help_ f (() <$ st') as

