module Wire.Sem.UnsafeConcurrency.IO where

import Imports
import Polysemy
import Polysemy.Final
import UnliftIO (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)
import Wire.Sem.UnsafeConcurrency

unsafelyPerformConcurrency :: Member (Final IO) r => Sem (UnsafeConcurrency ': r) a -> Sem r a
unsafelyPerformConcurrency = interpretFinal @IO $ \case
  UnsafePooledMapConcurrentlyN n f t -> do
    st <- getInitialStateS
    faction <- bindS f
    let action a = faction $ a <$ st
    z <- liftS $ pooledMapConcurrentlyN n action $ toList t
    Inspector ins <- getInspectorS
    pure $ fmap (fmap (mapMaybe ins)) z
  UnsafePooledMapConcurrentlyN_ n f t -> do
    st <- getInitialStateS
    faction <- bindS f
    let action a = faction $ a <$ st
    liftS $ pooledMapConcurrentlyN_ n action $ toList t
