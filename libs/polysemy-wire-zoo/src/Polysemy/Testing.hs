module Polysemy.Testing where

import Imports
import Polysemy
import Polysemy.Internal

-- | @'intersperse' m a@ runs @m@ before every action in @a@. In this way, it's
-- like injecting logic into each bind. Useful for polling asynchronous results
-- when testing IO.
intersperse ::
  Sem r () ->
  Sem r a ->
  Sem r a
intersperse a (Sem m) =
  Sem $ \k -> m $ \u -> do
    usingSem k a
    k u
