module Spar.Sem.Random.IO where

import Data.Id (randomId)
import qualified Data.UUID.V4 as UUID
import Imports
import OpenSSL.Random (randBytes)
import Polysemy
import Spar.Sem.Random (Random (..))

randomToIO ::
  Member (Embed IO) r =>
  Sem (Random ': r) a ->
  Sem r a
randomToIO = interpret $ \case
  Bytes i -> embed $ randBytes i
  Uuid -> embed $ UUID.nextRandom
  ScimTokenId -> embed $ randomId @IO
