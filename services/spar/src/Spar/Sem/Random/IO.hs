module Spar.Sem.Random.IO where

import Imports
import Polysemy
import Spar.Sem.Random (Random(..))
import Data.Id (randomId)
import qualified Data.UUID.V4 as UUID
import OpenSSL.Random (randBytes)

randomToIO ::
  Member (Embed IO) r =>
  Sem (Random ': r) a ->
  Sem r a
randomToIO = interpret $ \case
  Bytes i -> embed $ randBytes i
  Uuid -> embed $ UUID.nextRandom
  ScimTokenId -> embed $ randomId @IO

