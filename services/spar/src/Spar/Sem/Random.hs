module Spar.Sem.Random where

import Imports
import Polysemy
import Data.Id (ScimTokenId)
import Data.UUID (UUID)

data Random m a where
  Bytes :: Int -> Random m ByteString
  Uuid :: Random m UUID
  ScimTokenId :: Random m ScimTokenId

makeSem ''Random

