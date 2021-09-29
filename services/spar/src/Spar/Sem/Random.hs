module Spar.Sem.Random where

import Data.Id (ScimTokenId)
import Data.UUID (UUID)
import Imports
import Polysemy

data Random m a where
  Bytes :: Int -> Random m ByteString
  Uuid :: Random m UUID
  ScimTokenId :: Random m ScimTokenId

makeSem ''Random
