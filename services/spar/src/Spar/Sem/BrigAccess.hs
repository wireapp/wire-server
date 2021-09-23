module Spar.Sem.BrigAccess where

import Bilge
import Polysemy

data BrigAccess m a where
  Call :: (Request -> Request) -> BrigAccess m ResponseLBS

makeSem ''BrigAccess
