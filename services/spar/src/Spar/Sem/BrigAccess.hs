module Spar.Sem.BrigAccess where

import Bilge
import Polysemy

-- FUTUREWORK: A better effect here would be to exactly model the requests we
-- want to make of Brig. This would allow for non-HTTP interpretations.
data BrigAccess m a where
  Call :: (Request -> Request) -> BrigAccess m ResponseLBS

makeSem ''BrigAccess
