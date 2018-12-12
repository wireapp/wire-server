module Bonanza.Anon
    ( anonymise
    )
where

import Imports
import Bonanza.Types
import Control.Lens        ((%~), _Wrapped', over)
import Data.HashMap.Strict (filterWithKey)


anonymise :: [Text] -> LogEvent -> LogEvent
anonymise [] evt = evt
anonymise ts evt = evt & logTags %~ stripTags
  where
    stripTags = over _Wrapped' (filterWithKey (\ k _ -> not (k `elem` ts)) )
{-# INLINABLE anonymise #-}
