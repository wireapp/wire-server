{-# LANGUAGE NoImplicitPrelude #-}

-- | A version of "Options.Applicative" with extra utilities.
module Options.Applicative.Extended
    ( module Options.Applicative
    -- * Extra option readers
    , autoRange
    ) where

import BasePrelude
import Data.List.Extra (stripInfix)
import Options.Applicative

-- | A reader that accepts either @N@ or @N..M@ (not necessarily just
-- numbers).
autoRange :: Read a => ReadM (a, a)
autoRange = eitherReader $ \arg -> case stripInfix ".." arg of
    Nothing -> (\a -> (a, a)) <$> readEither arg
    Just (l, r) -> case (readEither l, readEither r) of
        (Right lv, Right rv) -> Right (lv, rv)
        (Left e, _) -> Left ("can't parse lower end: " <> e)
        (_, Left e) -> Left ("can't parse upper end: " <> e)
