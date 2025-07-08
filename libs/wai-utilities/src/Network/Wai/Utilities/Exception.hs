{-# LANGUAGE ImplicitParams #-}

module Network.Wai.Utilities.Exception where

import Control.Exception
import Imports

-- | `displayException` with empty `ExceptionContext`
--
-- Starting with GHC 9.10, exceptions carry a context that contains backtraces.
-- Displaying these  is not always desired; e.g. for HTTP response bodies.
displayExceptionNoBacktrace :: (Exception e) => e -> String
displayExceptionNoBacktrace = trim . displayException . toException
  where
    trim = (dropWhileEnd isSpace) . (dropWhile isSpace)
