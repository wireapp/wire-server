module Main (main) where

import Data.Text qualified as T
import Data.Text.Internal.Search qualified as TS
import System.Environment (getArgs)
import System.IO (hPrint, stderr)
import Prelude

-- Call path to AVX512 instructions:
-- main -> TS.indices -> scanOne -> _hs_text_memchr -> AVX512 code
--
-- Not all Strings hit _hs_text_memchr. A good input for this is e.g. "AWAA"
main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    (str : _) -> pure $ T.pack str
    [] -> error "Please provide a string argument"

  -- This calls TS.indices which triggers the call chain to AVX512
  -- Force strict evaluation of the entire list
  -- Use a multi-character pattern to trigger the memchr path
  let result = TS.indices (T.pack "WA") input
      !len = length result -- Force evaluation of the list
      !first = case result of
        [] -> -1
        (x : _) -> x

  hPrint stderr (T.length input)
  hPrint stderr len
  print first
  print result
