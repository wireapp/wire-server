module Main (main) where

import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

-- Minimal service to test AVX512 instruction compatibility on cloud environments.
-- Uses Data.Text operations that trigger the text library's SIMD-optimized memchr
-- implementation (containing AVX512 instructions: vmovdqu64, vpbroadcastd, kmovw).
--
-- Key requirements to preserve AVX512 in the binary:
--   1. T.findIndex - triggers AVX512-optimized character search
--   2. T.length + stderr output - forces text library code to be linked (not optimized away)
--   3. Runtime input (stdin/args) - prevents compile-time constant folding
--
-- Usage:
--   memchr-test [STRING]
--     STRING (optional): text to search in. If omitted, reads from stdin.
--     Searches for 'W' and prints its index (or -1 if not found).
--     Also prints string length to stderr.
--
-- Examples:
--   echo "Hello World" | memchr-test    # Outputs: 11\n6
--   memchr-test "Hello World"           # Outputs: 11\n6
main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    (str:_) -> pure $ T.pack str
    []      -> TIO.getLine

  let idx = T.findIndex (== 'W') input

  hPrint stderr (T.length input)

  case idx of
    Nothing -> print (-1 :: Int)
    Just i  -> print i
