module Util.Attoparsec where

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Imports

takeUpToWhile :: Int -> (Char -> Bool) -> Atto.Parser ByteString
takeUpToWhile maxCount predicate =
  Atto.scan maxCount $ \count char -> do
    guard (count > 0)
    guard (predicate char)
    Just (count - 1)
