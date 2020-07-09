-- | <https://github.com/hspec/hspec-wai/pull/49>, <https://github.com/hspec/hspec-wai/pull/41>
module Test.Hspec.Wai.Extra (
  bodyContains
, bodySatisfies
) where

import           Prelude
import           Control.Monad
import           Data.Char
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Test.Hspec.Wai.Matcher

bodyContains :: Body -> MatchBody
bodyContains body = bodySatisfies body SB.isInfixOf

bodySatisfies :: Body -> (ByteString -> ByteString -> Bool) -> MatchBody
bodySatisfies body prop = MatchBody (\_ actual -> bodyMatcher actual body)
  where
    bodyMatcher :: Body -> Body -> Maybe String
    bodyMatcher (toStrict -> actual) (toStrict -> expected) = actualExpected "body mismatch:" actual_ expected_ <$ guard (not $ expected `prop` actual)
      where
        (actual_, expected_) = case (safeToString actual, safeToString expected) of
          (Just x, Just y) -> (x, y)
          _ -> (show actual, show expected)

actualExpected :: String -> String -> String -> String
actualExpected message actual expected = unlines [
    message
  , "  expected: " ++ expected
  , "  but got:  " ++ actual
  ]

----------------------------------------------------------------------
-- from "Test.Hspec.Wai.Util"

toStrict :: LB.ByteString -> ByteString
toStrict = mconcat . LB.toChunks

safeToString :: ByteString -> Maybe String
safeToString bs = do
  str <- either (const Nothing) (Just . TS.unpack) (TS.decodeUtf8' bs)
  let isSafe = not $ case str of
        [] -> True
        _  -> isSpace (last str) || any (not . isPrint) str
  guard isSafe >> return str
