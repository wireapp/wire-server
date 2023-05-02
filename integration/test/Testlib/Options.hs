module Testlib.Options where

import Data.List.Split
import Data.Tagged (Tagged (Tagged))
import Imports
import Options.Applicative (Alternative (some), help, long, metavar, option, short, str)
import Test.Tasty.Options (IsOption (..))
import Test.Tasty.Patterns.Types (Expr (..))

data TestSelection
  = NoSelection
  | SelectMany [String]
  deriving (Show)

instance IsOption TestSelection where
  defaultValue = NoSelection
  parseValue s = Just (SelectMany [s])
  optionHelp = Tagged ""
  showDefaultValue = const Nothing
  optionName = Tagged "match"
  optionCLParser =
    SelectMany
      <$> some
        ( option
            str
            ( long "match"
                <> metavar "SUBSTRINGS"
                <> short 'm'
                <> help "Select only tests that contain any of the comma-separated SUBSTRINGS in their full name. Can be specified multiple times, e.g. -m test1 -m test2 is the same as -m test1,test2. The variable TASTY_MATCH can be used to pass this option."
            )
        )

convertToAwk :: TestSelection -> Maybe Expr
convertToAwk NoSelection = Nothing
convertToAwk (SelectMany []) = Nothing
convertToAwk (SelectMany (m : ms)) =
  Just (go m ms)
  where
    go n [] = match n
    go n (n2 : ns) = Or (match n) (go n2 ns)
    match :: String -> Expr
    match s =
      case splitOn "," s of
        [] -> error "impossible"
        [n] -> Match (Field (IntLit 0)) n
        (n : n2 : ns) -> Or (Match (Field (IntLit 0)) n) (go n2 ns)
