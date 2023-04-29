module Testlib.Options where

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
  parseValue = const Nothing
  optionHelp = Tagged ""
  showDefaultValue = const Nothing
  optionName = Tagged ""
  optionCLParser =
    SelectMany
      <$> some
        ( option
            str
            ( long "match"
                <> metavar "SUBSTRING"
                <> short 'm'
                <> help "!!!! Select only tests that contain the substring in their full name. Can be specified multiple times. Use this to select multiple tests, e.g. -m test1 -m test2."
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
    match s = Match (Field (IntLit 0)) s
