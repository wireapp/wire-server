module Testlib.Printing where

import Prelude

yellow :: String
yellow = "\x1b[38;5;11m"

purpleish :: String
purpleish = "\x1b[38;5;13m"

orange :: String
orange = "\x1b[38;5;3m"

red :: String
red = "\x1b[38;5;9m"

green :: String
green = "\x1b[32m"

resetColor :: String
resetColor = "\x1b[0m"

colored :: String -> String -> String
colored color s = color <> s <> resetColor

indent :: Int -> String -> String
indent n s =
  unlines (map (pad <>) (lines s))
  where
    pad = replicate n ' '
