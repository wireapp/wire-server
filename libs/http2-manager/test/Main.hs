module Main where

import OpenSSL
import qualified Test.HTTP2.Client.ManagerSpec
import Test.Hspec

main :: IO ()
main =
  withOpenSSL $
    hspec $
      Test.HTTP2.Client.ManagerSpec.spec
