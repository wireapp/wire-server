module Testlib.PTest where

import Testlib.App
import Testlib.Types
import Prelude

type Test = (String, String, String, String, App ())

class HasTests x where
  mkTests :: String -> String -> String -> String -> x -> [Test]

instance HasTests (App ()) where
  mkTests m n s f x = [(m, n, s, f, x)]

instance HasTests x => HasTests (Domain -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[domain=own]") s f (x OwnDomain)
      <> mkTests m (n <> "[domain=other]") s f (x OtherDomain)
