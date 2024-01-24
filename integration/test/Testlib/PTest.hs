module Testlib.PTest where

import Testlib.App
import Testlib.Env
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

instance HasTests x => HasTests (Ciphersuite -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[suite=" <> suite.code <> "]") s f (x suite)
        | suite <- allCiphersuites
      ]

instance HasTests x => HasTests (Bool -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[" <> show b1 <> "]") s f (x b1)
        | b1 <- [minBound ..]
      ]

instance HasTests x => HasTests ((Bool, Bool) -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[" <> show b1 <> "," <> show b2 <> "]") s f (x (b1, b2))
        | b1 <- [minBound ..],
          b2 <- [minBound ..]
      ]

instance HasTests x => HasTests ((Bool, Bool, Bool) -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[" <> show b1 <> "," <> show b2 <> "," <> show b3 <> "]") s f (x (b1, b2, b3))
        | b1 <- [minBound ..],
          b2 <- [minBound ..],
          b3 <- [minBound ..]
      ]

instance HasTests x => HasTests ((Bool, Bool, Bool, Bool) -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[" <> show b1 <> "," <> show b2 <> "," <> show b3 <> "," <> show b4 <> "]") s f (x (b1, b2, b3, b4))
        | b1 <- [minBound ..],
          b2 <- [minBound ..],
          b3 <- [minBound ..],
          b4 <- [minBound ..]
      ]
