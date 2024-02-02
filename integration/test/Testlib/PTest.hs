module Testlib.PTest where

import Data.Proxy
import GHC.TypeLits
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

instance (HasTests x) => HasTests (CredentialType -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[ctype=basic]") s f (x BasicCredentialType)
      <> mkTests m (n <> "[ctype=x509]") s f (x X509CredentialType)

-- | this is to resolve overlapping instances issues.
newtype WithBoundedEnumArg arg x = WithBoundedEnumArg (arg -> x)

instance (HasTests x, Enum arg, Bounded arg, Show arg) => HasTests (WithBoundedEnumArg arg x) where
  mkTests m n s f (WithBoundedEnumArg x) =
    mconcat
      [ mkTests m (n <> "[" <> show arg <> "]") s f (x arg)
        | arg <- [minBound ..]
      ]

-- | bool with a tag to prevent boolean blindness in test output.
newtype TaggedBool (tag :: Symbol) = TaggedBool {untag :: Bool}
  deriving newtype (Eq, Ord, Bounded, Enum)

instance KnownSymbol tag => Show (TaggedBool tag) where
  show (TaggedBool b) = show (symbolVal (Proxy @tag)) <> "=" <> show b

instance (KnownSymbol tag, HasTests x) => HasTests (TaggedBool tag -> x) where
  mkTests m n s f x =
    mconcat
      [ mkTests m (n <> "[" <> show arg <> "]") s f (x arg)
        | arg <- [minBound ..]
      ]
