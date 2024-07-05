module Testlib.PTest where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.Kind
import Data.Proxy
import Data.Traversable
import GHC.Generics
import GHC.TypeLits
import Testlib.Env
import Testlib.Types
import Prelude

type Test = (String, String, String, String, App ())

yieldTests :: (HasTests x) => String -> String -> String -> String -> x -> WriterT [Test] IO ()
yieldTests m n s f x = do
  t <- lift (mkTests m n s f x)
  tell t

class HasTests x where
  mkTests :: String -> String -> String -> String -> x -> IO [Test]

instance HasTests (App ()) where
  mkTests m n s f x = pure [(m, n, s, f, x)]

instance (HasTests x, TestCases a) => HasTests (a -> x) where
  mkTests m n s f x = do
    tcs <- mkTestCases @a
    fmap concat $ for tcs $ \tc ->
      mkTests m (n <> tc.testCaseName) s f (x tc.testCase)

data TestCase a = MkTestCase {testCaseName :: String, testCase :: a}
  deriving stock (Eq, Ord, Show, Generic, Functor)

-- | enumerate all members of a bounded enum type
class TestCases a where
  mkTestCases :: IO [TestCase a]

type Tagged :: Symbol -> Type -> Type
newtype Tagged s a = MkTagged {unTagged :: a}
  deriving stock (Eq, Ord, Show, Generic)

type TaggedBool s = Tagged s Bool

pattern TaggedBool :: Bool -> Tagged s Bool
pattern TaggedBool a = MkTagged a

{-# COMPLETE TaggedBool #-}

-- | only works for outer-most use of `Tagged` (not: `Maybe (Tagged "bla" Bool)`)
instance (GEnum (Rep a), KnownSymbol s, Generic a) => TestCases (Tagged s a) where
  mkTestCases =
    pure $
      uni @(Rep a) <&> \case
        -- replace the toplevel
        (Left _ : ls, tc) ->
          MkTestCase
            { testCaseName = foldr mkName "" (Left (symbolVal @s Proxy) : ls),
              testCase = MkTagged $ to tc
            }
        _ -> error "tagged test cases: impossible"

instance TestCases Ciphersuite where
  mkTestCases = pure $ do
    suite <- allCiphersuites
    pure $
      MkTestCase
        { testCaseName = mkName (Left "suite") suite.code,
          testCase = suite
        }

instance TestCases CredentialType where
  mkTestCases =
    pure
      [ MkTestCase "[ctype=basic]" BasicCredentialType,
        MkTestCase "[ctype=x509]" X509CredentialType
      ]

-- | a default instance, normally we don't do such things but this is more convenient in
--   the test suite as you don't have to derive anything
instance {-# OVERLAPPABLE #-} (Generic a, GEnum (Rep a)) => TestCases a where
  mkTestCases =
    pure $
      uni @(Rep a) <&> \(tcn, tc) ->
        MkTestCase
          { testCaseName = foldr mkName "" tcn,
            testCase = to tc
          }

{-# INLINE [1] mkName #-}
mkName :: Either String String -> String -> String
mkName (Left a) = \acc -> mconcat ["[", toLower <$> a, "=" <> acc <> "]"]
mkName (Right (fmap toLower -> a)) = \case
  [] -> a
  acc@('[' : _) -> a <> acc
  acc -> a <> "." <> acc

class GEnum f where
  uni :: [([Either String String], f x)]

instance (GEnum k, KnownSymbol n) => GEnum (D1 (MetaData n m p b) k) where
  uni = bimap (Left (symbolVal @n Proxy) :) M1 <$> uni @k

instance (GEnum k) => GEnum (S1 md k) where
  uni = fmap M1 <$> uni @k

instance (GEnum k, KnownSymbol n) => GEnum (C1 (MetaCons n p b) k) where
  uni = bimap (Right (symbolVal @n Proxy) :) M1 <$> uni @k

instance (GEnum k1, GEnum k2) => GEnum (k1 :+: k2) where
  uni = (fmap L1 <$> uni @k1) <> (fmap R1 <$> uni @k2)

instance GEnum U1 where
  uni = [([Right ""], U1)]

instance (GEnum (Rep k), Generic k) => GEnum (K1 r k) where
  uni = fmap (K1 . to) <$> uni @(Rep k)
