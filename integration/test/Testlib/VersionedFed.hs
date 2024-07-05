module Testlib.VersionedFed where

import Control.Monad.Reader
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import System.Environment
import Testlib.PTest
import Testlib.Prelude

data FedDomain n = FedDomain

instance MakesValue (FedDomain 0) where
  make FedDomain = asks (String . T.pack . (.federationV0Domain))

instance MakesValue (FedDomain 1) where
  make FedDomain = asks (String . T.pack . (.federationV1Domain))

instance (KnownNat n) => TestCases (FedDomain n) where
  mkTestCases =
    fmap (map (fmap (const FedDomain)))
      $ mkFedTestCase "" (natVal (Proxy @n))

mkFedTestCase :: String -> Integer -> IO [TestCase Integer]
mkFedTestCase name n = do
  v <- lookupEnv $ "ENABLE_FEDERATION_V" <> show n
  if v == Just "1"
    then pure [MkTestCase name n]
    else pure []

data AnyFedDomain = AnyFedDomain Integer

instance MakesValue AnyFedDomain where
  make (AnyFedDomain 0) = asks (String . T.pack . (.federationV0Domain))
  make (AnyFedDomain 1) = asks (String . T.pack . (.federationV1Domain))
  make (AnyFedDomain _) = error "invalid federation version"

instance TestCases AnyFedDomain where
  mkTestCases =
    fmap (map (fmap AnyFedDomain) . concat)
      $ traverse
        (uncurry mkFedTestCase)
        [("[fed=" <> show v <> "]", v) | v <- [0, 1]]
