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
        [("[domain=fed-v" <> show v <> "]", v) | v <- [0, 1]]

-- | This can be used as an argument for parametrised tests. It will be bound
-- to at least 'OtherDomain', and optionally to legacy federated domains,
-- according to the values of the corresponding environment variables
-- (@ENABLE_FEDERATION_V0@ and similar).
data StaticDomain = StaticDomain | StaticFedDomain Integer

instance MakesValue StaticDomain where
  make StaticDomain = make OtherDomain
  make (StaticFedDomain n) = make (AnyFedDomain n)

instance TestCases StaticDomain where
  mkTestCases = do
    feds <-
      fmap (map (fmap StaticFedDomain) . concat)
        $ traverse
          (uncurry mkFedTestCase)
          [("[domain=fed-v" <> show v <> "]", v) | v <- [0, 1]]
    pure $ [MkTestCase "[domain=other]" StaticDomain] <> feds
