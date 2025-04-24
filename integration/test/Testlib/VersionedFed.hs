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

instance MakesValue (FedDomain 2) where
  make FedDomain = asks (String . T.pack . (.federationV2Domain))

instance (KnownNat n) => TestCases (FedDomain n) where
  mkTestCases =
    let v = natVal (Proxy @n)
     in map (fmap (const FedDomain))
          <$> mkFedTestCase ("[domain=fed-v" <> show v <> "]") v

mkFedTestCase :: String -> Integer -> IO [TestCase Integer]
mkFedTestCase name n = do
  v <- lookupEnv $ "ENABLE_FEDERATION_V" <> show n
  if v == Just "1"
    then pure [MkTestCase name n]
    else pure []

data AnyFedDomain = AnyFedDomain {unFedDomain :: Integer}

instance MakesValue AnyFedDomain where
  make (AnyFedDomain 0) = asks (String . T.pack . (.federationV0Domain))
  make (AnyFedDomain 1) = asks (String . T.pack . (.federationV1Domain))
  make (AnyFedDomain 2) = asks (String . T.pack . (.federationV2Domain))
  make (AnyFedDomain _) = error "invalid federation version"

instance TestCases AnyFedDomain where
  mkTestCases =
    map (fmap AnyFedDomain)
      . concat
      <$> traverse
        (uncurry mkFedTestCase)
        [("[domain=fed-v" <> show v <> "]", v) | v <- [0, 1, 2]]

-- | This can be used as an argument for parametrised tests. It will be bound
-- to at least 'OtherDomain', and optionally to legacy federated domains,
-- according to the values of the corresponding environment variables
-- (@ENABLE_FEDERATION_V0@ and similar).
data StaticDomain = StaticDomain | StaticFedDomain Integer
  deriving (Eq)

instance MakesValue StaticDomain where
  make StaticDomain = make OtherDomain
  make (StaticFedDomain n) = make (AnyFedDomain n)

instance TestCases StaticDomain where
  mkTestCases = do
    feds <-
      map (fmap StaticFedDomain)
        . concat
        <$> traverse
          (uncurry mkFedTestCase)
          [("[domain=fed-v" <> show v <> "]", v) | v <- [0, 1, 2]]
    pure $ [MkTestCase "[domain=other]" StaticDomain] <> feds
