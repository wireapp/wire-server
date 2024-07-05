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

instance (KnownNat n) => TestCases (FedDomain n) where
  mkTestCases = do
    v <- lookupEnv $ "ENABLE_FEDERATION_V" <> show (natVal (Proxy @n))
    if v == Just "1"
      then pure [MkTestCase "" FedDomain]
      else pure []
