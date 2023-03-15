module JSON where

import App
import Data.Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Imports

get :: Maybe a -> App a
get = maybe (assertionFailure "expected value, got Nothing") pure

class HasId a where
  getId :: a -> App String

instance HasId Value where
  getId x = indexValue "id" x >>= asString

getObject :: Value -> App Object
getObject (Object o) = pure o
getObject _ = assertionFailure "expected object"

class IndexValue a where
  indexValue :: String -> a -> App Value

instance IndexValue Object where
  indexValue k o = case KM.lookup (KM.fromString k) o of
    Nothing -> assertionFailure $ "key '" <> k <> "' not found"
    Just v -> pure v

instance IndexValue Value where
  indexValue k x = getObject x >>= indexValue k

asString :: Value -> App String
asString (String s) = pure (T.unpack s)
asString _ = assertionFailure "expected string"
