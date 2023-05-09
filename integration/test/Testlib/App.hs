module Testlib.App where

import Control.Monad.Reader
import Data.Aeson hiding ((.=))
import Data.IORef
import qualified Data.Yaml as Yaml
import GHC.Exception
import System.FilePath
import Testlib.Env
import Testlib.JSON
import Testlib.Types

failApp :: String -> App a
failApp msg = throw (AppFailure msg)

getPrekey :: App Value
getPrekey = App $ do
  pks <- asks (.prekeys)
  (i, pk) <- liftIO $ atomicModifyIORef pks getPK
  pure $ object ["id" .= i, "key" .= pk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

getLastPrekey :: App Value
getLastPrekey = App $ do
  pks <- asks (.lastPrekeys)
  lpk <- liftIO $ atomicModifyIORef pks getPK
  pure $ object ["id" .= lastPrekeyId, "key" .= lpk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

    lastPrekeyId :: Int
    lastPrekeyId = 65535

readServiceConfig :: Service -> App Value
readServiceConfig srv = do
  basedir <- asks (.serviceConfigsDir)
  let srvName = serviceName srv
      cfgFile = basedir </> srvName </> "conf" </> (srvName <> ".yaml")
  eith <- liftIO (Yaml.decodeFileEither cfgFile)
  case eith of
    Left err -> failApp ("Error while parsing " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err)
    Right value -> pure value

ownDomain :: App String
ownDomain = asks (.domain1)

otherDomain :: App String
otherDomain = asks (.domain2)
