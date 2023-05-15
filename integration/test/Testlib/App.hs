module Testlib.App where

import Control.Monad.Reader
import qualified Control.Retry as Retry
import Data.Aeson hiding ((.=))
import Data.IORef
import qualified Data.Yaml as Yaml
import GHC.Exception
import System.FilePath
import Testlib.Env
import Testlib.JSON
import Testlib.Types
import Prelude

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

-- | Run an action, `recoverAll`ing with exponential backoff (min step 8ms, total timeout
-- ~15s).  Search this package for examples how to use it.
--
-- Ideally, this will be the only thing you'll ever need from the retry package when writing
-- integration tests.  If you are unhappy with it,, please consider fixing it so everybody can
-- benefit.
unrace :: App a -> App a
unrace action = Retry.recoverAll (Retry.exponentialBackoff 8000 <> Retry.limitRetries 10) (const action)
