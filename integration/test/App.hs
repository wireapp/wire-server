module App where

import Config
import qualified Control.Exception as E
import Control.Monad.Trans.Reader
import Data.Aeson
import Imports hiding (ask, asks)
import qualified Network.HTTP.Client as HTTP
import qualified Test.HUnit as HUnit
import qualified Test.HUnit.Lang as HUnit

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: App a -> IO a
runApp m = do
  e <- mkEnv
  runAppWithEnv e m

runAppWithEnv :: Env -> App a -> IO a
runAppWithEnv e m = runReaderT (unApp m) e

getContext :: App Context
getContext = App $ asks (.context)

getManager :: App HTTP.Manager
getManager = App $ asks (.manager)

onFailure :: App a -> IO b -> App a
onFailure m k = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \exc -> do
    void k
    E.throw (exc :: HUnit.HUnitFailure)

getPrekey :: App Value
getPrekey = App $ do
  pks <- asks (.prekeys)
  (i, pk) <- atomicModifyIORef pks getPK
  pure $ object ["id" .= i, "key" .= pk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

getLastPrekey :: App Value
getLastPrekey = App $ do
  pks <- asks (.lastPrekeys)
  lpk <- atomicModifyIORef pks getPK
  pure $ object ["id" .= lastPrekeyId, "key" .= lpk]
  where
    getPK [] = error "Out of prekeys"
    getPK (k : ks) = (ks, k)

    lastPrekeyId :: Int
    lastPrekeyId = 65535

-- TODO: add call stack
data AssertionFailure = AssertionFailure String
  deriving (Show)

instance Exception AssertionFailure where
  displayException (AssertionFailure msg) = msg

(@?=) :: (Eq a, Show a) => a -> a -> App ()
a @?= b = liftIO $ a HUnit.@?= b

assertionFailure :: String -> App a
assertionFailure msg =
  deepseq msg $
    liftIO $
      E.throw (AssertionFailure msg)
