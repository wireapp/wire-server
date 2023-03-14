module App where

import Config
import qualified Control.Exception as E
import Control.Monad.Trans.Reader
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

(@?=) :: (Eq a, Show a) => a -> a -> App ()
a @?= b = liftIO $ a HUnit.@?= b
