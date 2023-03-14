module App where

import Config
import Control.Monad.Trans.Reader
import Imports hiding (asks)
import qualified Network.HTTP.Client as HTTP
import qualified Test.HUnit as HUnit

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: App a -> IO a
runApp m = do
  env <- mkEnv
  runReaderT (unApp m) env

getContext :: App Context
getContext = App $ asks (.context)

getManager :: App HTTP.Manager
getManager = App $ asks (.manager)

(@?=) :: (Eq a, Show a) => a -> a -> App ()
a @?= b = liftIO $ a HUnit.@?= b
