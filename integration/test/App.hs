module App where

import Config
import qualified Control.Exception as E
import Control.Monad.Trans.Reader
import Data.Aeson
import GHC.Exception
import GHC.Stack
import Imports hiding (ask, asks)
import qualified Network.HTTP.Client as HTTP

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runApp :: App () -> IO ()
runApp m = do
  e <- mkEnv
  E.try (runAppWithEnv e m) >>= \case
    Left (AssertionFailure stack msg) -> do
      displayStack stack
      putStrLn "\x1b[38;5;1massertion failed\x1b[0m"
      putStrLn msg
    Right () -> pure ()

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
    E.throw (exc :: AssertionFailure)

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

data AssertionFailure = AssertionFailure CallStack String
  deriving (Show)

instance Exception AssertionFailure where
  displayException (AssertionFailure _ msg) = msg

(@?=) :: (Eq a, Show a, HasCallStack) => a -> a -> App ()
a @?= b = unless (a == b) $ do
  assertionFailure $ "Expected: " <> show a <> "\n" <> "Actual: " <> show b

assertionFailure :: HasCallStack => String -> App a
assertionFailure msg =
  deepseq msg $
    liftIO $
      E.throw (AssertionFailure callStack msg)

displayStack :: CallStack -> IO ()
displayStack = traverse_ putStrLn . ("\x1b[38;5;11mcall stack:\x1b[0m" :) . drop 1 . prettyCallStackLines
