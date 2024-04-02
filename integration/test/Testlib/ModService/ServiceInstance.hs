module Testlib.ModService.ServiceInstance
  ( ServiceInstance,
    startServiceInstance,
    cleanupServiceInstance,
    flushServiceInstanceOutput,
  )
where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import Debug.TimeStats
import System.Directory
import System.IO
import qualified System.IO.Error as E
import System.Posix
import System.Process
import Testlib.Printing
import Testlib.Types
import Prelude

data ServiceInstance = ServiceInstance
  { name :: String,
    domain :: String,
    processHandle :: ProcessHandle,
    stdoutChan :: Chan LineOrEOF,
    stderrChan :: Chan LineOrEOF,
    cleanupPath :: FilePath
  }

startServiceInstance :: FilePath -> [String] -> Maybe FilePath -> FilePath -> String -> String -> IO ServiceInstance
startServiceInstance exe args workingDir pathToCleanup execName execDomain = measureM "startServiceInstance" do
  (_, Just stdoutHdl, Just stderrHdl, ph) <-
    createProcess
      (proc exe args)
        { cwd = workingDir,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  (out1, out2) <- mkChans stdoutHdl
  (err1, err2) <- mkChans stderrHdl
  void $ forkIO $ logChanToConsole execName execDomain out1
  void $ forkIO $ logChanToConsole execName execDomain err1
  pure $
    ServiceInstance
      { name = execName,
        domain = execDomain,
        processHandle = ph,
        stdoutChan = out2,
        stderrChan = err2,
        cleanupPath = pathToCleanup
      }

cleanupServiceInstance :: ServiceInstance -> App ()
cleanupServiceInstance inst = measureM "cleanupService" . liftIO $ do
  let ignoreExceptions action = E.catch action $ \(_ :: E.SomeException) -> pure ()
  ignoreExceptions $ do
    mPid <- getPid inst.processHandle
    for_ mPid (signalProcess killProcess)
    void $ waitForProcess inst.processHandle
  whenM (doesFileExist inst.cleanupPath) $ removeFile inst.cleanupPath
  whenM (doesDirectoryExist inst.cleanupPath) $ removeDirectoryRecursive inst.cleanupPath

flushServiceInstanceOutput :: ServiceInstance -> IO String
flushServiceInstanceOutput serviceInstance = measureM "flushProcessState" do
  outStr <- flushChan serviceInstance.name serviceInstance.domain serviceInstance.stdoutChan
  errStr <- flushChan serviceInstance.name serviceInstance.domain serviceInstance.stderrChan
  statusStr <- getPid serviceInstance.processHandle <&> maybe "(already closed)" show
  pure $
    unlines
      [ "=== process pid: =======================================",
        statusStr,
        "\n\n=== stdout: ============================================",
        outStr,
        "\n\n=== stderr: ============================================",
        errStr
      ]

data LineOrEOF = Line String | EOF
  deriving (Eq, Show)

logChanToConsole :: String -> String -> Chan LineOrEOF -> IO ()
logChanToConsole execName domain chan = go
  where
    go =
      readChan chan >>= \case
        Line line -> do
          putStrLn (decorateLine execName domain line)
          go
        EOF -> pure ()

-- | Read everything from a channel and return it as a decorated multi-line String.
flushChan :: String -> String -> Chan LineOrEOF -> IO String
flushChan execName domain chan = measureM "flushChan" do
  let go lns =
        readChan chan >>= \case
          Line ln -> go (ln : lns)
          EOF -> pure (reverse lns)
  (unlines . fmap (decorateLine execName domain)) <$> go []

-- | Run a thread that feeds output from a 'Handle' into two channels.
--
-- (We could also duplicate the posic handle, not the chan.  might save a few LOC.)
mkChans :: Handle -> IO (Chan LineOrEOF, Chan LineOrEOF)
mkChans hdl = do
  chn1 <- newChan
  chn2 <- dupChan chn1
  let go = do
        packet <- catchEOF (hGetLine hdl)
        writeList2Chan chn1 packet
        unless (EOF `elem` packet) go
  void $ forkIO go
  pure (chn1, chn2)

-- | If 'SomeException' is thrown, show it, split up in lines, and feed it to the output
-- followed be '[EOF]'.  (But if the exception is 'EOF', do not add it to the output.)
catchEOF :: IO String -> IO [LineOrEOF]
catchEOF feed =
  (((: []) . Line) <$> feed)
    `E.catch` handleEOF
    `E.catch` handleEverythingElse
  where
    handleEOF :: E.IOException -> IO [LineOrEOF]
    handleEOF e =
      if E.isEOFError e
        then pure [EOF]
        else renderErr e

    handleEverythingElse :: E.SomeException -> IO [LineOrEOF]
    handleEverythingElse e = renderErr e

    renderErr :: E.Exception e => e -> IO [LineOrEOF]
    renderErr e = pure $ (Line <$> lines (show e)) <> [EOF]

decorateLine :: String -> String -> String -> String
decorateLine execName domain = colorize . (prefix <>)
  where
    prefix = "[" <> execName <> "@" <> domain <> "] "
    colorize = fromMaybe id (lookup execName processColors)

processColors :: [(String, String -> String)]
processColors =
  [ ("brig", colored green),
    ("galley", colored yellow),
    ("gundeck", colored blue),
    ("cannon", colored orange),
    ("cargohold", colored purpleish),
    ("spar", colored orange),
    ("federator", colored blue),
    ("background-worker", colored blue),
    ("nginx", colored purpleish)
  ]
