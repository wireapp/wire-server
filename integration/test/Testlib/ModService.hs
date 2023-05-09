module Testlib.ModService where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally)
import qualified Control.Exception as E
import Control.Monad.Reader
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Traversable
import qualified Data.Yaml as Yaml
import GHC.Stack
import qualified Network.HTTP.Client as HTTP
import qualified Network.Socket as N
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO.Error as Error
import System.Process (CreateProcess (..), createProcess, proc, terminateProcess)
import Testlib.App
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Types

withModifiedService ::
  Service ->
  -- | function that edits the config
  (Value -> App Value) ->
  -- | This action wil access the modified spawned service
  App a ->
  App a
withModifiedService srv modConfig k = do
  withModifiedServices (Map.singleton srv modConfig) k

withModifiedServices :: Map.Map Service (Value -> App Value) -> App a -> App a
withModifiedServices services k = do
  ports <- Map.traverseWithKey (\_ _ -> liftIO openFreePort) services

  let updateServiceMapInConfig :: Value -> App Value
      updateServiceMapInConfig config =
        foldlM
          ( \c (srv, (port, _)) ->
              c
                & setField
                  (serviceName srv)
                  ( object
                      [ "host" .= ("127.0.0.1" :: String),
                        "port" .= port
                      ]
                  )
          )
          config
          (Map.assocs ports)

  instances <- for (Map.assocs services) $ \(srv, modifyConfig) -> do
    let srvName = serviceName srv
    config <- readServiceConfig srv
    config' <- updateServiceMapInConfig config >>= modifyConfig
    (tempFile, fh) <- liftIO $ openBinaryTempFile "/tmp" (srvName <> ".yaml")
    liftIO $ BS.hPut fh (Yaml.encode config')
    liftIO $ hClose fh

    (cwd, exe) <-
      asks (.servicesCwdBase) <&> \case
        Nothing -> (Nothing, srvName)
        Just dir ->
          (Just (dir </> srvName), "./dist" </> srvName)

    (_port, socket) <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup srv ports)
    liftIO $ N.close socket
    (_, _, _, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd}
    pure (ph, tempFile)

  let stopInstances = liftIO $ do
        -- Running waitForProcess would hang for 30 seconds when the test suite
        -- is run from within ghci, so we don't wait here.
        for_ instances $ \(ph, tmpfile) -> do
          terminateProcess ph
          removeFile tmpfile

  let updateServiceMap serviceMap =
        Map.foldrWithKey
          ( \srv (newPort, _) sm ->
              case srv of
                Brig -> sm {brig = sm.brig {host = "127.0.0.1", port = fromIntegral newPort}}
                Galley -> sm {galley = sm.galley {host = "127.0.0.1", port = fromIntegral newPort}}
                Cannon -> sm {cannon = sm.cannon {host = "127.0.0.1", port = fromIntegral newPort}}
          )
          serviceMap
          ports

  defaultDomain <- asks (.domain1)

  let modifyEnv env =
        env
          { serviceMap =
              Map.adjust
                updateServiceMap
                defaultDomain
                env.serviceMap
          }

  let waitForAllServices = do
        env <- ask
        liftIO $
          mapConcurrently_
            (\srv -> runReaderT (unApp (waitUntilServiceUp srv)) env)
            (Map.keys ports)

  App $
    ReaderT
      ( \env ->
          runReaderT
            ( local
                modifyEnv
                ( unApp $ do
                    waitForAllServices
                    k
                )
            )
            env
            `finally` stopInstances
      )

waitUntilServiceUp :: HasCallStack => Service -> App ()
waitUntilServiceUp srv = do
  d <- ownDomain
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      ( \_ -> do
          req <- baseRequest d srv Unversioned "/i/status"
          env <- ask
          eith <-
            liftIO $
              E.try
                ( runAppWithEnv env $ do
                    res <- submit "GET" req
                    pure (res.status `elem` [200, 204])
                )
          pure $ either (\(_e :: HTTP.HttpException) -> False) id eith
      )
  unless isUp $
    failApp ("Time out for service " <> show srv <> " to come up")

-- | Open a TCP socket on a random free port. This is like 'warp''s
--   openFreePort.
--
--   Since 0.0.0.1
openFreePort :: IO (Int, N.Socket)
openFreePort =
  E.bracketOnError (N.socket N.AF_INET N.Stream N.defaultProtocol) N.close $
    \sock -> do
      N.bind sock $ N.SockAddrInet 0 $ N.tupleToHostAddress (127, 0, 0, 1)
      N.getSocketName sock >>= \case
        N.SockAddrInet port _ -> pure (fromIntegral port, sock)
        addr ->
          E.throwIO $
            Error.mkIOError
              Error.userErrorType
              ( "openFreePort was unable to create socket with a SockAddrInet. "
                  <> "Got "
                  <> show addr
              )
              Nothing
              Nothing
