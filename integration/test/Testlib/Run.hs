module Testlib.Run (main, mainI, createGlobalEnv) where

import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Aeson (Value)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import qualified Data.Map as Map
import Data.PEM
import Data.Time.Clock
import Data.Traversable (for)
import RunAllTests
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Testlib.App
import Testlib.Assertions
import Testlib.Env
import Testlib.JSON
import Testlib.Options
import Testlib.Printing
import Testlib.Types
import Testlib.XML
import Text.Printf
import UnliftIO.Async
import Prelude

runTest :: GlobalEnv -> App a -> IO (Either String a)
runTest ge action = lowerCodensity $ do
  env <- mkEnv ge
  liftIO $
    (Right <$> runAppWithEnv env action)
      `E.catches` [ E.Handler $ \(e :: SomeAsyncException) -> do
                      -- AsyncExceptions need rethrowing
                      -- to prevent the last handler from handling async exceptions.
                      -- This ensures things like UserInterrupt are properly handled.
                      E.throw e,
                    E.Handler -- AssertionFailure
                      (fmap Left . printFailureDetails),
                    E.Handler
                      (fmap Left . printExceptionDetails)
                  ]

pluralise :: Int -> String -> String
pluralise 1 x = x
pluralise _ x = x <> "s"

printReport :: TestSuiteReport -> IO ()
printReport report = do
  let numTests = length report.cases
      failures = filter (\testCase -> testCase.result /= TestSuccess) report.cases
      numFailures = length failures
  when (numFailures > 0) $ putStrLn $ "----------"
  putStrLn $ show numTests <> " " <> pluralise numTests "test" <> " run."
  when (numFailures > 0) $ do
    putStrLn ""
    putStrLn $ colored red (show numFailures <> " failed " <> pluralise numFailures "test" <> ": ")
    for_ failures $ \testCase ->
      putStrLn $ " - " <> testCase.name

testFilter :: TestOptions -> String -> Bool
testFilter opts n = included n && not (excluded n)
  where
    included name =
      (null opts.includeTests) || any (\x -> x `isInfixOf` name) opts.includeTests
    excluded name = any (\x -> x `isInfixOf` name) opts.excludeTests

withTime :: IO a -> IO (a, NominalDiffTime)
withTime action = do
  tm0 <- getCurrentTime
  a <- action
  tm1 <- getCurrentTime
  pure (a, diffUTCTime tm1 tm0)

printTime :: NominalDiffTime -> String
printTime =
  printf "%.02f s"
    . (/ 100.0)
    . (fromIntegral :: Integer -> Float)
    . truncate
    . (* 100)
    . nominalDiffTimeToSeconds

main :: IO ()
main = do
  opts <- getOptions
  let f = testFilter opts
      cfg = opts.configFile

  allTests <- mkAllTests
  let tests =
        filter (\(qname, _, _, _) -> f qname)
          . sortOn (\(qname, _, _, _) -> qname)
          $ allTests <&> \(module_, name, summary, full, action) ->
            let module0 = case module_ of
                  ('T' : 'e' : 's' : 't' : '.' : m) -> m
                  _ -> module_
                qualifiedName = module0 <> "." <> name
             in (qualifiedName, summary, full, action)

  if opts.listTests then doListTests tests else runTests tests opts.xmlReport cfg

createGlobalEnv :: FilePath -> Codensity IO GlobalEnv
createGlobalEnv cfg = do
  genv0 <- mkGlobalEnv cfg
  -- Run codensity locally here, because we only need the environment to get at
  -- Galley's configuration. Accessing the environment has the side effect of
  -- creating a temporary mls directory, which we don't need here.

  let removalKeysDir = gTempDir genv0 </> "removal-keys"
  keys <- liftIO . lowerCodensity $ do
    env <- mkEnv genv0
    liftIO $ createDirectoryIfMissing True removalKeysDir
    liftIO . runAppWithEnv env $ do
      config <- readServiceConfig Galley
      for
        [ ("ed25519", loadEd25519Key),
          ("ecdsa_secp256r1_sha256", loadEcKey "ecdsa_secp256r1_sha256" 73),
          ("ecdsa_secp384r1_sha384", loadEcKey "ecdsa_secp384r1_sha384" 88),
          ("ecdsa_secp521r1_sha512", loadEcKey "ecdsa_secp521r1_sha512" 108)
        ]
        $ \(sigScheme, load) -> do
          key <- load config
          let path = removalKeysDir </> (sigScheme <> ".key")
          liftIO $ B.writeFile path key
          pure (sigScheme, path)

  -- save removal key to a temporary file
  pure genv0 {gRemovalKeyPaths = Map.fromList keys}

getPrivateKeyPath :: Value -> String -> App FilePath
getPrivateKeyPath config signatureScheme = do
  relPath <- config %. "settings.mlsPrivateKeyPaths.removal" %. signatureScheme & asString
  asks \env' -> case env'.servicesCwdBase of
    Nothing -> relPath
    Just dir -> dir </> "galley" </> relPath

loadEcKey :: String -> Int -> Value -> App ByteString
loadEcKey sigScheme offset config = do
  path <- getPrivateKeyPath config sigScheme
  bs <- liftIO $ B.readFile path
  pems <- case pemParseBS bs of
    Left err -> assertFailure $ "Could not parse removal key PEM: " <> err
    Right x -> pure x
  asn1 <- pemContent <$> assertOne pems
  -- quick and dirty ASN.1 decoding: assume the key is of the correct
  -- format, and simply skip the header
  pure $ B.drop offset asn1

loadEd25519Key :: Value -> App ByteString
loadEd25519Key config = do
  path <- getPrivateKeyPath config "ed25519"
  bs <- liftIO $ B.readFile path
  pems <- case pemParseBS bs of
    Left err -> assertFailure $ "Could not parse removal key PEM: " <> err
    Right x -> pure x
  asn1 <- pemContent <$> assertOne pems
  -- quick and dirty ASN.1 decoding: assume the key is of the correct
  -- format, and simply skip the 16 byte header
  let bytes = B.drop 16 asn1
  priv <- liftIO . throwCryptoErrorIO $ Ed25519.secretKey bytes
  pure (convert (Ed25519.toPublic priv))

runTests :: [(String, x, y, App ())] -> Maybe FilePath -> FilePath -> IO ()
runTests tests mXMLOutput cfg = do
  output <- newChan
  let displayOutput =
        readChan output >>= \case
          Just x -> putStr x *> displayOutput
          Nothing -> pure ()
  let writeOutput = writeChan output . Just

  runCodensity (createGlobalEnv cfg) $ \genv ->
    withAsync displayOutput $ \displayThread -> do
      -- Currently 4 seems to be stable, more seems to create more timeouts.
      report <- fmap mconcat $ pooledForConcurrentlyN 4 tests $ \(qname, _, _, action) -> do
        (mErr, tm) <- withTime (runTest genv action)
        case mErr of
          Left err -> do
            writeOutput $
              "----- "
                <> qname
                <> colored red " FAIL"
                <> " ("
                <> printTime tm
                <> ") -----\n"
                <> err
                <> "\n"
            pure (TestSuiteReport [TestCaseReport qname (TestFailure err) tm])
          Right _ -> do
            writeOutput $ qname <> colored green " OK" <> " (" <> printTime tm <> ")" <> "\n"
            pure (TestSuiteReport [TestCaseReport qname TestSuccess tm])
      writeChan output Nothing
      wait displayThread
      printReport report
      mapM_ (saveXMLReport report) mXMLOutput
      when (any (\testCase -> testCase.result /= TestSuccess) report.cases) $
        exitFailure

doListTests :: [(String, String, String, x)] -> IO ()
doListTests tests = for_ tests $ \(qname, _desc, _full, _) -> do
  putStrLn qname

-- like `main` but meant to run from a repl
mainI :: [String] -> IO ()
mainI args = do
  let projectRoot = "../"
  withArgs args $
    withCurrentDirectory projectRoot $
      main
