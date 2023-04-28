{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
--
-- Table of Contents:
-- - SECTION_APP : App, Env, ServiceMap, Context
-- - SECTION_ASSERTIONS
-- - SECTION_JSON
-- - SECTION_REQUEST : requests and responses
-- - SECTION_PRINTING : Terminal and printingmodule App where
-- - SECTION_MODSERVICE : Start services with modified configuration
module Testlib.App where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (finally, try)
import qualified Control.Exception as E
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Catch.Pure (MonadCatch)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Retry (fibonacciBackoff, limitRetriesByCumulativeDelay, retrying)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as C
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import qualified Data.Scientific as Sci
import Data.String.Conversions (cs)
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import GHC.Exception
import GHC.Records
import GHC.Stack
import qualified GHC.Stack as Stack
import Imports hiding (ask, asks, local)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as N
import Network.URI (uriToString)
import System.Exit (exitFailure)
import System.FilePath (joinPath, takeDirectory, (</>))
import System.FilePath.Posix (splitPath)
import System.IO (hPutStrLn, openBinaryTempFile)
import qualified System.IO.Error as Error
import System.Process (CreateProcess (..), createProcess, proc, terminateProcess)
import Test.Tasty.Options
import Test.Tasty.Providers
import qualified Test.Tasty.Providers as Tasty
import Test.Tasty.Providers.ConsoleFormat

-------------------------------------------------------------------------------
-- - SECTION_APP : App, Env, ServiceMap, Context
-------------------------------------------------------------------------------

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadReader Env
    )

data AppFailure = AppFailure String

instance Show AppFailure where
  show (AppFailure msg) = msg

instance Exception AppFailure where
  displayException (AppFailure msg) = msg

failApp :: String -> App a
failApp msg = throw (AppFailure msg)

runAppWithEnv :: Env -> App a -> IO a
runAppWithEnv e m = runReaderT (unApp m) e

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

instance IsTest (App ()) where
  run opts action _ = do
    env <- mkEnv (lookupOption opts)
    result :: Tasty.Result <-
      (runAppWithEnv env action >> pure (Tasty.testPassed ""))
        `E.catches` [ E.Handler
                        ( \(e :: AssertionFailure) -> do
                            pure (testFailedDetails (displayException e) (printFailureDetails e))
                        ),
                      E.Handler
                        (\(ex :: SomeException) -> pure (testFailed (show ex)))
                    ]
    pure result

  testOptions = Tagged [Option (Proxy @ConfigFile)]

data Env = Env
  { context :: Context,
    manager :: HTTP.Manager,
    prekeys :: IORef [(Int, String)],
    lastPrekeys :: IORef [String],
    serviceConfigsDir :: FilePath,
    servicesCwdBase :: Maybe FilePath
  }

data Context = Context
  { serviceMap :: ServiceMap,
    version :: Int
  }

data ServiceMap = ServiceMap
  { brig :: HostPort,
    galley :: HostPort,
    cannon :: HostPort
  }
  deriving (Show, Generic)

instance FromJSON ServiceMap

data HostPort = HostPort
  { host :: String,
    port :: Word16
  }
  deriving (Show, Generic)

instance FromJSON HostPort

data Service = Brig | Galley | Cannon
  deriving (Show, Eq, Ord)

serviceName :: Service -> String
serviceName srv = map C.toLower (show srv)

serviceHostPort :: ServiceMap -> Service -> HostPort
serviceHostPort m Brig = m.brig
serviceHostPort m Galley = m.galley
serviceHostPort m Cannon = m.cannon

newtype ConfigFile = ConfigFile {unConfigFile :: FilePath}

instance IsOption ConfigFile where
  defaultValue = ConfigFile "services/integration.yaml"
  parseValue = Just . ConfigFile
  optionName = fromString "config"
  optionHelp = fromString "Configuration file for integration tests. Default: services/integration.yaml"

mkEnv :: ConfigFile -> IO Env
mkEnv (ConfigFile cfgFile) = do
  eith <- Yaml.decodeFileEither cfgFile
  serviceMap <- case eith of
    Left err -> do
      hPutStrLn stderr $ "Could not parse " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err
      exitFailure
    Right serviceMap -> pure serviceMap

  let devEnvProjectRoot = case splitPath (takeDirectory cfgFile) of
        [] -> Nothing
        ps ->
          if last ps == "services"
            then Just (joinPath (init ps))
            else Nothing

  let configsDir =
        case devEnvProjectRoot of
          Just root -> root </> "./services/.integration/A/etc/wire/"
          Nothing -> "/etc/wire"

  manager <- HTTP.newManager HTTP.defaultManagerSettings
  pks <- newIORef (zip [1 ..] somePrekeys)
  lpks <- newIORef someLastPrekeys
  pure
    Env
      { context =
          Context
            { serviceMap = serviceMap,
              version = 4
            },
        manager = manager,
        prekeys = pks,
        lastPrekeys = lpks,
        serviceConfigsDir = configsDir,
        servicesCwdBase = devEnvProjectRoot <&> (</> "services")
      }

somePrekeys :: [String]
somePrekeys =
  [ "pQABAQECoQBYIOjl7hw0D8YRNqkkBQETCxyr7/ywE/2R5RWcUPM+GJACA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQICoQBYIGoXawUQWQ9ZW+MXhvuo9ALOBUjLff8S5VdAokN29C1OA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQMCoQBYIEjdt+YWd3lHmG8pamULLMubAMZw556IO8kW7s1MLFytA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQQCoQBYIPIaOA3Xqfk4Lh2/pU88Owd2eW5eplHpywr+Mx4QGyiMA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQUCoQBYIHnafNR4Gh3ID71lYzToewEVag4EKskDFq+gaeraOlSJA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQYCoQBYIFXUkVftE7kK22waAzhOjOmJVex3EBTU8RHZFx2o1Ed8A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQgCoQBYIJH1ewvIVV3yGqQvdr/QM9HARzMgo5ksOTRyKEuN2aZzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQkCoQBYIFcAnXdx0M1Q1hoDDfgMK9r+Zchn8YlVHHaQwQYhRk1dA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQoCoQBYIGs3vyxwmzEZ+qKNy4wpFkxc+Bgkb0D76ZEbxeeh/9DVA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQsCoQBYIGUiBeOJALP5dkMduUZ/u6MDhHNrsrBUa3f0YlSSWZbzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQwCoQBYIMp6QNNTPDZgL3DSSD/QWWnBI7LsTZp2RhY/HLqnIwRZA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ0CoQBYIJXSSUrE5RCNyB5pg+m6vGwK7RvJ+rs9dsdHitxnfDhuA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ4CoQBYIHmtOX7jCKBHFDysb4H0z/QWoCSaEyjerZaT/HOP8bgDA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABAQ8CoQBYIIaMCTcPKj2HuYQ7i9ZaxUw9j5Bz8TPjoAaTZ5eB0w1kA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARACoQBYIHWAOacKuWH81moJVveJ0FSfipWocfspOIBhaU6VLWUsA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARECoQBYIA8XtUXtnMxQslULnNAeHBIivlLRe/+qdh2j6nTfDAchA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARICoQBYIGgzg6SzgTTOgnk48pa6y2Rgjy004DkeBo4CMld3Jlr6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARMCoQBYIEoEFiIpCHgn74CAD+GhIfIgbQtdCqQqkOXHWxRlG6Y6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARQCoQBYINVEwTRxNSe0rxZxon4Rifz2l4rtQZn7mHtKYCiFAK9IA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARUCoQBYIN3aeX2Ayi2rPFbiaYb+O2rdHUpFhzRs2j28pCmbGpflA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARYCoQBYIJe5OJ17YKQrNmIH3sE++r++4Z5ld36axqAMjjQ3jtQWA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARcCoQBYIASE94LjK6Raipk/lN/YewouqO+kcQGpxIqP+iW2hyHiA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    "pQABARgYAqEAWCBZ222LpS6/99Btlw+83PihrA655skwsNevt//8oz5axQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    "pQABARgZAqEAWCDGEwo61w4O8T8lyw0HdoOjGWBKQUNqo6+jSfrPR9alrAOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    "pQABARgaAqEAWCBMSQoQ6B35plB80i1O3AWlJSftCEbCbju97Iykg5+NWQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
  ]

someLastPrekeys :: [String]
someLastPrekeys =
  [ "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggwO2any+CjiGP8XFYrY67zHPvLgp+ysY5k7vci57aaLwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggoChErA5oTI5JT769hJV+VINmU8kougGdYqGd2U7hPa8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggPLk4BBJ8THVLGm7r0K7EJITRlJnt6bpNzM9GTNRYcCcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggqHASsRlZ1i8dESXRXBL2OvR+0yGUtqK9vJfzol1E+osDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggx/N1YhKXSJYJQxhWgHSA4ASaJKIHDJfmEnojfnp9VQ8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggVL6QIpoqmtKxmB8HToiAPxfjSDEzJEUAoFKfhXou06YDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggRs74/ViOrHN+aS2RbGCwC0sJv1Sp/Q0pmRB15s9DCBMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggtNO/hrwzt9M/1X6eK2sG6YFmA7BDqlFMEipbZOsg0vcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg1rZEY6vbAnEz+Ern5kRny/uKiIrXTb/usQxGnceV2HADoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg2647mOAVeOdhW57Q1zXDigDxRz/hB8ITFSZ7uo+pXH4DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggjddbHizABYOY0T6rvJeZCvV20dvTT9BYv95ri9bqSb8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggCKT/GspZquUY6vKC4TFvaFqTH1QGG1ptauiaulnfqkUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggv7bf/kEsTKFDGSgswsywq6AIxBq5AqZbLjDYDHfGjrcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggUbjGhhh8EwZEPSz+Y31rYNUu7jsRR8dy1F5FSiJXfXEDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg/4nz1uHiPBVGFvYjTMwGQ31bSFNctbU0r2nBtpsK9kcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggwbJDyKl7T3+3Ihc0YF06Dz2J11My5qn7JKG+U+ti8lQDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgglc6nCoZR2/qjLp0tr7vRyuXqb7ugdHHDadjX7zSl4uMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFgg5ER8h0/bIADXjBXe/XPKdzekgv6nhJ4hp3vJ3jtTSbUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggsgV6jq+GuNuvXk+ctHh570cNqEmfPhz34wcYCMCf9xIDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggdQdlPqkBw6+phKhohp3YaWQL710euZDnyMLFwf2cS0oDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggKlsI/snuQMoYcZRw/kN+BobPV5gwYeBClp0Wx9btTGUDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggtruFBClEgdPKvjpHsYLlWMev9L4OmYZwlxbY0NwvzOwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggRUdh4cuYtFNL46RLnPy65goYInyreStKwsEcY3pPlLkDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggQtT7lLZzH171F4jCbHNwxEAt28FwdQ8Kt2tbxFzPgC0DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    "pQABARn//wKhAFggQeUPM119c+6zRsEupA8zshTfrZiLpXx1Ji0UMMumq9IDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
  ]

-------------------------------------------------------------------------------
-- - SECTION_ASSERTIONS
-------------------------------------------------------------------------------

data AssertionFailure = AssertionFailure
  { callstack :: CallStack,
    response :: Maybe Response,
    msg :: String
  }

instance Show AssertionFailure where
  show (AssertionFailure _ _ msg) = "AssertionFailure _ _ " <> show msg

instance Exception AssertionFailure where
  displayException (AssertionFailure _ _ msg) = msg

assertFailure :: HasCallStack => String -> App a
assertFailure msg =
  deepseq msg $
    liftIO $
      E.throw (AssertionFailure callStack Nothing msg)

assertBool :: HasCallStack => String -> Bool -> App ()
assertBool _ True = pure ()
assertBool msg False = assertFailure msg

assertOne :: HasCallStack => [a] -> App a
assertOne [x] = pure x
assertOne xs = assertFailure ("Expected one, but got " <> show (length xs))

expectFailure :: HasCallStack => (AssertionFailure -> App ()) -> App a -> App ()
expectFailure checkFailure action = do
  env <- ask
  res :: Either AssertionFailure x <-
    liftIO
      (E.try (runAppWithEnv env action))
  case res of
    Left e@(AssertionFailure callstack mr msg) ->
      checkFailure e
    Right x -> assertFailure "Expected AssertionFailure, but none occured"

shouldMatch ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  b ->
  App ()
a `shouldMatch` b = do
  xa <- make a
  xb <- make b
  unless (xa == xb) $ do
    pa <- prettyJSON xa
    pb <- prettyJSON xb
    assertFailure $ "Expected:\n" <> pb <> "\n" <> "Actual:\n" <> pa

-- | Specialized variant of `shouldMatch` to avoid the need for type annotations.
shouldMatchInt ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  Int ->
  App ()
shouldMatchInt = shouldMatch

liftP2 ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  (Value -> Value -> c) ->
  a ->
  b ->
  App c
liftP2 f a b = do
  f <$> make a <*> make b

isEqual ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App Bool
isEqual = liftP2 (==)

printFailureDetails :: AssertionFailure -> ResultDetailsPrinter
printFailureDetails (AssertionFailure stack mbResponse _) = ResultDetailsPrinter $ \testLevel _withFormat -> do
  let nindent = 2 * testLevel + 2
  s <- indent nindent <$> prettierCallStack stack
  putStrLn ""
  putStrLn s
  for_ mbResponse $ \r -> putStrLn (indent nindent (prettyReponse r))

prettierCallStack :: CallStack -> IO String
prettierCallStack cstack = do
  sl <- undefined
  d <- getCurrentDirectory
  pure $
    intercalate "\n" $
      [colored yellow "call stack: ", sl, "current dir: " <> d]

-- prettierCallStackLines :: CallStack -> IO String
-- prettierCallStackLines cstack =
--   intercalate "\n" <$> mapM prettyCallSite (Stack.getCallStack cstack)
--   where
--     prettyCallSite (f, SrcLoc {..}) = do
--       s <- tryReadFile srcLocFile srcLocStartLine
--       pure $ f <> " at " <> concat ([srcLocFile, ":", show srcLocStartLine, show srcLocPackage] <> (maybe [] (: []) s))

meh :: String
meh = __GLASGOW_HASKELL_FULL_VERSION__

getSourceDir :: String -> IO (Maybe FilePath)
getSourceDir packageId = do
  ms <- tryReadFile (packagedbFile packageId)
  case ms of
    Nothing -> pure Nothing
    Just s ->
      pure (extractDataDir s)
  where
    packagedbFile :: String -> FilePath
    packagedbFile pkgId =
      -- TOODO: replace .. with .
      let root = "../dist-newstyle/packagedb/ghc-" <> __GLASGOW_HASKELL_FULL_VERSION__
       in root </> (pkgId <> ".conf")

    extractDataDir :: String -> Maybe String
    extractDataDir s = go (lines s)
      where
        go [] = Nothing
        go (line : otherlines) =
          case stripPrefix "data-dir:" line of
            Just rest -> Just $ dropWhile isSpace rest
            Nothing -> go otherlines

type SourceDirCache = Map String (Maybe FilePath)

getSourceDirCached :: SourceDirCache -> String -> IO (SourceDirCache, Maybe FilePath)
getSourceDirCached cache packageId =
  case Map.lookup packageId cache of
    Just hit -> pure (cache, hit)
    Nothing -> do
      v <- getSourceDir packageId
      pure (Map.insert packageId v cache, v)

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile p = do
  eith <- try (readFile p)
  pure $ case eith of
    Left (e :: SomeException) -> Nothing
    Right s -> Just s

modifyFailure :: (AssertionFailure -> AssertionFailure) -> App a -> App a
modifyFailure modifyAssertion action = do
  env <- ask
  liftIO
    ( E.catch
        (runAppWithEnv env action)
        ( \(e :: AssertionFailure) ->
            E.throw (modifyAssertion e)
        )
    )

modifyFailureMsg :: (String -> String) -> App a -> App a
modifyFailureMsg modMessage = modifyFailure (\e -> e {msg = modMessage e.msg})

addFailureContext :: String -> App a -> App a
addFailureContext msg = modifyFailureMsg (\m -> m <> "\nThis failure happend in this context:\n" <> msg)

-------------------------------------------------------------------------------
-- - SECTION_JSON
-------------------------------------------------------------------------------

-- | All library functions should use this typeclass for all untyped value
-- arguments wherever possible. This design choice has advantages:
--
-- No need convert value between different representations. E.g. if a function
-- needs a user id as a string, all these input types become valid input:
--
-- - String
-- - Text
-- - Value
-- - App Text
-- - App String
-- - App Value
--
-- Internally the function calls `asString` to convert to App String
--
-- Since (App a) are treated as first-class values values this means we can
-- compose operations that might fail without giving up nice error messages:
--
-- callMe (response.json %. "user" & "foo.bar.baz" %.= 2)
--
-- This can fail if
-- 1. the response is not application/json
-- 2. has no "user" field
-- 3. the nested update fails
class MakesValue a where
  make :: HasCallStack => a -> App Value

instance {-# OVERLAPPABLE #-} ToJSON a => MakesValue a where
  make = pure . toJSON

instance {-# OVERLAPPING #-} ToJSON a => MakesValue (App a) where
  make m = m <&> toJSON

(.=) :: ToJSON a => String -> a -> Aeson.Pair
(.=) k v = fromString k Aeson..= v

asString :: HasCallStack => MakesValue a => a -> App String
asString x =
  make x >>= \case
    (String s) -> pure (T.unpack s)
    v -> assertFailureWithJSON x ("String" `typeWasExpectedButGot` v)

asStringM :: HasCallStack => MakesValue a => a -> App (Maybe String)
asStringM x =
  make x >>= \case
    (String s) -> pure (Just (T.unpack s))
    _ -> pure Nothing

asObject :: HasCallStack => MakesValue a => a -> App Object
asObject x =
  make x >>= \case
    (Object o) -> pure o
    v -> assertFailureWithJSON x ("Object" `typeWasExpectedButGot` v)

asInt :: HasCallStack => MakesValue a => a -> App Int
asInt x =
  make x >>= \case
    (Number n) ->
      case Sci.floatingOrInteger n of
        Left (_ :: Double) -> assertFailure "Expected an integral, but got a floating point"
        Right i -> pure i
    v -> assertFailureWithJSON x ("Number" `typeWasExpectedButGot` v)

asList :: HasCallStack => MakesValue a => a -> App [Value]
asList x =
  make x >>= \case
    (Array arr) -> pure (toList arr)
    v -> assertFailureWithJSON x ("Array" `typeWasExpectedButGot` v)

asBool :: HasCallStack => MakesValue a => a -> App Bool
asBool x =
  make x >>= \case
    (Bool b) -> pure b
    v -> assertFailureWithJSON x ("Bool" `typeWasExpectedButGot` v)

-- | Get a (nested) field of a JSON object
-- Raise an AssertionFailure if the field at the (nested) key is missing.
(%.) ::
  (HasCallStack, MakesValue a) =>
  a ->
  -- | A plain key, e.g. "id", or a nested key "user.profile.id"
  String ->
  App Value
(%.) val selector = do
  v <- make val
  vp <- prettyJSON v
  addFailureContext ("Getting (nested) field " <> selector <> " of object:\n" <> vp) $ do
    let keys = splitOn "." selector
    case keys of
      (k : ks) -> go k ks v
      [] -> assertFailure "No key provided"
  where
    go k [] v = l k v
    go k (k2 : ks) v = do
      r <- l k v
      go k2 ks r
    l k v = do
      ob <- asObject v
      case KM.lookup (KM.fromString k) ob of
        Nothing -> assertFailureWithJSON ob $ "Field \"" <> k <> "\" is missing from object:"
        Just x -> pure x

-- | Look up (nested) field of a JSON object
--
-- If the field key has no dots then returns Nothing if the key is missing from the
-- object.
--
-- If the field key has dots (describes a nested lookuyp) then returns Nothing
-- if the last component of the key field selector is missing from nested
-- object. If any other component is missing this function raises an
-- AssertionFailure.
lookupField ::
  (HasCallStack, MakesValue a) =>
  a ->
  -- | A plain key, e.g. "id", or a nested key "user.profile.id"
  String ->
  App (Maybe Value)
lookupField val selector = do
  v <- make val
  vp <- prettyJSON v
  addFailureContext ("Loooking up (nested) field " <> selector <> " of object:\n" <> vp) $ do
    let keys = splitOn "." selector
    case keys of
      (k : ks) -> go k ks v
      [] -> assertFailure "No key provided"
  where
    go k [] v = do
      ob <- asObject v
      pure (KM.lookup (KM.fromString k) ob)
    go k (k2 : ks) v = do
      ob <- asObject v
      r <-
        case KM.lookup (KM.fromString k) ob of
          Nothing -> assertFailureWithJSON ob $ "Field \"" <> k <> "\" is missing from object:"
          Just x -> pure x
      go k2 ks r

-- Update nested fields
-- E.g. ob & "foo.bar.baz" %.= ("quux" :: String)
setField ::
  forall a b.
  (HasCallStack, MakesValue a, ToJSON b) =>
  -- | Selector, e.g. "id", "user.team.id"
  String ->
  -- | The value that should insert or replace the value at the selctor
  b ->
  a ->
  App Value
setField selector v x = do
  modifyField @a @Value selector (\_ -> pure (toJSON v)) x

-- Update nested fields, using the old value with a stateful action
modifyField :: (HasCallStack, MakesValue a, ToJSON b) => String -> (Maybe Value -> App b) -> a -> App Value
modifyField selector up x = do
  v <- make x
  let keys = splitOn "." selector
  case keys of
    (k : ks) -> go k ks v
    [] -> assertFailure "No key provided"
  where
    go k [] v = do
      ob <- asObject v
      let k' = KM.fromString k
      newValue <- toJSON <$> up (KM.lookup k' ob)
      pure $ Object $ KM.insert k' newValue ob
    go k (k2 : ks) v = do
      val <- v %. k
      newValue <- go k2 ks val
      ob <- asObject v
      pure $ Object $ KM.insert (KM.fromString k) newValue ob

assertFailureWithJSON :: HasCallStack => MakesValue a => a -> String -> App b
assertFailureWithJSON v msg = do
  msg' <- ((msg <> "\n") <>) <$> prettyJSON v
  assertFailure msg'

-- | Useful for debugging
printJSON :: MakesValue a => a -> App ()
printJSON = prettyJSON >=> putStrLn

prettyJSON :: MakesValue a => a -> App String
prettyJSON x =
  make x <&> Aeson.encodePretty <&> LC8.unpack

constrName :: Value -> String
constrName (Object _) = "Object"
constrName (Array _) = "Array"
constrName (String _) = "String"
constrName (Number _) = "Number"
constrName (Bool _) = "Bool"
constrName Null = "Null"

typeWasExpectedButGot :: String -> Value -> String
typeWasExpectedButGot expectedType x = "Expected " <> expectedType <> " but got " <> constrName x <> ":"

-- Get "id" field or - if already string-like return String
objId :: MakesValue a => a -> App String
objId x = do
  v <- make x
  case v of
    Object ob -> ob %. "id" & asString
    String t -> pure (T.unpack t)
    other -> assertFailureWithJSON other (typeWasExpectedButGot "Object or String" other)

-- Get "qualified_id" field as (domain, id) or - if already is a qualified id object - return that
objQid :: MakesValue a => a -> App (String, String)
objQid ob = do
  m <- firstSuccess [select ob, inField]
  case m of
    Nothing -> do
      assertFailureWithJSON ob "Could not get a qualified id from value:"
    Just v -> pure v
  where
    select x = runMaybeT $ do
      vdom <- MaybeT $ lookupField x "domain"
      dom <- MaybeT $ asStringM vdom
      vid <- MaybeT $ lookupField x "id"
      id_ <- MaybeT $ asStringM vid
      pure $ (dom, id_)

    inField = do
      m <- lookupField ob "qualified_id"
      case m of
        Nothing -> pure Nothing
        Just x -> select x

    firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
    firstSuccess [] = pure Nothing
    firstSuccess (x : xs) =
      x >>= \case
        Nothing -> firstSuccess xs
        Just y -> pure (Just y)

-------------------------------------------------------------------------------
-- - SECTION_REQUEST : requests and responses
-------------------------------------------------------------------------------

data Versioned = Versioned | Unversioned | ExplicitVersion Int

baseRequest :: Service -> Versioned -> String -> App HTTP.Request
baseRequest service versioned path = do
  ctx <- asks (.context)
  pathSegsPrefix <- case versioned of
    Versioned -> do
      v <- asks (.context.version)
      pure ["v" <> show v]
    Unversioned -> pure []
    ExplicitVersion v -> do
      pure ["v" <> show v]

  liftIO . HTTP.parseRequest $
    let HostPort h p = serviceHostPort ctx.serviceMap service
     in "http://" <> h <> ":" <> show p <> ("/" <> joinHttpPath (pathSegsPrefix <> splitHttpPath path))

submit :: String -> HTTP.Request -> App Response
submit method req0 = do
  let req = req0 {HTTP.method = toByteString' method}
  manager <- asks (.manager)
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { jsonBody = Aeson.decode (HTTP.responseBody res),
        body = L.toStrict (HTTP.responseBody res),
        status = HTTP.statusCode (HTTP.responseStatus res),
        headers = HTTP.responseHeaders res,
        request = req
      }

data Response = Response
  { jsonBody :: Maybe Aeson.Value,
    body :: ByteString,
    status :: Int,
    headers :: [HTTP.Header],
    request :: HTTP.Request
  }

instance HasField "json" Response (App Aeson.Value) where
  getField response = maybe (assertFailure "Response has no json body") pure response.jsonBody

splitHttpPath :: String -> [String]
splitHttpPath path = filter (not . null) (splitOn "/" path)

joinHttpPath :: [String] -> String
joinHttpPath = intercalate "/"

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.Value -> HTTP.Request -> HTTP.Request
addJSON obj req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode obj),
      HTTP.requestHeaders =
        (fromString "Content-Type", fromString "application/json")
          : HTTP.requestHeaders req
    }

addHeader :: String -> String -> HTTP.Request -> HTTP.Request
addHeader name value req =
  req {HTTP.requestHeaders = (CI.mk . C8.pack $ name, C8.pack value) : HTTP.requestHeaders req}

addQueryParams :: [(String, String)] -> HTTP.Request -> HTTP.Request
addQueryParams params req =
  HTTP.setQueryString (map (\(k, v) -> (cs k, Just (cs v))) params) req

zUser :: String -> HTTP.Request -> HTTP.Request
zUser = addHeader "Z-User"

zConnection :: String -> HTTP.Request -> HTTP.Request
zConnection = addHeader "Z-Connection"

zType :: String -> HTTP.Request -> HTTP.Request
zType = addHeader "Z-Type"

bindResponse :: HasCallStack => App Response -> (Response -> App a) -> App a
bindResponse m k = m >>= \r -> withResponse r k

withResponse :: HasCallStack => Response -> (Response -> App a) -> App a
withResponse r k = onFailureAddResponse r (k r)

onFailureAddResponse :: Response -> App a -> App a
onFailureAddResponse r m = App $ do
  e <- ask
  liftIO $ E.catch (runAppWithEnv e m) $ \(AssertionFailure stack _ msg) -> do
    E.throw (AssertionFailure stack (Just r) msg)

printResponse :: MonadIO m => Response -> m ()
printResponse = putStrLn . prettyReponse

prettyReponse :: Response -> String
prettyReponse r =
  unlines $
    concat
      [ pure $ colored yellow "request: \n" <> showRequest r.request,
        pure $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request),
        case getRequestBody r.request of
          Nothing -> []
          Just b ->
            [ colored yellow "request body:",
              T.unpack . T.decodeUtf8 $ case Aeson.decode (L.fromStrict b) of
                Just v -> L.toStrict (Aeson.encodePretty (v :: Aeson.Value))
                Nothing -> b
            ],
        pure $ colored blue "response status: " <> show r.status,
        pure $ colored blue "response body:",
        pure $
          ( T.unpack . T.decodeUtf8 $
              case r.jsonBody of
                Just b -> L.toStrict (Aeson.encodePretty b)
                Nothing -> r.body
          )
      ]

getRequestBody :: HTTP.Request -> Maybe ByteString
getRequestBody req = case HTTP.requestBody req of
  HTTP.RequestBodyLBS lbs -> pure (L.toStrict lbs)
  HTTP.RequestBodyBS bs -> pure bs
  _ -> Nothing

showRequest :: HTTP.Request -> String
showRequest r =
  T.unpack (T.decodeUtf8 (HTTP.method r))
    <> " "
    <> uriToString id (HTTP.getUri r) ""

showHeaders :: [HTTP.Header] -> String
showHeaders r =
  intercalate "\n" $
    r <&> \(name, value) ->
      C8.unpack (CI.original name) <> ": " <> C8.unpack value

-------------------------------------------------------------------------------
-- - SECTION_PRINTING : Terminal and printing
-------------------------------------------------------------------------------

yellow :: String
yellow = "\x1b[38;5;11m"

blue :: String
blue = "\x1b[38;5;6m"

purpleish :: String
purpleish = "\x1b[38;5;13m"

orange :: String
orange = "\x1b[38;5;3m"

red :: String
red = "\x1b[38;5;1m"

resetColor :: String
resetColor = "\x1b[0m"

colored :: String -> String -> String
colored color s = color <> s <> resetColor

indent :: Int -> String -> String
indent n s =
  unlines (map (pad <>) (lines s))
  where
    pad = replicate n ' '

hline :: String
hline = replicate 40 '-'

-------------------------------------------------------------------------------
-- - SECTION_MODSERVICE : Start services with modified configuration
-------------------------------------------------------------------------------

withModifiedService ::
  Service ->
  -- | function that edits the config
  (Value -> App Value) ->
  -- | This action wil access the modified spawned service
  App a ->
  App a
withModifiedService srv modConfig k = do
  withModifiedServices (Map.singleton srv modConfig) k

withModifiedServices :: Map Service (Value -> App Value) -> App a -> App a
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
    basedir <- asks (.serviceConfigsDir)
    let srvName = serviceName srv
        cfgFile = basedir </> srvName </> "conf" </> (srvName <> ".yaml")
    config <- do
      eith <- liftIO (Yaml.decodeFileEither cfgFile)
      case eith of
        Left err -> failApp ("Error while parsing " <> cfgFile <> ": " <> Yaml.prettyPrintParseException err)
        Right value -> pure value
    config' <- updateServiceMapInConfig config >>= modifyConfig
    (tempFile, fh) <- liftIO $ openBinaryTempFile "/tmp" (srvName <> ".yaml")
    liftIO $ BS.hPut fh (Yaml.encode config')
    hClose fh

    (cwd, exe) <-
      asks (.servicesCwdBase) <&> \case
        Nothing -> (Nothing, srvName)
        Just dir ->
          (Just (dir </> srvName), "./dist" </> srvName)

    (port, socket) <- maybe (failApp "the impossible in withServices happened") pure (Map.lookup srv ports)
    liftIO $ N.close socket
    (_, _, _, ph) <- liftIO $ createProcess (proc exe ["-c", tempFile]) {cwd = cwd}
    pure ph

  let stopInstances = liftIO $ do
        -- Running waitForProcess would hang for 30 seconds when the test suite
        -- is run from within ghci, so we don't wait here.
        for_ instances terminateProcess

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

  let modifyEnv env =
        env
          { context =
              env.context
                { serviceMap =
                    updateServiceMap env.context.serviceMap
                }
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
  isUp <-
    retrying
      (limitRetriesByCumulativeDelay (4 * 1000 * 1000) (fibonacciBackoff (200 * 1000)))
      (\_ isUp -> pure (not isUp))
      ( \_ -> do
          req <- baseRequest srv Unversioned "/i/status"
          env <- ask
          eith <-
            liftIO $
              E.try
                ( runAppWithEnv env $ do
                    res <- submit "GET" req
                    pure (res.status `elem` [200, 204])
                )
          pure $ either (\(e :: HTTP.HttpException) -> False) id eith
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
