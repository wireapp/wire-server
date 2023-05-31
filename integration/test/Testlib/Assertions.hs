{-# LANGUAGE CPP #-}

module Testlib.Assertions where

import Control.Exception as E
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import GHC.Stack as Stack
import System.FilePath
import Testlib.JSON
import Testlib.Printing
import Testlib.Types
import Prelude

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
    Left e -> checkFailure e
    Right _ -> assertFailure "Expected AssertionFailure, but none occured"

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
    assertFailure $ "Actual:\n" <> pa <> "\nExpected:\n" <> pb

shouldNotMatch ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The un-expected value
  b ->
  App ()
a `shouldNotMatch` b = do
  xa <- make a
  xb <- make b

  unless (jsonType xa == jsonType xb) $ do
    pa <- prettyJSON xa
    pb <- prettyJSON xb
    assertFailure $
      "Compared values are not of the same type:\n"
        <> "Left side:\n"
        <> pa
        <> "\nRight side:\n"
        <> pb

  when (xa == xb) $ do
    pa <- prettyJSON xa
    assertFailure $ "Expected different value but got twice:\n" <> pa

-- | Specialized variant of `shouldMatch` to avoid the need for type annotations.
shouldMatchInt ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  Int ->
  App ()
shouldMatchInt = shouldMatch

-- | Specialized variant of `shouldMatch` to avoid the need for type annotations.
shouldMatchBool ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  Bool ->
  App ()
shouldMatchBool = shouldMatch

shouldMatchSet ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App ()
shouldMatchSet a b = do
  la <- fmap sort (asList a)
  lb <- fmap sort (asList b)
  la `shouldMatch` lb

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

printFailureDetails :: AssertionFailure -> IO String
printFailureDetails (AssertionFailure stack mbResponse msg) = do
  s <- prettierCallStack stack
  pure . unlines $
    colored yellow "assertion failure:"
      : colored red msg
      : "\n" <> s
      : toList (fmap prettyResponse mbResponse)

printExceptionDetails :: SomeException -> IO String
printExceptionDetails e = do
  pure . unlines $
    [ colored yellow "exception:",
      colored red (displayException e)
    ]

prettierCallStack :: CallStack -> IO String
prettierCallStack cstack = do
  sl <-
    prettierCallStackLines
      . Stack.fromCallSiteList
      . filter (not . isTestlibEntry)
      . Stack.getCallStack
      $ cstack
  pure $ unlines [colored yellow "call stack: ", sl]
  where
    isTestlibEntry :: (String, SrcLoc) -> Bool
    isTestlibEntry (_, SrcLoc {..}) =
      "RunAllTests.hs" `isInfixOf` srcLocFile

prettierCallStackLines :: CallStack -> IO String
prettierCallStackLines cstack =
  go Map.empty "" (Stack.getCallStack cstack) (1 :: Int)
  where
    go _ s [] _ = pure s
    go cache s ((funName, SrcLoc {..}) : rest) i = do
      (cache', mSrcDir) <- getSourceDirCached cache srcLocPackage
      mLine <- case mSrcDir of
        Nothing -> pure Nothing
        Just srcDir -> do
          mSrc <- tryReadFile (srcDir </> srcLocFile)
          case mSrc of
            Just src ->
              case getLineNumber srcLocStartLine src of
                Just line -> pure (Just (dropWhile isSpace line))
                Nothing -> pure Nothing
            Nothing -> pure Nothing
      let s' = s <> show i <> ". " <> funName <> " at " <> srcLocFile <> ":" <> colored yellow (show srcLocStartLine) <> "\n"
      let s'' = case mLine of
            Just line -> s' <> colored blue ("     " <> line <> "\n")
            Nothing -> s'
      go cache' (s'' <> "\n") rest (i + 1)

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
      let root = "./dist-newstyle/packagedb/ghc-" <> __GLASGOW_HASKELL_FULL_VERSION__
       in root </> (pkgId <> ".conf")

    extractDataDir :: String -> Maybe String
    extractDataDir s = go (lines s)
      where
        go [] = Nothing
        go (line : otherlines) =
          case stripPrefix "data-dir:" line of
            Just rest -> Just $ dropWhile isSpace rest
            Nothing -> go otherlines

type SourceDirCache = Map.Map String (Maybe FilePath)

getSourceDirCached :: SourceDirCache -> String -> IO (SourceDirCache, Maybe FilePath)
getSourceDirCached cache packageId =
  case Map.lookup packageId cache of
    Just hit -> pure (cache, hit)
    Nothing -> do
      v <- getSourceDir packageId
      pure (Map.insert packageId v cache, v)

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile p = do
  eith <- try @SomeException (readFile p)
  pure $ case eith of
    Left _ -> Nothing
    Right s -> Just s

getLineNumber :: Int -> String -> Maybe String
getLineNumber lineNo s =
  case drop (lineNo - 1) (lines s) of
    [] -> Nothing
    (l : _) -> pure l
