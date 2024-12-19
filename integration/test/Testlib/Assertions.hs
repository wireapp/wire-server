{-# LANGUAGE CPP #-}

module Testlib.Assertions where

import Control.Applicative ((<|>))
import Control.Exception as E
import Control.Lens ((^?))
import qualified Control.Lens.Plated as LP
import Control.Monad
import qualified Control.Monad.Catch as Catch
import Control.Monad.Reader
import Control.Retry
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AD
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Lens (_Array, _Object)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Foldable
import Data.Hex
import Data.List
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Stack as Stack
import qualified Network.HTTP.Client as HTTP
import System.FilePath
import Testlib.JSON
import Testlib.Printing
import Testlib.Types
import Prelude

assertBool :: (HasCallStack) => String -> Bool -> App ()
assertBool _ True = pure ()
assertBool msg False = assertFailure msg

assertOne :: (HasCallStack, Foldable t) => t a -> App a
assertOne xs = case toList xs of
  [x] -> pure x
  other -> assertFailure ("Expected one, but got " <> show (length other))

expectFailure :: (HasCallStack) => (AssertionFailure -> App ()) -> App a -> App ()
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
shouldMatch = shouldMatchWithMsg Nothing

-- | Retries every 100ms until timeOutSeconds from Env is reached
shouldEventuallyMatch :: (MakesValue a, MakesValue b, HasCallStack) => a -> b -> App ()
shouldEventuallyMatch a b = do
  timeout <- asks (.timeOutSeconds)
  recovering
    (limitRetriesByCumulativeDelay (timeout * 1_000_000) $ constantDelay 100_000)
    ((\_ -> Catch.Handler $ \(_ :: AssertionFailure) -> pure True) : skipAsyncExceptions)
    (const $ a `shouldMatch` b)

shouldMatchWithMsg ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  -- | Message to be added to failure report
  Maybe String ->
  -- | The actual value
  a ->
  -- | The expected value
  b ->
  App ()
shouldMatchWithMsg msg a b = do
  xa <- make a
  xb <- make b
  unless (xa == xb) do
    pa <- prettyJSON xa
    pb <- prettyJSON xb
    diff <- -- show diff, but only in the interesting cases.
      if (isJust (xa ^? _Object) && isJust (xb ^? _Object))
        || (isJust (xa ^? _Array) && isJust (xb ^? _Array))
        then ("\nDiff:\n" <>) <$> prettyJSON (AD.diff xa xb)
        else pure ""
    assertFailure $ (maybe "" (<> "\n") msg) <> "Actual:\n" <> pa <> "\nExpected:\n" <> pb <> diff

data LenientMatchRule
  = EmptyArrayIsNull
  | ArraysAreSets
  | RemoveNullFieldsFromObjects
  deriving (Eq, Ord, Show, Bounded, Enum)

shouldMatchWithRules ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  [LenientMatchRule] ->
  (Aeson.Value -> App (Maybe Aeson.Value)) ->
  a ->
  b ->
  App ()
shouldMatchWithRules rules customRules a b = do
  xa <- make a
  xb <- make b
  simplify xa `shouldMatch` simplify xb
  where
    simplify :: Aeson.Value -> App Aeson.Value
    simplify = LP.rewriteM $ (\v -> foldM (tryApplyRule v) Nothing compiledRules)

    tryApplyRule ::
      Aeson.Value ->
      Maybe Aeson.Value ->
      (Aeson.Value -> App (Maybe Aeson.Value)) ->
      App (Maybe Aeson.Value)
    tryApplyRule v bresult arule = (bresult <|>) <$> arule v

    compiledRules :: [Aeson.Value -> App (Maybe Aeson.Value)]
    compiledRules = customRules : ((\r v -> pure $ runRule r v) <$> rules)

    runRule :: LenientMatchRule -> Aeson.Value -> Maybe Aeson.Value
    runRule EmptyArrayIsNull = \case
      Aeson.Array arr
        | arr == mempty ->
            Just Aeson.Null
      _ -> Nothing
    runRule ArraysAreSets = \case
      Aeson.Array (toList -> arr) ->
        let arr' = sort arr
         in if arr == arr' then Nothing else Just $ Aeson.toJSON arr'
      _ -> Nothing
    runRule RemoveNullFieldsFromObjects = \case
      Aeson.Object (Aeson.toList -> obj)
        | any ((== Aeson.Null) . snd) obj ->
            let rmNulls (_, Aeson.Null) = Nothing
                rmNulls (k, v) = Just (k, v)
             in Just . Aeson.Object . Aeson.fromList $ mapMaybe rmNulls obj
      _ -> Nothing

shouldMatchBase64 ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  -- | The actual value, in base64
  a ->
  -- | The expected value, in plain text
  b ->
  App ()
a `shouldMatchBase64` b = do
  xa <- Text.decodeUtf8 . B64.decodeLenient . Text.encodeUtf8 . Text.pack <$> asString a
  xa `shouldMatch` b

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
    assertFailure $ "Expected different value but got this:\n" <> pa

-- | Specialized variant of `shouldMatch` to avoid the need for type annotations.
shouldMatchInt ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  Int ->
  App ()
shouldMatchInt = shouldMatch

shouldNotMatchInt ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  Int ->
  App ()
shouldNotMatchInt = shouldNotMatch

shouldMatchRange ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected range, inclusive both sides
  (Int, Int) ->
  App ()
shouldMatchRange a (lower, upper) = do
  xa :: Int <- asIntegral a
  when (xa < lower || xa > upper) $ do
    pa <- prettyJSON xa
    assertFailure $ "Actual:\n" <> pa <> "\nExpected:\nin range (" <> show lower <> ", " <> show upper <> ") (including bounds)"

-- | Match on sorted lists (sets where elements may occur more than once).  (Maybe this should
-- be called `shouldMatchMultiSet`?)
shouldMatchSet ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App ()
shouldMatchSet a b = do
  la <- fmap sort (asList a)
  lb <- fmap sort (asList b)
  la `shouldMatch` lb

shouldBeEmpty :: (MakesValue a, HasCallStack) => a -> App ()
shouldBeEmpty a = a `shouldMatch` (mempty :: [Value])

shouldBeNull :: (MakesValue a, HasCallStack) => a -> App ()
shouldBeNull a = a `shouldMatch` Aeson.Null

shouldMatchOneOf ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App ()
shouldMatchOneOf a b = do
  lb <- asList b
  xa <- make a
  unless (xa `elem` lb) $ do
    pa <- prettyJSON a
    pb <- prettyJSON b
    assertFailure $ "Expected:\n" <> pa <> "\n to match at least one of:\n" <> pb

shouldContainString ::
  (HasCallStack) =>
  -- | The actual value
  String ->
  -- | The expected value
  String ->
  App ()
shouldContainString = shouldContain

shouldContain ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  [a] ->
  -- | The expected value
  [a] ->
  App ()
super `shouldContain` sub = do
  unless (sub `isInfixOf` super) $ do
    assertFailure $ "String or List:\n" <> show super <> "\nDoes not contain:\n" <> show sub

printFailureDetails :: AssertionFailure -> IO String
printFailureDetails (AssertionFailure stack mbResponse ctx msg) = do
  s <- prettierCallStack stack
  pure . unlines $
    colored yellow "assertion failure:"
      : colored red msg
      : "\n" <> s
      : toList (fmap prettyResponse mbResponse)
        <> toList (fmap prettyContext ctx)

printAppFailureDetails :: AppFailure -> IO String
printAppFailureDetails (AppFailure msg stack) = do
  s <- prettierCallStack stack
  pure . unlines $
    colored yellow "app failure:"
      : colored red msg
      : "\n"
      : [s]

prettyContext :: String -> String
prettyContext ctx = do
  unlines
    [ colored yellow "context:",
      colored blue ctx
    ]

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

prettyResponse :: Response -> String
prettyResponse r =
  unlines $
    concat
      [ pure $ colored yellow "request: \n" <> showRequest r.request,
        pure $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request),
        case getRequestBody r.request of
          Nothing -> []
          Just b ->
            [ colored yellow "request body:",
              Text.unpack . Text.decodeUtf8 $ case Aeson.decode (BS.fromStrict b) of
                Just v -> BS.toStrict (Aeson.encodePretty (v :: Aeson.Value))
                Nothing -> hex b
            ],
        pure $ colored blue "response status: " <> show r.status,
        pure $ colored blue "response body:",
        pure $
          ( TL.unpack . TL.decodeUtf8 $
              case r.jsonBody of
                Just b -> (Aeson.encodePretty b)
                Nothing -> BS.fromStrict r.body
          )
      ]
