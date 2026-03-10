{-# LANGUAGE CPP #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Lens (_Array, _Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Foldable
import Data.Hex
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Stack as Stack
import qualified Network.HTTP.Client as HTTP
import System.Environment
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

assertAtLeastOne :: (HasCallStack, Foldable t) => t a -> App ()
assertAtLeastOne xs = case toList xs of
  [] -> assertFailure ("Expected at least one, but got nothing")
  _ -> pure ()

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
eventually :: App a -> App a
eventually action = do
  timeout <- asks (.timeOutSeconds)
  recovering
    (limitRetriesByCumulativeDelay (timeout * 1_000_000) $ constantDelay 100_000)
    ((\_ -> Catch.Handler $ \(_ :: AssertionFailure) -> pure True) : skipAsyncExceptions)
    (const action)

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
  (MakesValue a, HasCallStack) =>
  -- | The actual value, in base64
  a ->
  -- | The expected value
  ByteString ->
  App ()
a `shouldMatchBase64` b = do
  let xb = Text.decodeUtf8 (B64.encode b)
  a `shouldMatch` xb

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
  la <- asSet a
  lb <- asSet b
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

shouldNotMatchOneOf ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App ()
shouldNotMatchOneOf a b = do
  lb <- asList b
  xa <- make a
  when (xa `elem` lb) $ do
    pa <- prettyJSON a
    pb <- prettyJSON b
    assertFailure $ "Expected:\n" <> pa <> "\n to not match any of:\n" <> pb

----------------------------------------------------------------------
-- Shape DSL

-- | A simple DSL for describing the recursive structure (shape) of a JSON
-- value.  Use 'shouldMatchShape' to assert that a value conforms to a shape,
-- or 'valueShape' to compute the shape of an existing value.
--
-- Object matching is /strict/: any key present in the actual value but absent
-- from the expected 'SObject' field list causes an assertion failure.
--
-- FUTUREWORK: heterogenous arrays (`["yes", true, 3]`)
-- FUTUREWORK: optional attributes
data Shape
  = SNull
  | SBool
  | SString
  | SNumber
  | SArray Shape -- (no heterogeneous lists allowed)
  | SObject [(String, Shape)]
  | SAny
  deriving (Show)

-- | Assert that @actual@ conforms to @shape@.  Provides a JSON-path-like
-- location in the failure message (e.g. @.assets[0].key@).
shouldMatchShape ::
  (MakesValue a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected shape
  Shape ->
  App ()
shouldMatchShape a shape = do
  val <- make a
  case matchShape "" val shape of
    Nothing -> pure ()
    Just err -> assertFailure $ "Shape mismatch" <> err

-- | Internal recursive shape-matcher.  Returns 'Nothing' on success and
-- @'Just' errorMessage@ on failure.  The @path@ argument accumulates the
-- JSON-path-like location prefix.
matchShape :: String -> Value -> Shape -> Maybe String
matchShape _ _ SAny = Nothing
matchShape _ Aeson.Null SNull = Nothing
matchShape path _ SNull = Just $ " at " <> matchShapeLoc path <> ": expected null"
matchShape _ (Aeson.Bool _) SBool = Nothing
matchShape path _ SBool = Just $ " at " <> matchShapeLoc path <> ": expected bool"
matchShape _ (Aeson.String _) SString = Nothing
matchShape path _ SString = Just $ " at " <> matchShapeLoc path <> ": expected string"
matchShape _ (Aeson.Number _) SNumber = Nothing
matchShape path _ SNumber = Just $ " at " <> matchShapeLoc path <> ": expected number"
matchShape path (Aeson.Array arr) (SArray elemShape) =
  listToMaybe
    . mapMaybe (\(i, v) -> matchShape (path <> "[" <> show (i :: Int) <> "]") v elemShape)
    $ zip [0 ..] (toList arr)
matchShape path _ (SArray _) = Just $ " at " <> matchShapeLoc path <> ": expected array"
matchShape path (Aeson.Object obj) (SObject fields) =
  let objPairs = [(Key.toString k, v) | (k, v) <- Aeson.toList obj]
      actualKeys = map fst objPairs
      expectedKeys = map fst fields
      unexpectedKeys = actualKeys \\ expectedKeys
      missingKeys = expectedKeys \\ actualKeys
      go (k, s) = case lookup k objPairs of
        Nothing -> Nothing -- already checked above
        Just v -> matchShape (path <> "." <> k) v s
   in case (unexpectedKeys, missingKeys) of
        (k : _, _) ->
          Just $ " at " <> matchShapeLoc path <> ": unexpected key \"" <> k <> "\""
        (_, k : _) ->
          Just $ " at " <> matchShapeLoc path <> ": missing key \"" <> k <> "\""
        _ ->
          listToMaybe . mapMaybe go $ fields
matchShape path _ (SObject _) = Just $ " at " <> matchShapeLoc path <> ": expected object"

-- | Format a path for use in error messages, using the document root (@$@)
-- when the path is empty.
matchShapeLoc :: String -> String
matchShapeLoc "" = "$"
matchShapeLoc p = p

-- | Compute the 'Shape' of an existing JSON value.  Useful for inspecting
-- what shape a response actually has, or for using a known-good response as
-- a shape template via @shouldMatchShape@.
--
-- Arrays: if the array is empty the element shape is 'SAny'; otherwise the
-- shape of the /first/ element is used for all elements.
valueShape :: (MakesValue a) => a -> App Shape
valueShape a = computeShape <$> make a
  where
    computeShape :: Value -> Shape
    computeShape Aeson.Null = SNull
    computeShape (Aeson.Bool _) = SBool
    computeShape (Aeson.String _) = SString
    computeShape (Aeson.Number _) = SNumber
    computeShape (Aeson.Array arr) =
      SArray $ case toList arr of
        [] -> SAny
        (v : _) -> computeShape v
    computeShape (Aeson.Object obj) =
      SObject [(Key.toString k, computeShape v) | (k, v) <- Aeson.toList obj]

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

shouldNotContain ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  [a] ->
  -- | The expected value
  [a] ->
  App ()
super `shouldNotContain` sub = do
  when (sub `isInfixOf` super) $ do
    assertFailure $ "String or List:\n" <> show super <> "\nDoes contain:\n" <> show sub

printFailureDetails :: Env -> AssertionFailure -> IO String
printFailureDetails env (AssertionFailure stack mbResponse ctx msg) = do
  s <- prettierCallStack stack
  ct <- renderCurlTrace env.curlTrace
  pure . unlines $
    colored yellow "assertion failure:"
      : colored red msg
      : "\n" <> s
      : toList (fmap prettyResponse mbResponse)
        <> toList (fmap prettyContext ctx)
        <> ct

printAppFailureDetails :: Env -> AppFailure -> IO String
printAppFailureDetails env (AppFailure msg stack) = do
  s <- prettierCallStack stack
  ct <- renderCurlTrace env.curlTrace
  pure . unlines $
    colored yellow "app failure:"
      : colored red msg
      : "\n"
      : [s]
        <> ct

renderCurlTrace :: IORef [String] -> IO [String]
renderCurlTrace trace = do
  isTestVerbose >>= \case
    True -> ("HTTP trace in curl pseudo-syntax:" :) <$> readIORef trace
    False -> pure ["Set WIRE_INTEGRATION_TEST_VERBOSITY=1 if you want to see complete trace of the HTTP traffic in curl pseudo-syntax."]

isTestVerbose :: (MonadIO m) => m Bool
isTestVerbose = liftIO $ (Just "1" ==) <$> lookupEnv "WIRE_INTEGRATION_TEST_VERBOSITY"

prettyContext :: String -> String
prettyContext ctx = do
  unlines
    [ colored yellow "context:",
      colored blue ctx
    ]

printExceptionDetails :: Env -> SomeException -> IO String
printExceptionDetails env e = do
  ct <- renderCurlTrace env.curlTrace
  pure . unlines $
    [ colored yellow "exception:",
      colored red (displayException e)
    ]
      <> ct

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
      [ pure $ colored yellow "request as command line: \n" <> requestToCurl r.request,
        pure $ colored yellow "request: \n" <> showRequest r.request,
        pure $ colored yellow "request headers: \n" <> showHeaders (HTTP.requestHeaders r.request),
        case getRequestBody r.request of
          Nothing -> []
          Just b ->
            [ colored yellow "request body:",
              Text.unpack . Text.decodeUtf8With Text.lenientDecode $ case Aeson.decode (BS.fromStrict b) of
                Just v -> BS.toStrict (Aeson.encodePretty (v :: Aeson.Value))
                Nothing -> hex b
            ],
        pure $ colored blue "response status: " <> show r.status,
        pure $ colored blue "response headers:\n" <> showHeaders r.headers,
        pure $ colored blue "response body:",
        pure $
          ( TL.unpack . TL.decodeUtf8With Text.lenientDecode $
              case r.json of
                Just b -> (Aeson.encodePretty b)
                Nothing -> BS.fromStrict r.body
          )
      ]
