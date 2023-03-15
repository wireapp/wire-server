module App where

import Config
import qualified Control.Exception as E
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import GHC.Exception
import GHC.Stack
import Imports hiding (ask, asks)
import qualified Network.HTTP.Client as HTTP

green :: String
green = "\x1b[38;5;10m"

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

(@?=) ::
  (Eq a, Show a, HasCallStack) =>
  -- | The actual value
  a ->
  -- | The expected value
  a ->
  App ()
a @?= b = unless (a == b) $ do
  assertionFailure $ "Expected: " <> show b <> "\n" <> "Actual: " <> show a

assertionFailure :: HasCallStack => String -> App a
assertionFailure msg =
  deepseq msg $
    liftIO $
      E.throw (AssertionFailure callStack msg)

displayStack :: CallStack -> IO ()
displayStack = traverse_ putStrLn . ("\x1b[38;5;11mcall stack:\x1b[0m" :) . drop 1 . prettyCallStackLines

class ProducesJSON a where
  prodJSON :: HasCallStack => a -> App Value

instance {-# OVERLAPPABLE #-} ToJSON a => ProducesJSON a where
  prodJSON = pure . toJSON

instance {-# OVERLAPPING #-} ToJSON a => ProducesJSON (App a) where
  prodJSON m = m <&> toJSON

(%.) :: (HasCallStack, ProducesJSON a) => a -> String -> App Value
(%.) x k = do
  ob <- asObject x
  case KM.lookup (KM.fromString k) ob of
    Nothing -> assertionFailureWithJSON ob $ "Field \"" <> k <> "\" is missing from object:"
    Just v -> pure v

(%.?) :: (HasCallStack, ProducesJSON a) => a -> String -> App (Maybe Value)
(%.?) x k = do
  ob <- asObject x
  pure $ KM.lookup (KM.fromString k) ob

(%?=) ::
  forall a b.
  (HasCallStack, ProducesJSON a, ProducesJSON b) =>
  -- | The actual value
  a ->
  -- | The expected value
  b ->
  App ()
a %?= b = do
  xa <- prodJSON a
  xb <- prodJSON b
  when (xa /= xb) $ do
    xaP <- prettyJSON xa
    xbP <- prettyJSON xb
    assertionFailure $ "Expected:\n" <> xbP <> "Actual:\n" <> xaP

pprintJSON :: ProducesJSON a => a -> App ()
pprintJSON = prettyJSON >=> putStrLn

prettyJSON :: ProducesJSON a => a -> App String
prettyJSON x =
  prodJSON x <&> Aeson.encodePretty <&> LC8.unpack

assertionFailureWithJSON :: HasCallStack => ProducesJSON a => a -> String -> App b
assertionFailureWithJSON v msg = do
  msg' <- ((msg <> "\n") <>) <$> prettyJSON v
  assertionFailure msg'

constrName :: Value -> String
constrName (Object _) = "Object"
constrName (Array _) = "Array"
constrName (String _) = "String"
constrName (Number _) = "Number"
constrName (Bool _) = "Bool"
constrName Null = "Null"

typeWasExpectedButGot :: String -> Value -> String
typeWasExpectedButGot expectedType x = "Expected " <> expectedType <> " but got " <> constrName x <> ":"

asStringM :: HasCallStack => ProducesJSON a => a -> App (Maybe String)
asStringM x =
  prodJSON x >>= \case
    (String s) -> pure (Just (T.unpack s))
    _ -> pure Nothing

asString :: HasCallStack => ProducesJSON a => a -> App String
asString x =
  prodJSON x >>= \case
    (String s) -> pure (T.unpack s)
    v -> assertionFailureWithJSON x ("String" `typeWasExpectedButGot` v)

asObject :: HasCallStack => ProducesJSON a => a -> App Object
asObject x =
  prodJSON x >>= \case
    (Object o) -> pure o
    v -> assertionFailureWithJSON x ("Object" `typeWasExpectedButGot` v)

asInt :: HasCallStack => ProducesJSON a => a -> App Int
asInt x =
  prodJSON x >>= \case
    (Number n) ->
      case Sci.floatingOrInteger n of
        Left (_ :: Double) -> assertionFailure "Expected an integral, but got a floating point"
        Right i -> pure i
    v -> assertionFailureWithJSON x ("Number" `typeWasExpectedButGot` v)

asList :: HasCallStack => ProducesJSON a => a -> App [Value]
asList x =
  prodJSON x >>= \case
    (Array arr) -> pure (toList arr)
    v -> assertionFailureWithJSON x ("Array" `typeWasExpectedButGot` v)

asBool :: HasCallStack => ProducesJSON a => a -> App Bool
asBool x =
  prodJSON x >>= \case
    (Bool b) -> pure b
    v -> assertionFailureWithJSON x ("Bool" `typeWasExpectedButGot` v)

objId :: ProducesJSON a => a -> App String
objId x = do
  v <- prodJSON x
  case v of
    Object ob -> ob %. "id" & asString
    String t -> pure (T.unpack t)
    other -> assertionFailureWithJSON other (typeWasExpectedButGot "Object or String" other)

-- There's probably nicer way to write this
objQid :: ProducesJSON a => a -> App Value
objQid ob = do
  m <- firstSuccess [select ob, inField]
  case m of
    Nothing -> do
      assertionFailureWithJSON ob "Could not get a qualified id from value:"
    Just v -> pure v
  where
    select x = runMaybeT $ do
      vdom <- MaybeT $ x %.? "domain"
      dom <- MaybeT $ asStringM vdom
      vid <- MaybeT $ x %.? "id"
      id_ <- MaybeT $ asStringM vid
      pure $ object ["domain" .= dom, "id" .= id_]

    inField = do
      m <- ob %.? "qualified_id"
      case m of
        Nothing -> pure Nothing
        Just x -> select x

objPath :: ProducesJSON a => a -> App String
objPath x = do
  ob <- prodJSON x
  id_ <- ob %. "id" & asString
  domain_ <- ob %. "domain" & asString
  pure $ domain_ <> "/" <> id_

firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess [] = pure Nothing
firstSuccess (x : xs) =
  x >>= \case
    Nothing -> firstSuccess xs
    Just y -> pure (Just y)
