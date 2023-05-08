module Testlib.JSON where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List.Split (splitOn)
import qualified Data.Scientific as Sci
import Data.String
import qualified Data.Text as T
import GHC.Stack
import Testlib.Types

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

instance MakesValue Response where
  make r = r.json

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
  addFailureContext ("Getting (nested) field \"" <> selector <> "\" of object:\n" <> vp) $ do
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
printJSON = prettyJSON >=> liftIO . putStrLn

prettyJSON :: MakesValue a => a -> App String
prettyJSON x =
  make x <&> Aeson.encodePretty <&> LC8.unpack

jsonType :: Value -> String
jsonType (Object _) = "Object"
jsonType (Array _) = "Array"
jsonType (String _) = "String"
jsonType (Number _) = "Number"
jsonType (Bool _) = "Bool"
jsonType Null = "Null"

typeWasExpectedButGot :: String -> Value -> String
typeWasExpectedButGot expectedType x = "Expected " <> expectedType <> " but got " <> jsonType x <> ":"

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
      pure (dom, id_)

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

-- Get "domain" field or - if already string-like return String
objDomain :: MakesValue a => a -> App String
objDomain x = do
  v <- make x
  case v of
    Object _ob -> fst <$> objQid v
    String t -> pure (T.unpack t)
    other -> assertFailureWithJSON other (typeWasExpectedButGot "Object or String" other)
