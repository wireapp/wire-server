module Testlib.JSON where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as KM
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Stack
import Testlib.Types
import Prelude

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
  make :: (HasCallStack) => a -> App Value

instance {-# OVERLAPPABLE #-} (ToJSON a) => MakesValue a where
  make = pure . toJSON

instance {-# OVERLAPPING #-} (ToJSON a) => MakesValue (App a) where
  make m = m <&> toJSON

-- use this to provide Nothing for MakesValue a => (Maybe a) values.
noValue :: Maybe Value
noValue = Nothing

(.=) :: (ToJSON a) => String -> a -> Aeson.Pair
(.=) k v = fromString k Aeson..= v

(.=?) :: (ToJSON a) => String -> Maybe a -> Maybe Aeson.Pair
(.=?) k v = (Aeson..=) (fromString k) <$> v

-- | Convert JSON null to Nothing.
asOptional :: (HasCallStack) => (MakesValue a) => a -> App (Maybe Value)
asOptional x = do
  v <- make x
  pure $ case v of
    Null -> Nothing
    _ -> Just v

asString :: (HasCallStack) => (MakesValue a) => a -> App String
asString x =
  make x >>= \case
    (String s) -> pure (T.unpack s)
    v -> assertFailureWithJSON x ("String" `typeWasExpectedButGot` v)

asText :: (HasCallStack) => (MakesValue a) => a -> App T.Text
asText = (fmap T.pack) . asString

asStringM :: (HasCallStack) => (MakesValue a) => a -> App (Maybe String)
asStringM x =
  make x >>= \case
    (String s) -> pure (Just (T.unpack s))
    _ -> pure Nothing

asByteString :: (HasCallStack, MakesValue a) => a -> App ByteString
asByteString x = do
  s <- asString x
  let bs = T.encodeUtf8 (T.pack s)
  case Base64.decode bs of
    Left _ -> assertFailure "Could not base64 decode"
    Right a -> pure a

asObject :: (HasCallStack) => (MakesValue a) => a -> App Object
asObject x =
  make x >>= \case
    (Object o) -> pure o
    v -> assertFailureWithJSON x ("Object" `typeWasExpectedButGot` v)

asInt :: (HasCallStack) => (MakesValue a) => a -> App Int
asInt = asIntegral

asIntegral :: (Integral i, HasCallStack) => (MakesValue a) => a -> App i
asIntegral x =
  make x >>= \case
    (Number n) ->
      case Sci.floatingOrInteger n of
        Left (_ :: Double) -> assertFailure "Expected an integral, but got a floating point"
        Right i -> pure i
    v -> assertFailureWithJSON x ("Number" `typeWasExpectedButGot` v)

asList :: (HasCallStack) => (MakesValue a) => a -> App [Value]
asList x =
  make x >>= \case
    (Array arr) -> pure (toList arr)
    v -> assertFailureWithJSON x ("Array" `typeWasExpectedButGot` v)

asListOf :: (HasCallStack) => (Value -> App b) -> (MakesValue a) => a -> App [b]
asListOf makeElem x =
  asList x >>= mapM makeElem

asSet :: (HasCallStack) => (MakesValue a) => a -> App (Set.Set Value)
asSet = fmap Set.fromList . asList

asSetOf :: (HasCallStack, Ord b) => (Value -> App b) -> (MakesValue a) => a -> App (Set.Set b)
asSetOf makeElem x = Set.fromList <$> asListOf makeElem x

asBool :: (HasCallStack) => (MakesValue a) => a -> App Bool
asBool x =
  make x >>= \case
    (Bool b) -> pure b
    v -> assertFailureWithJSON x ("Bool" `typeWasExpectedButGot` v)

-- | Get a (nested) field of a JSON object
-- Raise an AssertionFailure if the field at the (nested) key is missing. See
-- 'lookupField' for details.
(%.) ::
  (HasCallStack, MakesValue a) =>
  a ->
  -- | A plain key, e.g. "id", or a nested key "user.profile.id"
  String ->
  App Value
(%.) x k = lookupField x k >>= assertField x k

isEqual ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  a ->
  b ->
  App Bool
isEqual = liftP2 (==)

liftP2 ::
  (MakesValue a, MakesValue b, HasCallStack) =>
  (Value -> Value -> c) ->
  a ->
  b ->
  App c
liftP2 f a b = do
  f <$> make a <*> make b

fieldEquals :: (MakesValue a, MakesValue b) => a -> String -> b -> App Bool
fieldEquals a fieldSelector b = do
  ma <- lookupField a fieldSelector `catchAll` const (pure Nothing)
  case ma of
    Nothing -> pure False
    Just f ->
      f `isEqual` b

assertFieldMissing :: (HasCallStack, MakesValue a) => a -> String -> App ()
assertFieldMissing x k = do
  mValue <- lookupField x k
  case mValue of
    Nothing -> pure ()
    Just _ -> assertFailureWithJSON x $ "Field \"" <> k <> "\" should be missing from object:"

assertField :: (HasCallStack, MakesValue a) => a -> String -> Maybe Value -> App Value
assertField x k Nothing = assertFailureWithJSON x $ "Field \"" <> k <> "\" is missing from object:"
assertField _ _ (Just x) = pure x

-- rename a field if it exists, else return the old object
renameField :: String -> String -> Value -> App Value
renameField old new obj =
  fromMaybe obj <$> runMaybeT do
    o :: Value <- maybe mzero pure =<< lift (lookupField obj old)
    lift (removeField old obj >>= setField new o)

-- | like 'lookupField' but wrapped in 'MaybeT' for convenience
lookupFieldM ::
  (HasCallStack, MakesValue a) =>
  a ->
  -- | A plain key, e.g. "id", or a nested key "user.profile.id"
  String ->
  MaybeT App Value
lookupFieldM = fmap MaybeT . lookupField

-- | Look up (nested) field of a JSON object
--
-- If the field key has no dots then returns Nothing if the key is missing from the
-- object.
--
-- If the field key has dots (describes a nested lookuyp) then returns Nothing
-- if the last component of the key field selector is missing from nested
-- object. If any other component is missing this function raises an
-- AssertionFailure.
--
-- Objects and arrays are supported. Array keys should be integers.
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
    get v k = do
      make v >>= \case
        -- index object
        Object ob -> pure (KM.lookup (KM.fromString k) ob)
        -- index array
        Array arr -> case reads k of
          [(i, "")] ->
            if i >= 0
              then pure (arr !? i)
              else pure (arr !? (V.length arr + i))
          _ -> assertFailureWithJSON arr $ "Invalid array index \"" <> k <> "\""
        x -> assertFailureWithJSON x ("Object or Array" `typeWasExpectedButGot` x)
    go k [] v = get v k
    go k (k2 : ks) v = get v k >>= assertField v k >>= go k2 ks

-- | Update nested fields
-- E.g. ob & "foo.bar.baz" %.= ("quux" :: String)
-- The selector path will be created if non-existing.
setField ::
  forall a b.
  (HasCallStack, MakesValue a, ToJSON b) =>
  -- | Selector, e.g. "id", "user.team.id"
  String ->
  -- | The value that should insert or replace the value at the selector
  b ->
  a ->
  App Value
setField selector v x = do
  modifyField @a @Value selector (\_ -> pure (toJSON v)) x

-- | Merges fields if the old and new are both Objects or Arrays. Otherwise new
-- field overwrites the old completely
mergeField :: forall a b. (HasCallStack, MakesValue a, ToJSON b) => String -> b -> a -> App Value
mergeField selector v x = do
  modifyField @a @Value
    selector
    ( \case
        Just (Object old) -> case toJSON v of
          (Object new) -> pure $ Object (new <> old)
          nonObjectNew -> pure nonObjectNew
        Just (Array old) -> case toJSON v of
          (Array new) -> pure $ Array (old <> new)
          nonArrayNew -> pure nonArrayNew
        _ -> pure (toJSON v)
    )
    x

member :: (HasCallStack, MakesValue a) => String -> a -> App Bool
member k x = KM.member (KM.fromString k) <$> (make x >>= asObject)

-- | Update nested fields, using the old value with a stateful action
-- The selector path will be created if non-existing.
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
      val <- fromMaybe (Object $ KM.empty) <$> lookupField v k
      newValue <- go k2 ks val
      ob <- asObject v
      pure $ Object $ KM.insert (KM.fromString k) newValue ob

-- | `removeField "a.b" {"a": {"b": 3}, "c": true} == {"a": {}, "c": true}`
removeField :: (HasCallStack, MakesValue a) => String -> a -> App Value
removeField selector x = do
  v <- make x
  let keys = splitOn "." selector
  case keys of
    (k : ks) -> go k ks v
    [] -> assertFailure "No key provided"
  where
    go k [] v = do
      ob <- asObject v
      let k' = KM.fromString k
      pure $ Object $ KM.delete k' ob
    go k (k2 : ks) v = do
      v' <- v %. k
      newValue <- go k2 ks v'
      ob <- asObject v
      pure $ Object $ KM.insert (KM.fromString k) newValue ob

assertFailureWithJSON :: (HasCallStack) => (MakesValue a) => a -> String -> App b
assertFailureWithJSON v msg = do
  msg' <- ((msg <> "\n") <>) <$> prettyJSON v
  assertFailure msg'

-- | Useful for debugging
printJSON :: (MakesValue a) => a -> App ()
printJSON = prettyJSON >=> liftIO . putStrLn

-- | useful for debugging, same as 'printJSON' but returns input JSON
traceJSON :: (MakesValue a) => a -> App a
traceJSON a = printJSON a $> a

prettyJSON :: (MakesValue a) => a -> App String
prettyJSON x =
  make x <&> LC8.unpack . Aeson.encodePretty

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
objId :: (HasCallStack) => (MakesValue a) => a -> App String
objId x = do
  v <- make x
  case v of
    Object ob -> ob %. "id" & asString
    String t -> pure (T.unpack t)
    other -> assertFailureWithJSON other (typeWasExpectedButGot "Object or String" other)

-- Get "qualified_id" field as (domain, id) or - if already is a qualified id object - return that
objQid :: (HasCallStack) => (MakesValue a) => a -> App (String, String)
objQid ob = do
  m <- firstSuccess [select ob, inField]
  case m of
    Nothing -> do
      assertFailureWithJSON ob "Could not get a qualified id from value:"
    Just v -> pure v
  where
    select x = runMaybeT $ do
      vdom <- lookupFieldM x "domain"
      dom <- MaybeT $ asStringM vdom
      vid <- lookupFieldM x "id"
      id_ <- MaybeT $ asStringM vid
      pure (dom, id_)

    inField = do
      m <- lookupField ob "qualified_id"
      case m of
        Nothing -> pure Nothing
        Just x -> select x

    firstSuccess :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
    firstSuccess [] = pure Nothing
    firstSuccess (x : xs) =
      x >>= \case
        Nothing -> firstSuccess xs
        Just y -> pure (Just y)

-- | Get "qualified_id" field as {"id": _, "domain": _} object or - if already is a qualified id object - return that.
objQidObject :: (HasCallStack) => (MakesValue a) => a -> App Value
objQidObject o = do
  (domain, id_) <- objQid o
  pure $ object ["domain" .= domain, "id" .= id_]

-- Get "domain" field or - if already string-like - return String.
objDomain :: (HasCallStack, MakesValue a) => a -> App String
objDomain x = do
  v <- make x
  case v of
    Object _ob -> fst <$> objQid v
    String t -> pure (T.unpack t)
    other -> assertFailureWithJSON other (typeWasExpectedButGot "Object or String" other)

-- | Get conversation ID and optional subconversation ID.
--
-- This accepts subconversation objects in the format:
-- @
-- { "parent_qualified_id": {
--      "domain": "example.com",
--      "id": "7b6c21d1-322d-4be6-a923-85225691f398"
--   },
--   "subconv_id": "conference"
-- }
-- @
--
-- as well as conversation objects in the general format supported by 'objQid'.
-- Conversation objects can optionally contain a @subconv_id@ field. So, in
-- particular, a flat subconversation format, like
-- @
-- { "domain": "example.com",
--   "id": "7b6c21d1-322d-4be6-a923-85225691f398",
--   "subconv_id": "conference"
-- }
-- @
-- is also supported.
objSubConv :: (HasCallStack, MakesValue a) => a -> App (Value, Maybe String)
objSubConv x = do
  v <- make x
  mParent <- lookupField v "parent_qualified_id"
  obj <- objQidObject $ fromMaybe v mParent
  sub <- runMaybeT $ do
    sub <- MaybeT $ lookupField v "subconv_id"
    sub' <- MaybeT $ asOptional sub
    lift $ asString sub'
  pure (obj, sub)

objConvId :: (HasCallStack, MakesValue conv) => conv -> App ConvId
objConvId conv = do
  v <- make conv
  -- Domain and ConvId either come from parent_qualified_id or qualified_id
  mParent <- lookupField v "parent_qualified_id"
  (domain, id_) <- objQid $ fromMaybe v mParent

  groupId <- traverse asString =<< asOptional (lookupField v "group_id")
  subconvId <- traverse asString =<< asOptional (lookupField v "subconv_id")
  pure ConvId {..}

instance MakesValue ClientIdentity where
  make cid =
    pure $
      object
        [ "domain" .= cid.domain,
          "id" .= cid.user,
          "client_id" .= cid.client
        ]

instance MakesValue CredentialType where
  make BasicCredentialType = make "basic"
  make X509CredentialType = make "x509"
