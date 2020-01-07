module Web.Scim.Schema.PatchOp where

import Control.Applicative
import Control.Monad (guard)
import Web.Scim.Schema.Schema (Schema(PatchOp20))
import Data.Aeson.Types (withText, ToJSON(toJSON), object, (.=), FromJSON(parseJSON), withObject, (.:), (.:?), Value(String))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (toCaseFold, toLower, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Bifunctor (first)
import Data.Attoparsec.ByteString (Parser, parseOnly, endOfInput)
import Web.Scim.Filter (AttrPath(..), ValuePath(..), SubAttr(..), pAttrPath, pValuePath, pSubAttr, rAttrPath, rSubAttr, rValuePath)
import Data.Maybe (fromMaybe)

newtype PatchOp = PatchOp
  { getOperations :: [Operation] }
  deriving (Eq, Show)

-- | The 'Path' attribute value is a 'String' containing an attribute path
-- describing the target of the operation.  It is OPTIONAL
-- for 'Op's "add" and "replace", and is REQUIRED for "remove".  See
-- relevant operation sections below for details.
--
-- TODO(arianvp):  When value is an array, it needs special handling.
-- e.g. primary fields need to be negated and whatnot.
-- We currently do not do that :)
--
-- NOTE: When the path contains a schema, this schema must be implicitly added
-- to the list of schemas on the result type
data Operation = Operation
  { op :: Op
  , path :: Maybe Path
  , value :: Maybe Value
  } deriving (Eq, Show)

data Op
  = Add
  | Replace
  | Remove
  deriving (Eq, Show, Enum, Bounded)

-- | PATH = attrPath / valuePath [subAttr]
data Path
  = NormalPath AttrPath
  | IntoValuePath ValuePath (Maybe SubAttr)
  deriving (Eq, Show)


parsePath :: Text -> Either String Path
parsePath = parseOnly (pPath <* endOfInput) . encodeUtf8

-- | PATH = attrPath / valuePath [subAttr]
pPath :: Parser Path
pPath =
  IntoValuePath <$> pValuePath <*> optional pSubAttr <|>
  NormalPath <$> pAttrPath

rPath :: Path -> Text
rPath (NormalPath attrPath) = rAttrPath attrPath
rPath (IntoValuePath valuePath subAttr) = rValuePath valuePath <> fromMaybe "" (rSubAttr <$> subAttr)


-- TODO(arianvp): According to the SCIM spec we should throw an InvalidPath
-- error when the path is invalid syntax. this is a bit hard to do though as we
-- can't control what errors FromJSON throws :/
instance FromJSON PatchOp where
  parseJSON = withObject "PatchOp" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    schemas :: [Schema] <- o .: "schemas"
    guard $ PatchOp20 `elem` schemas
    operations <- o .: "operations"
    pure $ PatchOp operations

instance ToJSON PatchOp where
  toJSON (PatchOp operations) =
    object [ "operations" .=  operations , "schemas" .= [PatchOp20] ]


-- TODO: Azure wants us to be case-insensitive on _values_ as well here.  We currently do not
-- comply with that.
instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    Operation <$> (o .: "op") <*> (o .:? "path") <*> (o .:? "value")

instance ToJSON Operation where
  toJSON (Operation op' path' value') =
    object $ ("op" .= op') : concat [optionalField "path" path', optionalField "value" value']
    where
      optionalField fname = \case
        Nothing -> []
        Just x  -> [fname .= x]


instance FromJSON Op where
  parseJSON = withText "Op" $ \op' ->
    case toCaseFold op' of
      "add" -> pure Add
      "replace" -> pure Replace
      "remove" -> pure Remove
      _ -> fail "unknown operation"

instance ToJSON Op where
  toJSON Add = String "add"
  toJSON Replace = String "replace"
  toJSON Remove = String "remove"


instance  FromJSON Path where
  parseJSON = withText "Path" $ either fail pure . parsePath

instance ToJSON Path where
  toJSON = String . rPath
