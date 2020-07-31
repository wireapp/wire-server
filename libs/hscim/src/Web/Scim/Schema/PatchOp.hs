-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Schema.PatchOp where

import Control.Applicative
import Control.Monad (guard)
import Control.Monad.Except
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON), Value (String), object, withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly)
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, toCaseFold, toLower)
import Data.Text.Encoding (encodeUtf8)
import Web.Scim.AttrName (AttrName (..))
import Web.Scim.Filter (AttrPath (..), SubAttr (..), ValuePath (..), pAttrPath, pSubAttr, pValuePath, rAttrPath, rSubAttr, rValuePath)
import Web.Scim.Schema.Error
import Web.Scim.Schema.Schema (Schema (PatchOp20))
import Web.Scim.Schema.UserTypes (UserTypes (supportedSchemas))

newtype PatchOp tag = PatchOp
  {getOperations :: [Operation]}
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
  { op :: Op,
    path :: Maybe Path,
    value :: Maybe Value
  }
  deriving (Eq, Show)

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

parsePath :: [Schema] -> Text -> Either String Path
parsePath schemas' = parseOnly (pPath schemas' <* endOfInput) . encodeUtf8

-- | PATH = attrPath / valuePath [subAttr]
pPath :: [Schema] -> Parser Path
pPath schemas' =
  IntoValuePath <$> pValuePath schemas' <*> optional pSubAttr
    <|> NormalPath <$> pAttrPath schemas'

rPath :: Path -> Text
rPath (NormalPath attrPath) = rAttrPath attrPath
rPath (IntoValuePath valuePath subAttr) = rValuePath valuePath <> maybe "" rSubAttr subAttr

-- TODO(arianvp): According to the SCIM spec we should throw an InvalidPath
-- error when the path is invalid syntax. this is a bit hard to do though as we
-- can't control what errors FromJSON throws :/
instance UserTypes tag => FromJSON (PatchOp tag) where
  parseJSON = withObject "PatchOp" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    schemas' :: [Schema] <- o .: "schemas"
    guard $ PatchOp20 `elem` schemas'
    operations <- Aeson.explicitParseField (Aeson.listParser $ operationFromJSON (supportedSchemas @tag)) o "operations"
    pure $ PatchOp operations

instance ToJSON (PatchOp tag) where
  toJSON (PatchOp operations) =
    object ["operations" .= operations, "schemas" .= [PatchOp20]]

-- TODO: Azure wants us to be case-insensitive on _values_ as well here.  We currently do not
-- comply with that.
operationFromJSON :: [Schema] -> Value -> Aeson.Parser Operation
operationFromJSON schemas' =
  withObject "Operation" $ \v -> do
    let o = HashMap.fromList . map (first toLower) . HashMap.toList $ v
    Operation
      <$> (o .: "op")
      <*> (Aeson.explicitParseFieldMaybe (pathFromJSON schemas') o "path")
      <*> (o .:? "value")

pathFromJSON :: [Schema] -> Value -> Aeson.Parser Path
pathFromJSON schemas' =
  withText "Path" $ either fail pure . (parsePath schemas')

instance ToJSON Operation where
  toJSON (Operation op' path' value') =
    object $ ("op" .= op') : concat [optionalField "path" path', optionalField "value" value']
    where
      optionalField fname = \case
        Nothing -> []
        Just x -> [fname .= x]

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

instance ToJSON Path where
  toJSON = String . rPath

-- | A very coarse description of what it means to be 'Patchable'
-- I do not like it. We should handhold people using this library more
class Patchable a where
  applyOperation :: (MonadError ScimError m) => a -> Operation -> m a

instance Patchable (HM.HashMap Text Text) where
  applyOperation theMap (Operation Remove (Just (NormalPath (AttrPath _schema (AttrName attrName) _subAttr))) _) =
    pure $ HM.delete attrName theMap
  applyOperation theMap (Operation _AddOrReplace (Just (NormalPath (AttrPath _schema (AttrName attrName) _subAttr))) (Just (String val))) =
    pure $ HM.insert attrName val theMap
  applyOperation _ _ = throwError $ badRequest InvalidValue $ Just "Unsupported operation"
