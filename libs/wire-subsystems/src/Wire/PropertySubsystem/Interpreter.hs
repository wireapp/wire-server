module Wire.PropertySubsystem.Interpreter where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Ascii qualified as Ascii
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Properties
import Wire.API.UserEvent
import Wire.Events
import Wire.PropertyStore (PropertyStore)
import Wire.PropertyStore qualified as PropertyStore
import Wire.PropertySubsystem

data PropertySubsystemConfig = PropertySubsystemConfig
  { maxKeyLength :: Int64,
    maxValueLength :: Int64,
    maxProperties :: Int
  }

interpretPropertySubsystem ::
  ( Member PropertyStore r,
    Member (Error PropertySubsystemError) r,
    Member Events r,
    Member TinyLog r
  ) =>
  PropertySubsystemConfig ->
  InterpreterFor PropertySubsystem r
interpretPropertySubsystem cfg =
  interpret $
    runInputConst cfg . \case
      SetProperty uid connId key val -> setPropertyImpl uid connId key val
      DeleteProperty uid connId key -> deletePropertyImpl uid connId key
      ClearProperties uid connId -> clearPropertiesImpl uid connId
      OnUserDeleted uid -> onUserDeletdImpl uid
      LookupProperty uid key -> lookupPropertyImpl uid key
      GetPropertyKeys uid -> getPropertyKeysImpl uid
      GetAllProperties uid -> getAllPropertiesImpl uid

setPropertyImpl ::
  ( Member PropertyStore r,
    Member (Input PropertySubsystemConfig) r,
    Member (Error PropertySubsystemError) r,
    Member Events r
  ) =>
  UserId ->
  ConnId ->
  PropertyKey ->
  RawPropertyValue ->
  Sem r ()
setPropertyImpl uid connId key val = do
  validatePropertyKey key
  checkMaxProperties uid key
  parsedVal <- validatePropertyValue val
  PropertyStore.insertProperty uid key val
  generatePropertyEvent uid connId $ PropertySet key parsedVal

checkMaxProperties ::
  ( Member PropertyStore r,
    Member (Input PropertySubsystemConfig) r,
    Member (Error PropertySubsystemError) r
  ) =>
  UserId ->
  PropertyKey ->
  Sem r ()
checkMaxProperties uid key = do
  propExists <- isJust <$> PropertyStore.lookupProperty uid key
  unless propExists $ do
    cfg <- input
    count <- PropertyStore.countProperties uid
    when (count >= cfg.maxProperties) $
      throw TooManyProperties

validatePropertyKey ::
  ( Member (Input PropertySubsystemConfig) r,
    Member (Error PropertySubsystemError) r
  ) =>
  PropertyKey ->
  Sem r ()
validatePropertyKey key = do
  cfg <- input
  let keyText = Ascii.toText $ propertyKeyName key
  when (Text.compareLength keyText (fromIntegral cfg.maxKeyLength) == GT) $
    throw PropertyKeyTooLarge

validatePropertyValue ::
  ( Member (Input PropertySubsystemConfig) r,
    Member (Error PropertySubsystemError) r
  ) =>
  RawPropertyValue ->
  Sem r Value
validatePropertyValue (RawPropertyValue bs) = do
  cfg <- input
  when (LBS.compareLength bs cfg.maxValueLength == GT) $
    throw PropertyValueTooLarge

  case Aeson.eitherDecode @Value bs of
    Left e -> throw $ PropertyValueInvalid e
    Right val -> pure val

deletePropertyImpl :: (Member PropertyStore r, Member Events r) => UserId -> ConnId -> PropertyKey -> Sem r ()
deletePropertyImpl uid connId key = do
  PropertyStore.deleteProperty uid key
  generatePropertyEvent uid connId $ PropertyDeleted key

onUserDeletdImpl :: (Member PropertyStore r) => UserId -> Sem r ()
onUserDeletdImpl uid = do
  PropertyStore.clearProperties uid

clearPropertiesImpl :: (Member PropertyStore r, Member Events r) => UserId -> ConnId -> Sem r ()
clearPropertiesImpl uid connId = do
  PropertyStore.clearProperties uid
  generatePropertyEvent uid connId PropertiesCleared

lookupPropertyImpl :: (Member PropertyStore r) => UserId -> PropertyKey -> Sem r (Maybe RawPropertyValue)
lookupPropertyImpl uid key =
  PropertyStore.lookupProperty uid key

getPropertyKeysImpl :: (Member PropertyStore r) => UserId -> Sem r [PropertyKey]
getPropertyKeysImpl uid =
  PropertyStore.getPropertyKeys uid

getAllPropertiesImpl ::
  ( Member PropertyStore r,
    Member TinyLog r,
    Member (Error PropertySubsystemError) r
  ) =>
  UserId ->
  Sem r PropertyKeysAndValues
getAllPropertiesImpl uid = do
  rawProps <- Map.fromList <$> PropertyStore.getAllProperties uid
  PropertyKeysAndValues <$> traverse parseStoredPropertyValue rawProps

parseStoredPropertyValue :: (Member TinyLog r, Member (Error PropertySubsystemError) r) => RawPropertyValue -> Sem r Value
parseStoredPropertyValue raw = case Aeson.eitherDecode raw.rawPropertyBytes of
  Right value -> pure value
  Left e -> do
    Log.err $
      Log.msg (Log.val "Failed to parse a stored property value")
        . Log.field "raw_value" raw.rawPropertyBytes
        . Log.field "parse_error" e
    throw StoredPropertyValueInvalid
