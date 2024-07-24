{-# LANGUAGE TemplateHaskell #-}

module Wire.PropertySubsystem where

import Data.Id
import Data.Text.Lazy qualified as LText
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities qualified as Wai
import Polysemy
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Properties
import Wire.Error

data PropertySubsystemError
  = TooManyProperties
  | PropertyKeyTooLarge
  | PropertyValueTooLarge
  | PropertyValueInvalid String
  | StoredPropertyValueInvalid
  deriving (Show, Eq)

propertySubsystemErrorToHttpError :: PropertySubsystemError -> HttpError
propertySubsystemErrorToHttpError =
  StdError . \case
    TooManyProperties -> errorToWai @E.TooManyProperties
    PropertyKeyTooLarge -> errorToWai @E.PropertyKeyTooLarge
    PropertyValueTooLarge -> errorToWai @E.PropertyValueTooLarge
    PropertyValueInvalid err -> Wai.mkError status400 "bad-request" (LText.pack err)
    StoredPropertyValueInvalid -> Wai.mkError status500 "internal-server-error" "Internal Server Error"

data PropertySubsystem m a where
  SetProperty :: UserId -> ConnId -> PropertyKey -> RawPropertyValue -> PropertySubsystem m ()
  DeleteProperty :: UserId -> ConnId -> PropertyKey -> PropertySubsystem m ()
  ClearProperties :: UserId -> ConnId -> PropertySubsystem m ()
  OnUserDeleted :: UserId -> PropertySubsystem m ()
  LookupProperty :: UserId -> PropertyKey -> PropertySubsystem m (Maybe RawPropertyValue)
  GetPropertyKeys :: UserId -> PropertySubsystem m [PropertyKey]
  GetAllProperties :: UserId -> PropertySubsystem m PropertyKeysAndValues

makeSem ''PropertySubsystem
