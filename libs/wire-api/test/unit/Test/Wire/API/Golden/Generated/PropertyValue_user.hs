{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PropertyValue_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_PropertyValue_user_1 :: PropertyValue
testObject_PropertyValue_user_1 = PropertyValue {propertyValueJson = Number (2000.0)}
testObject_PropertyValue_user_2 :: PropertyValue
testObject_PropertyValue_user_2 = PropertyValue {propertyValueJson = Array [Null,String "N",Number (2.0e-3),Number (1.0e-3),Bool True,String "",Number (-300.0),String "4\986331\16330",Number (1000.0),Bool True]}
testObject_PropertyValue_user_3 :: PropertyValue
testObject_PropertyValue_user_3 = PropertyValue {propertyValueJson = Array [String "",Null,Number (2.0),Null,Number (0.0),Number (-1.0e-4),Bool True]}
testObject_PropertyValue_user_4 :: PropertyValue
testObject_PropertyValue_user_4 = PropertyValue {propertyValueJson = Object (fromList [("Q\1093861",Null),("e^ou",Bool True),("\STXr0ah",String ""),("g@",String "\SO\b\1000734"),("\135388\ve",Bool True),("\149403Of",Bool True)])}
testObject_PropertyValue_user_5 :: PropertyValue
testObject_PropertyValue_user_5 = PropertyValue {propertyValueJson = Array [Number (-9.0e13),Number (-3.0e-11)]}
