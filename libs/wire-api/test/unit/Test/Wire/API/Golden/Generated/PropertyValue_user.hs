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
testObject_PropertyValue_user_1 = PropertyValue {propertyValueJson = Object (fromList [("q.",Null),("\ETB\1005521",Bool True),("*",Bool False),("Z",Null),("\r",String "\1006990\174647"),("\21610\RS",String "L"),("",Bool True),("\RS",Bool False),("\120295",String "G\NUL"),("\1072466",String ""),("t\SYN",Bool False),("\1045495",Number (0.0))])}
testObject_PropertyValue_user_2 :: PropertyValue
testObject_PropertyValue_user_2 = PropertyValue {propertyValueJson = String ""}
testObject_PropertyValue_user_3 :: PropertyValue
testObject_PropertyValue_user_3 = PropertyValue {propertyValueJson = Array [Number (-100.0),Bool True,Bool True]}
testObject_PropertyValue_user_4 :: PropertyValue
testObject_PropertyValue_user_4 = PropertyValue {propertyValueJson = Object (fromList [("",Bool False),("7\DC3g",String ""),("M\ESCU",Null),("ry\GS",Null),("S2\1067241",Number (200000.0)),("\147653",Null)])}
testObject_PropertyValue_user_5 :: PropertyValue
testObject_PropertyValue_user_5 = PropertyValue {propertyValueJson = Bool False}
testObject_PropertyValue_user_6 :: PropertyValue
testObject_PropertyValue_user_6 = PropertyValue {propertyValueJson = Null}
testObject_PropertyValue_user_7 :: PropertyValue
testObject_PropertyValue_user_7 = PropertyValue {propertyValueJson = Null}
testObject_PropertyValue_user_8 :: PropertyValue
testObject_PropertyValue_user_8 = PropertyValue {propertyValueJson = Object (fromList [("",Null),("v\1111731",String ""),("\vV#",String "/\1108372o{"),("\1061248\1067625\188941\US[",Number (-40000.0)),("<Kl\a5",String "]\31794")])}
testObject_PropertyValue_user_9 :: PropertyValue
testObject_PropertyValue_user_9 = PropertyValue {propertyValueJson = Object (fromList [("$a?\fR\NUL6$^\NAK\175178;)\CANG\SYN",Number (1.7e14))])}
testObject_PropertyValue_user_10 :: PropertyValue
testObject_PropertyValue_user_10 = PropertyValue {propertyValueJson = Object (fromList [])}
testObject_PropertyValue_user_11 :: PropertyValue
testObject_PropertyValue_user_11 = PropertyValue {propertyValueJson = Array [Bool False]}
testObject_PropertyValue_user_12 :: PropertyValue
testObject_PropertyValue_user_12 = PropertyValue {propertyValueJson = Array [Null,Bool False,Bool True,String "\fK",Number (2000000.0)]}
testObject_PropertyValue_user_13 :: PropertyValue
testObject_PropertyValue_user_13 = PropertyValue {propertyValueJson = Object (fromList [])}
testObject_PropertyValue_user_14 :: PropertyValue
testObject_PropertyValue_user_14 = PropertyValue {propertyValueJson = Array [Bool False,Number (3.0e-4),Number (-0.3),Number (0.2),Number (400.0),Bool True,String "B*\\\CAN"]}
testObject_PropertyValue_user_15 :: PropertyValue
testObject_PropertyValue_user_15 = PropertyValue {propertyValueJson = Array []}
testObject_PropertyValue_user_16 :: PropertyValue
testObject_PropertyValue_user_16 = PropertyValue {propertyValueJson = Array [String "",Null,Null,String "\FS",Bool True]}
testObject_PropertyValue_user_17 :: PropertyValue
testObject_PropertyValue_user_17 = PropertyValue {propertyValueJson = Object (fromList [])}
testObject_PropertyValue_user_18 :: PropertyValue
testObject_PropertyValue_user_18 = PropertyValue {propertyValueJson = Array [Number (-1.0e-2),Number (-0.5),String "W",Number (0.0)]}
testObject_PropertyValue_user_19 :: PropertyValue
testObject_PropertyValue_user_19 = PropertyValue {propertyValueJson = String "\138730[\ETB\CANM\1029917c\63350\1003307_g\ENQ\189056[\NUL:.\1046286\CAN"}
testObject_PropertyValue_user_20 :: PropertyValue
testObject_PropertyValue_user_20 = PropertyValue {propertyValueJson = Object (fromList [])}
