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
testObject_PropertyValue_user_1 = PropertyValue {propertyValueJson = Object (fromList [("",String "Y\53360\1068633\14821w#\RSF_N\\"),("\n\59195fB\1054152\&2W",Bool False)])}
testObject_PropertyValue_user_2 :: PropertyValue
testObject_PropertyValue_user_2 = PropertyValue {propertyValueJson = Object (fromList [("\NUL",Number (0.2)),("",String "M\DEL"),("\1047731J",Bool False),("\\\EOT",Null),("\1037144",Number (10.0)),("F",Bool False),("}\48152",Null)])}
testObject_PropertyValue_user_3 :: PropertyValue
testObject_PropertyValue_user_3 = PropertyValue {propertyValueJson = Array [Number (-200.0),Bool True,Null,String "",String "w\72394",Null,Null,Number (0.0),Bool True,Number (0.0),Number (200.0)]}
testObject_PropertyValue_user_4 :: PropertyValue
testObject_PropertyValue_user_4 = PropertyValue {propertyValueJson = Array [Bool False,Bool True,String "\NAK\1083707\125250\1084990\134492"]}
testObject_PropertyValue_user_5 :: PropertyValue
testObject_PropertyValue_user_5 = PropertyValue {propertyValueJson = Object (fromList [("~\1033699 ",Null),("U\NUL",Null),("",String "#\ETB\989818a"),("\74000\1043775Pj",String ""),("jx",Null),("\DLE9X",Null)])}
testObject_PropertyValue_user_6 :: PropertyValue
testObject_PropertyValue_user_6 = PropertyValue {propertyValueJson = Object (fromList [("\\",Number (0.0)),("\169389",String ""),("",Number (-1.0)),("\1112351",Bool False),("\131978",Number (0.1)),(">",Null),("_",Number (1.0))])}
testObject_PropertyValue_user_7 :: PropertyValue
testObject_PropertyValue_user_7 = PropertyValue {propertyValueJson = Object (fromList [("",Number (1.0e-8)),(" ]`\17963Lz\1082117\1103233O",Number (9.0e-7)),("#5<\19547\GS",String "\141078\1082166\1093281\ra\1007349\DC2?")])}
testObject_PropertyValue_user_8 :: PropertyValue
testObject_PropertyValue_user_8 = PropertyValue {propertyValueJson = Object (fromList [])}
testObject_PropertyValue_user_9 :: PropertyValue
testObject_PropertyValue_user_9 = PropertyValue {propertyValueJson = Array [Null,Bool False,Number (-100000.0)]}
testObject_PropertyValue_user_10 :: PropertyValue
testObject_PropertyValue_user_10 = PropertyValue {propertyValueJson = Array [String "X\1008775_\119908\\\CAN\ETXN8\1012448\SO\FSE"]}
testObject_PropertyValue_user_11 :: PropertyValue
testObject_PropertyValue_user_11 = PropertyValue {propertyValueJson = Array [Number (6.0e8),Null]}
testObject_PropertyValue_user_12 :: PropertyValue
testObject_PropertyValue_user_12 = PropertyValue {propertyValueJson = String "f\r.{^\EOTE0Q\r%\\/*\6953D\993868\1109220\NAK\r\1003817\&8~U\49508R*\SI"}
testObject_PropertyValue_user_13 :: PropertyValue
testObject_PropertyValue_user_13 = PropertyValue {propertyValueJson = Bool True}
testObject_PropertyValue_user_14 :: PropertyValue
testObject_PropertyValue_user_14 = PropertyValue {propertyValueJson = Object (fromList [])}
testObject_PropertyValue_user_15 :: PropertyValue
testObject_PropertyValue_user_15 = PropertyValue {propertyValueJson = Object (fromList [("(e\"\178857%H_U\120069\DC3l\t\1077537\59725+",Bool False)])}
testObject_PropertyValue_user_16 :: PropertyValue
testObject_PropertyValue_user_16 = PropertyValue {propertyValueJson = Array []}
testObject_PropertyValue_user_17 :: PropertyValue
testObject_PropertyValue_user_17 = PropertyValue {propertyValueJson = Array [String "o",String "",String "\n",Null,String "\26498",String "",Null,Number (0.1),Number (-0.1),String "h",Bool False,String "",String "",Number (1.0),Number (0.0),Bool True,Number (0.0),Null,Null,Bool False,Bool True,Bool False,Number (10.0)]}
testObject_PropertyValue_user_18 :: PropertyValue
testObject_PropertyValue_user_18 = PropertyValue {propertyValueJson = String "H\ETXL\adK"}
testObject_PropertyValue_user_19 :: PropertyValue
testObject_PropertyValue_user_19 = PropertyValue {propertyValueJson = String "\SOH\EOT`A9\1004139_\78013G\STX}\174640o%T"}
testObject_PropertyValue_user_20 :: PropertyValue
testObject_PropertyValue_user_20 = PropertyValue {propertyValueJson = Bool True}
