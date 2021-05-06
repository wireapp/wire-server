{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AppName_user where

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
testObject_AppName_user_1 :: AppName
testObject_AppName_user_1 = AppName {appNameText = "\NUL\1059477\1078035_m\\\1011118(\ETBxv\24851Q\DELD,W\ACK\97531q8>[n\63755\47573nB="}
testObject_AppName_user_2 :: AppName
testObject_AppName_user_2 = AppName {appNameText = "\188426-\RS4y2^"}
testObject_AppName_user_3 :: AppName
testObject_AppName_user_3 = AppName {appNameText = "\1026299KI;\100440s\EM5)q4&w?e\178026Y\97713p\US\STX\2529\1097082\191344\57913)`"}
testObject_AppName_user_4 :: AppName
testObject_AppName_user_4 = AppName {appNameText = "p\ETX\1109067zOB\SUBH\6448;\DC1\38164l\ETXRL wL\1078980\DLE6,?2*\989073\SO\32891)"}
testObject_AppName_user_5 :: AppName
testObject_AppName_user_5 = AppName {appNameText = "V\36130K\RS\b]\b\GS\1093610\USjC\EMP\1008215\ENQ5C\1035983\12475\1016424\EOT"}
testObject_AppName_user_6 :: AppName
testObject_AppName_user_6 = AppName {appNameText = "\RS3*i\1065154\1074918s\EM'j/?\RS\DC4*!\SOH\1052389\n^\137943D"}
testObject_AppName_user_7 :: AppName
testObject_AppName_user_7 = AppName {appNameText = "wK\"\1051964\"["}
testObject_AppName_user_8 :: AppName
testObject_AppName_user_8 = AppName {appNameText = "\1070367G\99396\163590\bv\163597f\10801z0&\17445"}
testObject_AppName_user_9 :: AppName
testObject_AppName_user_9 = AppName {appNameText = "8\EMy\1004373\134646B\1013409@&6\"\ENQNtf\r\1074562'\ETX\ETB"}
testObject_AppName_user_10 :: AppName
testObject_AppName_user_10 = AppName {appNameText = "\NAK\1013022z\163708c"}
testObject_AppName_user_11 :: AppName
testObject_AppName_user_11 = AppName {appNameText = "\DC1\70718:wQ\35995[j*\\?c\37487`K"}
testObject_AppName_user_12 :: AppName
testObject_AppName_user_12 = AppName {appNameText = ""}
testObject_AppName_user_13 :: AppName
testObject_AppName_user_13 = AppName {appNameText = "\\\186848\EM\1085381aU#"}
testObject_AppName_user_14 :: AppName
testObject_AppName_user_14 = AppName {appNameText = " "}
testObject_AppName_user_15 :: AppName
testObject_AppName_user_15 = AppName {appNameText = "\STX/\986575\44086\SI\1007113\ETBPq\33410iH\51358\SOH\13025\156925-Z}Z\21309"}
testObject_AppName_user_16 :: AppName
testObject_AppName_user_16 = AppName {appNameText = ""}
testObject_AppName_user_17 :: AppName
testObject_AppName_user_17 = AppName {appNameText = ";\SOH\ETX;(S\1082022[T6B4"}
testObject_AppName_user_18 :: AppName
testObject_AppName_user_18 = AppName {appNameText = "\1077549t\1073997\STX\18028\1019297#fa\1053713$v\1045032\1099861\DC4i\FS\DC3\STXqtFr\59674\1076275"}
testObject_AppName_user_19 :: AppName
testObject_AppName_user_19 = AppName {appNameText = "\ESC\f H\188233\168176G\v\50547ei^"}
testObject_AppName_user_20 :: AppName
testObject_AppName_user_20 = AppName {appNameText = "\\m\b"}
