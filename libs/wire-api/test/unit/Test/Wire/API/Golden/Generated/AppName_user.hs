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
testObject_AppName_user_1 = AppName {appNameText = "\126118K\b"}
testObject_AppName_user_2 :: AppName
testObject_AppName_user_2 = AppName {appNameText = "\b\GS\5304\a\120560"}
testObject_AppName_user_3 :: AppName
testObject_AppName_user_3 = AppName {appNameText = "C\96949]\10247n\1050446"}
testObject_AppName_user_4 :: AppName
testObject_AppName_user_4 = AppName {appNameText = "\t\99256\n"}
testObject_AppName_user_5 :: AppName
testObject_AppName_user_5 = AppName {appNameText = "+\1112361j\1007660\1024639\EOT!\137216@\NUL"}
testObject_AppName_user_6 :: AppName
testObject_AppName_user_6 = AppName {appNameText = "\120037\ETX|xg\f\DEL\f\57871"}
testObject_AppName_user_7 :: AppName
testObject_AppName_user_7 = AppName {appNameText = "'X#\14663\1000208E\1027868\SOHt\40886\1058613-D\986466\DC3\121152\ESC3-B"}
testObject_AppName_user_8 :: AppName
testObject_AppName_user_8 = AppName {appNameText = "U\95262wJ\ESChTjWO\1106742RJ^X\1078235\ENQ\18046"}
testObject_AppName_user_9 :: AppName
testObject_AppName_user_9 = AppName {appNameText = "\v\vd,B\FS72@U!\25119l\150375"}
testObject_AppName_user_10 :: AppName
testObject_AppName_user_10 = AppName {appNameText = "'\132023\1064173\SYN\154700s\1016421@PT\SO0\\m\FS`*\1017898\EM\SIk\1028502Q\FS\r\EOT\1082459@:"}
testObject_AppName_user_11 :: AppName
testObject_AppName_user_11 = AppName {appNameText = "A9|\FS\145478&\SI\vVy\74066\1113896\4097\30631\175591\USQ\1012195%*\95792\RS\"\48263zv\STX69"}
testObject_AppName_user_12 :: AppName
testObject_AppName_user_12 = AppName {appNameText = "\95719X\137353O\1047725?&\30422,\1022905,U\CAN[\120618vkXP72"}
testObject_AppName_user_13 :: AppName
testObject_AppName_user_13 = AppName {appNameText = "\1099321\FSeW\1067373y"}
testObject_AppName_user_14 :: AppName
testObject_AppName_user_14 = AppName {appNameText = "+\135378\t\1098139=\990453)\1098557,\1020590I\189054\177600E\STX0\GSb\SIE\1052793}\24397:\ENQ\EOT"}
testObject_AppName_user_15 :: AppName
testObject_AppName_user_15 = AppName {appNameText = "\1005834\&57/\SOH\58753`\3940l"}
testObject_AppName_user_16 :: AppName
testObject_AppName_user_16 = AppName {appNameText = "\1008416\22766M\49196\FSg%\71425h\151165%\DC36?N\"\1066733\ETB\\\EM|Ud"}
testObject_AppName_user_17 :: AppName
testObject_AppName_user_17 = AppName {appNameText = "2\1087553{__\174599\NAK \50441\183412N\1083143\153057c\ESC\1058631<\168841\992583^\US\1071359\SIfC\168723\1790"}
testObject_AppName_user_18 :: AppName
testObject_AppName_user_18 = AppName {appNameText = "\EOT\39385\1073415m?"}
testObject_AppName_user_19 :: AppName
testObject_AppName_user_19 = AppName {appNameText = "1\146986h)\7975\1057591u\1077943\DC2<\1044282\993299\155433\147301x"}
testObject_AppName_user_20 :: AppName
testObject_AppName_user_20 = AppName {appNameText = "\SUB\SOH\1109373a\CAN"}
