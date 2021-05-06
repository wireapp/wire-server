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
testObject_AppName_user_1 = AppName {appNameText = ""}
testObject_AppName_user_2 :: AppName
testObject_AppName_user_2 = AppName {appNameText = "\35760p"}
testObject_AppName_user_3 :: AppName
testObject_AppName_user_3 = AppName {appNameText = "\ACK&\"\EOT\f?8\DC3J\DEL "}
testObject_AppName_user_4 :: AppName
testObject_AppName_user_4 = AppName {appNameText = "\1051633Nh\1083407y1\NAK\ENQ\60734\54385M\119254\32278\989737jq-\ai\1106948JW\ESC\43043\1067643"}
testObject_AppName_user_5 :: AppName
testObject_AppName_user_5 = AppName {appNameText = "\b'\145521\1060934\&6\NAKX&`z7)H\171429j\CANYM\SI3/Z#d)\1015194\178656X"}
testObject_AppName_user_6 :: AppName
testObject_AppName_user_6 = AppName {appNameText = "\RS\a\194753\58840\34932\SOb\1054040\ENQ\1031907\&8C\168132jM\997591"}
testObject_AppName_user_7 :: AppName
testObject_AppName_user_7 = AppName {appNameText = "\SOH\DC1ZI$\137912\RS\ETBwJ}(\a\n\1015865"}
testObject_AppName_user_8 :: AppName
testObject_AppName_user_8 = AppName {appNameText = "}\1107646!'\RS2\ESC\1022089!\DELk\a"}
testObject_AppName_user_9 :: AppName
testObject_AppName_user_9 = AppName {appNameText = "B\SI&}\DC2\92663T6s\FS"}
testObject_AppName_user_10 :: AppName
testObject_AppName_user_10 = AppName {appNameText = "5\SYNw\50868\1025600b{\147343Oqv9\ESC\1059544"}
testObject_AppName_user_11 :: AppName
testObject_AppName_user_11 = AppName {appNameText = "\159331c0(\51218}^\1021053\CANO;\122922{\1047456=?\134801\58447%\SUB\STX_\36455\1055769n"}
testObject_AppName_user_12 :: AppName
testObject_AppName_user_12 = AppName {appNameText = "9\b\15150\DC1\1034943\169697\1006304>N/^u\SUB\14339g\CAN\"\1066076\"\172607\5012\"\983217\\\135957kj\1062045_Z"}
testObject_AppName_user_13 :: AppName
testObject_AppName_user_13 = AppName {appNameText = "\GS\181661\ENQ\1036384d` b\ETX/&\161427+z\61681/rKx|J{\140013=H"}
testObject_AppName_user_14 :: AppName
testObject_AppName_user_14 = AppName {appNameText = "T0L`\992767\&7_M\73766\1009733\985960\1070361#dg\GS\1082896G\182577|\1068878#\1028747O\1027116\997924\1298"}
testObject_AppName_user_15 :: AppName
testObject_AppName_user_15 = AppName {appNameText = "Q] k\DC4\t}1=\1033145\38805\"\997376\98278\&8f"}
testObject_AppName_user_16 :: AppName
testObject_AppName_user_16 = AppName {appNameText = "\52467"}
testObject_AppName_user_17 :: AppName
testObject_AppName_user_17 = AppName {appNameText = "\173663\ESC5\1046424&"}
testObject_AppName_user_18 :: AppName
testObject_AppName_user_18 = AppName {appNameText = "\188153\72855"}
testObject_AppName_user_19 :: AppName
testObject_AppName_user_19 = AppName {appNameText = "?"}
testObject_AppName_user_20 :: AppName
testObject_AppName_user_20 = AppName {appNameText = "dS\FS\SUB\1024704T\DC4\1084627\15114j+MaA\98497k\ENQ{\74502h%e"}
