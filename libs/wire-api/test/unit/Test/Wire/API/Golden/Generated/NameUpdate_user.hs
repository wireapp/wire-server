{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NameUpdate_user where

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
testObject_NameUpdate_user_1 :: NameUpdate
testObject_NameUpdate_user_1 = NameUpdate {nuHandle = "52p\SOHbeY&o_\SUB\1006116\1078961c-\990478iT\EM\1088319ghI"}
testObject_NameUpdate_user_2 :: NameUpdate
testObject_NameUpdate_user_2 = NameUpdate {nuHandle = "\1058002\&2\26241\181674\169079e\ETXJ"}
testObject_NameUpdate_user_3 :: NameUpdate
testObject_NameUpdate_user_3 = NameUpdate {nuHandle = "\EM\133290_\DC4["}
testObject_NameUpdate_user_4 :: NameUpdate
testObject_NameUpdate_user_4 = NameUpdate {nuHandle = "Ka\r\b\RSwJS\98095C:[\n{\ENQ\RS8k\16786lVE\1001876"}
testObject_NameUpdate_user_5 :: NameUpdate
testObject_NameUpdate_user_5 = NameUpdate {nuHandle = "\137798WCQ &K>\aD\STX#\\|\1021211\988930\995050\179229zqo"}
testObject_NameUpdate_user_6 :: NameUpdate
testObject_NameUpdate_user_6 = NameUpdate {nuHandle = "\DC2\63897*\\:\1110116\&9/\DLE\1023374SD\1069853\1059130`Yr\"tL\1078696"}
testObject_NameUpdate_user_7 :: NameUpdate
testObject_NameUpdate_user_7 = NameUpdate {nuHandle = "1H\60050`\77873\1016283j\1079017'\1087704+&.Uc\1036547u\RS!~\1024564\160623*r\EOT\1059344="}
testObject_NameUpdate_user_8 :: NameUpdate
testObject_NameUpdate_user_8 = NameUpdate {nuHandle = "|$P\ESC\DC3\DC3\"\1060366j5\134292\1071161\DC2\b\DC4-bH\139147\1033984m\1083692)\SYN\US\58415\141024"}
testObject_NameUpdate_user_9 :: NameUpdate
testObject_NameUpdate_user_9 = NameUpdate {nuHandle = "\FS\"zqO\45035L9\EMCaV\111250u\1099377\v!\1083806\1053297S\1021968\DC1\170880)\ACK\STXBh"}
testObject_NameUpdate_user_10 :: NameUpdate
testObject_NameUpdate_user_10 = NameUpdate {nuHandle = "!])sTU\ESCtJ\DC2/Y\CAN"}
testObject_NameUpdate_user_11 :: NameUpdate
testObject_NameUpdate_user_11 = NameUpdate {nuHandle = "\1015388\1106715\&2\159425dPS\DC3%\1109976\44403vY\132398o\1049536\EM\NUL"}
testObject_NameUpdate_user_12 :: NameUpdate
testObject_NameUpdate_user_12 = NameUpdate {nuHandle = "\NUL"}
testObject_NameUpdate_user_13 :: NameUpdate
testObject_NameUpdate_user_13 = NameUpdate {nuHandle = "*E\SO\FS\1010527\1087790\141595'{\151452s\136497"}
testObject_NameUpdate_user_14 :: NameUpdate
testObject_NameUpdate_user_14 = NameUpdate {nuHandle = "o\1056480[N\38275\1075490\149737+w\1060201MWg\FSGMK73\1006466\1054166\169396\1015023\1053835"}
testObject_NameUpdate_user_15 :: NameUpdate
testObject_NameUpdate_user_15 = NameUpdate {nuHandle = "\EOT"}
testObject_NameUpdate_user_16 :: NameUpdate
testObject_NameUpdate_user_16 = NameUpdate {nuHandle = "\1111275#R\SOH\DEL\1073605\1074073i\DC1\ETBH+y\STX\ETXP;ZX+\\\195007^\SYN@].q\v"}
testObject_NameUpdate_user_17 :: NameUpdate
testObject_NameUpdate_user_17 = NameUpdate {nuHandle = "8LU\1091557\139585\178474\984501\&4\SUBb]\989771\51287\&0Q\STX"}
testObject_NameUpdate_user_18 :: NameUpdate
testObject_NameUpdate_user_18 = NameUpdate {nuHandle = ""}
testObject_NameUpdate_user_19 :: NameUpdate
testObject_NameUpdate_user_19 = NameUpdate {nuHandle = "\RS\v\1071352Y\a\26610;ll\ESCl4\ETX\1078990v\a[5f;1\4492\180079"}
testObject_NameUpdate_user_20 :: NameUpdate
testObject_NameUpdate_user_20 = NameUpdate {nuHandle = "\SUBC\RS\18510J\165011#\EM\97717\1104181\&5EE"}
