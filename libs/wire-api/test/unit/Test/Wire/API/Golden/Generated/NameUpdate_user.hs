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
testObject_NameUpdate_user_1 = NameUpdate {nuHandle = "\DEL\1088950\&6N\171437<c\181070PE\999291\155111\159548p6\165834A\1020420\SYN"}
testObject_NameUpdate_user_2 :: NameUpdate
testObject_NameUpdate_user_2 = NameUpdate {nuHandle = ".\1102232\60861\153625\ETXb<4\SO\1086978P\149425_"}
testObject_NameUpdate_user_3 :: NameUpdate
testObject_NameUpdate_user_3 = NameUpdate {nuHandle = "\1101580Px\1108144\NAK#\ACK\SOH"}
testObject_NameUpdate_user_4 :: NameUpdate
testObject_NameUpdate_user_4 = NameUpdate {nuHandle = "\1094583\1003485\&8 |\147126g\1014192&\a\1092562~"}
testObject_NameUpdate_user_5 :: NameUpdate
testObject_NameUpdate_user_5 = NameUpdate {nuHandle = "@2j\r}\v\1061913\139041@\DC1\39900\DC4\25287\EM\raA"}
testObject_NameUpdate_user_6 :: NameUpdate
testObject_NameUpdate_user_6 = NameUpdate {nuHandle = "f\140885Fm}TEc\1686\168490\1045934Zo\CANl\163770\1028449\US"}
testObject_NameUpdate_user_7 :: NameUpdate
testObject_NameUpdate_user_7 = NameUpdate {nuHandle = "r`\SYN\41715A\1077991\&5\1029807v$U#\SYN\65465\1085140Y\SO'\ETB-"}
testObject_NameUpdate_user_8 :: NameUpdate
testObject_NameUpdate_user_8 = NameUpdate {nuHandle = "0=l6\7241a\DC1qVD\SYN\GS-%\18945'G\ACK"}
testObject_NameUpdate_user_9 :: NameUpdate
testObject_NameUpdate_user_9 = NameUpdate {nuHandle = "\4876d3MHA\137111\CAN\1051814\1078637k\DELN\144009w\ESC1M\1113309\176750"}
testObject_NameUpdate_user_10 :: NameUpdate
testObject_NameUpdate_user_10 = NameUpdate {nuHandle = "\1057111U\63386\158557ei\ETX\EOTl\1092485\20726"}
testObject_NameUpdate_user_11 :: NameUpdate
testObject_NameUpdate_user_11 = NameUpdate {nuHandle = "7\ACKk\27522\n\FS\v\96565\128040#\SYN\136849\1086133\1023124\&2*zz2\a?&\1105833"}
testObject_NameUpdate_user_12 :: NameUpdate
testObject_NameUpdate_user_12 = NameUpdate {nuHandle = "\1014329"}
testObject_NameUpdate_user_13 :: NameUpdate
testObject_NameUpdate_user_13 = NameUpdate {nuHandle = "]\36516\&2z+\127065YX)yK\SYN]\1103467\1001445\1045826n"}
testObject_NameUpdate_user_14 :: NameUpdate
testObject_NameUpdate_user_14 = NameUpdate {nuHandle = "\\\23301~F\DC3\1082133,u-GfR%"}
testObject_NameUpdate_user_15 :: NameUpdate
testObject_NameUpdate_user_15 = NameUpdate {nuHandle = "\1069454-\NAK\1023766\DLEL"}
testObject_NameUpdate_user_16 :: NameUpdate
testObject_NameUpdate_user_16 = NameUpdate {nuHandle = "\26196\&6tYR\NUL\30714\CANf\1053301\ENQ$CFX"}
testObject_NameUpdate_user_17 :: NameUpdate
testObject_NameUpdate_user_17 = NameUpdate {nuHandle = "\EM\DEL1Q\EM\1096302QeQ\aDdZUgDO1\r\27404\EOTH\994220\182612\DC14\v!\ETXJ"}
testObject_NameUpdate_user_18 :: NameUpdate
testObject_NameUpdate_user_18 = NameUpdate {nuHandle = ""}
testObject_NameUpdate_user_19 :: NameUpdate
testObject_NameUpdate_user_19 = NameUpdate {nuHandle = "\DC20\DC1|TZ.6,\1058094|\1110548%\NAK\GSZ*s3\1050970*j\1086472\31632\NAK\nW'|"}
testObject_NameUpdate_user_20 :: NameUpdate
testObject_NameUpdate_user_20 = NameUpdate {nuHandle = "P\134199?\137560\127789\987308>vr\1073377\SO\EMz\1051670"}
