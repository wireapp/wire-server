{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConnectionRequest_user where

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
testObject_ConnectionRequest_user_1 :: ConnectionRequest
testObject_ConnectionRequest_user_1 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00006678-0000-5e5a-0000-6c18000070cd"))), crName = "B\1101058\SIO\SI\13449E\1029844?=B3\n]\166339o+.\nF)4\v,\f-av\46124", crMessage = Message {messageText = "m\1019616Nq]\ENQG=\FS'd7\1068762\&37I\24705\b`W\64217\SUB\1054457v{8\bX\172644x\SId\".T\1097817\SUBD$2\EOT\RS<\SOH="}}
testObject_ConnectionRequest_user_2 :: ConnectionRequest
testObject_ConnectionRequest_user_2 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00003d62-0000-47e3-0000-47f800003979"))), crName = "F\33945\USN6Gmn\EOTF\ETBw#\162538\1030418vx\RS,t6i\15984Q\SOH\"\18408@\1053405b", crMessage = Message {messageText = "\1021267\150766!\f\1063764>.z\GS\138456a\"8\1035890t\RSD\EOTn+|\138801\1019919\&2u\RSm\164513\1060655\ETX\ESCvI\RS\n\t\STX\f\37855=\SUB3\1041358\158709\"\9003*\bT\182606Ot3&\SOO"}}
testObject_ConnectionRequest_user_3 :: ConnectionRequest
testObject_ConnectionRequest_user_3 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00000032-0000-7a9d-0000-0a690000713f"))), crName = "\SYN*\SIk|`\177984r\97028\98091G\"\2929ATy{\DC2\1074850U\v\30044\1095123IvKHp\996066\CANW\GSg\1084807\&5p\1048441B\NUL\SI\1044861\DLEX\1040478\CANjy\EM4?$X\fmJn\1052233[\49657\SYN\146844]0n\NUL\nF\162904\ESC\as3\95218\DLEAK+\SO#s\1094629\EOTt(\SO|\149964\1034853@e\175477\bxh~\22996\15995\SYN\1110583\170226y\1081856C\STXM\20224Z\28871kI\SOHQ\163496\1034868\a~\917934W%\1049375u(\FS@?L]Gj\28988\ETXa9o\24295W>vG\16456\144943\1043459\b'\50949x\177786+\rd(5\ENQF)P%\48665\RS2\36403< <jq\DC2!VG\31576e\41442\174190", crMessage = Message {messageText = "b\SOH:3*\SI+(4S4\26867)P{\143148\1070587\155160-F\191065#+J\1112455\1036795\NUL]\1071246z\1033142\1092240\&4\1070253\ri\26896l\185674TF\CAN\CAN\38207`\1047808G9=#a\FS2-mE\SUB\144197\136322\b{\135156\1114102\ESC\180958\47926`\21556\ETB4\63831\172808Z\SI\1085086;\52035\169934\&5"}}
testObject_ConnectionRequest_user_4 :: ConnectionRequest
testObject_ConnectionRequest_user_4 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00002077-0000-3dfd-0000-24a8000013a1"))), crName = "\140456W\NUL \FSS#\156873U\DC1NZdk4wn\ESC", crMessage = Message {messageText = "\rq7\SOH`Ss\28100S\SUBr\DELq\132963\DC1r&\1110949~w\EM/\154557\SYN\SI]\ETX\t,rl^\174922\165049$\1040095\98700\1051927V\DC2h\1098435z\139322B\ETB\1072381\STX%N\NUL\52174\985271\1112729E\NAK/?l=N\SYN\134622\1047604\fF\DC2\1012009'"}}
testObject_ConnectionRequest_user_5 :: ConnectionRequest
testObject_ConnectionRequest_user_5 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00004667-0000-34d5-0000-61d800002cdf"))), crName = "8\ETB\DC3=?\ENQ?\NUL%B\1052969\NAKWe^5+O7je\27525/\1112223&\US0/\1080070\SO\NAK\GS\EM\DEL\1092402\&1\NULJ.n\17685\1022490q\1082871|\53909Z<\66795nX\US7\DC4JS8@\1002341\15514[\187942;\NULd\47843S]\n\ETB&A^\1020332\154501fg\DEL::`\US\151397w1\DC2\15502\2078:8\1011007\SYN\32869$\EMv,T\1057506\1060660\ESCe\1076078\135173\&1Tqdo3\1073976wv\997179&\1081872\GSbH\f\rO\10392 \161188\996783v2J\DC3\990626^(\1036656c\24000\8754+]]3L0-iv\152109?4@Y<\1105851\RS\1078715b/\DLE", crMessage = Message {messageText = "x{qaX\STXVSx0l\1065399c\39842\&9\37311\13870n\170341\ENQ\1053850~\ENQ\1007540{\1040167%j\1015935\1034280\1062025vv-'\49499\nZ!.\1043559a\1055804\52315\992414\150763\SI7X[\1068951\"joU\51239)o\STX<\DLE\163154J])\ETXX\\!\131705F\SYN\aF{i\1103527:\183895\CAN\DC4e7VlxME\134549R9~UWz)C\27657\5201\ESC5\1004629/\RS\EOT\DC3\DEL\45080h\STX~\1052654\993719o\ACKS\fOY\184928\127772R\SI#'\151280j%,\175000\&01\1066905 9\35790\1052576\68383\1081975\&4?\GS\190306\ACK\1026899\ETXp6Bu\ESC\1013329Y\EOTkt\41277\1098472P\STXK*\39941{\1022195\1041725\&7k\ENQ{G\1070410\v"}}
