{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdentity_user where

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
testObject_UserIdentity_user_1 :: UserIdentity
testObject_UserIdentity_user_1 = FullIdentity (Email {emailLocal = "\1074740\ry\40006p", emailDomain = "/H\47281}\52835\1033879\1085873Lj'\60903\23367\&9"}) (Phone {fromPhone = "+4135254508"})
testObject_UserIdentity_user_2 :: UserIdentity
testObject_UserIdentity_user_2 = SSOIdentity (UserSSOId "a\NAK-\49643" "") (Just (Email {emailLocal = "e\69850kN(\DC4oo", emailDomain = "\1015785TU\1064770n\36845W\SI)#"})) (Just (Phone {fromPhone = "+86257660557"}))
testObject_UserIdentity_user_3 :: UserIdentity
testObject_UserIdentity_user_3 = PhoneIdentity (Phone {fromPhone = "+028856158713165"})
testObject_UserIdentity_user_4 :: UserIdentity
testObject_UserIdentity_user_4 = FullIdentity (Email {emailLocal = "m>\8828\140797kG", emailDomain = "\52097\1052452\153400\DLEr\1048721\1034705\1010570\t&"}) (Phone {fromPhone = "+8391251089"})
testObject_UserIdentity_user_5 :: UserIdentity
testObject_UserIdentity_user_5 = SSOIdentity (UserScimExternalId "3\997051\999536d") (Just (Email {emailLocal = "C1c\\7\EOT", emailDomain = "\33179"})) (Just (Phone {fromPhone = "+77057407896139"}))
testObject_UserIdentity_user_6 :: UserIdentity
testObject_UserIdentity_user_6 = PhoneIdentity (Phone {fromPhone = "+43670072799552"})
testObject_UserIdentity_user_7 :: UserIdentity
testObject_UserIdentity_user_7 = EmailIdentity (Email {emailLocal = " \t\156976\191161$\STX=\111200\&6\DLE?N\187132\NUL~\vW\ETBd\"QK\SUB", emailDomain = "\GS"})
testObject_UserIdentity_user_8 :: UserIdentity
testObject_UserIdentity_user_8 = EmailIdentity (Email {emailLocal = "!\1087696v\13936\&9\\PI\58342g?\NAK9\EM\179228\1092696$", emailDomain = "\1043251\SI\1028302r%Q["})
testObject_UserIdentity_user_9 :: UserIdentity
testObject_UserIdentity_user_9 = EmailIdentity (Email {emailLocal = "C#?\n\1081984\1048481AwB:\100395\DC11C\n-~\NUL\187005\RS{m$k\172422", emailDomain = "\SYN\EM\EMD{\\2\SI#/]dH"})
testObject_UserIdentity_user_10 :: UserIdentity
testObject_UserIdentity_user_10 = PhoneIdentity (Phone {fromPhone = "+8254569277"})
testObject_UserIdentity_user_11 :: UserIdentity
testObject_UserIdentity_user_11 = FullIdentity (Email {emailLocal = "5OcM\1043166", emailDomain = "p\100098~"}) (Phone {fromPhone = "+8487926424"})
testObject_UserIdentity_user_12 :: UserIdentity
testObject_UserIdentity_user_12 = SSOIdentity (UserSSOId "" "-") Nothing (Just (Phone {fromPhone = "+72882586804847"}))
testObject_UserIdentity_user_13 :: UserIdentity
testObject_UserIdentity_user_13 = SSOIdentity (UserScimExternalId "\35470\&71[S|\FS\1036302\DC1") (Just (Email {emailLocal = "\183227(\1008585\&7\SI\1072672", emailDomain = "\185144]\58109hb"})) (Just (Phone {fromPhone = "+57526763"}))
testObject_UserIdentity_user_14 :: UserIdentity
testObject_UserIdentity_user_14 = SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "R;\a\181052\GS", emailDomain = "wh\ETX9*W=G%"})) (Just (Phone {fromPhone = "+93619187"}))
testObject_UserIdentity_user_15 :: UserIdentity
testObject_UserIdentity_user_15 = EmailIdentity (Email {emailLocal = "\1083534kO\148790U=\70743\aW\1091059'\997308\175631\1030045\ETXtY\1006990\1091071\&3", emailDomain = "\GSIK\"z\aw#0\ENQ"})
testObject_UserIdentity_user_16 :: UserIdentity
testObject_UserIdentity_user_16 = SSOIdentity (UserSSOId "\31196\83397<\1033545!" "") Nothing (Just (Phone {fromPhone = "+93536314164"}))
testObject_UserIdentity_user_17 :: UserIdentity
testObject_UserIdentity_user_17 = EmailIdentity (Email {emailLocal = "\18623\27702Y\SYN\993610w\FS< \100610L\t78fA\67109\CAN<B\154117\157511#,", emailDomain = "\175599(}o\1040183\50883kfP\DC4\SOH+|\175292"})
testObject_UserIdentity_user_18 :: UserIdentity
testObject_UserIdentity_user_18 = EmailIdentity (Email {emailLocal = "\140753by\987573w\EOT\STXu_0no", emailDomain = "\DC4\1088149\1002670\1070666Z\1002964#K"})
testObject_UserIdentity_user_19 :: UserIdentity
testObject_UserIdentity_user_19 = EmailIdentity (Email {emailLocal = ";\186170\GS2\1030574_KZ\1036477.\1017234\1052787\&6\1006290\SUB\EOT54\42429t\ENQ~[/", emailDomain = "\r\\{7j8+do5\RS\140255}kF\t\NUL%\\\ENQ"})
testObject_UserIdentity_user_20 :: UserIdentity
testObject_UserIdentity_user_20 = FullIdentity (Email {emailLocal = "\ESCkEGK\r\1086311\ETX\83354\1111320\ETB", emailDomain = "\"\DLE"}) (Phone {fromPhone = "+0619132053613"})
