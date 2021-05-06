{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Token_user where

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
testObject_Token_user_1 :: Token
testObject_Token_user_1 = Token {tokenText = "\35990\1040177-\1092475]uO"}
testObject_Token_user_2 :: Token
testObject_Token_user_2 = Token {tokenText = "F\1072723\STX\DEL\RSI\NAK\ACK\1090818\&6l\b&h\999858\1105167:\ETB"}
testObject_Token_user_3 :: Token
testObject_Token_user_3 = Token {tokenText = "\EM\1075744UjzHfUF\t5F\1090116\1086968\&2\182255\NAK*|"}
testObject_Token_user_4 :: Token
testObject_Token_user_4 = Token {tokenText = ""}
testObject_Token_user_5 :: Token
testObject_Token_user_5 = Token {tokenText = "ukF(\EM\129499{d6\179769\ETB\74393\153432"}
testObject_Token_user_6 :: Token
testObject_Token_user_6 = Token {tokenText = "\SI\32951D"}
testObject_Token_user_7 :: Token
testObject_Token_user_7 = Token {tokenText = "\1011389\SUB\ACK"}
testObject_Token_user_8 :: Token
testObject_Token_user_8 = Token {tokenText = "$?`|l17\59178"}
testObject_Token_user_9 :: Token
testObject_Token_user_9 = Token {tokenText = "4\ACK=u_Q"}
testObject_Token_user_10 :: Token
testObject_Token_user_10 = Token {tokenText = "\1095029+:m@\175408\ACK\EOTqY\f"}
testObject_Token_user_11 :: Token
testObject_Token_user_11 = Token {tokenText = "4eoO>\39902m(4j(pC\78265F\1112720\&7\r-\52752X9\a\ESC9\DC4M\139409d"}
testObject_Token_user_12 :: Token
testObject_Token_user_12 = Token {tokenText = "kx\DC2\DC4\1052080\DC4\991486\1006749\v\135811\GS\SOH\1072463'%=\1006337Q\99261BN\SO\1051063"}
testObject_Token_user_13 :: Token
testObject_Token_user_13 = Token {tokenText = "\v\38900,\DC1\135021\SO&\65915uA7SGc\3343\ETXZ#"}
testObject_Token_user_14 :: Token
testObject_Token_user_14 = Token {tokenText = "\1107781+D\1075456\94667,r\1102346En\159810e\1056695x\ESC"}
testObject_Token_user_15 :: Token
testObject_Token_user_15 = Token {tokenText = "\1032639\\\GSatX\1019753vD)uMo7?\1087674\140408\DEL\12317"}
testObject_Token_user_16 :: Token
testObject_Token_user_16 = Token {tokenText = "\ETX\144815\1094636;?\SUB\1017024q\b\1044600sO5Et0h+\28026qF$\35759\44627\148265"}
testObject_Token_user_17 :: Token
testObject_Token_user_17 = Token {tokenText = "\990664d{pl\99986\&8\1086152\&3"}
testObject_Token_user_18 :: Token
testObject_Token_user_18 = Token {tokenText = "\128509\aaMl\8371+\1096613i\DC3\v+\995639\23359ND1\1101410A 3\bcH\5811\1042842\EOT\985365\DC1Q"}
testObject_Token_user_19 :: Token
testObject_Token_user_19 = Token {tokenText = "\1104395\150065\176802C\ENQv\1107925\1078038F\NUL%y\ETX^\1085709\78616\NUL@\143707\SI\186288\1087514\CAN\40629\ACK(\1024873"}
testObject_Token_user_20 :: Token
testObject_Token_user_20 = Token {tokenText = "J<\STXL\1043056\179513\SIe"}
