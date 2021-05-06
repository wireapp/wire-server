{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginCode_user where

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
testObject_LoginCode_user_1 :: LoginCode
testObject_LoginCode_user_1 = LoginCode {fromLoginCode = "\DLE-tA%\987403\nlr%\ETXZx@\1013860\tC\1098908\ETBM|z\1098552B"}
testObject_LoginCode_user_2 :: LoginCode
testObject_LoginCode_user_2 = LoginCode {fromLoginCode = "\1019478\FSu\1105017Y\1080099&4W\SO\\+\v\83189q\1051064"}
testObject_LoginCode_user_3 :: LoginCode
testObject_LoginCode_user_3 = LoginCode {fromLoginCode = ")b\DC2 \101086S\94408\&7(MH\NUL\t,j\1074917C"}
testObject_LoginCode_user_4 :: LoginCode
testObject_LoginCode_user_4 = LoginCode {fromLoginCode = "\1029863m{\nh}\ENQ\\A\fc2"}
testObject_LoginCode_user_5 :: LoginCode
testObject_LoginCode_user_5 = LoginCode {fromLoginCode = "\NUL2 "}
testObject_LoginCode_user_6 :: LoginCode
testObject_LoginCode_user_6 = LoginCode {fromLoginCode = "|}"}
testObject_LoginCode_user_7 :: LoginCode
testObject_LoginCode_user_7 = LoginCode {fromLoginCode = "\RS/\1111140\157596"}
testObject_LoginCode_user_8 :: LoginCode
testObject_LoginCode_user_8 = LoginCode {fromLoginCode = "\DC1\v\54473sQUPI\1109717{V\151340\101048\ENQ(\1097070{\1070624l\41730\998631~\167760"}
testObject_LoginCode_user_9 :: LoginCode
testObject_LoginCode_user_9 = LoginCode {fromLoginCode = ""}
testObject_LoginCode_user_10 :: LoginCode
testObject_LoginCode_user_10 = LoginCode {fromLoginCode = "$\DC4>pC\1085512\70183wb\188966\ENQ\1047366Z"}
testObject_LoginCode_user_11 :: LoginCode
testObject_LoginCode_user_11 = LoginCode {fromLoginCode = "\t\162503/C|\1045044\989835\33981G\1073305L]|<R`"}
testObject_LoginCode_user_12 :: LoginCode
testObject_LoginCode_user_12 = LoginCode {fromLoginCode = "r\GSu\ESCr"}
testObject_LoginCode_user_13 :: LoginCode
testObject_LoginCode_user_13 = LoginCode {fromLoginCode = "R5%3\DLE@h\SYN\1022613\57773\EOT\\\1016949\RS"}
testObject_LoginCode_user_14 :: LoginCode
testObject_LoginCode_user_14 = LoginCode {fromLoginCode = "\NAK\58859\159933\DLE\CANiX$\US#\36312C8J\GS([\ENQ\v"}
testObject_LoginCode_user_15 :: LoginCode
testObject_LoginCode_user_15 = LoginCode {fromLoginCode = "\41162qFIh<\1018373\1064702q\SO\166601p<M;,"}
testObject_LoginCode_user_16 :: LoginCode
testObject_LoginCode_user_16 = LoginCode {fromLoginCode = "1b*\\\NUL1#\20252qB$\1071880F\18698\167847{^\ETBh\17511\990059uZk\997258g\r\1064194\1016283"}
testObject_LoginCode_user_17 :: LoginCode
testObject_LoginCode_user_17 = LoginCode {fromLoginCode = "\19655K\1012985\164444\\\189801Bt\USd<\145628"}
testObject_LoginCode_user_18 :: LoginCode
testObject_LoginCode_user_18 = LoginCode {fromLoginCode = "cX\DC2\SOD\139794a}2*\1040898"}
testObject_LoginCode_user_19 :: LoginCode
testObject_LoginCode_user_19 = LoginCode {fromLoginCode = "\DC3\1063173"}
testObject_LoginCode_user_20 :: LoginCode
testObject_LoginCode_user_20 = LoginCode {fromLoginCode = "\39666\986327\1015962@@K6\DC3f\126228h\ESCE\14559hs\ESC@\55063+\NAK\DC3\984867$9qB4%"}
