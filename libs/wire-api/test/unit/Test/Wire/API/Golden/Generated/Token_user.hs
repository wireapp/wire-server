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
testObject_Token_user_1 = Token {tokenText = "|Sl\19323]\\\1015655\&3\1014877\1060415K]\177835\SIL\STX\GS\SUB3"}
testObject_Token_user_2 :: Token
testObject_Token_user_2 = Token {tokenText = "\991015g"}
testObject_Token_user_3 :: Token
testObject_Token_user_3 = Token {tokenText = "\r)3q0t'\1054979,5\63366"}
testObject_Token_user_4 :: Token
testObject_Token_user_4 = Token {tokenText = "\DC1\1018108\1076921\a"}
testObject_Token_user_5 :: Token
testObject_Token_user_5 = Token {tokenText = "* 0\984903Ui\DC2K\24525q\USVq\ESC\DC32\178475G\1111875\36504\&5Q3\27724\ETB"}
testObject_Token_user_6 :: Token
testObject_Token_user_6 = Token {tokenText = "L\1020870\23398\10925+ds2\99450@\51441\986061\13648y \SYN\150148"}
testObject_Token_user_7 :: Token
testObject_Token_user_7 = Token {tokenText = ""}
testObject_Token_user_8 :: Token
testObject_Token_user_8 = Token {tokenText = "i\1080763\DC3:9\1038488\1111301nC{\v>\186326\1042436\DLEN*\164507\1023529\DLE8q"}
testObject_Token_user_9 :: Token
testObject_Token_user_9 = Token {tokenText = "\DELa\986683\DLE"}
testObject_Token_user_10 :: Token
testObject_Token_user_10 = Token {tokenText = "o7.Xb\983262"}
testObject_Token_user_11 :: Token
testObject_Token_user_11 = Token {tokenText = "es(\1052630["}
testObject_Token_user_12 :: Token
testObject_Token_user_12 = Token {tokenText = "l\167173\&1`6\997869\1033602\USM\SOH*\1044450\f\ENQ\151032\994662/1j7=\SO\ETX`t(E\1007934\DC2\145509"}
testObject_Token_user_13 :: Token
testObject_Token_user_13 = Token {tokenText = "\\\rles\151179>\1100310\SOH\1058269\1076255T=\1056929\RS\1086090\71704\&5vm\ETX"}
testObject_Token_user_14 :: Token
testObject_Token_user_14 = Token {tokenText = "\1061237Q\ENQ\177377>C\53933\&2@`\NAK\1594}\ETB\164291Ln\a\167155\174346\41699\1056876c^\1094633"}
testObject_Token_user_15 :: Token
testObject_Token_user_15 = Token {tokenText = "{"}
testObject_Token_user_16 :: Token
testObject_Token_user_16 = Token {tokenText = "X"}
testObject_Token_user_17 :: Token
testObject_Token_user_17 = Token {tokenText = "}\ESC\DC3D!\66281\1008727L[j\DC2(6f\STX\181148\51335\t\10208\42282\&0"}
testObject_Token_user_18 :: Token
testObject_Token_user_18 = Token {tokenText = "w\EM\148798"}
testObject_Token_user_19 :: Token
testObject_Token_user_19 = Token {tokenText = "y"}
testObject_Token_user_20 :: Token
testObject_Token_user_20 = Token {tokenText = "\CAN\EOT\ACKy(\161903nT+Q\DC2}%\51380\f\127092\1016092\23135\170544"}
