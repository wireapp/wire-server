{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Email_user where

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
testObject_Email_1 :: Email
testObject_Email_1 = Email {emailLocal = "\US\1013181]\1101057=\"P8\1054488\&9\SYN\ETB{\991604\\$EGA\1005926'v\DC1-", emailDomain = "%"}
testObject_Email_2 :: Email
testObject_Email_2 = Email {emailLocal = "LN", emailDomain = "N\1066271A\DLE;\CAN\DC2e\NUL[a/\f?#9J\DC1\22985I\bSm']^[3\133860"}
testObject_Email_3 :: Email
testObject_Email_3 = Email {emailLocal = "\1101165\128447,\\r\a\1078894\1065984", emailDomain = "\SYN\SYN\ESC7\1034747l\97389l\SO\ENQ\60024*\SUB\DC3^r%78"}
testObject_Email_4 :: Email
testObject_Email_4 = Email {emailLocal = "Y\10382f\1016083\&0\42031\132785%\b\97904\DEL\f\170922e;-\1063833\a\1094546\136306", emailDomain = "vI\1027172t\SUB\DC4\SO.AvPHU,Bx[\SO0"}
testObject_Email_5 :: Email
testObject_Email_5 = Email {emailLocal = "\989161\ETX\FS^\aCS\1046892J\131202\987775]\STX-(\DC4\1094576\ACK%G\\", emailDomain = "7`\23289\1055082\n9\fD\ETB#\1016886.\1009064\n\1056155\DC4\1061000C5L\1055228\DC4k\b?L>"}
