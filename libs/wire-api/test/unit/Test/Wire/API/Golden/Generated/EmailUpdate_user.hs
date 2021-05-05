{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_user where

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
testObject_EmailUpdate_1 :: EmailUpdate
testObject_EmailUpdate_1 = EmailUpdate {euEmail = Email {emailLocal = "EOvB\SOU\53185(mdQ\92409\GS'D m\47472 ", emailDomain = "\NUL\1073647d\997085\SYN\SUB\71680\NAK\994059\1040857\SUB\137155Q\1001578\ENQ"}}
testObject_EmailUpdate_2 :: EmailUpdate
testObject_EmailUpdate_2 = EmailUpdate {euEmail = Email {emailLocal = "6)\143636\&7\21016\16301N,1\172323\1094861\33295Q0", emailDomain = "\983645\DC2\"n,6"}}
testObject_EmailUpdate_3 :: EmailUpdate
testObject_EmailUpdate_3 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "\1111076\1056806B\1035408:^\SUB\1009346\1011531C\1048646Q\DC21A\15159({"}}
testObject_EmailUpdate_4 :: EmailUpdate
testObject_EmailUpdate_4 = EmailUpdate {euEmail = Email {emailLocal = "K(\39334\5855+\US\ENQV\19005r8\78163\EOT", emailDomain = "\ENQ!"}}
testObject_EmailUpdate_5 :: EmailUpdate
testObject_EmailUpdate_5 = EmailUpdate {euEmail = Email {emailLocal = "\ENQH}\1025465\&1\CAN/\SIFj\152592\1108550\1005338\\\f", emailDomain = ",\DLE\83149\&0\SUB\b\178571\1004645[\26282\SUBPIxd\1037763,:\NAK\388\168194P\164060\v\"UWoE|"}}
