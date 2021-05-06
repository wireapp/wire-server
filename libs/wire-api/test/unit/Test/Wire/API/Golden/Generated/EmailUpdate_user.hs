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
testObject_EmailUpdate_user_1 :: EmailUpdate
testObject_EmailUpdate_user_1 = EmailUpdate {euEmail = Email {emailLocal = "0\988778;\60786mM\148788\1002063\&0+\SUB", emailDomain = "VR^J*\SOH\EOT\SUBwb;-=\1022111\EM"}}
testObject_EmailUpdate_user_2 :: EmailUpdate
testObject_EmailUpdate_user_2 = EmailUpdate {euEmail = Email {emailLocal = "Xw\n!n\1083497\757\40103\&9\n\137960?S\131129\1105258", emailDomain = "+P_\SOHaik\51355\SOQ\1034293"}}
testObject_EmailUpdate_user_3 :: EmailUpdate
testObject_EmailUpdate_user_3 = EmailUpdate {euEmail = Email {emailLocal = "$\4486\66648<p<O\187751i\1099417\FSy\DC4\150961>\1000050y\987916&\ACKd\1088507\&8ZbD", emailDomain = "\1103057f\96288\1015533\1072972V\1031620\&6|\DC4>"}}
testObject_EmailUpdate_user_4 :: EmailUpdate
testObject_EmailUpdate_user_4 = EmailUpdate {euEmail = Email {emailLocal = "{D\153875Ik\999870\1090475\DC2", emailDomain = "\SO"}}
testObject_EmailUpdate_user_5 :: EmailUpdate
testObject_EmailUpdate_user_5 = EmailUpdate {euEmail = Email {emailLocal = "h=\152118\29935\993874\DC4\SOHr\78018T\f\FS^\41326-W\r8\ETB", emailDomain = "\147756<\SYN\168845\SOH0$4q\111047E',\ETXb=5\1061721\DLE\4007yH\997741\58083\1062531>"}}
