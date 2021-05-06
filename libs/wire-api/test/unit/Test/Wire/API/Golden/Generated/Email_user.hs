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
testObject_Email_user_1 :: Email
testObject_Email_user_1 = Email {emailLocal = "\SOHOZWiq\SOH3\SYN\1050573eR7", emailDomain = "\150363b\DLE\1027590\28275\&0ek8\EOT\EOT\145662\1074478\46521\1031047):N9\1007884S\11720S\119051o3K\RSE"}
testObject_Email_user_2 :: Email
testObject_Email_user_2 = Email {emailLocal = "\ACKKi\1054091\DELp\60832#20)j", emailDomain = "\SYN\GS\189628\1081120Q5!\6686\ESCzA"}
testObject_Email_user_3 :: Email
testObject_Email_user_3 = Email {emailLocal = "'\1038943U\1023112f1\SUB2\179812_\25572\SO\996603O\1095128r\NULG\989922p\168456\ETB", emailDomain = "=K',{M\1101328\71310\1076422\61886by\SO6\1113062\1042345\DC1\DC4kp%"}
testObject_Email_user_4 :: Email
testObject_Email_user_4 = Email {emailLocal = "\ENQ", emailDomain = "~QV\1068264%"}
testObject_Email_user_5 :: Email
testObject_Email_user_5 = Email {emailLocal = "\999636%q\1082066\FS2N\SYN\1066083g\rc{\1003212\ap", emailDomain = "\28355))\1058430\98850F"}
