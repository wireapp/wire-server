{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginId_user where

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
testObject_LoginId_user_1 :: LoginId
testObject_LoginId_user_1 = LoginByEmail (Email {emailLocal = "!\3994\&0\1104110\993057\141432\177623\DC1\1025975Kw", emailDomain = "#\187731\STX\42587C\1049778\FS>I\DC3"})
testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 = LoginByPhone (Phone {fromPhone = "+158870196"})
testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByHandle (Handle {fromHandle = "_qz7vrpp9ephc01mxczxarlr1.fz7bcea7417-30677gm.wd4259cypfrow14wwojak01b7gpb"})
testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByPhone (Phone {fromPhone = "+4663680366968"})
testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 = LoginByPhone (Phone {fromPhone = "+7132509448"})
testObject_LoginId_user_6 :: LoginId
testObject_LoginId_user_6 = LoginByEmail (Email {emailLocal = "7\"\25295h\1092816;\SYNJvd9%0\168733*\145437Cg\DC1\DC3", emailDomain = "pG%P!\"\US\GS\1078654g\\4D"})
testObject_LoginId_user_7 :: LoginId
testObject_LoginId_user_7 = LoginByEmail (Email {emailLocal = "\ESC\157526l\1024852\140510+xc\t\EM0:\DC2;s\aBT\ACK\nG$t\184102n\GS\987356", emailDomain = "\DC2a\1035941\179392\162301ce\1080871v"})
testObject_LoginId_user_8 :: LoginId
testObject_LoginId_user_8 = LoginByPhone (Phone {fromPhone = "+58462231"})
testObject_LoginId_user_9 :: LoginId
testObject_LoginId_user_9 = LoginByHandle (Handle {fromHandle = "n9iy_cyg"})
testObject_LoginId_user_10 :: LoginId
testObject_LoginId_user_10 = LoginByPhone (Phone {fromPhone = "+938295739630"})
testObject_LoginId_user_11 :: LoginId
testObject_LoginId_user_11 = LoginByPhone (Phone {fromPhone = "+53043284200"})
testObject_LoginId_user_12 :: LoginId
testObject_LoginId_user_12 = LoginByPhone (Phone {fromPhone = "+3715543821737"})
testObject_LoginId_user_13 :: LoginId
testObject_LoginId_user_13 = LoginByHandle (Handle {fromHandle = "ypcqx9j0m"})
testObject_LoginId_user_14 :: LoginId
testObject_LoginId_user_14 = LoginByHandle (Handle {fromHandle = "vd20"})
testObject_LoginId_user_15 :: LoginId
testObject_LoginId_user_15 = LoginByHandle (Handle {fromHandle = "mwgq6ce"})
testObject_LoginId_user_16 :: LoginId
testObject_LoginId_user_16 = LoginByEmail (Email {emailLocal = "fr%9\51388\1039001=\EM\EM\EM]^p\b 7-,\186688^9n=[", emailDomain = "vf\1058738\172187\DC2\40537ECs\8504\171427\1036277"})
testObject_LoginId_user_17 :: LoginId
testObject_LoginId_user_17 = LoginByHandle (Handle {fromHandle = "uj7uzr0f7t"})
testObject_LoginId_user_18 :: LoginId
testObject_LoginId_user_18 = LoginByPhone (Phone {fromPhone = "+50835506"})
testObject_LoginId_user_19 :: LoginId
testObject_LoginId_user_19 = LoginByEmail (Email {emailLocal = "/7\DC3v\95488yP*\EOT8>\EOT\ETB \SUB!\1088117\1537V\1024385.sm,\FS*L\63621", emailDomain = "s\155183`\1042307\v\CAN\"\DLE\1016225\&8\1029421$07N!"})
testObject_LoginId_user_20 :: LoginId
testObject_LoginId_user_20 = LoginByHandle (Handle {fromHandle = "r.c3"})
