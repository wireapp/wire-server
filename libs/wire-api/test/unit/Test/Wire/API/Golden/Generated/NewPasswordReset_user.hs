{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewPasswordReset_user where

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
testObject_NewPasswordReset_user_1 :: NewPasswordReset
testObject_NewPasswordReset_user_1 = NewPasswordReset (Left (Email {emailLocal = "?j~\r\DC1z\160525\SO<)1\EM\FS\ETX\1091461", emailDomain = "z\1001377}\1009986x-"}))
testObject_NewPasswordReset_user_2 :: NewPasswordReset
testObject_NewPasswordReset_user_2 = NewPasswordReset (Right (Phone {fromPhone = "+380819826007"}))
testObject_NewPasswordReset_user_3 :: NewPasswordReset
testObject_NewPasswordReset_user_3 = NewPasswordReset (Right (Phone {fromPhone = "+28832468036"}))
testObject_NewPasswordReset_user_4 :: NewPasswordReset
testObject_NewPasswordReset_user_4 = NewPasswordReset (Left (Email {emailLocal = "\CANB\17475\&9\11817\&6\180374\1053691,?~\1100688\15390", emailDomain = ""}))
testObject_NewPasswordReset_user_5 :: NewPasswordReset
testObject_NewPasswordReset_user_5 = NewPasswordReset (Right (Phone {fromPhone = "+586997986"}))
testObject_NewPasswordReset_user_6 :: NewPasswordReset
testObject_NewPasswordReset_user_6 = NewPasswordReset (Left (Email {emailLocal = ")\46036\FS", emailDomain = ""}))
testObject_NewPasswordReset_user_7 :: NewPasswordReset
testObject_NewPasswordReset_user_7 = NewPasswordReset (Right (Phone {fromPhone = "+642799721216"}))
testObject_NewPasswordReset_user_8 :: NewPasswordReset
testObject_NewPasswordReset_user_8 = NewPasswordReset (Left (Email {emailLocal = "\1096097J\v\133378^7", emailDomain = "\141183\997517x\678/\49847\ETX\1099654\165330\SYN"}))
testObject_NewPasswordReset_user_9 :: NewPasswordReset
testObject_NewPasswordReset_user_9 = NewPasswordReset (Left (Email {emailLocal = "Yc\1040694\169906!\FS\164449\1023097~J8ph!\a\159875", emailDomain = "\1085061_1\158370\NUL]\22674iA\1007189\SIS\1045545\1005249< \95909\&0C F3\1074354YA|\191192\45804\b"}))
testObject_NewPasswordReset_user_10 :: NewPasswordReset
testObject_NewPasswordReset_user_10 = NewPasswordReset (Right (Phone {fromPhone = "+7904678643199"}))
testObject_NewPasswordReset_user_11 :: NewPasswordReset
testObject_NewPasswordReset_user_11 = NewPasswordReset (Left (Email {emailLocal = "XB\vz\9096_o\998450;|5\148000e\71853\177359A1\DLEp\ENQ\b", emailDomain = "ls\";\US]"}))
testObject_NewPasswordReset_user_12 :: NewPasswordReset
testObject_NewPasswordReset_user_12 = NewPasswordReset (Right (Phone {fromPhone = "+0530960155778"}))
testObject_NewPasswordReset_user_13 :: NewPasswordReset
testObject_NewPasswordReset_user_13 = NewPasswordReset (Right (Phone {fromPhone = "+619674854468649"}))
testObject_NewPasswordReset_user_14 :: NewPasswordReset
testObject_NewPasswordReset_user_14 = NewPasswordReset (Right (Phone {fromPhone = "+937557826524051"}))
testObject_NewPasswordReset_user_15 :: NewPasswordReset
testObject_NewPasswordReset_user_15 = NewPasswordReset (Right (Phone {fromPhone = "+546038762"}))
testObject_NewPasswordReset_user_16 :: NewPasswordReset
testObject_NewPasswordReset_user_16 = NewPasswordReset (Left (Email {emailLocal = "OfWEb\DC2nK\100326", emailDomain = "J\SYN\990516a{\94049Z,im0>~\DLE:I\984585["}))
testObject_NewPasswordReset_user_17 :: NewPasswordReset
testObject_NewPasswordReset_user_17 = NewPasswordReset (Right (Phone {fromPhone = "+281078418004"}))
testObject_NewPasswordReset_user_18 :: NewPasswordReset
testObject_NewPasswordReset_user_18 = NewPasswordReset (Right (Phone {fromPhone = "+304716464610617"}))
testObject_NewPasswordReset_user_19 :: NewPasswordReset
testObject_NewPasswordReset_user_19 = NewPasswordReset (Left (Email {emailLocal = "\vYhx\49096&HH?t3\SUB", emailDomain = "a\STXb\26731\171713\SO3,Bm\170162\NULt\68022"}))
testObject_NewPasswordReset_user_20 :: NewPasswordReset
testObject_NewPasswordReset_user_20 = NewPasswordReset (Right (Phone {fromPhone = "+33564459491"}))
