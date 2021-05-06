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
testObject_UserIdentity_user_1 = PhoneIdentity (Phone {fromPhone = "+02235530"})
testObject_UserIdentity_user_2 :: UserIdentity
testObject_UserIdentity_user_2 = SSOIdentity (UserSSOId "\SI" "") (Just (Email {emailLocal = "\SOH", emailDomain = "Z\SI3"})) (Just (Phone {fromPhone = "+884759865"}))
testObject_UserIdentity_user_3 :: UserIdentity
testObject_UserIdentity_user_3 = FullIdentity (Email {emailLocal = "),gE\1012227\ETB", emailDomain = "\RS\51820?\989815\vX\20677\ACK3\18140\1005338O\134525\&3"}) (Phone {fromPhone = "+942260291290"})
testObject_UserIdentity_user_4 :: UserIdentity
testObject_UserIdentity_user_4 = EmailIdentity (Email {emailLocal = "cI\1006028", emailDomain = "w;]Z\137781\156500\1044109"})
testObject_UserIdentity_user_5 :: UserIdentity
testObject_UserIdentity_user_5 = SSOIdentity (UserScimExternalId "\33273\1044193\1009512>V\1099487\&9P@") (Just (Email {emailLocal = "\174070\ACK\DLE!{\SOH;", emailDomain = "\14879\DEL"})) (Just (Phone {fromPhone = "+743128893434"}))
testObject_UserIdentity_user_6 :: UserIdentity
testObject_UserIdentity_user_6 = EmailIdentity (Email {emailLocal = "", emailDomain = "\70844?\ESC5\166694!"})
testObject_UserIdentity_user_7 :: UserIdentity
testObject_UserIdentity_user_7 = FullIdentity (Email {emailLocal = "R\1028550H\SOH\144704L\119126%l", emailDomain = "G6+\28185]>%L6\1028834=%bc,"}) (Phone {fromPhone = "+38563932203"})
testObject_UserIdentity_user_8 :: UserIdentity
testObject_UserIdentity_user_8 = SSOIdentity (UserSSOId "%lE!e" "9c\991266\DC4") (Just (Email {emailLocal = "", emailDomain = "{hR\1017268\ACK\38234p\1098970"})) (Just (Phone {fromPhone = "+833643157367605"}))
testObject_UserIdentity_user_9 :: UserIdentity
testObject_UserIdentity_user_9 = SSOIdentity (UserScimExternalId "\93974\97333") (Just (Email {emailLocal = "\DC4\1093406Z$", emailDomain = "L\DC4-\STXi~\SO\DC1\DC4e"})) (Just (Phone {fromPhone = "+21683298114970"}))
testObject_UserIdentity_user_10 :: UserIdentity
testObject_UserIdentity_user_10 = EmailIdentity (Email {emailLocal = "\141938\DC4\RS_\n\1048551\190968)E+n\SO\174285!h c\r\139966\&8?\"", emailDomain = "+=F\DC1\131464:"})
testObject_UserIdentity_user_11 :: UserIdentity
testObject_UserIdentity_user_11 = EmailIdentity (Email {emailLocal = "=q]p\ETBi", emailDomain = "\ACK0R3\176963\92281s\70727\r\59479=D1\563\1105401\181785Y\996353J\149611"})
testObject_UserIdentity_user_12 :: UserIdentity
testObject_UserIdentity_user_12 = PhoneIdentity (Phone {fromPhone = "+17821696379"})
testObject_UserIdentity_user_13 :: UserIdentity
testObject_UserIdentity_user_13 = EmailIdentity (Email {emailLocal = "\43668hN\1106358A\1017347y\SOP1\\\FST\EM\EOT4#jb", emailDomain = "ST\n"})
testObject_UserIdentity_user_14 :: UserIdentity
testObject_UserIdentity_user_14 = PhoneIdentity (Phone {fromPhone = "+25417069834"})
testObject_UserIdentity_user_15 :: UserIdentity
testObject_UserIdentity_user_15 = PhoneIdentity (Phone {fromPhone = "+24689802452"})
testObject_UserIdentity_user_16 :: UserIdentity
testObject_UserIdentity_user_16 = PhoneIdentity (Phone {fromPhone = "+96883055"})
testObject_UserIdentity_user_17 :: UserIdentity
testObject_UserIdentity_user_17 = SSOIdentity (UserSSOId " \\}\2370k" "}") (Just (Email {emailLocal = "\1028916e\990753\STX\1075617", emailDomain = ""})) Nothing
testObject_UserIdentity_user_18 :: UserIdentity
testObject_UserIdentity_user_18 = FullIdentity (Email {emailLocal = "\1113532`\141890Dm4\52661\DC3\996706A\STXA", emailDomain = "\fT~5\b\29962\37156)0\RS\1040652!7\1090099"}) (Phone {fromPhone = "+95805791"})
testObject_UserIdentity_user_19 :: UserIdentity
testObject_UserIdentity_user_19 = FullIdentity (Email {emailLocal = "\\\141977^U\ACK51", emailDomain = "Jh\SO%.6\137469z2\SI\1063789\137149\US"}) (Phone {fromPhone = "+099864179"})
testObject_UserIdentity_user_20 :: UserIdentity
testObject_UserIdentity_user_20 = EmailIdentity (Email {emailLocal = "k\60231\GS\984401\&5-\1081982\n\1076586T\FS\14279\NUL\83458\STX>k2\SO)zk_\RS\FS(r", emailDomain = ""})
