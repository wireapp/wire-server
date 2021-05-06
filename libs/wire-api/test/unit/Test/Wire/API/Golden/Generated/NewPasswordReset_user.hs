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
testObject_NewPasswordReset_user_1 = NewPasswordReset (Right (Phone {fromPhone = "+36512032769713"}))
testObject_NewPasswordReset_user_2 :: NewPasswordReset
testObject_NewPasswordReset_user_2 = NewPasswordReset (Left (Email {emailLocal = "\93773C<^\DC2\SO\1099025", emailDomain = "$v\1000873\4524\b\1026655\ENQ"}))
testObject_NewPasswordReset_user_3 :: NewPasswordReset
testObject_NewPasswordReset_user_3 = NewPasswordReset (Left (Email {emailLocal = "b\SUB\1032593x\f\EMdg\SOd\ENQ\CANAvN<ou\21677>\b\58977?Y-hh\GS", emailDomain = "\1043054M\"&nH\1087515\1088661A\1015662h"}))
testObject_NewPasswordReset_user_4 :: NewPasswordReset
testObject_NewPasswordReset_user_4 = NewPasswordReset (Right (Phone {fromPhone = "+745139382"}))
testObject_NewPasswordReset_user_5 :: NewPasswordReset
testObject_NewPasswordReset_user_5 = NewPasswordReset (Left (Email {emailLocal = "v\t\94503\&0\FS\1081955,T\1021533_\NAK\GS6$N\7206*\DEL\992987\44498V/J\"\14285!X", emailDomain = "\ENQ\189306o7\EOT2"}))
testObject_NewPasswordReset_user_6 :: NewPasswordReset
testObject_NewPasswordReset_user_6 = NewPasswordReset (Left (Email {emailLocal = "\182569\&5N\STX\1075752\1066012", emailDomain = "G$\97896\&5\62636\b"}))
testObject_NewPasswordReset_user_7 :: NewPasswordReset
testObject_NewPasswordReset_user_7 = NewPasswordReset (Left (Email {emailLocal = "\r)SUS\1103397A\rU%", emailDomain = "\SOH~\67214\\\1057179jC\\\990170\1042044y\1055394m;=\17753\5721xT\FS\1003232\bq\188150{*"}))
testObject_NewPasswordReset_user_8 :: NewPasswordReset
testObject_NewPasswordReset_user_8 = NewPasswordReset (Left (Email {emailLocal = "C\aNp3>\1053577<\v\1079032\21797\DLEv\1063971U\1086194d7\33211", emailDomain = "\1042331"}))
testObject_NewPasswordReset_user_9 :: NewPasswordReset
testObject_NewPasswordReset_user_9 = NewPasswordReset (Right (Phone {fromPhone = "+23304663090513"}))
testObject_NewPasswordReset_user_10 :: NewPasswordReset
testObject_NewPasswordReset_user_10 = NewPasswordReset (Left (Email {emailLocal = "mf(Xl\1102780T\RS\1079114t\1065177}\999163$4\ESCe.2q\993985", emailDomain = "wDm=}.\1013204\n\138169"}))
testObject_NewPasswordReset_user_11 :: NewPasswordReset
testObject_NewPasswordReset_user_11 = NewPasswordReset (Left (Email {emailLocal = "K\174628\FS=D'\159430/H6\173119\35717?{A\DLE\998574\&5\ETX#\30989X\1033330i", emailDomain = " oi+mwz'bY\DC4\1023417-G?\DC2\94268W\DC2\1059108c\183522s"}))
testObject_NewPasswordReset_user_12 :: NewPasswordReset
testObject_NewPasswordReset_user_12 = NewPasswordReset (Left (Email {emailLocal = "\1033196O(\984108(cS7(v", emailDomain = "\1008487\999975)\1066373m\189702\SUBcH+\\.\1003699C"}))
testObject_NewPasswordReset_user_13 :: NewPasswordReset
testObject_NewPasswordReset_user_13 = NewPasswordReset (Right (Phone {fromPhone = "+85769712131"}))
testObject_NewPasswordReset_user_14 :: NewPasswordReset
testObject_NewPasswordReset_user_14 = NewPasswordReset (Left (Email {emailLocal = "fP_\1018931\SO\"_|y1\t\1081866\DC17NTL/\166125\1021173r?KC\EM9\178145", emailDomain = "\1063535\"\DC2]c\1001164\EM\74562N\1069673\&1s8x>i.\1008508L\185975}\1022760\1087333>s)\SI\n\fK"}))
testObject_NewPasswordReset_user_15 :: NewPasswordReset
testObject_NewPasswordReset_user_15 = NewPasswordReset (Left (Email {emailLocal = "E\CAN5z^\nno\743%O\RSP\49064\&2\SI\fEel.", emailDomain = "|,\1037431\992756 {qU\RS\1046018Q\SO\1044422\b\50372\174124\tDf\1015225#Dl\NAK5\99312\SOH\172767\b"}))
testObject_NewPasswordReset_user_16 :: NewPasswordReset
testObject_NewPasswordReset_user_16 = NewPasswordReset (Left (Email {emailLocal = "y+\1089870\141337c\STX\FSI\GS #\1074282\\\1008364\DEL\128267L\18364}\123624R\182755Mz\1047652\162425J", emailDomain = "<\NAK6Z\f\"\77980mJ\DLE^\ESC`Ws0PL9c1\155892\&0\1105092U\SIE>"}))
testObject_NewPasswordReset_user_17 :: NewPasswordReset
testObject_NewPasswordReset_user_17 = NewPasswordReset (Right (Phone {fromPhone = "+498113042216"}))
testObject_NewPasswordReset_user_18 :: NewPasswordReset
testObject_NewPasswordReset_user_18 = NewPasswordReset (Left (Email {emailLocal = "\77944)\"_\t", emailDomain = "Z\SOH+\v\1026728"}))
testObject_NewPasswordReset_user_19 :: NewPasswordReset
testObject_NewPasswordReset_user_19 = NewPasswordReset (Right (Phone {fromPhone = "+2846406053910"}))
testObject_NewPasswordReset_user_20 :: NewPasswordReset
testObject_NewPasswordReset_user_20 = NewPasswordReset (Left (Email {emailLocal = "\1032278'\143688\1060243\182892\STX.e\1056899$k*\36051A\vXM\DLELfh8O=", emailDomain = "dY\STX\DC3\1104920\ENQ\1088053\&2\999159\153632"}))
