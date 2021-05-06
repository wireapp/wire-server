{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CookieLabel_user where

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
testObject_CookieLabel_user_1 :: CookieLabel
testObject_CookieLabel_user_1 = CookieLabel {cookieLabelText = " d$\1022896mVJT9\SOh\RSHs\999002\&3JO|kt\158307=\13305y\\"}
testObject_CookieLabel_user_2 :: CookieLabel
testObject_CookieLabel_user_2 = CookieLabel {cookieLabelText = "O|E>\176\STXE\152719\1112977\1059558vI\185126"}
testObject_CookieLabel_user_3 :: CookieLabel
testObject_CookieLabel_user_3 = CookieLabel {cookieLabelText = "m>M\NAKT\RS(v/,\61970$PA\129330H\SI\1107118E[\139147a3"}
testObject_CookieLabel_user_4 :: CookieLabel
testObject_CookieLabel_user_4 = CookieLabel {cookieLabelText = "\"&C0\94957!\SYNo#L\SOHT&U\999359%`E\1038542\1098132:f>b\DEL{[\121286"}
testObject_CookieLabel_user_5 :: CookieLabel
testObject_CookieLabel_user_5 = CookieLabel {cookieLabelText = "\1088911"}
testObject_CookieLabel_user_6 :: CookieLabel
testObject_CookieLabel_user_6 = CookieLabel {cookieLabelText = "in\"4\aiQ"}
testObject_CookieLabel_user_7 :: CookieLabel
testObject_CookieLabel_user_7 = CookieLabel {cookieLabelText = "^K\SUB\STX\1107284\&9T_[\161614\65197m\1026718="}
testObject_CookieLabel_user_8 :: CookieLabel
testObject_CookieLabel_user_8 = CookieLabel {cookieLabelText = "^M\DC2n"}
testObject_CookieLabel_user_9 :: CookieLabel
testObject_CookieLabel_user_9 = CookieLabel {cookieLabelText = "\vCxee8}\\mg\985831O\78021^!$"}
testObject_CookieLabel_user_10 :: CookieLabel
testObject_CookieLabel_user_10 = CookieLabel {cookieLabelText = "V"}
testObject_CookieLabel_user_11 :: CookieLabel
testObject_CookieLabel_user_11 = CookieLabel {cookieLabelText = "\36237F\fx>\1081284\94494\184874$\\Q\49181KLQ\4934OhY?"}
testObject_CookieLabel_user_12 :: CookieLabel
testObject_CookieLabel_user_12 = CookieLabel {cookieLabelText = "\r\1062705d\1067572\&9\1065928KS'#"}
testObject_CookieLabel_user_13 :: CookieLabel
testObject_CookieLabel_user_13 = CookieLabel {cookieLabelText = "\144914Km\1040067\ETB\ACK\25511\GS\50134:|\DEL\14333\NUL.G\184623A.\t\NAKh<Sx\988259SE&9"}
testObject_CookieLabel_user_14 :: CookieLabel
testObject_CookieLabel_user_14 = CookieLabel {cookieLabelText = "h9\DC1\f\16524u\FS\NAKl\24819G\DC2"}
testObject_CookieLabel_user_15 :: CookieLabel
testObject_CookieLabel_user_15 = CookieLabel {cookieLabelText = ""}
testObject_CookieLabel_user_16 :: CookieLabel
testObject_CookieLabel_user_16 = CookieLabel {cookieLabelText = "MD\1030806\16392\&4\60804\GS\147913LcA)X?\SOH\ESC@5F'\498\95040\NUL\1106437\a4"}
testObject_CookieLabel_user_17 :: CookieLabel
testObject_CookieLabel_user_17 = CookieLabel {cookieLabelText = "$'\DC3;x\USJ1a\1109627]#l\ETX4SkQf%Mhdq\188129Q,AR?"}
testObject_CookieLabel_user_18 :: CookieLabel
testObject_CookieLabel_user_18 = CookieLabel {cookieLabelText = "_:\49210t\181065I6Eds\1112012\\=O\1034084l\a"}
testObject_CookieLabel_user_19 :: CookieLabel
testObject_CookieLabel_user_19 = CookieLabel {cookieLabelText = "P\DLE\170188@\aoJ]h\1101439\&2rS$\vQ\1015262\ETB\n\142093\EM(rpP@\1099027"}
testObject_CookieLabel_user_20 :: CookieLabel
testObject_CookieLabel_user_20 = CookieLabel {cookieLabelText = "\158477\11462\RSuij\11994\NAK\DC4\1055964\1113751_5t>jY\42024p\45281\1001457\GShz"}
