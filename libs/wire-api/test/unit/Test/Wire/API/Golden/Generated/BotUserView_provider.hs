{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotUserView_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_BotUserView_provider_1 :: BotUserView
testObject_BotUserView_provider_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000000000002"))), botUserViewName = Name {fromName = ",\154187Y\1056616\SOHds\144186'\1014471m\4978\rJ6\1016500\163425X\39734C%[)\bu~T-wz\r\1037279C#SFX\f:k\32842\1030928nP\\\1024156\10926x:\1059489I\DC1\1039445\ETX\25188_\EOT=\SYNO&^mG\1070912;\1013104\ETXQ,c\ESC \STX\1102991\1023041\a\44490\1039922JHO\993098W`\1053237\DLE\995397\1066037x\1007699\23744\97829>\1042850:*?%\52143\9405\DC3\54102\1090230@I:\1004079z\134348p"}, botUserViewColour = ColourId {fromColourId = 2}, botUserViewHandle = Just (Handle {fromHandle = "ba3"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000002"))), botUserViewName = Name {fromName = "T8BS\144641[\EOT\1040073\\]\SUB\r\1067949F\ETX\EOTFu\1029770\SYN\EOT\DEL\DELO3\ACKgs.\163842\&6\RSt\ETX((\96237\1112125\&7A\GSS7A~\138947*\fJcSa{"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "685226c"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000700000005")))}
testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000700000006"))), botUserViewName = Name {fromName = "\24377"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "-l"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000001")))}
testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000700000001"))), botUserViewName = Name {fromName = "Qti\133355,\97404>\1019605/\995289\SOXjZ\989433\1022086S\13044\16274\59796\134069$ezj\1048685~8w[\EM%<2\t\STX \DC1"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "lzii9mr7dbnreenoyry670m7-xvfisx6ie0z-ku3b_xs_zmz2g5urkly.zjsdfkvundkcifi10e_46s4f2x4nqv9994l3ijlz-rvrjbq25qgddj8mwm8jcue7z9vmnpmqph0v0lc6r5smpx.myu3ye6um9.5qjutq-fxte2qnj6gf-vq1a53e"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004")))}
testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000200000001"))), botUserViewName = Name {fromName = "\US;\SUBN-\1102843\68838\n/\STX2\1110542\SUB\RS0xn\ETB\CAN\EOTW<Y\37909S,\136302\SOH\1064334\3869\ETX\f\1054004\SOH\r\64508Yqv\164329tgs\\\1036494yM\1070825A%0\DLE\4504P\1053995\1077182ar\164480L:\991281\996522\ENQ\143749f\1104123\1028934ZH=^}\1004688\&8?o06BnU\62718\DC2L <c\ETX\1108571\SUBi:?\DEL9\1052254\SI\f\24851\DC1GO\SO\1042206b"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "z.yxp4_"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000300000006")))}
