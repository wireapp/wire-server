{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewBotRequest_provider where

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
testObject_NewBotRequest_provider_1 :: NewBotRequest
testObject_NewBotRequest_provider_1 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000003"))), newBotClient = ClientId {client = "8"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), botUserViewName = Name {fromName = "\ACKL3\ab\SIM3\161079\1095141\&5\ESC\GS\99204n\SI\DC3kyA\NUL\27999@\1065911>iTA\\\190566:\FSBH\1101714\984128&T\DLE\1041273\175093r\1105590X7)\165314d$4\ESC\31448Z\1014155\1003982\19151z\NUL<ab\1016991*L\1023911\5855\1013339\1104430\60011\1093681\9253\1074724{\t\58988e/\DC1+\35120\aKw\1003521Om\139987E\1064192\ENQ)-{\1010336Xr\DC1gJ\ESC;O\1021495\1110940\143646\&9\52551\154316Y\DLEpq\DC1p\184641\1097515\NAK\1112449\GSuV\64369\19632\17314\1016780Y"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "x_m65k1h.8fi3oknngc46sflo.ig64etuyknvqln0x8qgqaaozhc18hmqrgc7b5ypotkky5h4onr9hb8o879x0aus_v03lxx1u-6d-8xgpqwi_wgo8ye9nf35o9q604lhdcju-8ly7ksor3c5g-nil0ky1e6.h4v0caiv9-8pio09s9t32dnh8m"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) (Just "") ([])), newBotToken = "\NUL\t\1059896", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.IK, lCountry = Just (Country {fromCountry = GF})}}
testObject_NewBotRequest_provider_2 :: NewBotRequest
testObject_NewBotRequest_provider_2 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000004"))), newBotClient = ClientId {client = "e"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), botUserViewName = Name {fromName = "W\SUBC\177227\r5$D\1104110\NUL\1032536i\a\1071315\140087X\STX\142666\188640-\11402\30563\SOH!\n\1091529\1012011J\\T\nHPS\147779=l\\\1017632}\t\158835\DC1\1082799?\RSW&D\1045363p-\"\169883\1032682V\1070805]gW\EMdT`G\29616A\1032656`\179839\RS-sa\987993 d8T\SI\984589Q#oxA\1044402\&8\148653\149005\1068087C\183405A\SO\23086!rR\n\ACKl\ACKWu\bm\33733t\1098759])\v\n\USn\DC2\25495Yg\b1\n\SUB\US`\177270h"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) (Just "\39898") ([])), newBotToken = "\1103232", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = CA})}}
testObject_NewBotRequest_provider_3 :: NewBotRequest
testObject_NewBotRequest_provider_3 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000003"))), newBotClient = ClientId {client = "8"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), botUserViewName = Name {fromName = "H\54327\",\ETB\1085451A8Lu\1068852\EMqz\DC4pRs<f\ETXg\"!c\1064087?Ji\"\SOwI\t:cOg\61372\34678\EOT)\188884\USJk\995792\171618\US\t\59362\1068396k{w\DC1*H^B\CANlG\1020333iJ\ETBA\38715\DC4N\fj\"`\ESC\997418]\1059070"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "lfnbv"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) (Just "") ([])), newBotToken = "", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = LT})}}
testObject_NewBotRequest_provider_4 :: NewBotRequest
testObject_NewBotRequest_provider_4 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000002"))), newBotClient = ClientId {client = "3"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), botUserViewName = Name {fromName = "\FS\RS\r\160343`\FS\1051513o\b\100088c\n5~\1102350\&1t$\DC1f\171677\n\149343\100194S`\n\22153\EM\1055627\&7^\170073\57676SS\ESCT\127929o\1062022t\DC4\EM\EOTy\DC2\1001748\SIgHaBmq\149929!\1006135z\1049410(0`<a\1112357(\DLE\1035213\1094798\&8\99951\RS\995673\DEL\1002997UMt\986076s"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "vtnpeo.thzwhzpwij3qpaohd79.nwdg96mpk-xhhp5.shr1ss9c3un5hvygivuwnxw5s7"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) (Just "C") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "k4lomrf0voj34r0he54adntdsyp5o_17mwbiqw_jydlvxlrksdegrdeh2saskcnguznht9o5gv4ju6ckogsd"))}])), newBotToken = "4", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.SL, lCountry = Just (Country {fromCountry = ST})}}
testObject_NewBotRequest_provider_5 :: NewBotRequest
testObject_NewBotRequest_provider_5 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000003"))), newBotClient = ClientId {client = "10"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), botUserViewName = Name {fromName = "\RScu\98258\"\GS<^Fy\ENQ\SIM\150744{\1061061]\1042596w\157244\DLE\SO&&;\1032507&\DC40m\1060612Kw-\SI^\156745jA T \132620\1028767*Rm\SYN\64301S$g\DC1!\188123\SO\SUBk?E\44797\170793}\1018465v\181884\1112536>j\FS\134189\aC\161286\165272fA\1023631\ETX!]y"}, botUserViewColour = ColourId {fromColourId = -1}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) (Just "") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "i17w9hmdv05i0zun0adfg1el1cy8z_di5_qst_6uwdpqoh__mqodopxdbkqoqatkn6n5hckpmbarjrg4eb97jx"))}])), newBotToken = "i\120679", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = CO})}}
