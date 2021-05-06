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
testObject_NewBotRequest_provider_1 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), newBotClient = ClientId {client = "a"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), botUserViewName = Name {fromName = "/\175478\DLEF\6386hy:\133756RG9 \DELY\1051002?\1017254\1083967\1046215:\2423$K+\ESC\SOH\SOH\143723\1049068F\1056001^\1100123Bp\"\137805i*\ETBIj\f\1014238\DC4D#Y\44457\1113963s\69942\SIe??\5855c\NAKE\995059E\SI\1111823\DC1\DC4\ENQ&\26182?\n0\SUB\16144\SI\NAK~k\\g\168741\&5 "}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = ".pp-k-p51bt-uifbslbx4ea0d8cfgf99-t-dhdt81eojtlqwovinj3latlirsp-z1g_nnm4w313vutf204k54vjz--kecwgsx3vn9k3u0jxqpvfql5x.p.7l_x51niqrowwc"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) (Just "") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "egi2ubvchnnwjzp2np4vf_0p_3x93ogm8a9c2_kog84fa0es3ydpz2fgyuy6cab392hmdmut"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "1ywv"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "asut3gme_sl"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "71cwx0j6arq2cfumyin7268wlcg9ydlwxnr44ystuvy8tbwtxhzs6tgcmm4cqcnv8ne5crsx8rh04emsr4lmi3wejpt7az1u"))}])), newBotToken = "Q_}", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.BG, lCountry = Just (Country {fromCountry = MO})}}
testObject_NewBotRequest_provider_2 :: NewBotRequest
testObject_NewBotRequest_provider_2 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000001"))), newBotClient = ClientId {client = "a"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), botUserViewName = Name {fromName = "\1051597\988293\ETBo?Fmfh!IG9\94666\GS#GH\EOTt\1007141rx\DC1\GS0f\t\ETB\a./\1097119jt\ENQ<\1102273Y\25619(c8\1072416\US/\99189\78438`J\DC2b4.?w\44434z\168387\SYN\98109\ACKS\1085287<\EMi5\FS\1082090E\SI\US\993701d85%m"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "q11j-fpf6wgog6m9hgy65wzeo4r.h_uuk3unyve4zqt2fzo31d1dow8z-ycqi4.bfk64tixch409ccks.goa7i_km97t7h.mc0or__89__8-efn4ij_fg4ie7d-swu4vctezzolqscq1ma62xxn4uekbrifyr7y--8ytk385boq"}), botUserViewTeam = Nothing}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) (Nothing) ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "j9e9l4pc7u46ghvqe7ulqfcbj8ru0j64zeq2gs4isody0b22r9ld2_81neas6tq7skkfoumet82wgsr9vg1g_47v7kpjv4vk5fwvnsa"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "ozbyrnzrkhzjcexdkdg8802oi0b61j0ssh_ity0wnpc6r45myp2ylso4mq3jk5wrc_5xm8848bvvbddni26ukfluuzj4fk6nsupwxm5a13hs3pgv7adn8hdwiqoivu"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "xlb75t62am1nojcdkzitbj9ikz_llhuosxbanb_ignez1kks4mpb8kc772hd87gt235xpurkz_oeln"))}])), newBotToken = "\\\1092760", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = GY})}}
testObject_NewBotRequest_provider_3 :: NewBotRequest
testObject_NewBotRequest_provider_3 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000004"))), newBotClient = ClientId {client = "f"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), botUserViewName = Name {fromName = "%^\DC41\SOHg\983849"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "pkict40d6nqvfwsmjf9577zqlyg3yvc5r0qu9yx64bxllwn2rvfh46f9qufn"}), botUserViewTeam = Nothing}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) (Just "\a") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "ymxuzm55zbpt6hrajodq_1i7tjfg15kbipiilpd45b47it50cpc2op2d9jd7yqesiktu9uro7zvjuwk_fceoggi7rmrv55dl2yfpizp"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "n9yz_xesfkuipt4je3_2zmxu1hz4uom26n81rvej00bl0avvfjy0i_ug_9v5es"))}])), newBotToken = " !\ESC", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = CM})}}
testObject_NewBotRequest_provider_4 :: NewBotRequest
testObject_NewBotRequest_provider_4 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000004"))), newBotClient = ClientId {client = "a"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), botUserViewName = Name {fromName = "\1030345\20403R\188007y\STXg\DC3\NUL\158703JiF\SOH\1036490m\EOTs\DC1iKSo\ACKDn&n\v\EM!&*\FSFn\1003888\GS6\NUL\127544\1056313\7735\SI\119241\1014152\ENQ\v\1049129Z\SI*\ENQ\29548\25430\1067192\DC2e\1077561r&-zf6\FSm\31054>\141122\83283\&7\SUB.\ESC+h`k9"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "c68_9he.u0vyci3oex94.znvq.pr-zp_axpyla0klasbea6c7q6nn5.7vo8izjihekb5ekk0yjifa0cen47vg20esxz5j.yr46ed-p_-64zsg3ow1rnbb1gp3s--a59l-i.jsct1759v44b"}), botUserViewTeam = Nothing}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) (Just "") ([])), newBotToken = "]", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.CV, lCountry = Nothing}}
testObject_NewBotRequest_provider_5 :: NewBotRequest
testObject_NewBotRequest_provider_5 = NewBotRequest {newBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000003"))), newBotClient = ClientId {client = "3"}, newBotOrigin = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), botUserViewName = Name {fromName = "\1002590OT>\69644\b6?\\NE7A,\151956(\133465\22865\1103689 \1047612+\184188j\991686\ESC\1092924\DEL}\43258+[\SUBrG\993399OC+mn\48794\ENQd\29277\146415LivA\a[\1032585'\SOH\31323\ENQtD\54558\DC3~\1066200\43910\&7\1085675[A\1076326\26055\95230\144248l6\180322\SYNx\"s\n\152572\187112'\162472\1016137\1053873\DELFBA\b#M\DC4\ETBw\1090676\38489P\1076694\62009\rf1\1112023l\153958\132548W\ESC\fAm!\1011440!/0\78792\168633D\986428"}, botUserViewColour = ColourId {fromColourId = 0}, botUserViewHandle = Just (Handle {fromHandle = "nh"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}, newBotConv = (botConvView ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) (Just "") ([])), newBotToken = ",0\160200\DC1\1069617", newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Nothing}}
