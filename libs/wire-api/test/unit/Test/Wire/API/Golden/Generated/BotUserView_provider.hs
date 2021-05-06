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
testObject_BotUserView_provider_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000600000006"))), botUserViewName = Name {fromName = "\191358K$zr~iS\EM]4\995324v!w1\70665o\992517B\SOA\27031O\63673~\53124P>?\21203/\ntZ7E\DC3N+\1055187rvw[ui;///R=Q\DLE\DC27\tF!\43645'+ZG\DC4\176250\DC2\b2\176788"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "5as5rjuy29jur.izlgezo24v9.dq-shgjhu73a89ck8nhq2os1ng.92a9rq_n_ms"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000600000000")))}
testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000800000006"))), botUserViewName = Name {fromName = "\100561\39982\ETB6\33667\1082720&\"\DC2']qE\1015428U\164225\1081860\53934\RSu\167622D\RS*9\1102021g=\1058052=9\vRA\GS)\DC1\ETX\SI5\\\1064393ks\\\STX\f=\100868\NULaz\70475v;VXF|9\1093009]\69773;\t./\v\1050337&\1027838W+x\129481d\1083884g\162495\1046697\1046015Gx\1068513\68005\&9v`}\1094694\NAK\60356n\1098505\v7\50598"}, botUserViewColour = ColourId {fromColourId = -3}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000700000004")))}
testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000200000001"))), botUserViewName = Name {fromName = "R\138801\GS\DC2{\CAN\77983j~g,\"xg\ETB\"J\DC4\1111812\1062578<\ACK@C\5187E\ai\1040426T^jK7QFP\1033787W\FS\119244\FS\92333m\EM\1072497\SYN\DC1[9\1018739=|\DC3\1031122\GS\161188\vb6d6\189726\1098900\&8\EOTk`\1189x\t\151186MW\61530\1025713\983525\SOxYE\US\1069239\60064\v\12341N{b\a[\ETBQ@2\1054035\32376/\ETX\1028022DMm"}, botUserViewColour = ColourId {fromColourId = -5}, botUserViewHandle = Just (Handle {fromHandle = "mn-."}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000100000007")))}
testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000600000000"))), botUserViewName = Name {fromName = "\1099640\SUB~S_`\166287B\1091782e?\34208\1066456\186694v\EOT\44802vA\DC3l\1086589p\f\SUB?\1242T\DC3<\1026557T;\f\"\tX\EOTa \f\166468l\161913TQ.w<m\NUL#z"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "vod2ytmcb"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000200000005"))), botUserViewName = Name {fromName = "\1101622\&7\5968\&7E\b_F\SYN\t9>`\RSa\DC3#,\v]4U\STX\EMq\187378\NAK\3463\ENQN\1079999\DC4l5\NUL\1007433D"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Nothing, botUserViewTeam = Nothing}
testObject_BotUserView_provider_6 :: BotUserView
testObject_BotUserView_provider_6 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000600000004"))), botUserViewName = Name {fromName = "\30469\&9^\DLEqT\b4\DC4\1004197\b\38792o"}, botUserViewColour = ColourId {fromColourId = 8}, botUserViewHandle = Just (Handle {fromHandle = "x8ucn9oo_b"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000000000005")))}
testObject_BotUserView_provider_7 :: BotUserView
testObject_BotUserView_provider_7 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000600000007"))), botUserViewName = Name {fromName = "\178810\1076658I\94886b\1085125q\156452F{MLh\bZ\1071354vU=xA\194704\ENQR\18129O\1016089\1033816;\172410\1079222\24687\r\1010736;\DLE\190060t\171058\1074451Y\\H\60690@Po-4\66478\&1AK|.k\28952\DEL{J\tT\1006329UZ/\v\ESC@\22407s*OKS}\DC3\ACK\40551\57672H\99493\985342\DC2\SOK\CANS_w\SOH3\NULJ\DC3\ETX\CAN\185452\EM7\1036334\NUL\162282}\\T\ENQY\FS,i"}, botUserViewColour = ColourId {fromColourId = -4}, botUserViewHandle = Just (Handle {fromHandle = "qkn0.eb03s"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_8 :: BotUserView
testObject_BotUserView_provider_8 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000300000003"))), botUserViewName = Name {fromName = "P\992134\&1\ETB\1109840s$\1096139\987237j\1107533kaBQ\133724e\173013\SI\1093557\23491\DC2\f\1080702*\1059041K\82952+\1052436'&\190011r\1030921\1059914)\1032679\\\ESC/\SO\1023173\\lN\b:\GS\162753\990188z{KNz|[\32649r\990898\1053518"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "6lh6"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000200000000")))}
testObject_BotUserView_provider_9 :: BotUserView
testObject_BotUserView_provider_9 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000500000001"))), botUserViewName = Name {fromName = ">`SGd\4978iG\\Ald^^tH33`\1101028\DC3E\1084001\CAN\CAN:\EM\1058580c1134K\1056457\EM\CAN\EOT\DLER\RSo\GS7X-\ETBhB\\%+\EM\ETXR\1028715K}\STX\DLEM4\717\SOH\1018810\&2\DC3F\STX\NUL\DC3\63209\1061247!\984341\&9:"}, botUserViewColour = ColourId {fromColourId = -1}, botUserViewHandle = Just (Handle {fromHandle = "6okbvr94kbecrurlj15k_4fgr-bd9e0rupuw7rzz.rg7cvi"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000800000001")))}
testObject_BotUserView_provider_10 :: BotUserView
testObject_BotUserView_provider_10 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000100000008"))), botUserViewName = Name {fromName = "Qt\"\US;a\DC2\28413\1001348`<\995593Og-\\\DC1<\1003416\DC2\a\1077645Zex\NUL\1014949\1026749\NUL\147668)WH\178156\&2\176785)\1108255CP;v\131274Z \148105c\50067?.\a\1088363WN\EMQA"}, botUserViewColour = ColourId {fromColourId = -8}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000600000004")))}
testObject_BotUserView_provider_11 :: BotUserView
testObject_BotUserView_provider_11 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000001"))), botUserViewName = Name {fromName = "=r\1004836i!\US\19445{^\1043814\DC42\SIc\t\35009\995172B\1091050R\181342\DC1\166524\STX\SUB|\984670\40024\49957Py\184426\120988V\1073600\\`w!tam`7Z\1684\"\NUL6e\984065f/\1026442\&7L\ACK\DC3Z4x\1038391\&4~\RS_9\EM\SI*!^\f_\173350_\CANH\SOHJ\127869\&7x1&4\1061572\USZz\191302&gI\67095c\DELuRf"}, botUserViewColour = ColourId {fromColourId = -6}, botUserViewHandle = Just (Handle {fromHandle = "0bp_nfrl"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_12 :: BotUserView
testObject_BotUserView_provider_12 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000006"))), botUserViewName = Name {fromName = "=oE6\ETX\SYN\1043707>\2553@\29045mm\1030897\&8\984164\SYN\180474\94401.e.>\998392\&4\6827'(nR$\28746a\176535(1\151837N\15550SSf\US+.Q\74639LX\169555\1017292l\SOH\NAK\USF&zfc\1084787\1048895V?\24005*9n\EM*\989164R|-\57949CmG\EM\DC3t\1059920:\39477g\46253 \ACK`WP\9618\FS\ESC%b\DC2\b\SO\1084017H6"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000")))}
testObject_BotUserView_provider_13 :: BotUserView
testObject_BotUserView_provider_13 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000100000005"))), botUserViewName = Name {fromName = "{1>\25179\ENQ \DEL;&y\1035702e\175767P\18827\1033907\132677G?Jg\CAN\1113646\1090011\f\v#%\1093760\1067326\1064802\&4wi\NAK\EOT\30253:v\1063292ClM\6471\DC3\t\ETX\fv\147981\1046060\1035529\39404\37394\SI*IC\EM*N\983780\1044062JO"}, botUserViewColour = ColourId {fromColourId = 4}, botUserViewHandle = Just (Handle {fromHandle = "lf54dy3ad-"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000300000004")))}
testObject_BotUserView_provider_14 :: BotUserView
testObject_BotUserView_provider_14 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000500000001"))), botUserViewName = Name {fromName = "\SUBSAoxd\1014551W\EOT^QG=\SO\SOH\EOTJu3\SUB\SYN g\vY\CAN\1097024Z>r\95957[\DC2)a\US.\1017216\US\998363\32132\SI\EMR\CAN\EM\SYN\NAK<\100401\1010677"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "mtf.qeop06vytv79-t7oyb96roec3e54jxetc481oas4hvltw_yz5zmunesgv"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000800000007")))}
testObject_BotUserView_provider_15 :: BotUserView
testObject_BotUserView_provider_15 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000000000007"))), botUserViewName = Name {fromName = "\53749\ESC\136888\EOT?\ETBHbD\ETBF\EOTN\1003616\95857b\21463]w!\EM}\NAK~\1064980\1092128\ACK\1064403TY\52681u=p\SO\1068031"}, botUserViewColour = ColourId {fromColourId = -5}, botUserViewHandle = Just (Handle {fromHandle = "319c7ia0u9ipkge9jfvu_h5ves24gj58k6qszmni9us.44k87e28jh_w7d283yxwxa0hunrg_6wj5ctqmb_czo4uj93_yl6pcww8w21"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000600000008")))}
testObject_BotUserView_provider_16 :: BotUserView
testObject_BotUserView_provider_16 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000400000000"))), botUserViewName = Name {fromName = "\EOT\149551#*\SUB\983947\ETX78\FS?z\150800f$=\19359`\t2\134731\DC4\SOHqP*\1042834\&4K\ETBkY\54989`\GSZSR\40097\a\1069135\189807\FS@h\SOH*'\149238\DELt\"D%\1104684\EM\26726i<#\NULQ2p\1111620\CANH\1030591/\20894\&1I\16189\SOH\GS5a\1103929\SUB\166168H\t3\1036567C\DC3?\54526$j}n{5\1009344`7@\998553\24248},m\149570j\vK&[\999306\1000132WbY\1094096\"\ESCq\178677^C\SOH/@"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000007")))}
testObject_BotUserView_provider_17 :: BotUserView
testObject_BotUserView_provider_17 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000600000005"))), botUserViewName = Name {fromName = "\US/\ETB2\1016213V!\26245\5518x6Cs\181412rLV\DC3\ENQn$\1014668rV\1071109\EOTyPuW\RS\61605\345\31640"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Just (Handle {fromHandle = "i882pae3e51tvqqwwm7t.pi9zros-pjbrg.4u5aogu.y2w2slpm37nqkrq56d6hpb-4l369r2j7esb2th_x-vm25tqv_2s84t7m3aywi0bq2tantll6taet6z7eno-jvrg5w3l3disy440cuf.43j1.fkwvha"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000800000007")))}
testObject_BotUserView_provider_18 :: BotUserView
testObject_BotUserView_provider_18 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000400000007"))), botUserViewName = Name {fromName = "YP{@\ENQ\144486\126118\21442Z\"\SUBeO\1041509\&5\ENQbE6\1048446\NAK}\7442A\160447\SOH%C\97621(\100651\rn\984499lw\v\"\140703da\1071066Hj*p,\8232WGDr\984355\156491\1039263\ACKb\ACKva\1077981\CANC>@\STXZ\1057260L\1012757^z\1048507bb\DLE I\\\SI\GSi\1022157=z\171824!\byQQm4:=*\SO\1048730w\CAN&e\1063625G\CAN\nT7Ll)\26463\22250\1055675|n"}, botUserViewColour = ColourId {fromColourId = -8}, botUserViewHandle = Just (Handle {fromHandle = "ax5z05-u9sae3lvinv3-vbmft2idm.b"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000600000008")))}
testObject_BotUserView_provider_19 :: BotUserView
testObject_BotUserView_provider_19 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000600000003"))), botUserViewName = Name {fromName = "\1027958I\120524/\142466fM\NAK\1110666\992795O)a\ENQrt3AL%\a\DC2`)\NAK\DELj"}, botUserViewColour = ColourId {fromColourId = 6}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000800000000")))}
testObject_BotUserView_provider_20 :: BotUserView
testObject_BotUserView_provider_20 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000800000008"))), botUserViewName = Name {fromName = "1\999761\132396\NAK ;\160049Ib\148018\1087770\66894&,7\DC2\1046061kT\14470>dY\157630{b\ENQ\63266r\52473=8y\SO\EM\190750nXct_G>\EOTibj\"!Z\68779Yl\NAK\1023291\181002\1037025.\100525r\SYN2\1030588\139936I>Jt)\1034375\122899W\1106971]\1089175\145951xDB1"}, botUserViewColour = ColourId {fromColourId = 8}, botUserViewHandle = Just (Handle {fromHandle = "49f1xe6..kq-85ec3d8q9l6xos538of4x1jp0ccgjj.c3ep32tsg_vriw.i-9ftrp69xs9sjmikxc1otrsbn57dvewa69a7ln7hhm0flku0zgibm3.wnru289mlpv01pvadt-pc33wm13xj5hungd7z24mbz3xg-pypyk7iyvudw23bl-s8pytr0nop9wv0-uivzy"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000700000003")))}
