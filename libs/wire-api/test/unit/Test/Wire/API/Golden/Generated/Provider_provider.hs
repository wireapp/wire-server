{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Provider_provider where

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
testObject_Provider_provider_1 :: Provider
testObject_Provider_provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000003"))), providerName = Name {fromName = "a\DLE\1002155\51633Tw\997723}c<vK\20441)\992844"}, providerEmail = Email {emailLocal = "su\nE", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "|"}
testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000600000003"))), providerName = Name {fromName = "A=\US_!V\989804Kx\22362LN\rE\1024054\172923\1025149O\53495*\NAK)>\1082381\SOH]\96759G\171520\165838\ETX}\161723\b\a\EM\ENQq6pF\8420NB\GS=-Jz4UT\ESCddjKh\CANy\156627\US\34489e%\1011270\&5)nG\178366EiBr\a%pj\11650\59255"}, providerEmail = Email {emailLocal = "n", emailDomain = "\138279ve"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\DC4"}
testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000700000003"))), providerName = Name {fromName = "o\143207\35324\44306\34180\185710>;\f\100436\997151\1044777\EOTp\92883D\a\vqUBcq@$ao\1097474*\992799R\1095174e0p.5^~tp\DEL\\\fzu\43961\RS$i\54555RRb\SO*E\DC4\ETX\fX[2TX\182094\1082681\DC27r\74067SsYt\NAKdi^\ESC\1044265<.\EM\13875G|iWMA\CANTY\1087623L\ETB\1039114&\1029502{*bGa\143494iqSj\b\182971KK\ESC\1105737:9CF\164879\&8"}, providerEmail = Email {emailLocal = "", emailDomain = "\SUB$\1012881\bZ\FS"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\128580\160215]`x\152710"}
testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000600000002"))), providerName = Name {fromName = "$X{\1110059m![E \46452\SYN\998260Q6Pm\\\NAK\1071734:2T\1012932z\DC3\v\EOTZ6\r\15297[7/\1027367\983473'\1011116/\36743\1100138\EOT\1063942\1015740fW)aL\1040514\&2"}, providerEmail = Email {emailLocal = "k0\4709\140616\DLE", emailDomain = "X"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000700000002"))), providerName = Name {fromName = "\985838@\133234\15101n\162265\120307?\40111cE#t\1004166\1060407ma\155807M\US(\1100498CJ\35945VG3Y8\r+\137457\ESC\ESC"}, providerEmail = Email {emailLocal = "\EM\r{6|\DC2", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "[Y"}
testObject_Provider_provider_6 :: Provider
testObject_Provider_provider_6 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000600000002"))), providerName = Name {fromName = "\183780(F<#^&\1027183=\CAN\DLEV\1087769[\9134\n];-\58420|G$\1099141R\DC1x#MO\128625G\1015841\DC2Z\187619?\DEL5cno;\170111nF\994683\1052934|\FS\CAN\1016716RY+\74426\ETB\178484\RS\1009898V\190788\NAK\990840\129559=\nmH\ACK\189741l\144211{\SIQ;7z\1032305A\92368xG\989461\DC3!]*RG\1020660;\40411\1087700c\1098432a\SUB5:~,\ACK*\1008324Q\ESC\fE/R\SO\10773?F\143044\95777\1041417F\1060258"}, providerEmail = Email {emailLocal = "[\SO\1050812FA", emailDomain = "6\SYN\ACK<b"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\995596"}
testObject_Provider_provider_7 :: Provider
testObject_Provider_provider_7 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000800000006"))), providerName = Name {fromName = "\1055053V=\1040528j\5691\1019671Q3\1029805:\1082354d\DC2h`er[\SOH@|Gy\NUL\FST/j\1097639\95190\&6~\b\CAN\187438v\CAN\DC41p]D\NAK\1002180v\30332F\vU\17449\156161\13763\133633%5\NUL\999564x\CANHY_\58952l\FS\984874\96875\&9>\173382\1045632r6r'{4V\US\177886g\GS_ryd\SOH\60115\SUB\n\1087733\GSM^e$E\186561\CAN\176530o]\157268\&9;2\SOo-7G\43459\78073\1094629\ESCB\1091250$#\ENQ*\169360,/"}, providerEmail = Email {emailLocal = "", emailDomain = "\1031305,\GSj"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "G\4030\SO\1000731\183726"}
testObject_Provider_provider_8 :: Provider
testObject_Provider_provider_8 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000300000006"))), providerName = Name {fromName = "\1079780\1020456\ETX\165807%\191340\&4\1065904\1112964_\1112743B5\t\DEL\144873\1066026\5743F0\"\1068466a\65703<\r\32040\NAKbV\1096220\1032753(3M\1059620\1022668Cj\SYN\5532JPZh0*\988113A\DC2<j\DC4\b5\155970q\1012276)\a\n\a\EM<gti\1054339]8\bC\1009079\1025118-\\\FSMd{k\NUL\1032079\131794\NAK\1006982\139466-\SYN\1100290\&5\ETB\SI(\1011055Ea\78668\SOH\1069538\1024542\1015608X\174842q\1017673X!\DC3\SOHS\1106586\156845\166355E[G8\1026054Y\SYN"}, providerEmail = Email {emailLocal = ">a", emailDomain = "\ESCp\SO"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\DC3N"}
testObject_Provider_provider_9 :: Provider
testObject_Provider_provider_9 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000500000003"))), providerName = Name {fromName = "\ESCz&\99210\158030\185397\SI\1071367oL2b\RSXn\27963OD_Wl\DC3b\NUL_-^f\RSJq\n%\1103779\GS_\53267l;\1086424W<8\62077 Aw@\100822\97968`\1079458uM8\46320\46801\"\SO\US\1002429_:`6\tF@\ACKI1\156587=\1107093\NAK\988840Z5\1047962\&5\DC1\NUL\t]U\990976\172856d\ACKo\24290o\CAN>}d\186706\DLE]\97037T\ESC\194881\&1\1074683(zj9m\1086362\DEL`g\1089419C/E\1108721\179055\996253U\SOH"}, providerEmail = Email {emailLocal = "", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "`q\1065452o"}
testObject_Provider_provider_10 :: Provider
testObject_Provider_provider_10 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000200000004"))), providerName = Name {fromName = "\42181f8bht\SOH|\119266E\46799"}, providerEmail = Email {emailLocal = "#\SOx\ETX", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "HL6e\1101173\r"}
testObject_Provider_provider_11 :: Provider
testObject_Provider_provider_11 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000003"))), providerName = Name {fromName = "\36404\1048755\1067327\SI\1012547\78189\1098493G\STXD\DC2Tf\1000918*t+\5582%a\92620\1018849\GS'/\1015823-\ESCKx\ETB\FS\CANb\92522\STXyg.\DEL(Ge/>B\155668\NUL\ENQ7\154754\1032435\SYN\1071798\1083287\nT\SYNX \1102520Z.1"}, providerEmail = Email {emailLocal = "\1090687eKu", emailDomain = "M4"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_12 :: Provider
testObject_Provider_provider_12 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000100000003"))), providerName = Name {fromName = "x\NAKm\1029602\CANT`&u\DC2\44410\30805\132844!\ENQ7\nw\ACK\SO\13211\131307\SOH\1088913<U\1720g9m\186826\1113557P\1023532\13920\fGC3\EM\SI\aaT\33002\1084847\DC25t/\146499\60396G\189822\&8\7331$\SYN\\T>]=9\1020300(\64944\ENQX\SYN\FSk>XU\119979\65571EG\151064k\STXj\ESC\GShZ\DC3f\DC4\SUBT{c\175754\1003261.M\FS\DC4\ENQvuR9"}, providerEmail = Email {emailLocal = "le", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\SYN"}
testObject_Provider_provider_13 :: Provider
testObject_Provider_provider_13 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000600000004"))), providerName = Name {fromName = "lifvi\1090795\5264\9247/\ENQ\ETX\CANyki\NAK\1108292!v7\ETB"}, providerEmail = Email {emailLocal = "o4\RS", emailDomain = "\\w"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1065629\SUBFz\1007997n"}
testObject_Provider_provider_14 :: Provider
testObject_Provider_provider_14 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000600000004"))), providerName = Name {fromName = "lu\ACK;W<\USt{\ETB\1061189*7\n$\SO\1010582l3\999746-\ncSt9\SO~T\EM\SI9?g\SUB\50803O:N_b]\DLE\152650\&1\ETX<\1106912\STX\994370x\35546"}, providerEmail = Email {emailLocal = "u\DC1\5551", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "hn;"}
testObject_Provider_provider_15 :: Provider
testObject_Provider_provider_15 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000300000002"))), providerName = Name {fromName = "oDR\DEL0\"h\54166\"\184067k\53059\EOT\vt`5O\1062041\18853\DC4\r&\GSrgj(\1087467u;EN\1093948:\DC4tvi'TW\68805*\1083207j:z\USCF;\vQ"}, providerEmail = Email {emailLocal = "", emailDomain = "\14317"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\STX\983293\SYNGS\US"}
testObject_Provider_provider_16 :: Provider
testObject_Provider_provider_16 = Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000000000001"))), providerName = Name {fromName = "0\SI\98297#\1065843\173859Pr#\176044\ETX\1093134\1037929I}d\EOT)\92672|t}!x,F.'x}V\999198;\aX\1027644\SUB\1083430\t\1106185\&4\98467PD\165060\CAN\ETB\rAb~*f\1023834\158667\\\DC3\SOH"}, providerEmail = Email {emailLocal = "a", emailDomain = "\SYN\FS\rMX\ACK"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\ETXfw5"}
testObject_Provider_provider_17 :: Provider
testObject_Provider_provider_17 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000800000001"))), providerName = Name {fromName = "%\154141U*\1055223\DC4\bU\34208\ETX\168637m_9%\1075656W\DC2\ACKsdT\"(\1109158%u\EM\NAK:W=\1044553S\DC3T\GSDB\111256\ETBT:r \1106178Sk!r\EMO\157774\"*.>M\991836e|\173322\991334K:>\151345RpkcD\1062285)&D^-\a}\32547l?\\\136927\SO\CANUk'_O\96062"}, providerEmail = Email {emailLocal = "3eD1", emailDomain = "A\178171F0<\NUL"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "q~j\1022527"}
testObject_Provider_provider_18 :: Provider
testObject_Provider_provider_18 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000400000004"))), providerName = Name {fromName = "\19530(M\1036320"}, providerEmail = Email {emailLocal = "F\182273I36+", emailDomain = "?\ACK\139684G."}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1009985"}
testObject_Provider_provider_19 :: Provider
testObject_Provider_provider_19 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000500000002"))), providerName = Name {fromName = " hH\1100961,\1063364\DC3us^*\ENQ\SOi\ETBdG&M\SIp\154362\833\52398\SI\US\1057707\1017768[TD\131579H\1105446\1068986"}, providerEmail = Email {emailLocal = "", emailDomain = "Wj\a"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\SI\DC1@"}
testObject_Provider_provider_20 :: Provider
testObject_Provider_provider_20 = Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000700000001"))), providerName = Name {fromName = "\DELfY~nt\NAK\1113690\aWl23\40971,\1113213\1094846l\94582gGj\DC2\STX\STXBMWw\DC3\48524&)\RS\133278\18990^y@\b\1081082:\r0D,AZ\31552\1019084\ESC-]\4372R\59177#c\1009888\184371\DLE\63361\67291\95741\188162\STX{M0PS@S\f\1108365{\26383y\983320m\FSZ\1080829\NAK$[(\NAKb\DEL\45631zErZ-\DC4L9$"}, providerEmail = Email {emailLocal = "M\SUB", emailDomain = "\12240"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\"=^\DLE;"}
