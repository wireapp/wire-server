{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where

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
testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "\1040766D\1089170\&4[*n\1087389p_F\t1^bE\a_\DC3G>aaRG\186804\"gc.(\1096363\RS\DC4#6;\nB\60100M#\49487\125250\54667B\1038580TTs\EM8\GS&(`<\138312\DC2!\999964\SYNivy\1074157\1087200\a\69911>\1016794\985406\1082669i$\SO\1009816{"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "K"}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1090680"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "$%\1050055/V*;r\1030616g\1107538XJ\v\1000455w\SOH\995560S@n\ACK\157213\r\4719\24575\DC4\1090027n4\DC2\1104130CX\1022192\142086J\1058710\1043491*\166451\b-O\SI\168256\ETXh\1053226\157755\137262XHh[A\1031896\t\1110202URv_\DC2P)gu\DC2\DLE*O \DC3\1099186Vo\991168\SYN;3\1064079?$N\SOoV 2,\SI\tJ\1111480KBa\b'J\1088011\f$%\142342\US\ESC\nW~\1059166\136074\1904Tr\1066289qC\39102\180274o\1090141\&8\1087546o\21684\EM"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "6l\53513"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Just (Name {fromName = " \157635\DC3\1002581\97868;\t\134576$puL\1012520\1023305\175223\146975\1085876s\19287Z04\1027703R\NAK\FS\f5K\DC4\987217\&2x\DC1\US\1033930:[cd\53518dLj\1099905\1030795\&2K\DLEyB\1109138\&3;y\GS\SOx\73928\992835\STXQ\SYN\SUB\DC4<\163381\\\182507\DC2\ACKo\169152\&0P\DLE8Zil\73441\&2b\135092f\DC2\rj\DC4X\r:\DC2\ACK\1000168x\983606\v\NULM9>\151836E"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\DC4R"}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "80(K\14884SP4\1029089^:p\178141\25282T1\1032798D`l[c\SIT\1042577\DC3L\t\1073277\DC1\n\DC1'\171104\FSyH\t+9\1008313cWF\1021038~s\bd\CANju=\"\1005311\r\1040175\DLE\1110914B@\DC2 0\STXT7\1049182K\FSO\n,\DC2qDQ (\\\DLE\SUB\EOTep7\1004332qv''\1112783{\DC3@My{n8\18605q\998345#Hgd\189911m\39044A\flS?uh4\139673\&4Hz(\nv\34756"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "S2\STX\DC2!-\ETB"}
testObject_UpdateProvider_provider_6 :: UpdateProvider
testObject_UpdateProvider_provider_6 = UpdateProvider {updateProviderName = Just (Name {fromName = "\18122R\"#?\EM\EM\1062531\ENQ.\138713\&3\23737\992462J\f&\CAN\DC1\144992\DC3:\SO\SUB\ACK`4y\ETBA\154107;g\ESCtT?\1061034\ESC\DC4r\1059465\989948\1025214{\1070242n\DEL\EM\29027e\STX?\GS\152292d\DLEQ"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\52917\SOH\8912\13513Q"}
testObject_UpdateProvider_provider_7 :: UpdateProvider
testObject_UpdateProvider_provider_7 = UpdateProvider {updateProviderName = Just (Name {fromName = "\138888\41406Fr'\984547x\FS\bAYs5\DEL\DC4%M3\138325O\\\fA\69810\&5\137132\57442W\SO\SOHI\r_I\1068415X\990157GL\1030211\1028087;n\GS\EOT-\1015430i"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "z'\DC4Ed\"*\FScr"}
testObject_UpdateProvider_provider_8 :: UpdateProvider
testObject_UpdateProvider_provider_8 = UpdateProvider {updateProviderName = Just (Name {fromName = "WCR[|\1020790n\DC3\r`iO;g\141678\1060690g/By0-D\EOT\150961\175634\EM\EOT"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_9 :: UpdateProvider
testObject_UpdateProvider_provider_9 = UpdateProvider {updateProviderName = Just (Name {fromName = "w\100272Dw\48258an*\47662\NAK\123187ez\45348\74561\US\ENQ\EM>j\DC4)\151497\129510\1017814L3\DC3\DC2Y\ENQ\CAN\GSQ`\FS\ENQ\36569!\164306J:\1053518W\DC2fk\1044476\1062365\1013689\SI\171513\EOTHg\ETB\DC1Tl\NAKtV>Z\41368\&8\1107632^7\172840;A2a?V<\FS\ETB\1056782v\1090749'\ETB\DC1D!MU5\ACKX\176664h\DC2\161325\118793\ENQ\EOTX\rlM\464j\167698si<{\STX"}), updateProviderUrl = Nothing, updateProviderDescr = Just "G_I\b\1003088\12212\RS"}
testObject_UpdateProvider_provider_10 :: UpdateProvider
testObject_UpdateProvider_provider_10 = UpdateProvider {updateProviderName = Just (Name {fromName = "0z\NAKc}1J\1051344\1042970\&1)\100405}\t2`x\1007109\DC2\SO\94894\1100600\&3F^\1038708JP)\ESCzj\DC1C\FS\4920Eh\163824`\SYN\1063754DU\1052144dFF\1067422\68362\14449\1096189\&6\DEL:9\1064660Z\16158q\EOTW\DC4\157796R7\96246q\1010450\DEL\n4\NUL\NUL\995015&gF\51731iG\\6\tg"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "@,\1096026GB\DC2W;\1088033"}
testObject_UpdateProvider_provider_11 :: UpdateProvider
testObject_UpdateProvider_provider_11 = UpdateProvider {updateProviderName = Just (Name {fromName = "\9865\194604\CAN\10763\UST\RSj\1042092\ENQU_^e\1094811-\1029200\6295R\RSx\DC1RBN\1104136\1034644_biH\1050841\r\USt\DC1\1111529\22133n\21624%\997710\NUL\US\129508T\1094411\990354\1100409\SIj4\DEL\1012166\10139\f\EOT\ENQ\SYN\GS\166361\"X\EM]\20642-\1042347A\DEL\ENQQ\149407\GSL\188345t<\178158Z\154981\ENQ_?\1013514\1009820rX\DLE\984431\GS+\1057953\191130\&2.Ma\99177\"\ACK\SOG*ZuX/`TM\94016\182062\162526"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_12 :: UpdateProvider
testObject_UpdateProvider_provider_12 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Nothing, updateProviderDescr = Just "\30677(2\47907ES\145428\DC2c"}
testObject_UpdateProvider_provider_13 :: UpdateProvider
testObject_UpdateProvider_provider_13 = UpdateProvider {updateProviderName = Just (Name {fromName = "\32056I\SUB\138015@\1005979\FS#\SOl\1033825F\153101;\1039684\161341Yj\1050110\140145\nn\72144\8087S\1108330\1000861x>\1041884\155849#:\f\DLEQT\bt}\29576\1076650\&8CI\fN\"{)\1072260\1051841w\17134{|\52428\74766:\RS\DC2 e\SOk."}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\SYN\SOHh{%\31682\65863"}
testObject_UpdateProvider_provider_14 :: UpdateProvider
testObject_UpdateProvider_provider_14 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\r\26991\185053YJ"}
testObject_UpdateProvider_provider_15 :: UpdateProvider
testObject_UpdateProvider_provider_15 = UpdateProvider {updateProviderName = Just (Name {fromName = "\v\178867%\1012069|Yvu\62749@\23566a\1046485\SYN\36526){\133479\1033196\1071278\150276dL\131209*\ETX\1058836W;k\"(3\ESCd\SOH*9'Oj\180685?.\190147%j\1041610\128403\&8|2\119029g>"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1049180\DC1c"}
testObject_UpdateProvider_provider_16 :: UpdateProvider
testObject_UpdateProvider_provider_16 = UpdateProvider {updateProviderName = Just (Name {fromName = "b\147362e\191014\SI\GS\148283-\SYN\990998-K\ENQn\ESC~\151634\RS\EOT\983543\132514\1065273*\120358J2SCu\170978\ETB\ACKu\1059089\&7`"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\RS\DC1\128756j"}
testObject_UpdateProvider_provider_17 :: UpdateProvider
testObject_UpdateProvider_provider_17 = UpdateProvider {updateProviderName = Just (Name {fromName = "lU\ETBg@^"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "q\NAK"}
testObject_UpdateProvider_provider_18 :: UpdateProvider
testObject_UpdateProvider_provider_18 = UpdateProvider {updateProviderName = Just (Name {fromName = "?\1032957\135959_;\DLE\f>qO\173610OIn\74605\ACKk\GS\1064823jN\174283\EMsj\"\n\1044097\1061476\t>\USiH2\US\SYN\1035929\48521<\NUL\EOT\185970AT\vj\ETBc\173498\SYN{\1093570HD\RS\1009198\1091621\&9V\fxyy/\SYN\STXz]ZWs\1096439\151994\167553\RS\1057709\168502q\SI8N\1042505\SOH;\GS%G`j\DC1ILQ,\GS\33420\SO\ETB\SO\DC1\40102\94252\1062873\97000\&0\987413\SOH\DC1\STX\STX23ZW2!~"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\29469s\46046^\1100263\51478]\n\134393"}
testObject_UpdateProvider_provider_19 :: UpdateProvider
testObject_UpdateProvider_provider_19 = UpdateProvider {updateProviderName = Just (Name {fromName = "\US0\40065\EMF%\65673\DC45Wq\1113015r`uR\RS\DEL&\STX\1107001%b\DEL\60354\1098186\&0\170909\CAN\1022568\1089029\1093821C\DC3\b\55220\&3\"n?8E\CAN\1003206B\ETX/\DC3!EWdP\SUBLO9\SI\ETB\t+\SOb\26989On(\121098BDCo\160785~L\26871\f\CANW\186614\&9l\ENQEXTk\a\DC2Z,gc?8\18635\138882[\SO=5zV3\142382,\1031327a6\1104983\35783yB"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\FS\126236Y\152246K2"}
testObject_UpdateProvider_provider_20 :: UpdateProvider
testObject_UpdateProvider_provider_20 = UpdateProvider {updateProviderName = Just (Name {fromName = "\f\2530rl%qm\1080264h\18567*Z\1079543\t|\ESC\RSJ\DC2\1002509)c<\STX\EOT]\1021902\aw\932d9et+C\SI\1105717rLb<\174267\61453]CK[\99056\1098319\1057044_.\SO(3\1104358\4907\1068327\1054643UZk(\1108995F\20543>"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\1032727\172007\160458>"}
