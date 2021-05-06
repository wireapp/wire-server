{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamUpdateData_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_TeamUpdateData_team_1 :: TeamUpdateData
testObject_TeamUpdateData_team_1 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\CAN\149699\GS\1041394 \144711d\999663\&9\140226Jw\\\1099699B\a\35920+\1071003// @\SOH\DC1\NUL\44646W\DLEh\1007733c=-\1045682Z\ETX$ac(/:KKMp\GS2u/p?\ACK\1072387\"\1088592cT|\21614\ETBS\ETB\1110267'M\DC1x\ESC\44786\1016541\1029232\ri")), _iconUpdate = Just (unsafeRange (">\143877\&4\33143?0BI\1006612\160090D\ETB\DC1z\36556~_\8779)zg\SUBq\ETB\151365\ACKRvoz\1089897\ENQ\t1\1088069AD\11249\SIA\SUBR/\18740\154163\121334\&4~I)|\NAK\97384\SO\15856<\34863\&4\992449z\DLE\1098598\DC1l\1109992\1049943\22004:\1059633{\99355-<K\160160lRu[\8538x\1033071??'-\DC3uyc\1047943ShB\SO*!\143\1079674Gr\40333Y\998703\SI\1025380\33037TsG\1085321\1087223\1002304\51613\1062246(4A8'\US\165816\138417\SYN\1076245\1093976$\35118\DC2\5514\v\EOT\61618\38896Wv\STXbH\SI\137003\&3V\b\n>#\CANH\b+f\39901!\1084773k\SI\985659\&1BA;\150378\44645\GS\51957\49200o\ENQJ\45284f+LB\SOH\188985:\NUL8\19546\CAN\DC3\1057144\135138\GS\172999}}V\97008'[\b\168540R\1046495\1003822\152449{\987048\15451\ACK^jFK\SUB\1018255;@\NAK\187838y\132383c\DC3\STX H\CAN*")), _iconKeyUpdate = Just (unsafeRange ("},LEL\1047599\DC4gK$\SO!'#!\fuu\1063958\n"))}
testObject_TeamUpdateData_team_2 :: TeamUpdateData
testObject_TeamUpdateData_team_2 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\1024376\SOHS\RSd(\995631\f\142944k.\SUBX&(^g\r<>Zb$\1079206Bz\129337\n=mAZ9\NUL\RSE7)Y\t\1006602\&8\DEL\SOH\ETB~J->\STXB`J.\"\1018028p`\144255\FS")), _iconUpdate = Nothing, _iconKeyUpdate = Just (unsafeRange ("{d!$\EOTK\DLE(nL\CANf\\\47773c\1102778\1089868Oe\1015584\153374$\190477[\EOTg\33393\&2IbH%\ESC L\64686T)jH&\34658*?R\t\171199^\1081269\992316;]#lX\DLEp\36665\1112468\74571\46767a<\30490\DC1_N\173861/\DC4!\16291s\1062515+-D\1014496F2IY/\44001$\SYNT\12508>\44649U\ETXrP\STX\1049230-\1022508f)\NAK\1013633\5738(A\1043988S[,\SO|\GS\154388$\EM\DC4"))}
testObject_TeamUpdateData_team_3 :: TeamUpdateData
testObject_TeamUpdateData_team_3 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("&'t7\DC1\ETB\1101172J\30134D =\152122bH\b\61921\ETBA4\1029670\983422.u\ETX\46532\169176\t[~Q\1004386L\NAKjSu\154129R9jZ\DC1lyn+QY{\40816U\46663\1033599\SOH(\NUL\140545\144029;\168283vN\v\1015922B\1098693\182290\17494\1025194\1021995[\1000840=f\ESC")), _iconUpdate = Just (unsafeRange ("\148555\n7r\a\25319f\t\1068196MFV\1057131\1019158W\1069634\STX\147498\&6&\b5\\\DELI5Y`o]L(0T\28912\&1?A-ubbUp\1106341q'IiaU&])\1026737!D\r\16280\ACKS\24367P\NUL98&@\67286\DC4\FSVPK\SIii\t]\DC1Q%\1003629~\t\148420(\31348H\155609\NULnV,#rv\1038968\DC1\1001265\166999s#\ETB\fx\5966U\ESCAt\158056]\1081566@^Us\fl\33741V\1028550)\EMy\DC2)\EM\186179\156833\DELNm\1044659hpP\ACK(N=\STX\95869^\1073378Xx\FS6\176560\&1)\983693:\ESCE\57499\t\1088581*k\12980H\ESC\CAN\DC1K}\CAN\FS\1053658H5\fE\r\5003\NULc\15697{\144932 \190938\"P*\160205KB\ACK\f\1000622?\49291\US.T\EM\1083920I5Nl\f\b\EOT\SO")), _iconKeyUpdate = Just (unsafeRange ("\120803;dUu2\ETBmT+Po\1110754\20176+\170305M\1083050B\1044612\&6\140724k\18559T`\49760p\NAK\NAK\DLE\DC1E\DC17\187872cn\6166\rn@K\1062534\44061g\1640qP&\14613\165661\&5NId\1078443hschGb\US\142067&2\NAK?f\STX=\SUB\158150\1105095ROX\14333\a\GSu]N\138158g\38304%A\1091885\1085353rW\1082135\GS\170179\1002941\&3\tAh6[\1039104-\SYNV\166846o5]\ETX(ar\185366\128546b\60668\DC1[E(y\1018710\r,\GS\3078\GSU\163371\173627\&8\134109\1076098.s\94907\STX\132051Hd=\183089\&7\EOT\1113981,\EOTY\ENQ\1017377U\44271\SIM\1101470\168466\1056278\161782\1027713;vkB*\1105738\SO\35705p\194985hy\GS\1001548\994725C\t!H\1056954\&4h0<7\186245L%1\1082877\1021050\ETB\1110334Gq\1106297\EM\1093945\72709jA/\46714"))}
testObject_TeamUpdateData_team_4 :: TeamUpdateData
testObject_TeamUpdateData_team_4 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\64752Yva\133046gZ;qVIY\132671D\188871c\1066466\1009777\&0s&\154382\119918\1068826t^\37962K85w*`f\26995l\nB(x\b$yt\ETB{\SOH(g\1035965\&7\170549{\25824;!\165925\USn5\98455tsKu\STX\DC2\1002105\1092230\164703<\36028\171013\1074088c\\YSx\n\ETBW\1028486\100242-g3z\NAKmK\1109072\1058296Vf\nAn|\155318\EMs\1070587\rE\DC1$<\NAKeETIsp\EOT\NAK\f=~\99304q\1045522\SOH\1070041\21689L\984934\1008633\&9\FSOXj\1031946\&3")), _iconUpdate = Just (unsafeRange ("F1~,mV\1001903\ak&p*\b&\35095v^\1032899Rvo\SYNg\b]Bl<\1085638F\190245?\60192.\fI#\187138\118903\SYN3\1029653\2347\152496l\145728\DEL%\139821\&9\b,D\995508d*?\998788`t\f$\SUB9\DC4\1102642\&2\18898\b\DLE\995246Y;\DLE\ENQPA\GSt\183669SO\r\EOTx\10876\&3]L\r\1003975\&0ZP\CANH\"}\53938%*}\SI\45517V\74468~N\ENQ)@\1046943er\1023569u'\1109132\49097\DC3c~Wd6\DC3\tt7#\GS.m\1112896p\994856JZ>\136035]|%\1093443?-=\EM\SUB\1052329Hyl\189478Tb_2(e:\f\1035118}C(\DELF?\STX&\39784\&1bX1\146876\\\DEL\119824\166753\&3\1088663}!&=Q\vP\1085072\58681\NAK\1099723\1106720\STXd\NAK\DC3\1044035I~\175843d\ESC:s")), _iconKeyUpdate = Just (unsafeRange ("|\168517\DEL\1043382\63076Cq\DC25\SI\v*Z\145818\&5D9\46097:\US;y\1061706\1030zZ\986657%\3624*Fmt\1060466cu\176217l\1084715\163833Db@w\v>\1001051\v\176704Na\\G1\DC4cHC&[\1070594(lLG \1029819\ETXlN\ETBx\155984\1054862a\35238\132244-\DEL=.\DC4\GS\RS:\\\FS\1098768\&6\GS\194690-()\1066031b\151274L\1060917Z\24989\SOpZ\ACK["))}
testObject_TeamUpdateData_team_5 :: TeamUpdateData
testObject_TeamUpdateData_team_5 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\ACKe\ESCF\1066073Rk\1106726X@\59061\62139\SOl\ESC\1057494W!\12210\b\NUL^(`\1092488pp")), _iconUpdate = Nothing, _iconKeyUpdate = Nothing}
