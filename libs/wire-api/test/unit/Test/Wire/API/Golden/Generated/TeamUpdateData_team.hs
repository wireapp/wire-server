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
testObject_TeamUpdateData_1 :: TeamUpdateData
testObject_TeamUpdateData_1 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("M7w\189507\143540h1\DLE\44616_+f\SYNo\SIQ$0\1026627$\US\USHT\EOT!kh\1014854N\166619\1086634/\21816\ETBt\40207s+Kl|l%fh!\SUB\1062651\DC2\1089227\NUL\bF\t\1080021\&3\27485\158810\DC22s9d\174496*=&NW\1094087\NAKd\"\US(C\nr\SI\"&RH\DC2,\64433j\1022574\b\917617\DC3ts{G\180799\168400\DEL4PB\24254\ESCIvQ[C\986236\1010009G\143330\21427\a\30188kps^C\ETBl,H\SYN%\165012\1050053X\67304*p}\34667-\17968\SUB\1005111\990819\1035986a\NAK\SUB9F)\NUL\61426\12394WH`/#%\"j\24991(\DEL\1088076\RSr\RSj-C7%\b\SYN\STX2SX\190731\1063304\&3\t.\ESC\DLE4<9i\SO{d,\NUL?\1013834OnU\SUB#g\175135\DC2\2328\12278`\1053445\DLEuw\1071740\1042871\tSd\DC3C2\43328iC\US-\1083820\t46F\b\ENQ J/\1100728\146180\n7\NULT*\54083jJ\19005t\1027540\tY")), _iconUpdate = Nothing, _iconKeyUpdate = Nothing}
testObject_TeamUpdateData_2 :: TeamUpdateData
testObject_TeamUpdateData_2 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("'\134901\1091279:\47649\SYN!\r-\1061131\US7\149615\1042228\183050D/u\1048119<\995893\47684\DC2U\STXpU\169277\DC4A\33915V\f\GS\bkwU\DC4F\998203/\127090$'\DC1J\t]@\174443\ESC9~\1004087GKO\ESC\24088\1006293\1006911\EM\43063\1012094\163231A\f\46087\DC3O|B\FS\ai\73837\NAK\t\990171\SYN\DC2%Ta\FS\463\64702\61517\1087999\SIx\180749\1022215\40634\CAN\SO+[V\20954)5Yk\1035284f\184662b\992942\US)N4]O\83488\ESC+<R\1109957<\US8L\138259,N\1091477\1015025\US}mH\1045838r\163041Q^\STXb6\1050875\28687=y\59102\1027974\EOT\bm>\92506")), _iconUpdate = Just (unsafeRange (">E\175671\1086978}\DC3v\1025153\&7\r\149010\1047366G)\179447\18845sH\SOAHgs\ACKn\988093\RS)W.IAT%i$\ETB$\46720n\b!2\r\CAN\132857\SYN\20861k'\DC3P]\1067648\1080325Y,Dnwh-\f:Ex\1068116\1011245t\1064951T4\EM\SOH\1010208A\DC1{\183315kKQ\157985\166253?T)5[N\v4)\13914\&9\CAN\1055152i\148989\150320Qtt,r\DC2Oo?q\DC1\DC2O}\1088397R\8270a\30526T\DC3$a\3139\1069797\DLE")), _iconKeyUpdate = Just (unsafeRange ("o\RS\1001937\&7\"\139420:\1104807\"R\731\a\STX\FS\14477\1108152kcUj\1005077\1089438\ENQ\32587\ESCe]ha \1071976_\1092434Z\1014765\&5\r\1112529CW\12436Z\153597Y8<4nI\DC4R+hS\SUB\1010227\1054057c\ACK]L@g.\1058305\DC1nJ>L\1098722n/\STXc*\GS&\b=\CAN`_"))}
testObject_TeamUpdateData_3 :: TeamUpdateData
testObject_TeamUpdateData_3 = TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Just (unsafeRange ("\1111813zGO1Mm\ETB\DLEs\1040466>\99947X\fbG\1046075\179426\ACK)d\GS9\1058702\FS\rZ\SO\RS@\136159\ETXE\51784];;[=\99865=o\1067128\1074044E\SOHUr\22668(N\54802\&7\1030488Qn\66852k\1039773\92612E1|m=FZn\64501\ETB\1036042O@C\SI\64569\1039276\ETX\171135*x\SIp/wW5\139291A\1095574y")), _iconKeyUpdate = Just (unsafeRange ("\SUB/\32899\EOT\RS$X\53982\1084880\1004475\189789\143230jt:PS7\NUL\1084681\4730\181891\1108186?^\EOT?X\ESC*km1V\SOHj\t"))}
testObject_TeamUpdateData_4 :: TeamUpdateData
testObject_TeamUpdateData_4 = TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Nothing, _iconKeyUpdate = Nothing}
testObject_TeamUpdateData_5 :: TeamUpdateData
testObject_TeamUpdateData_5 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\43073s\1083994a=\1009148\SIo\t\GS@\1068702\&3\173466 C\"y\f\183084\97400\f\1025738_E\143169\1870\1082356~\ESC\992848\32512)\1011628\&8\154922}\1071867\SOH\63714}\ENQ)mR+\92464\21847=\1038627i=\1069291\"W|dx\FS\STXC\991097\RSd,xk\SOH\177066yf\DC3fIs}M\1000995\DC1n|'0An \SOH\f\50430\EOT;@\1073856\144915`m\16896\188997\ESCnA\ETXB\fh\f\\\1030449c\SO/\STX\3856\tM\120198$\SOH\SI}\72257n\ENQ\ACKmQpv-a\1077792BL\169497^\b\168182nk/RC-<\aF\1054173v\1096334(Y.s\DC1ZP@\10114z*6l\n\137075M\1064826=\10398\177123dkWGvT\61345\bG\NUL`\1002441YV_\180829Qu#\SO\16710\178311x\157846<RP\154091n[\ESC\SO\45903p\ETXi\162714\GS\188429a49j")), _iconUpdate = Just (unsafeRange ("C hoff\1062418t\61661\1097529I\USE\FS;H\989578\DELt\EM6 \1053564\142585\1031203gKx\998450*W\155761i\169505f\118954IEW\11291~!\ETB/(f\DLE\1004805|-\"\998892\\\111030Y\30926\CANm\f/#c\b\136465V\EMqKr1\"]\8341\to\t2\1078962T\\\1099335HEV\\:\28609e#9\US\RS\STX\1092800|1YQV>\983223$s{\154575-V$bE7)\140077;\SO\r\1107990\&9:\DEL(}h\SYN\45886\br4\DC2m\10889%\tIsMD\1009715X9[\160002hxNr4\184407")), _iconKeyUpdate = Just (unsafeRange ("\995654\ESCP\t0\n1\175871\1103574Lq\v\NULTcIIc)\1046854\98759\171078\ENQBg T\DC3\vn8\1109360P2\1010459;\94787u2y\96078,Oq\166947\SYN\SO\1025014(N\GS)c?6\DC1~M\USNV\b\t\166518Y\164619J\GSAM\167125\1046778\161511\NUL\ETB\SUBX\EOT@\DEL\US+{\1006978\176078\US\ESCApt(h\DEL\ETX\985314S\1109970j\US+|\1000266\1004970\1094913\&3\1046271<\171481g\989492\1007239:ma\v\154730\176333Y\96949\167780\SI6\USG\1068641\33997^(#}\DEL\vE?'\1051734\1062938\1029634\r1 \1069267xHKBT\bKE\DELB\ENQ\45141O\FS~i\SUB.\FSF~4\139447X&\"\ETB\DC2\NUL\1056000\26069\1287>\f\1060305\5193\SI^aNr\176387\t\ETB 5\GSK\DC1\173034P\SOHVJWe&\1036485\"\CANIpI]e\1110163\998846 )\47898\SIl\1113650\12274\&1"))}
