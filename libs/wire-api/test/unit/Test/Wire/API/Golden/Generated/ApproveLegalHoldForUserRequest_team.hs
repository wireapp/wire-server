{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ApproveLegalHoldForUserRequest_team where

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
testObject_ApproveLegalHoldForUserRequest_team_1 :: ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_team_1 = ApproveLegalHoldForUserRequest {alhfuPassword = Just (PlainTextPassword "#\DC1\97930\t\n,v\1040948.\SOHy\16841\1067393\r<\58601J$pGa_s\37747\1081872x7\1007354B\179985\1030447\1006650\r\1078792o\"\DC2eG`\ENQ9\DC30\150536'd\EMaQ\"SZ\184535\b$\1096399\18178\1105169}\567]vE\1023404\EOT\42573\1018041]\r3\1110842A:wDq\6457%\SO\a\US%J.\145354lx\1092476U\1043856\&1\SI\DC3@\DC3\1012872_\187507>t/<\SUBQ\170804\ENQP\26518t\NAKRZ\155097zJdoO*.\SIOP\NULB\65486/\160842\DC3{!\154963\993371]\DC2MA)A\ve\SUB\65187\FSn\b\177311|\16482\\OYg\DC1_2\SI\US\SI[\187137s{\1048505u\SO>&\133725R;6\NUL\\)\65757\67982=(d\DELs\NUL\NAK\n\niQ\RS\DEL\1081838U\STX\1112672\n+\67142\1051681Y!<\SUB\70706#2\DC4\1064025\988781=cZ\r\STXiD$\DLELDSl\3427\t\1071487\1026050\18748&\DC4\163678A<E\DEL\ETX\1025101d\GS\1055207\29521R\STXg\tND\191353\144734)\1034282u 1\113670\SI\1045033{vF\DELZx\29524\EOTG\27657r\1112703\RSM\1087997\983046\138860^[S?+\NAK$\CANN\1055573\NULK8\1554h|\a\NUL\n\DC4\2852\ETB\ENQ\58391seZK\1045800SoW\\\988394\1038104\44673EU\RS\1096712\viz\5612Q\54048\a_&Y}5wYNw\917544S\1009999(\149767\53118Xc\132018\SYN\995571\"\183770\1092107\SOH_\SOHVl!;'\34343\1020450.je\985757Cf\NULF\DC1%I\\\1051058Z\1078211\SO\t\NAKi3\US\131386!\1003727v8\44180CcD\165915`\STX o\1093248\&1\SYN6\997754:pE\\>\994882TyxopG\ACKPvYd\DC2M^\39873\10351jyw/F^%C:\139771\f\EMs2\NAK*\1072539|\ETBOw\1009551\183494kX\t:\STX\NUL,\985525\11202\EOT\21022F\1041187\1049006,MF5\62626\11038\1046826\&2\992316\1042535\1537\DLE_I\NAKqX\74052\33496\998750qW\1079528\16291S\EM\1109798\71703A\173687\1100137\40885d\994482\171958K\DLEp\78871\SI_A\1020408\SUB1\96251>*3\1073728\45382\ENQC\1014611.#\141027G\f\137853X\FSmB;i\16160S\987543\1093544\47460\v\63867jS\182104\&4\DC1\r\ETX\b0\1065928\&0\NAKr\DC4K\1034675v\1086324R\DC3\1037725\tABLp\FS\SYNjb\NULCfknJ+YXs\DLE/UBY\bmF\CAN[\145175\168774oN\138437fV\120252\48726\a\1105596qc$?\RS1L\SOHS:Gx\a\ENQ\174026-$?!5\ENQ:[\EOTDm\1070813Re\DC4d\13958-T5YZ e\1015226%_\1102079\1089034/\1074865p)J\154796JV /\175814\SYN\SI$)\"\tVU\DC2|\131595l\1039631G\1012017\av\160493S\184023@\DLE\rR\f\1003121\GS\DC3\1029207\SOHy\1100158\nw9\10884D>\1080167\178387)\SO\NAK\1032118\NULqJ\1093651\15408m\DC4mC\137208\28407[\NULK\1048038\1050375\ETX\21206\SYN\STXJj`|?\184142\NAKqjh\SI\ACKti\129402\53171\"\44888Hjw8\177514#\165218|Af\ENQQy\"\1092951\DC3<C\STX#0\137755\17177\1056327F0:)KD9X\1089258\1007611\132935P\t\172882\GSI\194673_\1008687\&3__\ETX\1079686\aV\190149\1023070\CAN6\\9\1009819\1079799\7021\31395d+}\SYNS\37062c\\{\DC1\DC4x?\1024193\ESC\983160-;\1068247\44142u5`un\ACK\FS8\RSE\NUL<,K^\1093229n\52273\36905J\998677F=0Kem{4\1108768\42087:\1079442p,Y3[\f\b\186899mY\a\1006090M\SYNZ\v\1084279\1018335\RSl\1105299\1032770\b5\rV\a]=j\vo\SI\ESC\59963l)DW\1068632\5055\DC1\NAKL8\nf5\185716\25978P\DC3[A?&\8217\29235\&4\SI\EOT'u\1037863\1096262./J\1043534g\NAKXB\68646\&6Z")}
testObject_ApproveLegalHoldForUserRequest_team_2 :: ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_team_2 = ApproveLegalHoldForUserRequest {alhfuPassword = Nothing}
testObject_ApproveLegalHoldForUserRequest_team_3 :: ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_team_3 = ApproveLegalHoldForUserRequest {alhfuPassword = Just (PlainTextPassword "\NAK\32852\b\1044404uR\ESC\96199.\NUL\19180\DELA\992565\157529\133986\1092373\994184\163602!\7341s\DC35\145233(l(\67269\34462\62835,\n\US\a\v{B#To\154876R)n3\SI(4 &\SI%\17358.\1005867'%F^\185133\&3M\145606F'J\123169~\369\DELq\189765\169036\DELjVOFS\NULQ\ENQ\FSE\1089387\1062598\&8d\1016405<\189865\ESC\48739Q\ETB4O\ESC\186133e\SUB\7792 \14735\f\1020901/\ETXw\1092704\b@\162356\US\ETB\32958\NAKs\1089148<\172688n\tio\143478\42579QhF\DLE\1066072?8KArf68\ESC\b\158470'\ESC=\174347LV]%E(4W(\ACK\1029577?\DEL\164803<\125187\EOT\"\1029549T,|e\1037610L\23994\121356\1026175\b\1003022X\1037360\&8\1092511\SUB\ETB\27922UEp+S\SUB/II(Vu\1000304\GSK\94691\1008832\985763\&0\US5';r\1016640\1016170\&4Sv\20703\&5\1104532@Hj\996461q\DC4\1061616\&6J2OC:J_\96986\"aY4X%\1056168\&5\7251(`\EM(r8Wa\1112639~\DC4\994910U*5\1087603IS\DLE\DC2\1001248_\120226bu\\E\"\143933<\165589\n<3\1002034\1082058I\USv\\e\1015349-0r-m\EM^5\1055166J\ACK\f,NScj\DC29\1013136\4368I{\1060067yiN4\1015300\170051e8L\12849Y\r+\NAKA5\153752xvJi@P\182163")}
testObject_ApproveLegalHoldForUserRequest_team_4 :: ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_team_4 = ApproveLegalHoldForUserRequest {alhfuPassword = Nothing}
testObject_ApproveLegalHoldForUserRequest_team_5 :: ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_team_5 = ApproveLegalHoldForUserRequest {alhfuPassword = Just (PlainTextPassword "J\US\1005911\120912@PNh\SOHNC'G%/?\ETB]\189831\1012484kE9J3\1096763\1086602\1055149\21675M\171099~\60274Hb\au\DC214n\42487\&7Wf\v\1018914A{\1011938x\1065100k\142534NM-\100580\US>\188443\&4,\151763\DC1Sn\f\1005204C!Q\1096398+|\1068175j\1031371d3g\190617")}
