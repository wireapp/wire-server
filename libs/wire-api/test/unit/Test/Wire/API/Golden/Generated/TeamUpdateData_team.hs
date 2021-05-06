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
testObject_TeamUpdateData_team_1 = TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Just (unsafeRange ("\176429\8900C}h\134466W\SOH\nE3\DEL\1011267\n\EOT1O%I@yO\188279\1084264t=:\ETB\t|U\41463Z\DLEdQ\GS\1095164\127034\&4W\DC1_EmGc\38710\tg\1054617\1023839s&\DC4\DLE\132013st\DEL1\1052959d\DEL\1111931\DLE\1100999#Y\t13278\1058346\1039199\97313kX1\NAK\1016795BH\r\135296etdha\96137")), _iconKeyUpdate = Just (unsafeRange ("\ACK\"\16909N\SOHd\171282nbpr\1085239}o\175842@\1108202V\SO\SIb\ETB<SS\DC2\1108443\992301"))}
testObject_TeamUpdateData_team_2 :: TeamUpdateData
testObject_TeamUpdateData_team_2 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("R\24807Cc0\996317\161178\&0e>igL\ETXe\ENQw\24132\NAKA\NULg\141320\&2\SYNi\DC3\1110441\171439K\v\58833y'w\20205\1064661\&8\ti\34295!2`\180788\GS\171771s\1002343N2Xa\1099665.W\US\68881zo\n!\DC1RBGo\SOH\32895|2\998806\&5y\1064679~\1058631[=,.\34787\1037732f\1033514d{G)\SOH.\STX\1034050\145280\39281K7:I\DEL\160988\v\36062o\181378:\1075009\"ba\185242\DC4\131991J\FS7\131231FsX\tY!$")), _iconUpdate = Nothing, _iconKeyUpdate = Just (unsafeRange ("'\ACKs6\NUL\1059596CDv6V2sS@Lq4A//Drh\CAN\1096193\&2#\FS4yMwI}\170665\1109553\1102090\989165\1082921R\986816m\n\151019\187332*\996689sn*$\142749]h\ai^!\SUBa?\r=*\26610\vk=H+\2385I|o\169385\1023335\1107091\1011596i\1002248{a\b\1017145A\66765=DT\EMj\19791'5]iFpg\NULp[\138380O`O\989332/H_G\t\ENQ\169550\1076154z\SUB\1023475L\1091188Sc\12413\1080039,\1081716\1068199\1093215\1091200\160934*V\bf\54391jvg\1110302\181829vP\ESC#U\EOT\r\SI0\a\SI\7785\&0#X}j<\DEL\SUB\1049424\18864?\GS\RSJ\ACKK\186114\&3\1097920\ETX}!__\27804a\1090346An\r@\DC2dc\n\66275 B23\DC3\1016454<w7v\DLE\"\DC4f\1000150\DC3\1053346Q\US\173872\ACK?I\78215\126979Z=\SUB\188834`\NUL\53433|\22412x\1103290\1075284HC\fMf-\1087584ZGv\DC1[.K\127139"))}
testObject_TeamUpdateData_team_3 :: TeamUpdateData
testObject_TeamUpdateData_team_3 = TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Just (unsafeRange ("=\1052704S\nZ\182330\DEL\1012554+\ACKN@;mJ<\SOH\990765\58263gh\8523YT\DC3\137379`\120696S\1077970\DLEE\187694bp)I\NUL6K4:\ETX\SYN\71710)\ENQx*]|C\ETB\SI&g\1091728_;\1071318}[Hn`\fYBO^,\SI\1060829\SUBz\4948n\162764Z\1109540\1000441S\vfH\n:V)>Em\\\CAN\1049839\1020089\155521\DEL\f8_HVA\53030?\NAKE\DC2\185161SG\14844\150259\&7\DC4w\4186X;u\1051438\EMrmdVxKk0Zu\1027589\996500\EOTp\ESC\EOT+=\1102614hF\1021154n\CAN\SIx4$\1110289B\ETB\EOT|p1Khq\SYNMo\SOH!+#\999826m\141347m\RS\CAN\1103296i\131335\DC2\987462A\141150C\EM6\157584\v\1087385?\181244T\194854\ESC\ETBYeF\51418\135621\ACK\1044691\ETB/\992567\SIs_\EMo\DLE\9951Z\167710\1081218\1107899S^\1034310Hu\US\SUB\CANA")), _iconKeyUpdate = Just (unsafeRange ("\18051Es&\1073486-BJRm;\ENQ\1023296qK\162403\&0eN\DC3J\1030041QG\1025822o\156754\4866\SO\183558\&1\ETX^\1004880B\v\EM"))}
testObject_TeamUpdateData_team_4 :: TeamUpdateData
testObject_TeamUpdateData_team_4 = TeamUpdateData {_nameUpdate = Just (unsafeRange ("\156439( T\SI=^nH>\184369f\SOH\999144\&6\EM\1079967\GS\1012236\128593\1094487\46127jAlUc\ESCP\DEL\1105825\DC2\ETX\NAK\153927#wd\1062259X\\\14406\SI\135868%T\1080423\DC3\1064049\SO\58076\EOT(hD\"\1033089w\1095498r} \59078\&3_\1083832\986454\DC1\ETXe\1009931T\DC2")), _iconUpdate = Nothing, _iconKeyUpdate = Just (unsafeRange ("m3\1042460r{\ENQ\r\SUB($\DLE\994620Jd\1017126\1041108[\179290\&5 <AWGY\23287|\54211\DC32\vg\bQ\45843PgWj\DC1D7\993125\93031Vr\1029117|!\a=+o|/'\ETX\1007769\78838_+\1057110\ACKCx)\170997\CANap-"))}
testObject_TeamUpdateData_team_5 :: TeamUpdateData
testObject_TeamUpdateData_team_5 = TeamUpdateData {_nameUpdate = Nothing, _iconUpdate = Just (unsafeRange ("H[T&-\CANhH]7^\1100254|\29878r9\171944\180853 \GSm71KK\1062362i\1082083Al\179168\1066854*U\28060\1015629lJw\DLE\SOH\\s\52135\49528-\ACK\1021686dY\188526\1039567\9704\EOT~\USz|V\tf\1013122JQ\ACK\1028941%E\65557f\1081597:\SOi\155139T*?\133786\1005265;&%gv7~\1002674Pd/\ACK~\1046926d]_\1019100\STX5g*%\t\987298\1008935\r\131765\&5M=\1112207B\DC4k0\198\138657\SOM9z_=Ayu1^4u\CAN\ETB|\FSz]D\175598z\176826\r}wUG\174681>\USK[CHb\160292\&4\12996\a\1100841\1097063l\99471\33790\150894")), _iconKeyUpdate = Just (unsafeRange ("\28621\GS\SUB\1048545Z\1113350\1099591S%'J\1027904\1015223\SOH\8528(\t\NUL$V}\159237\189420\DC3\136619J\SO\DC3\100270KE?yOd\188321\1010371*[U(\a6S\992751\NUL\167008\FSCy\EM\ETXDv\r^+1(\989154\987762(~=.\ETB\1095725\tc\DELx\DLEz\SUB]UP\a\1030063\160313z0-5\98127kZ5lC \147815[\SIp\f\f\1073711Pj\53132o\ENQc\34595*\127366Z\tA\DC1\STX[\46092+3Z\1013088\&0\SI\t\r\n\DC1[\1001574\65047\1025866O=f\309Ge:n\1033668\t\1099190\32195\1086918t\SUB#G\15542\&7aV+y\US\1103368!!a\1035047\1062385|V\GS\SO\1031795\986347Ii\DC3vI\156218\13703\54303\DC1\32829L\168277\NAK\6668\tz\NAKy/\186570\1080411,\SO\ETB\RSe+\1030169Y3\1023221\1061690c"))}
