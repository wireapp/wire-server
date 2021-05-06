{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamMemberDeleteData_team where

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
testObject_TeamMemberDeleteData_team_1 :: TeamMemberDeleteData
testObject_TeamMemberDeleteData_team_1 = (newTeamMemberDeleteData (Just (PlainTextPassword "'R3sU\"d\SUBP*]l\152866<\ENQ\RS\DC4\191366v7a\US^r\1052940;wi\1031225\1079811\NAK/uCvg|s.\991339\1037932\aB^\1098780$Sq\1077868jeR4\1002322\1079401\983355jy\r\NUL\RS\EOT\1082086\152212\4240\SOH\1010721T\3051\NAK\NAKM\1018661\EOTon|\b\19033\1027649)<P^0B?xz07%\58194Z5\97912\131366\ETX u\989830p<>]\NUL~\180760\SOH{#p<Fn\DC1h^2\DEL\1078065:+\29111\SO\SOHS\n/q\45149\186566\DC1A%T\1038494v\CAN\168422\1016871\&7<K\t\183038\RS\1030493Z\n\1098187n\ENQEX=Q\96549`3\1069925\1080085\165075h\1096828\1112285\DC1\DC3\189527h\1038069\59382\1018757\62739\EOTf\1024156B~\1055979\"\187241\1098754J\1101963RR|\1078218\142935\131581\138874\\\59259j\ETB\DC4\ESC\1042675qC#{\ENQ!\32512j\DC3\989238\&2p\ESC$~W\ETB\ENQ,84 \SOHJ\992397A5\147381Xv\SUBKn\188487W\45787!b\ETX[tfofr~\1072501L]\1063182n\CAN\SOH\DC1&\1013165\DEL\153450\60590/\DELF\STX8\1035130oi\DC1Zd\1112146\1092621\&1\NAK\CAN^k=\148122s0\ESCb\1011758\1036986\\`g\145063\1005913\ACKx\a\1066862\3365eq\182505\b%j\127047P\25999[A\997078\ENQl\177513V=\SI\162321\999547\ETX\rk\CAN\999442\28050_\128795\ETXK\v!F=D^~\179271iC\f\DLE\SI.(+\174896OU\39969\EM\1044819\1081474n\150638\ETX\r\1066193\1013670x!\DC3\26676iL\1062789g\CAN\ETB.\EOT\984295o\1049154l&L3\58627DK<6l0\1091428\1107874\74268y\DC1\DC3\1104427\DC43\1043223fM{/\n\1104193v\ETBpYuSh\127283\DC3V\1007192N\1076471|\1013523nCb=\US]U~1ady\985537(>\1055912:\146551B\51663\STX\29228\EOTJa}Z\DC3\1059128\EM)y\1109794d?5\152111\151772\1051807\b\SOH\120585\148537g^l\SOH\168857\vyNk\151864\USo\EMO\SUB\a?\ENQ\v\FSIUo\1092788J?r\NUL+\DC49\1104354R,K\1006859%\1068584\47393\94522IP\1064027\1010892\STX.\SI@\183027g@\25247\1084285(\1026099\DLET8(L\1001189e>V{\148827\1035824\SUB\179432|\f")))
testObject_TeamMemberDeleteData_team_2 :: TeamMemberDeleteData
testObject_TeamMemberDeleteData_team_2 = (newTeamMemberDeleteData (Just (PlainTextPassword "u\152148\139674hF_#x\25007\USQ.E&Y\1021334^d\f\99240\ETXJ\ACK\SOH\ETB\158667G\148401\GSH\1041541n\1062732q\157006E\f\EM\1101932b}\ETX\23010!_\1016930WjJ5e\1028616 \60886\1050097\v\STX\1094164\1108713\SO5z\988677\1042553\nj/\988603\173476\1038371\6254\ETBG{\a\1049500\1095843^\ENQ\ACKi|\NUL\EM\19449SVw\ETXq\f\"\DC2^#\59417\&5\1074155]7\148663q\SII\1084576M\14042\1063421\1000294r~=\1005185\SO$rK\SOo:P_ryd\1023047&\14237\26088\60175!\99492)@\DELa\n0\179969F\1006783k\tv+pg8MGX\1101190[\ACKn\1079176M_N\1034112\&9\NUL?)\t\9731\ETB\ETB\1003490\993020\1076315\1039896\1040664T\66695\1106269*](`3M\\\189800O0\NULJ\171772\990699\151495A\EOTY\1084350\139584\DEL\NAKs\984713\"\ACK\146749nr;\ENQc\"c\ACK\bT\1101387\1033563n^\EM\DC2\5935\1023351)/;7\"k\127237H8\t\b\SUBc%\1054175\46451=UU\11660Y\128388F\RSf-\NAK\1100248%w\CAN\1046051\1020722'\a#;\b\ETB\ACK-ef\DC2\1068551\"w\1029631\EOT\SYN6\51939\135349JD\1081920\4104O\164011\DC1\96516\1008655=\SUB1\1001014K\1011603`\994675|cuE\34345\&3\1026958\172105\1018397\153216M\1002795u$Z\62683\tHf-Rs>NI%\NAK\RS\ETXE\\po\33311\&8\ESC?1K(D i\1037634N<1>R\ETX'\16989\&6#\ETB#m\168969\175315RU\ESCW9=\1048524\1022243\FSU4>\23882\STX\GS,\SUB\ACK\r\72320\83395\47565\1038340\a*\161154\ESC\ENQ\1083972\DC4\ENQ7\67292c\SYN\SI\17268\SIN\NUL\59409K\vP\14528D1\1101904XEc\\\f:\DC4\1012799i\DELN\DLEv\194661\r\EOT\1101482\RSu\DC4&M\180813\1082790\1040401i\170751\1016622Jnh\f8\DC3Z\DEL>nX#\f6P\1001778\28859\1017633\ESCu+I~\179875\1002310\&1c%\186190a\98554JN\146955\n\DC4u\GS\1015927\156896\160490\RS\EMCH\t[0G\1064531\25938Nj\32633}\150882j\34525DUm\1043060_\b")))
testObject_TeamMemberDeleteData_team_3 :: TeamMemberDeleteData
testObject_TeamMemberDeleteData_team_3 = (newTeamMemberDeleteData (Nothing))
testObject_TeamMemberDeleteData_team_4 :: TeamMemberDeleteData
testObject_TeamMemberDeleteData_team_4 = (newTeamMemberDeleteData (Nothing))
testObject_TeamMemberDeleteData_team_5 :: TeamMemberDeleteData
testObject_TeamMemberDeleteData_team_5 = (newTeamMemberDeleteData (Nothing))
