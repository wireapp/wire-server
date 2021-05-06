{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveLegalHoldSettingsRequest_team where

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
testObject_RemoveLegalHoldSettingsRequest_team_1 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_team_1 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "\n\1001646\nfVhq\1079706K\SO~\USC~")}
testObject_RemoveLegalHoldSettingsRequest_team_2 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_team_2 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "(\23881z8\139189\&5_w,\bJU\RS\DC1\EMYl'ty\STX:_Q\ESCRm\1041105\ACK\1094664e\1052698\1008083\1015772BO\51029\165583\99009\1038119\n2\43603\&3\1017263V!\SYN{\118857PLmHel\128948q<\b\EM\EOTk=\15998ro5Z*R{s4\174315z\ETBj\148744\SOH\1053945\1013870\DC1\986391\170607\GS\53251\1098414\110672~X\12555\&1\2284\ENQ\178624\&2\6056X=\ETB<\159124\&5\72747\181980hWa\137379\ETBo\ESC\41810\1046042{\\k_\FS5\128288$k3C\48887\152164\NAK\ACK\1103578C;\a\1022590J\61621\14497\53578\ENQ\rA\1056159\ETB\SYN\b\181070`\1095871k ,.6k8\146550\176564TAV\EMDu\1100316\&8|E\CAN?\NAK.R#K\ETX$.\156037c\111308W\vS\DC4\r`p\1039266hO5\4250\EOT\ETXh[^\917993\1102720w\159465 ?T\1024629\ACK\987038W\67325.)\SOHF\170078\SIr\1074020]HJh\31091\bbx[\140983vpNW1 \ETB'R*\1086492t4\SYN\181427\149312s3j\983049N\1051256~13\EM")}
testObject_RemoveLegalHoldSettingsRequest_team_3 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_team_3 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "#\ETB\n/z]E\1072650\&9\DC1sv\t\b{\f\CAN;\1050827\DC3\SUB\16982\1110859WQl\SIBg.R{.#/0\US\995216!!\153798=\2459ab5\7149\72708]h\1072419t\176730J\1010499z\r0\74938:\STXg\40634\tO~jzy\1092144\NAK\DC3Q@\f\DC1\SYN/C\1101729\SO\1038297mc\SYNi)&\65160\&2;\1033661_z.\59293a0\983870T\b#*\DLE-\1019352\1103902yx2I4g\\u\1090970.\176313K)i;\FSi!-20b3WD0\t\53006I,\1064119\SUBL4LR8\7380-\985303\1046407j\ACKP\47588\98985\SUB|1\1055882}G\SOHj-8\28902FcB1\SIB\166507\&1\163212\STX\DC3G\ESCg*E\1080661My\185685j\NULz\\x\GS\1035436,8Tb\35797\1045991K#Q\b\1073714{\1077629\33367\EM\v\SUB\100709iP`\787\992188l h\1107969b,+\ENQ0J]\ETX\1041344\990788\NAK\STX\RS\153334-x2Q\US\SI\1011466h\EOT\"\147917\SI|G9\US\181797;5:\1091647\DC3g\SYNy\DC1r\a\22375\143489+v\ng\1062675nL\CANE\SUB\166975\&3kI \1057440i\18188W0Z8hl\SO\66754K\n\EOT:\vv\1090076\1080031lIS\22570&:Y\1041261tuoa\134216\184536\1069121q/\b\1059596Q^!\167778_\1026537\1005088\17091\&3-I\ACK=\DC2\EM[\SYN\DC1*QHohF\US*dZl\f\EM7\EM\188626\989016qP\r\1836J\23720\ETB\110594\ETXq\ENQ1nr\161722\NUL\171200>p\bD \GS/\190698\NAKw\1022597\NULK3\1009633\SIx\b&\ETX,\1061886\1107203\37997I\132899\GS+\151385\134791\US{ijM\EOT\CANb0\DC3r;4\DC3\53585\64070\162401]\111090\t\66743#\1057740BAeo\996980M[<k\1095150C\995834\1100044\1030498A\72163\\A+`>fu\99504\1104436U\tN=$-\"qq)_\SO\CAN=Ok\1063692\SI=)\1040202B\1018978Bds\SUB\DC3\r\134145\&0,d|M>Z@ \95361t$\1067863\DELsMIr_J4\61385U\n\ETB\43007\190705k\176045\ESC5Kn\1051435PT\SOY\1073471\ENQJxk\FS4oh\SOH#.\999358\ESC\61978\ETB\28220\172538<\DC4\NULR/\132789\ETB\1110157O\1037007\119582J\DC3\DLE0\fz\USY-hLn\1109849>\1083437l6n4\DC3\ESC\1057323\1091697u\DC3\12515\&5\DC2G\SOH\182428lD!\1114043\1108500\110662\NAK.<\179049n^{")}
testObject_RemoveLegalHoldSettingsRequest_team_4 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_team_4 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Nothing}
testObject_RemoveLegalHoldSettingsRequest_team_5 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_team_5 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "&\NUL\SIFq\63936\CAN\125240\28037\1055811\1106946\&4\1068423}&U}d|\999043,x\3896\v\DEL\37491?\983311\41657\EOTE\n4ktv\1049840\tyU\986555y3\1062997\&7q\994183=+\12939Ryz:\rq*{\b\1078469\&9\SYN\1052092D\ACK\1011025TD\1105285p\DC4@XO3tsD\1032801%B\998418\69414K|4:XTC\1056723da[ \\\1053540N%\DC2^\138115:\989764ws\DC1\119105yAVN\177613K\1028031;\181511\159431\ETBK(\1020422r_\r\DC4}\1053393F\"\39375T7r\1008907\a\152833\9586\1024974\&3O\NAK\DC1\168825e!l4\ACK^pB8\154695\b1\133554\SO\1055446\1086066WM$\ESC\1083826\1056485\7479\63071A3\142581\100808J\STXwq\60423\1079309<\NAK,\154923%4E\SYN\ESC\1041319\1008999\NAKXz?O!J7ix\14188\SOH\ESC{p\162016ME\1000280\991832=2l\993237i\fh\178766&E@8\ACK\ETXf\SO.\41915Q\vxp(\\\39732\&2+\1059718\1049998\ENQ\16309\1112020\SI\1022189yT\156397\1089103\b\1010936\bQ\21104\61815\NAK\140003\168008\ETB\DEL\STXs\185592\1027718\171437Q\991378b\1075187\DEL!@\143916\ACKv\152184s:\99354\ETXb)Y|`d\DLEF\1041831\1051949gk\RSqW !\1080754aMg\1021413Q2\ESCo\152290\1097753\ENQ*\1111664wb\EOT\ENQ\1008368\STX\1053040\&3\ESC\f%(\RSUS\43415\"\SYN*\31272\983953\US\DEL$6FR\DEL\ENQ\\\1084638\991863jAS;4!\RSmB'L\EM&}n\DC3\SUBiV\SUB\13893\176252\1054846=\DC29zJ\1111631\DC1r<\1054287@\120691g6\SUB\1067751\DELeY.g{_pW\ETBh9U?8)U\994882\&7\134230`\1056870j}\ETB\133975#b\121254\1103991\ETBe\SYN\61499\ETB\b&6d\USTT-X}%~0\74956\DLE\RS\EM\NULJ\CANQ\SI\ENQwZ=Q!\b\998878\1077656\63519\1099970Y\STX*Z7[\36226\51947C?\DLE@\b\128598K^\ESCc4Q\SIJ>q\147213\STX\DC2X<m\25604f6)B\t\138055\SUB\155196C@%K9\DC2\73070\GS{\138578i\166812\SOH\68649\DC4\DLEV(\CANW+V\DC3~\b\SUBB\ETB>\9176\DC4\1082232a\1102616\STX\174833y6\1080266V\21328\1090943\22601\984028o%bf\SOkg>gr/R\NAKX1\25276{i=\17121\1058380V1\DC4\1101693Q\ETX-\182532\GS3\1080376\135735S5U98M\1027352mI\NAKn\57420\61520e\EM\f+eB\63506\&7Ecv\18256\SIWY\141135\DEL\16159\5183a2&\72210\1106470_\39952\&9\DC2q\ETB\153885\1072338B\FS\1057892v\69446\SOH\145704z\EOTz1&PI LD\US\ETX\13146M%\1066890\DC4\b5a\1098906_\156690C_")}
