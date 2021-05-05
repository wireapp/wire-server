{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BindingNewTeamUser_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_BindingNewTeamUser_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_1 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\DC3g\1014405d&\1050325M\NUL1@\DC4\v}\1005651:\1095497\STX\nzr'E\SO.\1028986\1001067@?\t8\983230\SOH:f\f\98787]\995920W\1085338r\RSP`|\ESC\1084578KmA&+\r\SOn\"\159618f\"\ff\37981g\ETX\1113810\DC42T*e\176381\CANsv{2VR\35503\1110916\">\43452\&4p\1053180M<\1062766\38536eAKGO\DC4f)i2<;\1083814#\1046032\996288\DEL\30330j\EM\145786\SUB\SIC{-j&\NAK.{\ETBi\ETX")), _newTeamIcon = (unsafeRange ("B\1049528x\SOw?`\1075060!\160811\&1\40140\1049917+^\145362I\DC1\138361['F9\USo.\am\ESC@udl\RS\983623Na\DC3.@%t2i\1100636L\1008122=\1033586%\DLEhNGk9\48330\1031021\983204%\n~\995305\vxC\46060$q)w\f\ETX\f78cV\RS\173\ETX9h\1020721\SI#W\44969I\SI\aQf\132222r\NAK\6061V@\ETB9j\165857|\1063761\1111161r51=\1070431\t\RS\132776\147266\v-\69901\&8x\990829eugee\EM\45516\52778>e]?S\SYN\1106514\f\163229\61098H\EML\\@\33981tj\1073420\131620*\ENQ\1041520_W\"opZ\1055197\"\1092909\DLE\181651T\68786&\110800\32210-2\140001t_x\44625\1042522")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_2 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("S\1036951(\815\ETB#")), _newTeamIcon = (unsafeRange (",slK\CAN4rI\157416\SI\1069879\DC1\"\\\62749EW\US\1047301\990729\EOTZH\DC3\SObnCn`\tU;127Z\1089323Itd\EM\46521\998761\1014873Vv+f\96664\1050743(\141069NH\152441\ETB:\SUBg\1108111\&1\SOH\58792\994800Z\121145A2Ga}\155284N\171377mc(U{\DC3o\135561\NUL\989628\26233}z!e3/\37220\1013020&QSZ+@pZ\\&ba\1023377$c\992049V\STX\1010915\SOH\173324\GS[3H\995816Y\62809\n.\n\DELQ")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just CAD}
testObject_BindingNewTeamUser_3 :: BindingNewTeamUser
testObject_BindingNewTeamUser_3 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\v\ETB\1078628\19389\ETB+c#\60383\EOT\147998xX\DC4}>_m\52149.\31083.Z!-&\STX\1028011CpT\1018072\83387\11485\SUB\DLE\NAK2}\DC3,$4\169546\SO\ETB\146177>\147557X~lT;\RS\f@!A\134630zB\1093792")), _newTeamIcon = (unsafeRange ("\13036\&9\CANB\31401v.+&.\1105323\GS\73734\SO\1036001\46410\NULc\SUB\34192N\171463^h-\986766\195072jn\188132\DC2RP\1057643Qm8\\D\1045059!\SOH8\t\1076501TP:okc\1039633\997855\996290\&4cx\162153u6\FSn\141jZ%\1034636/*;\ETX\181058_\FSb\24236|\NAK\SIH\SI\rW~Sa\1104173]/\73869hI<h\36277\142891OG\FS\t\b<\1024076\141067d\20790d\f\a.\95532\r\ACK*\1061898\SOG\DLE_\1053809M=\1105412C\1037858\1059622>u}Z\162965w\NAKfy\ESC# v\47178=v\FS\145664\1113365ZQ<h\bF&\153076mKM\1003836V\1041244cR\179099@P/\1000670\t_\1054453A0\32277\24058C,\17264g*F]\CANx\EOTdv\DC1\1081312\1100156m(\ETXm\CAN*\DC1\1090658\SOHY\EMT")), _newTeamIconKey = Just (unsafeRange ("\a@\32525L\SUB\16939:fW:V\r\1095605)aj\NULV2(\37763\&8A\DC2\1010993\v\1008728\b\1094484o5\64810\1082867\997538q'\1094950\1084216\994270;*`oq\1111486\1001598d\ACKIO&\40256v3-\b\DLEjb\996833jX\1073726\NAK%W\143465\16408\NAK!h\1040860\1066347j\DC44h\a\1043838O\1019581K\188400B>\RS\ESC+\1102401>F\1022661Q\134058\NAK\156917F\DC2L\STXc\DLE\SO\139007Mh3\154542\1040741\46971\1022065\154622\175003\64188\ENQ\DC3\1038170")), _newTeamMembers = Nothing}), bnuCurrency = Just SSP}
testObject_BindingNewTeamUser_4 :: BindingNewTeamUser
testObject_BindingNewTeamUser_4 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1025645\&5H(oDC\1023462\&6\9670\156301\b\1005095\NUL=+\ETXi'\ACK2}\149593\20470\ACK,1H\99318`\EOT\a\1062227v\ESC\48573\US\146966\NUL&U\ETBKa\ra#\ENQg\nS\44307\64198'\1099629hG\992024}\r\DC2)\132191x\1105711F\3381M\1069991c\ESC\96951:\60089\EM\1081005ZOcV$\t\57719\1064747op\166698G\1104323Wm\a`T6\DC3!\169056\1048582\1041175\DC3i^I\13783\153432\&6gBR\vAEb3\134667\1028200+\DC3\1063287\RSD9")), _newTeamIcon = (unsafeRange ("Ie\1088810E\"\48301>rj\DC4FI@\1002485\133823i\8340vL!\f\170284,\94716a\50378AG\35717\8788q\r`\EM\64891u;\RS+\GSpy\162222y\173426Z\22580%\NAK?_D\RS\181432g\1007086\ENQ\132869\ENQM\SOHcL\DC1\t|\1083091v\1002731>)W!\132702ig\"}Qy~Fd\167791d\37264U\1097930\128274\NUL\DLEs\1111089\NUL\"\ESC\1012922D\1041062\1029847\58598Mmo\996903h\ENQk\47094yf\1049492\52015,\ACK-\16064\1077094V;0\1015597\181387#\f4V\132106;m\ENQ-\163381q~\DEL\1046279X[)Z>p\EOT.L1.\32996AFf?4w%8\1049447t\177595\1025450\987394\":\a;7\46605W[Wx\1053656T\59200h\98828,<\42533\\0\1027877u\"3/\94511+:2.\64607]'\ACK\SYN\28310\"~\EOT-\n\27424\1003225\983466l\153293&\ESC\1026160v\154535Zt")), _newTeamIconKey = Just (unsafeRange ("#Z|\ENQd\66761\1098103u=\SYNh-YQm\FS3\60742[=\99362l.\1107813`AA\FS\1065458\NAK8\25575\1070322C%T\18946\&6\99049j>\132827\1009162\US\1046997\1052176\ESC\DC4s)22\DC2^8\SI'\n!\NUL\156552\ETB.\ESC\3973\b>s@%~\180499\181786_29\ETB G5%i`EH\168765`]F\4497\1062746\bV0X@T\1079017[\NUL\190496z\78593>\995253s`\1083957\97877pX\54949Du=b=G\SUB<Z\ACKi$\178939")), _newTeamMembers = Nothing}), bnuCurrency = Just UYI}
testObject_BindingNewTeamUser_5 :: BindingNewTeamUser
testObject_BindingNewTeamUser_5 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\n\DEL` ~n\1081999?{\65532#\bs\19062\CAN\1587bT\rr.\SI\1062045\1083451\136026AST\99161I/\US'b+\1073163>\RS\rX;\EM\SOV\1108161]\1081438P\147851\1083938`OMgJ{\EM\170188KD\1015846e\1104997K/\52362Tr\\,A\ENQMo\6365\175011\RSLY$\1076133X,\29325\61838wp\155062\&6\NAKUI\v\SYN\EOT^i\ETBvW&b\44905\135774\FS\160986\32481-\USD\ETB:\1095140d1\1076095\&6\15990\184409Uv\DEL\DEL\160205\51840%j3H\EOT\STX\177586U\1047068\8616O\65739\1084908\177024!\RSpE\49397\EMl$\1069105\1068561a\24396\1028258\&7Zaip\RS\DC3~#by\DC1\1069386n&\1021529R\NAK$\ak\"\328\NAK\STX \31805\1057685.2w2%\b'\EOT\127140\&1yS b\CAN\1005560\USe\99535L\EOT\RSA$\DC2\997153\51374\1017725\51128(/Bh\a5hX&\SYNCc\ENQ\EM\CAN\1053637>\DEL\52368&@(Sm>\98421\170562\US=2\1073270\&2>\1044610s1i")), _newTeamIcon = (unsafeRange ("+\995562\f\ESC1n\46132\ETB\DEL5F\ETB\1005888\52438\aX2K)\SO\185749\1112753d\989795ytkt\CAN\NULk\v\r\DC3M\vv\1087213$J\SUB\134238_\DC4?\DC4")), _newTeamIconKey = Just (unsafeRange ("\1091671\\\DEL>#ygp'h(\47860\156982Y\72261\134706\184349a\45461>\1069402\"\173225Y\3461Z!\DELG\v\FSt\15248k\100064+\SId\SYN<]\ETXc\EM]:_\a\52692r*\992130\1090683\&5K\ETXM\1099787\1056569\23175\ENQK\nV\FSA[K\1053771\19218Q4G\186281D5\59390\1024974\155997\&8Q\ETBtu8t#2\DC3\66658\a\141208\1034553\\\1000586\1074780W7\SOa\US\v`\FS\41137N\b&Y#_!5\983266$Q\EM9gu\FS\DEL\EM\999066\54799\SYN\145979)wCUWEd\b7b\1089576g\CAN\41392.\990688r\98798\155630A\fx\10246\184831f\18021\1019867\987915\DC2\DC2\STX{\1090625'L\NUL\SI\189308o\1037295\984456\STX!f\120311\39584\1104514F@BIl)o\158274R\DC3\EM\147358\STX2\1016186\ENQ\186812b\t\ESC\31865\1050105\STXgi\1032987\ESC\1040380\1086271\983723\&2\DLE)\SOt\DLEi\146304S\SI\1016916\SOx\CAN~\ETX\1052334)\990415W\133212\DC4Q\179537\&6\ENQ\SOHxw/\DC4\ENQ9p\SYN\66330)T\\\995760v\1019996")), _newTeamMembers = Nothing}), bnuCurrency = Just MDL}
