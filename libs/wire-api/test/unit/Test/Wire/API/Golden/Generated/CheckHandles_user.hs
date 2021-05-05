{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CheckHandles_user where

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
testObject_CheckHandles_1 :: CheckHandles
testObject_CheckHandles_1 = CheckHandles {checkHandlesList = (unsafeRange (["Yx\SUB#\60525\SUBp\37142\1040883\ESC","\DLE?$\a\RS","=","\"\tZ\RSz\\\1046904\164727G*wm\FS\17377\DC3","sL\SUBPS\ESC/","\997126\1028148p%\1087349\25437af\r\DC3","\aa8\NAKB\ETX66\SO9","Y\1043929a\52688C\68312","\EM.\68247\185543\&6b\135084",";`\ETXs``\190434W\1079466p","\rd\1087137\33397\SUB\146860kG9owdC,\1084864","y.\119913<N\1034008","\1105673)\1064564\43702","$\1004960\FS\1067825\1081879\1095736nP\NUL\1044286\SOHU","","\v\60427\40030","[","u\984994\1092020w\vr\CAN+kAp\DC3\1091628","kx\SO\1071795\&8",":\US6\1087498'%\SI\EOT0\1022895","\EOTAr\r\1020201\1104061a\ACK","\179901V\n\SIo\SUBt#\158252)&D","m\SOH\61001ZD\CAN\1045184/(R\95295\SUB","+\DC3x\13937~\1003474\37143\1016575\1023602","\151843>\188614e;\ENQ\1082920;CQn\FS\NAK","\95124\1003472\1111641]h\44849_I\1053030\2292f","\f\ENQzz\189894\58005","l\1062931\DC4;_\1029673C","\1014011\&4\FS}`(8\GS\1098704\1100984(W_","\170599\DC1\CAN\19316\44703\vR-\1075278Hw)\1037375","k{w\133340xo\73883@\tJm\1039660\SUB\DC2o","t\121419\1002311[","","r\1023071\997321\1091806^L\\_\1019548\ETB\92559","(\153043\GS;\60107oFbR\ACKS\DC3"])), checkHandlesNum = (unsafeRange (10))}
testObject_CheckHandles_2 :: CheckHandles
testObject_CheckHandles_2 = CheckHandles {checkHandlesList = (unsafeRange (["kZ\DEL\DELM\1098328J$\985051a\STXk\1048425a ","\CANT_y\1037770\DLE\140634ZE)\190763","7M\EM3_oxSt#\a","\152626+p *+kZG6m\t x;","\RSA\RS\RSETd\NAKQ\DC4","-C\983912\"","*\RSh-\1004045\ACK\54940\DC1F\DC4\ETX","\1027691\ENQI]\DLEL\983742{\97979\ETX+k\1024323\118963","B\154469m","\150239kI\EOTR/*l`\170592","*\1033454H\SUBm\1112401(&\ETX\132704NC~\1041861\1101456","","\\","","=*Ft\ETB\f5L\988430\1027346(\146885","b%?\EM]\DEL\GS0ouu7T","\57939\1075557{\141012,\RSs\49480\FS\vy","\142124\53650au\DC4\26130\ETX\174956u|","j\998046\CAN(_\1074871\FSR\v3\1051378.\DC3#","\EOT\171287\1096514k5 w>\138855L\177039S","]?\1048800?4tji\SYN?\23586V\SOHu","\1025873\aQ\SIz"," WLM\1019441\1095580\GS\1103293^\ACK","\b\SOHb\FS\1065884.%;\1086330\f{f","V\99593\1010854QC\1107447\v\EMI6\SI5","\1100176","\1047635;q\177802","\n","\30648z\1048337H2\a\r!\50756QQ","\55285G~$\1039393\148085\&7q\1078529V\96030\1035662\62105","rw","\60426\47876En<\32410bc\179111w1P\ETB\993694","\180010|","/t\FS*","5~\t\33125FMRb\49630.\95815","\1099544\1080971\CANF#I\FSxe\995363 ","\DLE\fQ\"\ETXTo\t)$","=\DLE\64911\&0uPc>1MV","*\997450\1019806\DEL\158887,\145398\SYN|\1073989@",")\SYN\DEL\DC1\163173"])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_3 :: CheckHandles
testObject_CheckHandles_3 = CheckHandles {checkHandlesList = (unsafeRange (["\1080080B",">/s\DLE\SI\1092183tS[\a","\NUL\"\15354!\ETX\t\DC2I\SI\45760\\j\1057612","@C\184096\1026056\1059932\43254\ACKp\ETX\b\1065344\1085386","\9714'<E6\1026074\aQ\ENQ\65285C#\FS\1111647^","ro\1011961f\128582,\EOT\NAK=","\SUB\1110220j}\1052109!$\ETX","","J~\DC4&\185707^\NAKt%\134843v\rD","t\DEL\1095436c\SOJ_Nr\NAK\1017387\&8\25563H","=6/-R\ESC","","x\ESC(\ESC\9547\1035760\f","\167747\GS\GSEq\CAN\41978\28577\SOHP<\SUBfo\1055090","`b(i\STX\14086\1097851\EOT\1039909De\18087D","qW\49088\136755\138289\188328,3","u|^Yw\137547c\GS\SYN\FS|D\CAN\DC1h","\1030335\983326\&1\187391S\47024\&8\92981B@D2w\EOT_","mBM\61542\SOH:1\ETX\n\1050448\SIM%c","\58565\ETX\ESC","/N\990039*a\1031789\58675","{<\135013\ETB","]S;m\184366[\142771\142246\1078130t\96670\7591x\54956","\NAK","\177569\EMw\166865\a\1113439\fN\NAK;R.\25960\189163","r\"\1088493\997753+#","\n$>$\40110=\ACK\RS(;\54489wdCi","\NAK\160339","`%b8","\60704.\155216\1085457<9C\DLE\"","O\1046303\1040893\SUB]\n\1084672\1008061(",":=\vv\1090451\SOH}","y\162133q\1049059\a\1112841\43707\&9\99398","Rx%\1069660\1067330\EM\149972DQ;t\188727\EM"])), checkHandlesNum = (unsafeRange (8))}
testObject_CheckHandles_4 :: CheckHandles
testObject_CheckHandles_4 = CheckHandles {checkHandlesList = (unsafeRange (["b5","R","\21501","\EOTk\1100364\38465q\DC1F","\v\\]p\1056160\1020860\GSh\998809\RS\SYN","'\65329\DLEsU\1087001x\22172","\173328\f\SO\DLE\r\ETB","'#=%\987369;\995078\STXy\FS","4_\147352","CBi\129577\n\78298\148501E\160915\1099406c\17399\USr\1101431","","\NUL\186617]G\1060909\184883\73891;\GS32(","\7358\SYNJ\176828\"g\998698*\135569v","\ESC1z\SYN;d","\SO\1021158N[tM\GS\ESC","kftkH\NUL\DC3\1069369A0\166695","\1001804\994899\37806\STX\1070952\DC2\21728","\ESC\RSQ\36952fY\STX\SI","\1045192\54076\1034301j\52574","<\DEL\1069931\t\SOHMn^m)\SOHk\EMD","\US","\1045566A\30472\1076105'\EOTU0b\177603$\997417\SIg","\39561","x|\1051230D[\EM\1062320\60868","\STX\SIZ","\1108523z\1102971\a\1037299;\DC1\31126\a\989541\34354a <H","\FS_\1030114\1033705\DLE\GS24\1085948\NUL\183824OA^","p","b\"w\ESC\ACK\tx~","0\ACK\1047764\1080224 ","F\\;'","b/\NAKA\bh>\1094212\\c\EM\18319Zd","","#jH\183717\DC1wF,","\2726\DC3\1013163\1069810F\68252\DC1?_\1003902)eAu","w","\1039439SIa","z\n92B\v\EOT_r\DEL\ETBa","8\1009223}\8093oc\990851)e]\n\DC3\1016046","\n\EOT","\1084464g\ACKB\ESC\1029204","H\97435\&9q","","$XuG\SOH)L\\\DC1\NUL9W$\1099396|","\1015161","vFFu\146650=E\134253\144877q","W\CAN\1014642\36599\niU","S\US/R\96259\14027\v\1088943?\DC1Z","\GS\n-\45460",""])), checkHandlesNum = (unsafeRange (1))}
testObject_CheckHandles_5 :: CheckHandles
testObject_CheckHandles_5 = CheckHandles {checkHandlesList = (unsafeRange (["\EOT3\nCl\1040588x\71040uN(`4\1041502","CwW)\33084TR","\1049423\1031258\1109055\161841T35","f7b*c\US_2i","}]u\1079928\EM)\151170X\1057077J\NUL","Ck\175596\ESC\r-rR\36207N","\1026827lX\f","M","\1030268\179448\\1","ys\1057464~\FS\1082294^","\43931I\173867O\1066579\1068273\vjw\f\178497W\SIhm","1\NAK- MoU\151594'\DLE4\NUL\134975\&0\1043435","\1093141?","","","\128430\"\151526e\a\CAN@PC\6055n\NAKSJ","\1063117\140341\"O\\\54175sr8\46710\&9\1024730\bGp","Zw\ENQhF\1011498K_","\20165/","\988915\n\153593\&5\144732\&5\RS9Poq","","\"\66027","\b[","!tP\r\987218)\SI\1075498\&0\30551\&2[C","\995780T","@\991677\24827\NUL\CANy-#=\991298\&2\DLEk","`7D","b9SVFH","s\SI\GS\1108289N7","m\CAN1E7\DELU","\164150\&6\US","y\ENQ\26639*\1044419\r","\EM<\ai~.\NUL[\95388\\\EM\a","","HK\146129\&6Fb\DC1","\990482~\DC3K]J\17810\&9a","\46722\ESC*","\ESC","\1059382\&5EOMM\1028426\NULs\182362T\DC3\GS]","q!BtJ\DC2E#)","\1110325I\1090758Uvj{\NAK\113822\917976p","xq<;i\EMf\ETBF3\1097171`","\GS`=Q(V\1094473\1055076\23302a","\184130y\1051412q\1000730\DC2 \f\1105853~b","Q\ETB\NUL \128072\NULl\1002597\f}\1072870","\44530\ETBk9b*Z\69969m9\1019443W\vG","P"])), checkHandlesNum = (unsafeRange (2))}
