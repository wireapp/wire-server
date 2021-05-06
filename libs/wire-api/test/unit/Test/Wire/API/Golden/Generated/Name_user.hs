{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Name_user where

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
testObject_Name_user_1 :: Name
testObject_Name_user_1 = Name {fromName = "\175307S\DC24\1100302\174658\17977\1077643\1062884eOP`V1\ESCT#q\1036479L\ACK,\NAKVns\1062170\16114\25840(\1088692m\ETX\NAK\145964\fi98L\ETB_\bb\1027340\STX7bK/wj\DEL\r"}
testObject_Name_user_2 :: Name
testObject_Name_user_2 = Name {fromName = "\SYNUj"}
testObject_Name_user_3 :: Name
testObject_Name_user_3 = Name {fromName = "bX\182535\190005\&4\SIv3\SUB[[\1056543\&0\46942nf\FS\99170n\186644\v \RSU\r\EMV\1036202\996816t5h\1099471\NAKp\1111433M\46707\1100721*.\v\GSL)t\170060\STX\DC3\991332ah|vaS\SUBK}5\NAK\1057071\1029460\SUBjRi\USJX\1061156z\SUB\DC3h\1112437qMLY\NUL4\\\64940\1097655uT`p\17351rB"}
testObject_Name_user_4 :: Name
testObject_Name_user_4 = Name {fromName = "`sE\1100215&=\1111200\33433:H\1000913\ab\NAK\SYNQ\DC1\CAN\1009666\188650\68018L\1052849$7\1086325f\189003\39793f\138755 D[N\993480A2B\132731<\"v]u\167887\172989\1074579\1054396P\NUL7\74232#\28873\ENQ\\6X|w&F)\DC2\4152#\SI\ETB:]KMNON\1091566#\136737b\"\984055\&2x{\ACKWLryY\ETX\152730|\vi4\1014262<|\DC1\DC4\ESCE\DC3g\DLE6\DC1\985999E"}
testObject_Name_user_5 :: Name
testObject_Name_user_5 = Name {fromName = "4\ETBXf\FS\v#\141883)\DC2\RS\NAK\1070218f\1040578\1098923s\163624R3E36k,L\SYND!D\1001289?V<\ACKxN"}
testObject_Name_user_6 :: Name
testObject_Name_user_6 = Name {fromName = "7\63445\SI\ENQ@\32148\&0.v]WD\SOH\f"}
testObject_Name_user_7 :: Name
testObject_Name_user_7 = Name {fromName = "]m\94000\b\t(\"\CAN1&\DC1\160641G\83223\18018\1094333\992103\t\51662\1067121\1108252\b(\STX\1013157lW\n\1046090}*\SOH\f>\ESC\147121\&8\997547J\36717K\ESC\CANV\1005219nB\1015216Lt\tRS38v\129299C\184888\DC4\US\vr\DC3\8921\1088391F\988899\61499\ETXo\993125Q\986687l\1084697\DC4)B\17931"}
testObject_Name_user_8 :: Name
testObject_Name_user_8 = Name {fromName = "\a{\DC3\SUBes\1009165\ACK\NULuB\DLEx\19975\SYNI0f.\66017EH2w+$\v7\18740]\1006608`\1078250\1010246\SYN\1002360\1079817\NAK\1057435Jj\33218\1089015\34248?"}
testObject_Name_user_9 :: Name
testObject_Name_user_9 = Name {fromName = "`o\1104473s \1062121\DC4\ACK]4\139353\&1\STX6\EMy\162694\57698Q\131571\bBh\111001\CANi~X\142550v"}
testObject_Name_user_10 :: Name
testObject_Name_user_10 = Name {fromName = "\1050824\155322oZe9Q|\DEL\83504\34080\21194W\142105\1110603>`\"o\1071465/\SOQc\99257lX\1057087\DC4\ENQ\179265 LW\13467\SYNq"}
testObject_Name_user_11 :: Name
testObject_Name_user_11 = Name {fromName = "\DC2{73Y\1078460\1104396\DC1\1025708\&5uB\ETX"}
testObject_Name_user_12 :: Name
testObject_Name_user_12 = Name {fromName = "hsF\173991\1044255\984885\1064426Bg\STX\ETXp|\CANE\144409^$u"}
testObject_Name_user_13 :: Name
testObject_Name_user_13 = Name {fromName = "\63112k\SIF\FSQ\135065\CAN\1060944\&2J\SI\NAK\ESC*\1060678W\a]a1%s_\v\1093799\DC3+"}
testObject_Name_user_14 :: Name
testObject_Name_user_14 = Name {fromName = "\STX\1046129\b\ESC\SYN\GSDd\"P\RSyGO\74790\1029087&QC\1026931\SIY\1076085\1109874\NAKIm:r\US|\19161\&6\1004353i\b]\DC41\1059079\US\ETX\fW\ESC\1075053z\DC4*\EM\1085104j\1078675mI\14540\997072@NcN\DC2\1042615WZ\1062374\&6Bx\999116s\30258\fz\1058833i\NAK\DC1B`\"P\v7g\8475\156211\&59\100152f\NAKD\8240>\987462\1059761\SUB0\f\1053656\DLEJ\83035p\ETX@\1093607['1\1073540n\17715P8{!\1068747\1033753o\FS= :"}
testObject_Name_user_15 :: Name
testObject_Name_user_15 = Name {fromName = "88R<\1020174i\ACK\US\vt\69673\ESC/?\132473^\CANd6N\1026911\USJw\f'|W$\1020908J%\FS\1050755\988080Bt_;0\ACK\991364\35630\SOH#Ye\22538\bna\178191\&7^\NAKL\1026457\984581\1034017\12526o\1091993\154571'\RSe.\1017187\f\172767\1016464\&5-\RSK<\177558<\r]b\996446n\1052651\DC36I5 %w\RS'K\a1\1034488f^3"}
testObject_Name_user_16 :: Name
testObject_Name_user_16 = Name {fromName = "\SUBA\1001565y&?a\nuC?B\SOH{q5=T\1096771\&2\ACK%akW\138996wJR\fR\20673\134464)"}
testObject_Name_user_17 :: Name
testObject_Name_user_17 = Name {fromName = "?w\US9\EMZ`\1050267\1042434\ETBo\".v\1025637\1011562ex\DC3.{K!+~h\EOTE\NUL]\",\1039244\144082\148072\DLE]E\f\vG\DC4\133880\14056>\998869}\6802\ETB\62510{]n\184255\36335\179292\1060271`:WA\NUL{\984104"}
testObject_Name_user_18 :: Name
testObject_Name_user_18 = Name {fromName = "\EOT;\1034959\SYN\146838a\1070837u\ETBdjQ\DC3p\138331\&1o'p_~kiO~~\21192\"\DC3\185608RKA\v\1047747Q\1105603~\DC3\FS\190778YM,l7}\189722S8(\DC3?|?;\1105432S\157756\26081\138562\b\49645\1095116B\1003208\&0|b\1024958\1101278\17752vC\ESCN\a1\995140\vi\1082158\tb0G\1057355K'\997721y\n\1108936\&7/\"j\"\1094033A\\\DC3d\41666Fq\DC1|m\DC4O\SIUvo#J[8\DC1}C\SI\110976Z"}
testObject_Name_user_19 :: Name
testObject_Name_user_19 = Name {fromName = "\179276p\NUL\37677rg52{|1\b\"\194594\b\1066484\ETBbj\"=\1092800\1052159\RS\62812\1000472N>44w\US\194920s\SI\983167`~{\\"}
testObject_Name_user_20 :: Name
testObject_Name_user_20 = Name {fromName = "Ki\42644$'7\SOH\ACK\SO\US?\1016926n\1009832A+\1106832\1066880\NUL\FS\1061577\&3\1107902\1040620\&4OV\EMQ\DLE'\1001491NxdP]\995523o\ESC3\1089126i^I\ENQ\a\1000237\GS\1059655\DLEpt\175936\1102581:\1099787\998231 G\SO\nq\"\1074436?Q$+!hX\SOH\SOA\133567aD\1038443\1111067+\ESC6v-D\SOH\ENQ\GS\1059735\USO\EOTqWc9y\f,V\ENQp\asWD`[3s5"}
