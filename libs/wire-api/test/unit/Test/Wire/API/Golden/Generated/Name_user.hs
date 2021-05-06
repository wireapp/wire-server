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
testObject_Name_user_1 = Name {fromName = "|me\150496N<r5\CANHV\25842\&0d\78381\ETB\17937C\163884\GS\NAKc\STXy\151565\tc\1005066\984264U\ENQ\1087126}\1109579\1094320))2\RS\t\1068135\&9\190648~`\150139\58962\&1\r$\136286\1049699#\9186v\USG\\\t\GSM-\151019\t\6726q\140867t`Z\177365\&25\151094<]/\n\1075126\ETX2H\137691\45073m\17584\&1iI\19338\CANDL\159929\1082592\&8VQ\66305Wi@#B\38213\1057623\a\78531\f=\187522\US\DEL"}
testObject_Name_user_2 :: Name
testObject_Name_user_2 = Name {fromName = "\131691:NN?C\"\tS=v\95089d\1024493\ENQ\DLEhwa\25654\1007540\29741\tB\1076305V\ESC^\1073329L;=\1038433&\6692q[\1085502s\t\21656}\138887\1113568\4408J_m\1017260~T\1107965\v\1033963\43262\&2mX\GSla\DC4sK9\GS$"}
testObject_Name_user_3 :: Name
testObject_Name_user_3 = Name {fromName = "}A\989836\EOT\DC20\178535\DLE\163669%M\177300j"}
testObject_Name_user_4 :: Name
testObject_Name_user_4 = Name {fromName = "N\ETX\SUBx9\1066060\SO|up!\"\f/)9(\1049355T;"}
testObject_Name_user_5 :: Name
testObject_Name_user_5 = Name {fromName = "){\US:\1049841\&1\29703!,~K\152116\32405\ENQ>5\USG|@\158822p\EOT\SUB\97078\147632\143187\1053443\ETB\35893\135893\EM1"}
testObject_Name_user_6 :: Name
testObject_Name_user_6 = Name {fromName = "\ESC\1042075\DEL\nd\986184\ENQ\983939\1089923\1073188\133729u\175622HLv\DC16\ETX\t\50012#\28831\74243,Cc*f\DC4\94903\8044`\3051\&0LGb>\1082607\&6*X\1052729\6077(<\vd\a\1070146\147569\181239!}\1067968l*8[\ETX\SYNJ\1101023\US,\DC3#\b.L}\NUL\NAK\"\1086689Lu\STX^9T\120896\RSa\182859\GS\33615l.\b54~\148778"}
testObject_Name_user_7 :: Name
testObject_Name_user_7 = Name {fromName = "\ACK\1049481YH\10098#\188647\50158CI_\168653r\4473\1014147\CANb7}DAkK\4540\&88\DC2+\1088540 \1034223y\64573f>z\59568\&02ei\SUB\DC4B"}
testObject_Name_user_8 :: Name
testObject_Name_user_8 = Name {fromName = "\61239\1103382\STX5Px}d\127173\DC3\SOe@\190853s\63641+h]m\US\1058287\&8\US\1002858\SYNL\1033088\1112091\&4f+`\1008120]\133658\bK=\17732n:\110877\ne-[W\SOS*\SYN\34410zxsG\STX^X\1008620YM@+#\n_(~X!E\28297\169302o\167496\&3\ACKP:\1000722Ay&O\6512\1094860po4XZZtxu\DC2\SOH\51894\1017062\GSg[ \70849\\g\b\1035144\v\176500S*\DLE\EOTx?\FSe$\145277p"}
testObject_Name_user_9 :: Name
testObject_Name_user_9 = Name {fromName = "X \190543=AZRb^-g\v\b]"}
testObject_Name_user_10 :: Name
testObject_Name_user_10 = Name {fromName = "\1058280\GSC\1046697Xa\DC4V\172946I\FS?3\ESC\EOT\SUB`w2p\162637\1049324\41714<\SOHk\191137\990977\&4~\1106939F\1019227Ql\SO,\1090496j\1015701\&7\994788_e?\ESCe%\98378\\!NX\1443I<%\rm!\997839gd\NUL2Oc\STXOzR\1060512\SOHi\1021269\b|[O\ACKPJ\45168 \"\1010118\&7\43336KvHK\STXF\190614N\ACK\DC1\n\1040041j2\GS"}
testObject_Name_user_11 :: Name
testObject_Name_user_11 = Name {fromName = "\SO!%o\b\ESC\EOT\ACK8N\n\169640oaD\f\987710\174745 \997298[dh0\178718g5\34150\25338\rv.]\34684\1044476({\167101-\1018456+\DLE2>@\GS\SIoB\1014887..n\1097125xt\1010225\1105361X*\1035826\&4\GSd|'*3d3=\ACK\170810g\8223\83074\EOT3\DC4\SOH\DC2\EOT\SYN+Kb\CANj_=\v"}
testObject_Name_user_12 :: Name
testObject_Name_user_12 = Name {fromName = "\155259u\170429\USOD\41575g`\n\t,\28723"}
testObject_Name_user_13 :: Name
testObject_Name_user_13 = Name {fromName = ":4C\ETB\1036260a\142128\&5\1015373\STX3\39673\NAKML\1046872\1075618u\134971\SI\FS\SYN\1097984o\\OI\1091309d>\SYN\47446x\EM{7[o\1039343\174948\a\f"}
testObject_Name_user_14 :: Name
testObject_Name_user_14 = Name {fromName = "\1041478\\\GS\167449\\\NAK/;dsUB\1089528:%\186130c\SYN\132302d\EOTn\1036590\&0\36593\188978tSh('i\5115JM.`\177611"}
testObject_Name_user_15 :: Name
testObject_Name_user_15 = Name {fromName = "\1059826\144662\1081018\184545\1075844ud\RS\1014493\DC1\n4\DC1\20219V\STX? 9f*d\1039625~\"5#\170675IWeUA\153352's\EM\ESCcu{\USs\165496pXB\"$"}
testObject_Name_user_16 :: Name
testObject_Name_user_16 = Name {fromName = "Bh\EOT\US&{/\DC2\1094380{\27545|z"}
testObject_Name_user_17 :: Name
testObject_Name_user_17 = Name {fromName = "!$\DLEh,\EM\"\FSL\DEL-|\183243\141171*\1058539\&0}\55055\SYN\r_\41590\78336\STX5IgYH\1014063Ch\ENQ\EM\4444F\16683\&3\1033373\DEL\t\EM6\74981,{4_\SOHLo$\43039%\ESC\67096\a\1034194\&08_\ACKZ\1005087[d]\1108217\&1\1052591x@C\RS\SOHjE>>\SUB\126519\1066855"}
testObject_Name_user_18 :: Name
testObject_Name_user_18 = Name {fromName = "\f\1096358Q,\52764\995337k\EM\133728\DEL\1045994\EM\190722\&7\1067496\DC3\1091733<H\97961\1111646\DLEB\EM5\SOH0)A\1087156\1063111\\\63574\166469;\152179\&2L/\999005\995885bV\"I0Yk\38419~]y\DC3I\ESCbxZ75>tDnEf\132248\168543w\1041422\&5;\DC45\ETX\54817\184798\&9r0\129030\&9\EOTFtL\t{;G\ETB"}
testObject_Name_user_19 :: Name
testObject_Name_user_19 = Name {fromName = "\ETXOd3w1\1069491\vr7\EMp\1030489\186312\US\96151)k\1042541\1082692o\23018\33239&\1003209\"e\173208V\11267\US\ETB\DC2)|\161987\152734\1060589\1112376\1031555\&1q3\1105546)\78037\ESC?\120424$m|\1094482\73129;\20395Z_${Q\GSVy;|`\a\CAN\1069520Z\13111:\994163\78179scln\97386\n\ENQ#\DC1\tGi e\1101154\ACK\1068991|\31057N\1040881\175589\1110501K\EM\ENQ\1103302^Q'\1002790"}
testObject_Name_user_20 :: Name
testObject_Name_user_20 = Name {fromName = "J\34094\NAK\ETB-\165825un3\15403Iyk6z=~5\RS\155294\&7\DEL(\SO*`G\1046229\SIDv\1050820=h"}
