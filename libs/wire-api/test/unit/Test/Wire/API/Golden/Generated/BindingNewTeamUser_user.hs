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
testObject_BindingNewTeamUser_user_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_1 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\v\178253\ACK-IdE>+\SUB~\33283\20414\172192N@PL!3\a>l\EMlVFXR9#(\60818<\999942W\1101433\&6[\985135,\SYN/\996072\1072520\194643 +\1022599]1x\a\1111213")), _newTeamIcon = (unsafeRange ("\136191E\178887\78839\ENQMg\n\ESC\NUL\59714pC\SYNFM\16209\a\DC28\EOT\DELi7\141755_\1094604\STX.\DC4vs;_V?Yi?#!\157105}]\1093028\18272 9\1075727\&1\SIEm\b{\1113235$\a1qH\n\100555\998171}=\ETXl\EOT\135299w\SO\&H\CANh\CANf6ymA\DC4<\143583L\1094349Y\175083\vT\1093550MlGr_\NUL*.\1099744\&7\1043708\1034462R9\49320\157393gY\1040950\1028639\61422o\NAK\18729\DC4")), _newTeamIconKey = Just (unsafeRange (":\FS{\47142\&1=H|\ETB_L\v\1039781\&1\DLE\1106747\CANvh\4500\SUBLg\1062143\94443\NAK\ETX:H.|V\1050652i\RS5h\v\69863\&7\RSz-g^\SO\ESC\83298Qy\SO\1107006\178679H\1073480\&3?\ENQ`e\"t1{W\RS\1056552g\998032\DLE\NUL*\NULG\SYN?\ESCh\1020730\143301\1106668\999944")), _newTeamMembers = Nothing}), bnuCurrency = Just HNL}
testObject_BindingNewTeamUser_user_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_2 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1102518\1098926 39\171767\65931\1059278F\1048828\94417D\1070656\b\1058139w\1046712Q\ENQe\1015527 \US'N\">`E1Rr\GS0:)F8\v:s\SUB$50>\63723\1074864f\r\145406/<\ACKS\1004203C#\31440\1113901\&9q,\992132\1075990\1083354GzVx\95216\&6\FS@W;.F\1035623\19430\50656\133060\CAN\RS\1016467\1080164\DEL\SIw\t\1074152P:F\48159dZ\NUL\19959\96400")), _newTeamIcon = (unsafeRange ("\ETB\118908\175055\ETX\1069018\987986q?$D\1045409#\148136\1012109B\189922[\STX(\GS\1032021\SUB$U\15454\SIT\US}")), _newTeamIconKey = Just (unsafeRange ("'EL\DC3\1073569-[^\24194t\STX\aUOuej\178175RG\ETB\1038624o\174900V\SUBrMN\40099#\141639\DC3k\40814g\170293\NUL~{mO@z\64713\7549_\29775\1009813T:\1001969,\1067067p~\EM8\985044GF\DC1@\1047997=\1025250T\28674:\134979N~\135564\1052027i\984481}\ENQ\FSd.\1110638RA ZCp\\\135886\STX$p\FS\22928n\59885\&4\148524c77\1010158<n\1057219\&1\NAK\1035510<p\bP!c\apsu:qdIDe\154536\146617\68148)N\1070526En\ACKT\EOT4]$T\1054931\139449(\92669\ETX\994038\DC2?\ESC\FS-\RS+\30057\&8Fj\138395\146511`\1045664*\RStZ\RS\119179\1051506T\GS@\1076334J\SUBL\36100\SO\t\164270D\1033219+\148391P\RS_*\NAK9{\GSEGAo\DC2Z\147432\&1N\1026032\1087390\bCb\151113]Q<s\189654QH\1079083\16319\DC1\1006499\28904\DLE\DC2\v\SI\1043558\1006807r`\SOs\USYC&[R\DLE\DC4K")), _newTeamMembers = Nothing}), bnuCurrency = Just CLF}
testObject_BindingNewTeamUser_user_3 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_3 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\60203g\30949[,\181969\US4\1080267\1023644\b\t\SO\989236.&VA\1065060:;\172758N\1041888~\ENQ\SOH\188919&\SUB\992665!\1090666\ESCj\129337\1090910\44206U>TtJ[3[\32844R\100157\1042371^!K\3309\DC2zy\f1\1090602bA>\NUL/|?\1106368\1074791(t\177128BF~\\H\166946-T\DELF1m\153388\186526ryB\DC3v.\NAK\1074933\t\984197;9#)*\44589\162715?$\DELg\SYN\26288!*\1078708eld&\DC2^e(\ENQ:\vM\1077466\&8L!\60998\96699\DC15(tv\994328kA\SUB^\ENQF\\\CAN\66725\135106\DC4e\DEL\1111717\&3n\92728\1008466+i\FS\1092351x14<C\11819b\n+\190326$<Nw\1017056\DC2\vm\95485\1090510\140434\1109640\n\160972P6\NAK\tnMn/")), _newTeamIcon = (unsafeRange ("\59271w\US?Qs\ESC^\46486+\155223\&1\ETB\FSz5\GS\11478\1068736`q\DC4Vty9\98441~\US<FF\GS;\DC1~z\110688'hD,\"\t\1065894\&4\54517\EOTF!\EM%H\21347\DEL`\a3qmO^MEi4g\NAKY\DC2\54935\139050\142369\&6\DC12txU*du\DC3:\186785znG2\118936.\SYN\DC3\146395\28270c\1089728\DLE~8\CAN>\58790\&0HT\US-P\988303\1004002\&4\1094170\1082805\&4\1090400T_i\STX\1037530\991690\181640==}\NAKL\1054650\78590\194701\155830\fT\99737e\163558\&4\SI\38476\1044472\1010854X\ACKp>\191373\54892f\1003280pt\45159\1093019W\ACK\NUL\SOH\160032 \EM~R9(\168195\CAN9\33441\1010962$Y=\v\STX\1078262p\ETB\986103\EOT\1077513\4706\&4q\137909\\[yj\ENQ")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just XXX}
testObject_BindingNewTeamUser_user_4 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_4 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("t5\v<\18463\&8@\1035600\38509\f\121396\r\7461u\FSQ\139753H\ETXP`^Z6\72203D1 -\23031'\182573C\1043828\US6*\95970a\50987\GS\DC3\SUB\174176\1064450\"1^\NUL&?DlV\DC1X\4520s7]>\ETXKVv\94484}'c0\917889\181837\71325?t`\1039418{\37932\1047486s\SOHBNo]V\175892\1106771yuk\335$\120731{\t<\1065929\FS\44726(\GSb<R),\fv\52653.\STXPT\SI\NAK}\63259>\SOp\96305@\1021945\FS\1070049`\DC1eA\397\rIg\\W\1081586\ACK\99634/:xgdO\1041376\1092444\1033331\SOHi\ETX\1102104&o?#\1025456I\NUL='\1033522\DC4c\143239\1056395m*!\\")), _newTeamIcon = (unsafeRange ("\185145\FSPc\11215aII\92166\SI;A\FS\11433+|j\991372l\SOHvo\DLE\1033710\165568\991141q\DC2QpxEy\1039431!_\1032268\14898*)==P\1070916\136351gB5>|x\SO-\183674")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_5 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_5 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("G\1097103Q\vb\ACK,}xT-\GS.\SUB4vf\1080606|\t\bzrV[\1055106R\n\1089672*9\1011532|\149093U\1087102\RS\"^2\a3\"3?\ETX\1070248h\"j\1086912=\1019171\NULbd[\DC4|\92331\134509\78882\f")), _newTeamIcon = (unsafeRange ("\31859M1\133261qu.\6551'\DC2;UW\1071893E\CAN\STXx\1010478VtF\96718m-\DC42\191314\1009008\41078\187076aX\1091044M\764-e\137642\ETXyhz\DC2\\\1067691\60972\1017321j\"`\rXDPN(R4\DC2\1057891%\995520]\CANB.\1062170.E~\1043389Z\NAK\DC3kq\bxn5@3\ESC\SOHt\"\176826\1073639z\161725\49763}\1042903m_\1037743\ENQ(y\na\DC1{%\994919g\"eE\65672\1085214Ht\ACK\EOT|\188633[\1006459<Xw\RSi\EOTB\GS\DC1K\134954\95331\178875\"\EMLZ)Ew\ESC\1093239hr\v4\1089235~!$\16240\NUL7\1005038N\1019m\1024816T\DC1\ETB\47375\1071851\&8\1094296\27756D!b\43081\&5Q\1077293\160569\&7\54252\191064/\1081550U\DC2a\48182xfG\1045354\142175$Q\11717*\USm(\STXO\rliv\FSt\1059161'@%io'\1099032\DC2\1004178L`\169610\31766\1034220L\EOT^Z\ETBb\v|\SOX%,kc{\37667\SOHBlRTJC\43205")), _newTeamIconKey = Just (unsafeRange ("H\1070560\CAN\161706;B\48339&\ETB\EM\DC3\188471~S\1037974\&3\1029265j\STX\1006512\SI7bk\1088086\1014023\NUL_\NAK|.\DC23\DC16\1082180;\1013547'\ETB\SIB`r\1016435\\\1019588-I\SYNM\1028202\33753P\DC14\1068668\GSUG`Xbt>h")), _newTeamMembers = Nothing}), bnuCurrency = Nothing}
