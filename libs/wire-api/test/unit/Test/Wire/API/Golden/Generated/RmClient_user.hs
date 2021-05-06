{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RmClient_user where

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
testObject_RmClient_user_1 :: RmClient
testObject_RmClient_user_1 = RmClient {rmPassword = Just (PlainTextPassword "m\EM\99626\161481\DEL\1111909\28221\ETB\100097\1093989\nB\ag9z\EM1n>\t\DC4zucLR@\",2a\1094317\153121\US\SO0J\"O\1019047>\991950\46088WKntm\1075684V4?L\NAK\n~H J\SOH\ETB\19473l]i\CANV\5907+\FS\179551'CP\EM\rB\168723&\SI\129312S&R\DC2\1035138!0\DC1*I\1055913_(0\ACK0\t\1098021Ry\26444I\SO\f\NUL\1105061\&8\STXn>g^I\1024052\1081571\1102429o\SOH\175723`\bo\ESC{\5729:\NAK= S\nvU\1058562\DLE\1072829d(\986044\&9\GS\1008481g$\DC4?\DEL5#KH\1017854[$\141517\62863\96370\1094194,^E6\1032508\CAN\DC3\DEL\63770O\ACKF\1058411\&7NUm\ACKZ\140142\DC4K\176303f\1077879M\a\DEL\1065809\&6\37968\DC1\FSj\1104671n\993500\95405Y\1007788\58728:\US>U\ESC7)_\1019519&\1107331\v?\187864H\988217\14201\179988,-\1087440\1038854$?\1111558\1025122\1057495\30159J++\SI\1089395\1092614\SI\1097420\1062549\999014\t\DC2\1044444\12063\995430\1049364[\CAN_\vBq'\r\1046530\a\SO\ESC6\DC4@\155640\&0\1045955o$\1010650\1046916',0\DLEbV\t0[\1035613\&5\52682\&58:I\185383kN\n3]`~A\SYNm_\\Y\20574\1079538F6")}
testObject_RmClient_user_2 :: RmClient
testObject_RmClient_user_2 = RmClient {rmPassword = Just (PlainTextPassword "pr\1053430Q\GS;$d")}
testObject_RmClient_user_3 :: RmClient
testObject_RmClient_user_3 = RmClient {rmPassword = Just (PlainTextPassword "\r\a\97463\1052264[\STX\DELo\t<\1033485\ESC\1044412a{YkVuUfC\64644\SYN\ESCD\1030712\162874\190708*\36988\1105232\GS^%\78737\DC1\FSgw\164821`e\DC2\DEL|\DLEj3Tk^B9\ETX%\STX\1068119U\ENQdF\1037782(t`-*\8098\SYN\DC1\1008741\STXx\30117.=\f\ESC\985586e\167019Og6i\18844\1085177}S\133850,\1033331&fI\ESC\1082000\DC2g\146502\1082933mOitxtt\DEL\ENQ\98739\STX\69600\&9:\ENQV\DC4\ACK\a\GS\1097989[\156916SD\r\r\6302\DC2yi\1075793\FS<w\RS\RS:\167196U\163255h$\69386\1058564t\SO\12319'A|# \996153nSp!\NAK\1000120/6;\1087699^\172054\1099952s\1004036\&8Gc\DEL4q[\1055218J\1050683\995170/\1111733\&3\SUB\SOHNb@R8)C\1057362\180042H\DC3\FSt9Bo\39618\1099490d\USl\DC4ms\157128\133138>b=\1053503\NAK\1039453w\1106222?\95641\1052259\EM#\v>0|\DC3\ENQ\t/C\1009270G\75019\\(M\1057508P\985448\\\29174tQ\992323\CAN(c\1109506bp\1070817\RS\1002734,{=\1072442u\98596KIcj\25216?|\NAK|\172919v\1006087\EOTO\1065269\f\"%rdq\EOTk\12337Yq?T\ACK)K\DC4@\1081916Q%v5\145518\1040987\"/>\34695sA\ENQ*\ACK?FtK\1028703w\NUL\NAK>\1054387i|^/\998861\986608\&32\SI\995410eX\1073836\&2\153376\1025696DPAlL 9F\RS\46383\SOH\1026381CapNGoy\100400\60012\1081442Tf\RS\991920\38919\RSJ\993996\&3 /+cr\ENQ\NUL1o!6\94511\28643ThLg5js\NAKx\1097823<\US\ETBQY\40629ei*\153483B5lH\ACK\ESCjSO\20683\71721O,\SUB5sD\1018680\&0\139136\1000441$\167302VPu\1058689\a\146226j-\DEL\SO\1029471\1043246\&9{r\aD\155061\62017\31946\DC4\ETB\1065421x3\r}\ETBM\ENQ\SOH7.\156561\FSL*\1105862\145009<\1072426?P|\1107733\DLE\1045077\165017MG@?\1063475o\b\1077286\SYNq#G;;\\\1026734\1001575\42597\39259fUb9\125121")}
testObject_RmClient_user_4 :: RmClient
testObject_RmClient_user_4 = RmClient {rmPassword = Just (PlainTextPassword "\137594C'\10064\v)R\DC4\1011135\1010884\ENQ\GS\66608L);\153695\ACK\11683-\65878`\169263\1014256y\1004260Wwk\b\176101\1051146[\ETB\143724\152993\&7K\1045205\96610}LEV\1054879x2[\1026032\ENQJ+\1074983\158554\1098358\CAN\1018416\DLE:\142483\74836`0\STXcJ\1054243\1047620$\134256\DC12?\1006075\DLE\51128\SI\125102\rAl\18767\1056471\SUBy$7j:cq\189937]M\83211\GS\69652\SYN\1068593VkP\991499\185387[\1024849\t\SI\128301pzY\1028597WL\161744\&5\180093\1059586\ESC\b\ENQ\1013174\SI4\EOTEJ\54561\999341\&3\1076159Z\128871\NAK\169255]6ho\t\159032\1046673r|q\ESC\DELLu9\38714\SIe\184120cg\160496Ya\997777\fS\1047078\r\52698\161710\GS^\\1\1070861\CAN\182920Am.s\a'\DEL(UnB}+0\DLEY\183415\1013671d&6\1054167\DC1e\1080478\1004607\SYN\176162\RS\ACK\1074884Q\1068933K\97897\1010543c\137203~:\t\22915>U\GScT\1033294O\169145If4\134986\t(qW\153477\134782\EOT JL\SO'A\DC3\DLE[f\191429\49635\161212\f\USu\DEL\ACK&\1031827e\ETB\20367/NZx\168807\1058979\177512W`E\94543\1085645Yk\985221\nD6Ux;$&\1049278\RS\f\"\DC4\f\62193\NUL\EM<\1063887\67258\1018221\1064781=R\62230\1050152\185296\18434\\\v\DC2\FS\23305xxa}u<\ENQ\DC4w*#6\EMT&1 C0\r\n\FS\135962\14834X\10425\1007457+l\SOHe[<\fV$\158112\ETBF\f;3>i\1088216\SO\1090677\1058526\1113420\"TG +\1069008\1064657E[\DC3\138771\1033237rR\1049796\a>mmyiV&g\1009324\EM\FS(zS\17497\SO\DEL[w\39265\SYNN\ESC2a\1045019\78061\1050798wE_ElH\aKD\CAN8jD\1107232E\fB_\51744-H\178670\NULU\57506%\NUL\185089-W\STX\135748\48159\99303\52710Qi6\1035008\EOT\31047\1004431\1036567\96044;.\f,\bs\24550\&9\1101125p-\DC3g=O\STX\fdmANN\FSR\1055573\SIyH*\189387\SOR\GS\\~^\149678\GS@\r\1079062\73854V\t:R\ETB\EM\1038778\1066879\1006010\119822_!!\170833\SUB[\\,BV\998095%*dM&\NAK\SO,\f\DC3hT\168903\DC2EC}\1070216\r\94903\1032814%\1026450-]'h;>Xj\191260T>[^H(\48528\172809\STXM\999294\EOTh8*\STXJ9\49276X\a\1011635D!\95316\SUB\STXsz\GS\NUL\170511o\SOH#\1083907Z\119212\1008024\GS\EM!1\1070698\1066568")}
testObject_RmClient_user_5 :: RmClient
testObject_RmClient_user_5 = RmClient {rmPassword = Nothing}
testObject_RmClient_user_6 :: RmClient
testObject_RmClient_user_6 = RmClient {rmPassword = Just (PlainTextPassword "9,L\187751\49912\1050260\&6S$\38500\172579\8279\US;(`w\140471\37308p2A\1072195\ACK\RS\173635;b\172467D|\8846\37682VND\1070619pB\1108246rj}aQd\8560\1014235?{35%}D.?\164117AJ\DEL6Qd\1059684\1098988\1082976j\RS5\1047233\GS\110847\CANW\119302/\USeF\184511\987964~/\1026951\188285}q`Zk\54877\&4w^\999911]\SYNz\40432rw\FS\43809^\1076402#\1066869\1081507Qg7(\27848z;82|d.&\96027=\62173\&7i\CANp\9393v>\1024735wyi\986215\&6\1071502>\DC3\33406\r\GS\1101790\\i\986538D*\ETB3\11387\nV\1045144D\DEL&O\DC19K0Kw\a@cR\FS\DC2\174696\v\1045168`G\SOH\EOTh\ESC\1106402z-\156152FN^\SO\167355a\1009458`g\b#sZ\DC3\15160\1100149PFn\rWM\1068179s\5461\41019cZ\USB\1011315\43224D\EOTZgq\989799\DC4P\152147\69446}d\151246\183248\140266|\SYN\167913M\95808\&9\tz\ETB\n\ENQ\1069976fwm\138873\1060119\&0\141315Cj\1051831y'gD\39268m7p*\1002967n\138026k\1056989\1061015\CAN\DELn\SOk7WN\FS\1024642\1112472)Q\1043358\v\DLE8\DEL<]k Q9\178797\ENQ\1008753\51138\1108277?\1081746}W@kL\SYN\tw\66764b\1108317\&43\GSvQk]tg\154864i\991475w<\1016640|\1062109\CAN~Kz\SI\37633:\f`Y\ETB#\52578\\\ACK\57398\147803\&2c0\n\a_\a\988483D\993682\&8\161043\45797O&1k\1070162\1019092b$\1055375\15719_\34763\SO\a\1054555k\15390I\190932;i!\r0\147234\186280\CAN%\172829V\a\1101548\113686\ETX\FS\ACK\178624C\1005638\ETXY\1607\11298bJr9l\CAN\DC4\1027575\1085780\37947\1055313\&0D1\FSI\DC1a\EOT\DELN\1070770\DEL\95490\&0\ACKF+;\ACK\1008144G\153126\121479\1044727\NUL\1012574nlpun\132183xfRA6\33844\f\RS\NAKz\1041941\t\173567}&OK*)L\15281\ETB \1001341\SYN\ETB_")}
testObject_RmClient_user_7 :: RmClient
testObject_RmClient_user_7 = RmClient {rmPassword = Just (PlainTextPassword "\SUB?Y>\118892c8{u\SOGR2O\DC2I{\SI>G=\1009956\DC3\SUBf2\txu\CAN'\RSA\35396#\8017!\127369\SO\ACKN3A9\1113176\DC2<\ETXDo7%\FSp%dh,y\nV\SO\162145v\ENQMJ\1080962(sKE\n!\SOH{\v\1021920<\DC3\GS\63009\GS{CG\1096421\11043?<j\1073608*!\f\70388\DC2\1033704cY\1066312\DLE#\v\SOH\ESCSR\993099\FS6lg\27423wltu`z8H_\1032090\tQ\"6\SYNw\983587M\59454\&4rB@\DC2\DEL\30100\f\USq<t\1057418\&1L\STX\EM4\DC2\ACK?z\DEL\DC2\GSK+M\54059|\1056645\1033265\996917\&1\184309,h6\142298Ju\120501\t\EMgt\1058136\DC2qm/\NULF\b\77942\96678}sJ\ETB2\ETX`\47671T\nS@=Bq}Z\1062381\1060864\&9E@\149430)v\1014749\&4L_O%?\ESC\ACK\DC13Q\\T\DC3j@\vx:^$\4842\ae\DC4(\1035909\&7*Q\1066921!W\SOj#\SUBwb<D\DC2\DC2.B)\53396e41\SI\t\1021385W<\SUB\ENQ\986149\SUBG\FS]yX\49954\1020322mv\DC1\32970[}\57453cW>\64012,(\DC2r\DC3\92715r'\SUBa\RSw\ACK)\DC1s\EM\1110833\1056137\1094396#\RS\1112742\187544f.\1093927z\1023937Iw+9\2676\1063726ZaHro.b\65223M\tWUM\21339\&1*\r\63360^oy5^}\188906!\1043609\1006724Wh5\NAK=}7\1102883@\1088946\2794\&6o[~bog<\1037651\1098260_M\1015552D\169962m\1044103\1026744H&\FSU![\1072939!Fn\rcu\DC3R\n<r@ 0f\US]T\38393\993208>;&.\GSm:ONt\987646\1034845\174924\&7\1004417h\1002114\65230\39924uZG\SUB\DC4\FS\"v\137453O+\1055609I\v}!h\\\16048\983299N{J\DC3n\1104086\126994_\b\1002027\rs\1021480`7Tu\DC3\DC4\1048703^m\b\tk\1080102R\b\FS*\1064855L\ETBVn\11256)p.\184901iV)\21378Xu\153753\&9`^\119181e:W.\997235Z\f7\1094482=X@\",\1091035\1113281ziA\DC2\157336g\67315z\1109261KL\1092672\SO\171864kzt\fYI\30054a\121502\ETB\SOH\1091024\118909xp&\aZ3bv\1061130\DC1wQ\149693\SIn4\"=x\1086856z\1013426\152644v<\1097242\EOTz:ze&\EOTb8\ENQ\SYN\9401?\33785\985468\&45\1091830?\FSD!sZ_0Y\"n\35246\128702\1051450j1~\USB\184710A\1086494\v\"\1009096p\SUB\ACKS\DC2\1103940\83081\NUL\FS\41660\RS\1045364F\"'\195045H\NUL&kP?\DC4M.G\GS_/\DLE\v^G7^\EOTQ\SYN\165895y\SO\DC1\57508\1035584\&1=n-e\53803te\131360] \ETX\16509\22412\1093946\34464/l\94340\1058769.\58371}-|hW\FS\1042857]y3B-#*S\74408\ETX\1042244T\SIo\DEL\78891\1027597\EMr&\ESC\1013203\CAN2\32373\FS\1043972` JS\aX\a\1087457]\DC4pH\SIs\ETB\STXv}tj\21467p\"Z6F:\f\f\1076701l\aYY\134072\FS(\SUB(\SI\118807\ACK\1055906\1032199wx\SUBk_\r\ETX}8\1096115\1073043JN\DC2cs;\95027R0\\\GS:[<Z4\GSz&\1093054\SI5\51716\165716cpzRl\SYN\NAK[s1t\1000380DHqbhXW\58391$'\t\1045586H\1000534p\tp\EOT\b\ESC\31911\EOTpU\156048t\f\ESCt`\45759\28861\rLWV\ETB.iuL\NAK\ESCT]g\CAN(Q>7I\12565\ESC\DC2PJ\a}}&BG;#\SOH\167084t\RSa.z\NUL%yt\1007527Q\68615\ETB\1025531\ETXr0\1047965\1065375\27038}b+V\1075681\&0\r\DLE\1073647^`\b) S\rR\r\1037337\ETXK\SI0k9H{\DLE:[;\169975\ETXJ\DC4\GS*\a\SO\STXK")}
testObject_RmClient_user_8 :: RmClient
testObject_RmClient_user_8 = RmClient {rmPassword = Nothing}
testObject_RmClient_user_9 :: RmClient
testObject_RmClient_user_9 = RmClient {rmPassword = Just (PlainTextPassword "{V|n\CAN@{\1074465h*{\996373K,p\1114043\ETBT\1091332bQx\alMl0\1010988\f~{~\1086573\988064#\RSY,U\1062141\1088598\1030437\NAKW%\f\993352ohS\EOT&R\992587_\984170\DC1\48619\1012953T9<\1073263\1020172\183963\&62\8357\995933\1038471\138952\&4\1022068\ESC\n\158701:\58293/Jy64f\1035208BS\NAKGm!\1228\ACKoI \185077\RSo$D\18546\1069871\&7a?5H\ETB&\DC1\988000\1114098\4849\26110\EOT\144798\176116D\RS\1111171\f\187006\DC4CH\t\t\156076yCU0kqp\b[\EOT\ETB\b\1103269\143230q/\DLE\GS6vF).1RE\"\154486\ACKs\SOH\985403\v\DC3\1028083\29633WI^$Lv\t\994220Dc\US\"s\1057073\15196\1097617'[t|\991070\fk\a:4\NUL\1040876y\1097592j'<l.\t*Lk\f8`.P0*j\1088956\&3r\173656>J'\f6|\b\97145\15474\1107648\\\1060213?7y\1100451@\1052983S\EM\1107263\4354b-\FS\1004563\1049485\tI*\1027660{\RS\50213Z\DC4\ACKV2]p\atel\47569y\83375\15062\SO")}
testObject_RmClient_user_10 :: RmClient
testObject_RmClient_user_10 = RmClient {rmPassword = Nothing}
testObject_RmClient_user_11 :: RmClient
testObject_RmClient_user_11 = RmClient {rmPassword = Just (PlainTextPassword "\GS\24707=]Y\34695\SIE\ACK\SUB\SI<LxU\1042311\t~\GS#\ESC'd\1045579>\NUL&/-A\DLE\CAN\48663uI\124971D,I\STX\42141\&6_*K\NAK\ACK\175960\139119\36696Vs\162013YkaG[soB\ESCk\155021\DLE\15392\SOZ=\rQ\33391Q)\ETBo+\21494UTH('\1023972\1038257\1080010\1016700c2\ETX\185304N8D\15790\SUB|\1090105MT?\\\US\98228h\US\1032996A\STXd\1055587-)\ETX\96031\1085193t<Y}\1001115R\173423\t\US\1046793&\148416\1060426c\CAN\60935t#-w\1059486\162121,\152299o\1113344\176332\DEL2\EOT\NAK{}s\SOHU\1050419e\167895\18042\160638Y\15114\&5\NUL\13516\ETBB\SYN\"\ETX#\1048479GQ\150714\r\1055258\55124\44844\118901\a9ph\US\175752BW\1111101\SI1\NULqf%F#d\1099406nZ\45387O\ENQ\137144\142987\r\1018243=\1100662\&7\165906V\CANM\FS\49735\140920M\1081823C \992409)\DC4x\1075947\DLEeNYN\DC2\1031722\127512\40211qU(R!\ETB`7\1094792\&5\1002585\ESCUf\1091714p21!)\984387\ETB\98778\thW\1001244\DC3\100909p\SYNL<+\1082233Y\RS6\v\140536*.\1081810]a\989083-\40521:\151407\NAKb\ETB1\66900x\DC4\n\41858\DLEs\DLE;\27947\DELGnl\"WJ\NAK\RS\a\ENQ\1070620\186716#lPt\DC1\1019414\"\1061707\141957\34647/**|\EOT\SYNX \RS\ETXfJ9\1065298\&8\57954BG\SIQ4\1043661@\1007107\1022658.f\aRr{\29024=V\4272(\72422\92938txQ!\1059135<wF\n\21695e@\DC2\1088629x\161645r1\43571\"YSY9\47019\&1\43155iY_\DEL\SYN5\28882#\172494we\GSO}\ETX\\m\EOT\ESC\1012527\1095338<%\34085K\EOT\17497y1\DELWx\ENQ\26182\1028046\138170=3\1082242*bXL=0F\128377c\1069506\33125?\161206x\1099817\RS8d,\ESC/p\RS2\27948\ACK\100340\nZ\NAKA\1011601\&39\FSf=\ETBY\1028306~\13465h\13417\f,a\985564~b]Ah\SUBp$\1074401\US\ESCt!J\1066820Y\147741U\ACKM@V/T\STXz\1059589\&4B\n\992582y\t\1052484\1003770(e\20344\FS\1027921Oo1J%\1039288YpQ>~g\1025393;TcDA\DC1r&g)\140707?#V\a=\NUL4\177727\1052137b-H\1046288^\129506\&8Fi4WR\DC2\164194\n\NUL\SUBp([c;\FS\SImr\US\174906")}
testObject_RmClient_user_12 :: RmClient
testObject_RmClient_user_12 = RmClient {rmPassword = Just (PlainTextPassword "u\144733^|,\994523\ETX} =s\1061832\ENQF\94925\171779Q\SOH^\1050573\b\FS\1111899\&0\53039_Yt\1003540\GS\1027837z\992010%U\f\1014375\ETX}HM\127925")}
testObject_RmClient_user_13 :: RmClient
testObject_RmClient_user_13 = RmClient {rmPassword = Just (PlainTextPassword "'\34741\ENQmb\r\1028155e\1088471e\USjt\74616\&65@.gXWG\SYN\174826\EOT\153343\148615xZ<`W\fr\1041272A\US\1098636\US>=L0\RS%`\SYN\v\1055638`M\SOH\DEL\CAN2\SUB\n\138277\32336uu&\1026299\182126\158265\1038980+.\US\n\40033a|\1054266\1002033p\a\1087453\t\r\DC2s\1045532ahGXH~\1066612\&5\n\185575s\1016743\ESC]$\ACK\SYNK\CANf\EM\5587$\EOT>O(\1038486Y\136771V\STXlbPh\ESCe!>\SOH\1052715I\169686QuO\1111345f=;\USKem{r+\"\173214>\152654\FSZ\SYNF\DC4\SI>m\ETXss\DEL\STXHO\78051C\EOT\997476~j\n}\15227fy,U\STX\1005175!\bov9\142412)-A\173685\nL\153194f>\98548qb\23042F\fF\1113212\151075\25834\DEL\\\EOT\"\ETB\169483_Lek`A\41372\185260\&0]l\1017020\&1l\b\141214\a\48491.p\ETB@02\"\177125\7382L[\NAK\EOTdgk\CAN<_k\ESCC\1086478\179395\DELqdv\DC4\n\1052204E\CAN\r\US\900\CAN\137\&9!\US]e\rWVX\1038109\&2\1027905&\GS\t]e`Nt\EMX\190504\SI\120577\128665H\SO\1000557\f\tnTq|jn\178438-,\1012652\ETX\1087822\&5\"~\GS\ESC\119622H5z\986793\1011249\ti,u\SUBI|\US\993036\DC3\DEL`<c\ACK;8\1075101\1093242V,y\993960\988356\157318DZ?\95414BZ\SOH\SIh~W\163490\ETB}\\\RS1\35347-L\166234M\1089286\1029283?\59549\1041520\1080613W\SOHYuE\ACK\21071WzX\EM[,[\t\187532v6m\16063q\DC2M#\996687f\1024819K{v+\voDo\byEsi\27726:^iDJ?k!#m+\GS\18653\984987\GS\118897|\7848\17280H\1014699^+\EOT")}
testObject_RmClient_user_14 :: RmClient
testObject_RmClient_user_14 = RmClient {rmPassword = Just (PlainTextPassword "\CANNXCBm\150448gUt/\1106904\rp\SI5!?\1084311K8\v,$\EOT\ESCS{DK\986725)\NAK\167153\156311\98484Aw=\EOT\28430,e\SOHK\1061461\190372\US\FSN\1081243Q\1003210e\999542\FS\179003$k|;c\1034024\SO\USunD\1026283rRCq+`\ACK\GS0\92407\as\162085\DC3G>\DC2\SOHX\1111295\ENQZ\v\1028919.R9LrNc\DEL\146444#E\1016309\1106879g?8\a0\v\149729\1076039\45911\tb7\EM\DC1-Y\DC2x\29741\NUL\183928)\30245f\NUL\f_nQ")}
testObject_RmClient_user_15 :: RmClient
testObject_RmClient_user_15 = RmClient {rmPassword = Just (PlainTextPassword "\51245\&7m\DC4\DC2nA>ML\SYN\a\134159yKW\NUL\"\DEL\139614;E+\DC2=\EOT\DC3\185268\65533:Qf#\STXYR~\1051152\SI0Zm\DELr\133969Fj\986983\24857f\29166\&6o\148352\1076032qG\n\30740K\3314'\50879r6M7F\1112421P\9195Nl\aA\nz;\bF&ROe/\"x\EM\1007829\&09\DC3G\DEL\r\RS8\20475\100020\v\29054Sx#\36650\144564\SUB\ETX$C\STXe\1016899\1105764oi\1048328\991214\170666F@wYKV\1080349EZjF@ \14590\1043454\EOT( O\43139\&2?~\tqDUb4\1004913\ACK\EOT\DC1{\nWPYkE\159909x\170988O;j\EMXO+\14420H\US]\SYN\62964\94475\1044893\SUBFnb\83320\29623")}
testObject_RmClient_user_16 :: RmClient
testObject_RmClient_user_16 = RmClient {rmPassword = Just (PlainTextPassword "\134264|B\SYNcV5)\GS}\1063275~2\61151\&9mwM\FST ")}
testObject_RmClient_user_17 :: RmClient
testObject_RmClient_user_17 = RmClient {rmPassword = Just (PlainTextPassword "4S\13375$Z\132907OM1\177064*'(^[122\SOK:\SI\ESC^Luu\51669\SYN=&m'HV1X\63757\119993\1110725\140394k1o\f\1027987s=\DELw\145069}INJ\SOH\1045043>\94450)\140048#'3t\SI\1050544\DC1\r\SOu&\SO\SO~\37198D\ESC\USmp_}\146104\GSb|r\1112335$-m\NAKB\1024952S\71846V3\147538\&8a\985060^\US\1062347=\SOH>?9\134806?\SO\1077576\DC3Q\1062062'\1057605\NAK+\20994A\r@S\NULM(k/\US\ETBHcv5\1016366JR.\SIC\n3\FS\DLE.\172287\UST}d(\1095961\t\1099862z>\DLE\1048868\1006561\aU|\1065648\EMxDfl+\121257G\1093261\17105]\EMS%O\a3I\1075891\60963\164359\SYN.5p^\FS\NUL\3900%xYw\52696\1028056\FS\1050235\DC4\STX\171417)\ACK\186353v#\1068852\&4\122881<m\994734\NUL\n>\NAK\66314\28548*z'\183472\5055xf\EOT:[\DLEZUt\1083526\DC1\f@\DELA\ESC\DC3`\NUL~\a\SOH\174151\22970\147875\a2H\993590 W\1088672\1053026\1086465n2!sp}cC@0#\27834\SIN\82965\159790\14887jl5\166585\1094257{6\1003809S\ETXK\GS\SIs\ACKAK\ETX\ACKN\1053229,\SOHX\1032296\176060m7N[e$\1032357B\SUB\1012194Y\1017863h\1074374\&9\1361]\ACK\27557@T\1060403[\1046525\nz\SI\FSn>5B0\1039702\&8 \NULtD\991545}C\no\CAN\b+BF?I\1085611\f\94462_<\94994Fr<\r4l#K\999891\DC2IB\NULI+b\\\ACK \SOH\1067680\DC3\1071495\998848l\DC2\ETX\\\EM\ENQ\US\FS\1036996\35928L\SYNYC\1101456?-od\SUB22,\1081702\DC4\26455W2\STXp[z\CAN\NAK\STX3Sz\SID>\ETX}f\r\1039870]\43001\DC1\1110230\1025029:\STX{\98299I}15\984426qs\9792x1~\1014035\ENQ\nw\nQ\128828+=\EOT1\r")}
testObject_RmClient_user_18 :: RmClient
testObject_RmClient_user_18 = RmClient {rmPassword = Nothing}
testObject_RmClient_user_19 :: RmClient
testObject_RmClient_user_19 = RmClient {rmPassword = Just (PlainTextPassword "\173432\US\1072842\10120\DLE=`\1057434S\SO\t\NAK]\DEL\NUL,c-t]4\174015\40581\ESCa\71317|oG\SUB\1074048\96778\CAN\NULx\1079330\SO\ETX\US\SYN7\1070830I?\1034330;|\60010\NULJbz)\1062237\SUB\DC4\1104239I\1069501|\1058861/B\DC4C.6tdx\SI_N\ETX\32879C6Ae\1017698<Jq\997022\bR\1023253\EOTWd\CAN\DC3g\1007670\1024701Ar\128476Z\110697\NAKYi\32162kVN\64502\142113\b\DC2\156644\CANj\EOT2\EOTdf.\99452\&9\"-\1033911?\fr\1091970f\119124Wk43\172705\SOH;\DLEK[\5200\DLEk{_\STX\45635\176724mL,]Sj.Mj7\177127\1085704=\SUB\1076489\100616(g;f/$\987156\156969h5\nD\36272\1009202p5q\ENQE<\1068855\185718pv\1104621\NAK+\EM?\1105720\NULTa\44700\1105070.\1110449W\f\EM\164471c\28874\3198l\ESC\ny\988779^)\94868\154300{\1014903\r\1023195~\1028023o\98083\US{jD\v:I)Az\ENQEi\135412S\RSGq\121425\ENQl\162304AK%A\185123\vz1\1083169\160679\38579H\ETBIF\1102453\ESC\SI~\DC1O\1042872E~48\ACK\1092264\&02\ESCm\1026130g\t\NULE\182082W\SI\1083534+\f\1048637\1065400\1027531H*Q\1084916\ENQ\ETX\DEL\1013179\US\1003119V%R\159916\ETX\DELs0^\bD%\144582@?/\ETB\1099248S\180198{ge)\ETBY\156972\DC4\7882\22513\ETB\FS\16865}]B;'~@?Z,\ETBvJ>O:G/1Q\SO\74483K61_T\997778\&1~ec~w\27080WVM<!sN\DC18\177655\163426C\1079473l\1102359|f+\177712\SI3y\995894\153548p\DEL^\1035892\146151\NAKoH\1075550y\143708\163141\1021498\162494?\146093=\SIB\ACK@6\1026371:\1029730w\179753\r\1063669\1107957\GS\SOraw\NAKI\ENQDF[\FSy\1073760\1102242(\9035{LS\159681o\\\DC4\1010118\1020661 \54534nT\1001529\&6-\1085640\&6?\1044282'1\1074365Fj\SIx\ts?\1068457J(\184624\ENQ>\1099178j>\DC2|`j\r!p\ETXJ'\DLE|\165651\ETBJt?\25046\177341\1012976\&4\SOH\b\11596\1088709\137660\FS!@~\1077482'h9d6\1112578\n\1101076\RS; -\nUZ\1073240&e\1092881N\STX\119155;3yn\STX2k\\")}
testObject_RmClient_user_20 :: RmClient
testObject_RmClient_user_20 = RmClient {rmPassword = Just (PlainTextPassword "rf\1042417\b\EM^w\NAKq\1097681\DC2\22709}\1050955\CANta)`\SO%\SOHE\b\NAKw\NAK3_\1044559\1061926\1109086@v\DC3$oex\124977\RS\1095446K)\132159W]")}
