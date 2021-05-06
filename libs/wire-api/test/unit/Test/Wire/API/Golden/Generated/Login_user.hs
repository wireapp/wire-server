{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Login_user where

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
testObject_Login_user_1 :: Login
testObject_Login_user_1 = SmsLogin (Phone {fromPhone = "+88276265993934"}) (LoginCode {fromLoginCode = "\95040\DC3A\bq*"}) (Just (CookieLabel {cookieLabelText = "\1016225\157799\&5$n"}))
testObject_Login_user_2 :: Login
testObject_Login_user_2 = SmsLogin (Phone {fromPhone = "+20907972947"}) (LoginCode {fromLoginCode = "\1111011["}) (Just (CookieLabel {cookieLabelText = "FJ\\Kf0s\1007632~\a"}))
testObject_Login_user_3 :: Login
testObject_Login_user_3 = SmsLogin (Phone {fromPhone = "+296624860803"}) (LoginCode {fromLoginCode = "fb\1027695\96681"}) Nothing
testObject_Login_user_4 :: Login
testObject_Login_user_4 = PasswordLogin (LoginByEmail (Email {emailLocal = "P", emailDomain = ";\\\184572\CAN\151324\DLE\1016142"})) (PlainTextPassword "\nhm\FS\1029889'\1104037P{\US/\19337-I\FS!U0\150820\993279az\NUL9m3\1037948A\FS$\990557J4\1007906$n\RSm\1109421dN\n.\67674\&4lx\20946\540y\SYN\CAN.4\GS\142865\"\29672Z\150438\&0\59009qn\1019284+8;\60072\&9\83016=\63691BO!*\NULn9k\54609z\139932FF\154856\995005X\164021p3A\GS8+i!2\46878\992782\DLE^Wz\94468\1057588{\171047\1014523G1\DC2&\NAK5: \1094312\GS\DEL|\DC4\1029650W\SUB\1071147\ESC\167625-\SYN\155694RY(\v\146367ec#\1040479\51379^w\25009\SYN\35350\SOH\149628\168395K95\tP\SUB[\121322`\tO\49714L\7806y\52179H!`Ke\b$\NUL\\_v6nm\EMg\989391\&7\SOH\NAK\nAH\1041440\FS\r\SI\1069016\1040467R\177215E\162358/UT+\1075656\SOgq\5805~6-\EOT\DELFr\138475cLrH\1011069\SOHc\1033603A\SOH\1070813\194743\SYNyBxd8|3v;)\\4H\1112901g\a\ETB%\135325d}'P,\25293.t\45836m\DC4\39865u\60048Nl\DEL\RS\60473\&2\1045227?qWk\DEL[\ESC;-\US\1056605$\1190s:\EM+]\SI\1068053C\FS,\b/~9\120794\v\th`u\1056383V\DELO\78576-&O\41343S\FS\1007912Q\vd3\EOT4\1021318\b\ESC/\v4l\1072618\SUB\1087279C\1109674.\59826E5\132731b8c") (Just (CookieLabel {cookieLabelText = "\1051479;"}))
testObject_Login_user_5 :: Login
testObject_Login_user_5 = SmsLogin (Phone {fromPhone = "+51423392600604"}) (LoginCode {fromLoginCode = "0<\rh\1021328\EOT"}) (Just (CookieLabel {cookieLabelText = "r"}))
testObject_Login_user_6 :: Login
testObject_Login_user_6 = SmsLogin (Phone {fromPhone = "+736612719"}) (LoginCode {fromLoginCode = "L"}) (Just (CookieLabel {cookieLabelText = "\1035948z)("}))
testObject_Login_user_7 :: Login
testObject_Login_user_7 = PasswordLogin (LoginByEmail (Email {emailLocal = "\NAK0(h", emailDomain = "e\ETB\29871n\1109610\1073985"})) (PlainTextPassword "U\53834\DC4\fL@#\1032090j\21272\DC4ZIh\41715\&5H\EM\1048658q\25760\174011GC\166597\\\50848\158648Ta2\n1\DC1)\986609r\40760UJ\aBW\1019917\EOTELC\1070599\aUQ9Lx i\1078033#c!\ACK\132466\139931O\ENQeZ\DEL,Y957\"\1027250\ETXH%ht\146210\1088470\DLE\990684*%\NAKh\ENQ\1097210\US\182622\DC4\DLE!\1062604\"]\83483\&1U\53749\SI/k\994210N\131166\NAKIl\4055\NULY^Cc\SYN8@\44896 6&2r9q{\38855 dB\157677\1023421/t0HW\1068680s 09\1020930\USE\ETX\1041672\SYN\DC4P?%\EOTSGO\190969\1072144TWb\EOT\990292+4\STX0\1093000\26665`\174348\f!\SI<\NUL-\GS\164803\EM_6\t_\NAK\156941\SO\US\987223\133396\1018406\1065026\GST\52441`\fr9\149031\1075965\US\1079852\148729\6264U\63096\26679H\DC2|eWP\DC1\185385\1074448h[\SUB\US\ETXV\rM#v\1082828J\n\DLE_t\159388\25070&7\bvW8&\ETBA\1093817\1036653Zk\17715csD\41349\DC4\31607|\997409\1017805v}a\RS\SYN\1078797\12242\ACK3\rR$\94368E\16824\&0K\1101499AX;C!5y\1004967\DLEPt\NAK\1002463M}(\EM\58755EJjc\1080198rT\GSL\b+\146147\147745r\7689/\144375]\ENQ\1036255\1022252\r\1093116K(#=\36827\184539R\27646\1089975~Ai\29845o q>``I(\FSG\ETBLgO\DLEp\EOT\b\EMj7u[\"\GSn@|\151061\US\1054268\9777b\1019749\DC2\SIU^sk\1069130\1035679-\57399Zv\146379\23177\999702I&\1021182f,)") (Just (CookieLabel {cookieLabelText = "q\SOH"}))
testObject_Login_user_8 :: Login
testObject_Login_user_8 = SmsLogin (Phone {fromPhone = "+3637481903"}) (LoginCode {fromLoginCode = "\DC2\52891\999358\6080Ss\98168\1016591\SI"}) (Just (CookieLabel {cookieLabelText = "\SUBak\997428\15598|I\23080%"}))
testObject_Login_user_9 :: Login
testObject_Login_user_9 = PasswordLogin (LoginByHandle (Handle {fromHandle = "upnt5nqse8963kn9ckihvcq2qpb0ljg0wm1zsqx5.nfz0hix1kex_a2__rfuus67ai-5v7wllceha7qiny8.np084p5bqpepstz1gg.qxs5k5d0s0qot4hl18bndsnat9ew4n.jl64ug6ej7c5c4ma108-yzuh3._hsz1m.y6vwblgvkjngk21.d2nc2-j2rq9pxrng-"})) (PlainTextPassword "-k\NUL\1058926E\DC2\48749\63859'\26241)H@\1021173Z\rU\ENQl\1059349ns;2\EM'eG\1060065\STX\95334\1099400\985710\92406c\132236U\988398\DC3\1076401\GS=&U\27757\SOH!9\ENQ\EOT \131220\47589y,J\101042\171272Nj\1082124\SYNg}F\NAK\vO0\170241r`0\\HY\n~Y?\985900m)\fA\n\984988sAWh~\v+2<of?>\1056511") (Just (CookieLabel {cookieLabelText = ";\1051002\1030704u/\40190w"}))
testObject_Login_user_10 :: Login
testObject_Login_user_10 = SmsLogin (Phone {fromPhone = "+886306644"}) (LoginCode {fromLoginCode = "u\41157e\181986"}) Nothing
testObject_Login_user_11 :: Login
testObject_Login_user_11 = PasswordLogin (LoginByEmail (Email {emailLocal = "?", emailDomain = "\1097418"})) (PlainTextPassword "H\175785\ESCm\1038290UwJq*\29770\69879_c\CAN\156155\a\va\NUL\1062312\bYmv\985\v=\71044 \162517Sl\\\51795Na;hNt\1039784\rxEoW\1067899k\ACK\1090154KQ\157244wzz|\990768<\1077363\984366\&3yW`\bi2Y\n\1079364\&56\tU\r\53797|\ACK\1087388s?^{\1056674\1031770!;.zB\188954(\ENQU\53768#\1111772,:\"\NUL\141173G*\SUB\1059165\&6\"K\GS\54174A;*4u\93070\GS\1064132\SOt'1\1096407\ETB)Q0\170525a1\136327|$\NAKya*&&qb\FS\r\1072640\NAK\1066707\v\1061780O:Lr\10485H\STXU\174975nZ[N\157339\141689S\1063757\f<\1097455{\NUL\985611']\v\986848A]m\\\1048070<<\1102886\167851\97970\1091367?8T;\183575\&6\1026844\31885?\35767\987632Z\1026346\NAKT%v_\EM6\194611c\tlwYdN\1036425\&2\92756\1052869yI\42029\n\FS\144731c^+ECN[*a\EM2[\SOHz\145673%z\1108886_[\1093534?w#H'W}NM\121308\46507\99530\983172&\NAK\DC1\a\1098514\STX4Q\1045158\52046I\139164o\1094297\"si\163887\&9n3<\ETXQ8\\*T\nB \NAKAE\a=;@\1029495C\54312\1088714m\1028302k4zA\156725\1074930O\1032067E\\\987655\r{+\SYN#x\24978\1061621\63227\DC1\1090801v\ETXFOg4\1042066\STX\1040601n@\132826\SIu%\SI\58911\9164\&1\1102059\48711\1073317D~\999970\1086366\&8a\1010891\983956\1068514{\ETBu\52879xBdkR\1102620F\39648\&3Q$zdvi\1035392xG\21082\ETB5\1002953Z\1003435(sf>\ACKU\62759\DLE9`M\1013234\&3\22274!\97498\n\agD$K1YcAm\1005609\DELO@oHK4\RS\ESC\DC1\SO7\"A]\148890d\a1\25099\US$T+q\187114+x\DC2\RS1\1042809\53078\DC45;~o%\168507>}X\n\EM\\\143304RX\142402\190658H\1096558Q\1058273\EM\vjgE3?e\SOH\DC1\8153\167022#@D]xI\1068880\142924\162521\NUL]\1029428~\98920\1094102C\1104579w.U?\SOH\1036755`\GS5x\983440k2\1087662`\ETB\DC4l\97711OD'\EM\1031506\178100&\\3\US\\E\1016373\40611<wx&*$\58026\GS\")q#hem\ACKt\ETXQy$\NAK\SUB\ESC\97797X\1033632F}\134662\NUL\DC1\186790\156236\&3\155785\&6\NULs'k?'\CAN\FS\141047;S\1044253+4\132319l\48478\a\nTnq\ESC\EM\\{x!G\DC4\1060546\DC4e\179823\100365\1023130~\RSY\"eF\SO\169040\50100L\1051149m\EOTRv$lt\50678\NAK@\171610,\1079228G\151657\1065931<\n\DC306\ETXj\1070137\138012\1056702y\EOT\151171Oy +\1001703d1\CAN)?t\NULgV+J[\DEL\148296\f\995389\&5}G\1080674^\ENQ\STX\1057249E\SI\998097 \1510\&5\t\r\1112221\1054385xT\f\5617\&9\US\1049415\985466_@-7\185768\b\1055898\nDW:R\EMX@\DEL =\fu1\97408\1096839k47>?&cG@\a\"R3\133519\32129\n]\DC2r|V+") (Just (CookieLabel {cookieLabelText = ":b\1057443\1104420\17927"}))
testObject_Login_user_12 :: Login
testObject_Login_user_12 = SmsLogin (Phone {fromPhone = "+723539093967776"}) (LoginCode {fromLoginCode = "?\58627\&7\59654"}) (Just (CookieLabel {cookieLabelText = ""}))
testObject_Login_user_13 :: Login
testObject_Login_user_13 = SmsLogin (Phone {fromPhone = "+35303466173"}) (LoginCode {fromLoginCode = "\ACKO"}) Nothing
testObject_Login_user_14 :: Login
testObject_Login_user_14 = SmsLogin (Phone {fromPhone = "+818937428"}) (LoginCode {fromLoginCode = "W3"}) (Just (CookieLabel {cookieLabelText = "J"}))
testObject_Login_user_15 :: Login
testObject_Login_user_15 = PasswordLogin (LoginByEmail (Email {emailLocal = "\134714C=\v\FS", emailDomain = "\1091187-lF\171779`{t"})) (PlainTextPassword "\52436\DEL\1013957\ETXB)\8233\1074023D`M\\\165841z\1114052}9\25730\\)A\1074223\SYN##\DC4\US4\1071993O}^n\161648\1036231Uj\15514 WT\r'\1083156Lt4\1095651\RSD\CAN\147916\t=\ESCAU-c)\43899/p\13832\1014311\1105163\DEL\1074157\SOIO!nD\50358a\DC2\ACK0;\FSVb\1073790{7l\166833L\ENQq\131478\t{y\50734wF,\ETX\1091516\1070827Q\94663\b\bj\SUB\1033385g\41388\167530<t\ETBMMz7.#\94375+^L\SUB\DC2D+e\\\189389$\STX\27912B\SI\160956\&46~Z_qW*7\1109593\7016l\173871\&98\1035077\1080520\20989\996321\ACK3TTW\SYN\FS\CAN7\rC\CANZ\SO6\7229w\95921\18599NGta\1058110\47185<\1104509r:g\EMXUeb\181303$\STXjV\SOk\15640\154222J\DC3\1027763\1080166\1093302>,S\988864\SOH\DLEA:\170732d\DLE\FS~o5\1042389\134860\rl \1044425\GSv\1072586Z\140017\&9\US\SO\183664P\v\159679T\4752\NUL\EOT\161627\1103745\&1bu\1027262\&6f\1089614;\165525\1579h\72390\DLE_?\1084984\39411\1063432B\991653\STX\SO[r\1101048Nm\ETB\34074\1100711$G\998774]p:'U\ETBX59}\70090\996473>\t^\14325\SUB\DC2!!\64887hc\1084930\1072042$?7`\DLE\72705\ACK8\SYN\891#g6rD\992679\b\atXf\1092437x0\179486\&4\98752w\fj\994015\&8K]Ur2|\124945\1040211 G[kM\SI2pI\EOT\ESC?\1031707\ESC\9545\ACK\ah\nX\53974fm\r/S&u-\1048747\CAN\ACK\NULo6\999035\r,\139538V\1053219\1109608v\1073359\1084939\995181H\22776n\feb\SUB\USZ-t\161925\1031384&^tL\t\139357\1102788b0K\1047836cAi,\FS8$\1071612l9\1055650M_\52664F\b\1091956Z^\47529r%?\1070852b\DLEt\"\1105907U\145010pPG\SOI\EM\140281\20114\f7g|\141908\&6]\173719U\EOTQ\9357\SYNo+q4\DLE`b%E\1084596\n\1030721\\\EOT\ETB\1013574\169721\143628\EMA\1003893XB-\1051160\1044549\136433\135633\9622") (Just (CookieLabel {cookieLabelText = ""}))
testObject_Login_user_16 :: Login
testObject_Login_user_16 = PasswordLogin (LoginByPhone (Phone {fromPhone = "+70397044"})) (PlainTextPassword "|\11498z\1054175.\983875C\17848VV\1112451_\151013\183473n\t7Kc&\SUB{\1103725\1078031?\127372S\SOHv/ 61\1072807\n?\SO\b\1065974\&0\ENQ)|h\EOT(\167359\94748\97087\1024232\159321\1037476&\33079\ESC\DLE\169626\RSz\1013894\DC3\182419\1073732jTS\1081184\DC2!GQ\"AV\NUL\1107976O\989568\1039084@A\fZc_\FS\SOc\5788:rus\22649Mt=l%}\1014993\&8\14139z\DLE\15628E!\RSP]+>\1065358Dd$\CAN\97970\133802\180239\EM\42860F2\127992J)\167549Q\22704S\SYN\EM\1084176\1086908\65567k\154359\78605\6560&A\1096143J\SOH\97284\ENQ\\\rl\172636>i\1081522j0\1030975\a2~d\140631>,8w|Ws\DC4d\26437-V\94842\ETBU\1103197\144894\&8l5\USw\1047943\49779i!\a\1028542\1003138au8\EM):d\DELE\986201;o\17344\148102\&7\r'\US\EM({\36650\178732\STXXj\1000336\1039829\DELaK\EOT\1037845$\1043825\1111055\999726\1096325\vRZA\DC2\DLEx\DLE\1060374\&7\DC1o(\138538:p\166345\DC3\GS\62590\&9@a\191098\1034055\&18}w\48504#\1005185\179605u1fp\179980\23069/e\18461sJ\STX\33617\t]kW\156054<\139936UTdo\DC2$\163838Q\147818\NAK'\FS\42172\153109\&6L&\58577i\6670^\146824yV\ESC\1042469\&3-\GS\40927|\140435\1003841\176713K\DC1\157924\151785z\RS\1108311\148950\STX/\ENQXj\145727s\1038626I\190197\19409=E\163910\"\148067\188355V\GS\50639\DC1\1095284w]\1021454\DLE?\r\45422_<W\1060266i2T\43383\\#sBs\1016818ZR+\DLE^\ACKV(]\74547\1026617>\tB4\DC2\b\1060770\73971\ETB\1019694mQ\8848\157842\US*E\CANN 5\154145\ENQ\5305#`l$L\v&~+\EM\21987EYT\"\NAK 8\ti\v\997334\1014026\&4g-s4\1041983\1029961h\1065231_+\1010483\SYN\ENQ\26938GIB\1111794\42937\NULH\1036825%p@|\3379f\118866\121243le7") (Just (CookieLabel {cookieLabelText = "sD\136952Z\NUL\SO"}))
testObject_Login_user_17 :: Login
testObject_Login_user_17 = PasswordLogin (LoginByHandle (Handle {fromHandle = "q6o0h1spfwcf44vuawn3-3n_0cqvp-7mckei"})) (PlainTextPassword "\1101641M~M>ho\98001%\b}3wx v2\EM\USh\1002055i\ESCg\190237]\FS\1111148qm fyx?s\1082610\156476V\1068693\a\1105320\CAN-qX\DC1+\148132\"\"b+}b5\DC2\STX\SOH\EOT\175413\RS\69742(K>:\14859\78409w*F\148938-b2G*d\GSp\51656\ETBI\30492X;\EM\165456HJ/\60487\DC4*\a\n`\49894\&4Y\DC4\SO\1053583iQp{\5587\1069649zB4DV\178373wK\STX*i\DC43\US\DC3\SUBY\b\SYN\1074754,\RSn\ETB\1071257\23285].\987399\1084489hv\SOX\aY\155568\ETBbg$\1012460\"\173565\EM\GSi\SYN\22460H\1032819\18691S\992781^n\1022760Am$6\SYN\SI\37372w( '$\1037787\SO6\1081472\&0\\?%\SO\EOTW\61726_L\178970n*S\119882W|%*\168308B\1006524$S>g\988115\GS\41922VTX\175554\NUL\49167\65361\a1\131678\&41(*\NAK&D[\SYNXH:\US!\DC1/F\ACKC\ENQ\1040578K8\t\1070847\1067726\ENQ\1024551-\1068488\US\CAN{XNN\DC2\427!k\"!-N) \GSp\SYN\SUB\1059429\&7\ETX\RSE\1005022>\ENQC\ETB}mfM\SUBRk\996873ZBs\SOHEE4M\SYN\1037942V\1050801\188487d\1003154MYn\1078419\FS7UA\SO}\DC2s*\58170\SUB(\f\DC3\v\1031276%e$\1109190\1081219\STX") Nothing
testObject_Login_user_18 :: Login
testObject_Login_user_18 = SmsLogin (Phone {fromPhone = "+507209861614"}) (LoginCode {fromLoginCode = ""}) (Just (CookieLabel {cookieLabelText = "&`\158654"}))
testObject_Login_user_19 :: Login
testObject_Login_user_19 = SmsLogin (Phone {fromPhone = "+155886325231"}) (LoginCode {fromLoginCode = "\96681\1027865\987278"}) (Just (CookieLabel {cookieLabelText = "6\36620D\DC2\NAK\2024n"}))
testObject_Login_user_20 :: Login
testObject_Login_user_20 = PasswordLogin (LoginByEmail (Email {emailLocal = "w\149808(\181062\78466^3", emailDomain = "\95855"})) (PlainTextPassword "\150008\98874\1110313+\1106204\&6\nR\SO\1109429\183920V\"eWI4\DC2\1032687\142513\1024640(\155153\EM\CAN-\96560!\69404\1097035\28697\165491/.d\v\1060125J\ENQ.\1077447\178667") (Just (CookieLabel {cookieLabelText = "q\137228\66666K\NAK("}))
