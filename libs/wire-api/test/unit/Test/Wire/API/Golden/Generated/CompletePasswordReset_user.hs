{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CompletePasswordReset_user where

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
testObject_CompletePasswordReset_1 :: CompletePasswordReset
testObject_CompletePasswordReset_1 = CompletePasswordReset {cpwrIdent = PasswordResetIdentityKey (PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("yg==")))}), cpwrCode = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("rc-LMfdwAKYApuGggCIMgodd5mA0fPSty6jqnAJm1whe=Oo0Bx7nh3axCpoK1tP6g=YMeUfs0qu82g=SVcR6R4wdoyRDzSKfWFN0Vi57qZa7JjmIAxI6t9_Nz=qjOTpqoAiWZqnF85gCZ-5CMLr-fHfemI2=Mkkm=-Kf4u=xRpg12SVn87NOUCTn72GjfkBKj8V-BLRO3fjh0bfQv0Ba5tbsVisO5aZRwmb=-wb7Zf28gL-sDlLMwb0Vl9uW6be2do9naj8pMcIgc_3DUMFPL1RzKMax1y7f3XGg0lN5jRo5UdYu-ZgRQu9aoXyqDw6J6QrT=J_US1ZOVs4xN1EvLp24Ezr=x5rDCqN8TDH_giuXq2nMbVw9M3E-9Ajo42KtRW9QmTtT1-ihaErmyqQ3HsnUIccd--BZxhKShLlNWr-ZYFByF4z0PYHFFF5DwhwDQJqvhVti1xU6L37nNLnJK6TSZfIEsPZn0wXXgpVnDUgz_Nyimqz8Jbu6pkWeLbRNO4D=QSth87CM_4fgPOgC9t24XfBAxDT2qe94UxawtRwLzFq7csEchOQHktJVu4VJ5_mHpHKzGfS=2XSjCFFvnxGvHkfdzCMnL8lSuLsa2Xx0VMrBY_vdb5p13swDxSsnN8u0DG81I2FPUObyNjjBZ8gvNhrQtmH2oO0o=VI6dscNTqwG6FSwBBEbnqQJhP7nksTsteaww_kr7YsVKDO2ywFM7Q9CqW3OTl810MAVanW")))}, cpwrPassword = (PlainTextPassword "c\EM\22061;8D\1050785f_V&m%\177702mH/-G\18687l\US\39711\1069513|\1085411[|")}
testObject_CompletePasswordReset_2 :: CompletePasswordReset
testObject_CompletePasswordReset_2 = CompletePasswordReset {cpwrIdent = PasswordResetEmailIdentity (Email {emailLocal = "\EM\6069\3725O", emailDomain = "nHK\ACK\r"}), cpwrCode = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("qSsSEad4pV6-TA05ZOjwS7RZrGOkOZwnJ9aXtA9audrwGv8LaiA300h52hPHyUIX9yWzCfzvrW4CbVqrXdKiCBqMjRFqG9GhwOAVxt-4j__qz=2V6naMLR4GuTW0eKseXomvGap_AYgyMYyxo0GDNuBk_6VHXXrYeIH6XCL1h2F9QYL=9A8xaE5YcaKzv_iTiv1lnKwSQ3=X=-HZeOkQqAPIMPVMqIdqqVLydUglO0epAC=Or-HldUZGW4zM2KmhHSefmyTTxIlP86JLTPKHjz-Oi29MTT7coa4Q=krTOQBRfbH8UriqHMDYvPaV8Rr-d62o9YvibZGbDMMB5xhIhRsC3b40lPMwETvS5sw3-a7RT1kz8PdXdGJlVWeEZcQU3B1==vBVjdimv4OH6FAF_v=4g30fD5okmA=WDkshiNrF30XUIEcXXBfOjM0bS2D8-h5HoAgbtQR7Qlw6NZg87-8bnH1Dx7jcM=VnF0XZVlmccQ3VW8w8pAsWwdxkZ03LXLMqkTR4wEezA55mii7fI57KO_43-R=QXoVArv2xYzPrt9GVJvgQSmaM00eB0Ca3COX3rAjcfscqQw37mfVZ-_3oj5wPAXBnIVmr2QMKQwnr85l4=6KlHgD_EeT03AJRLr94XkgQt71Y3d1GjaIlLgqmPxwjDy0iI8j13ZBDuwWqNBFN6zzph7hEK63dz5p_nptQ6tgfXV9ZmDAkEAJgmIXnx8bY7-0ek9FjOGbfo5wOAssqi068Ng44_YzhFggNiucvzPBWPF9J6K-5AVCQqnS7uzwZiiHyBbOJtESe0TlLaGKin1MUZq_aH0QNMw5JStXdAc1K4hbIvqxXxNd18yWPqLc-_hneDpKzWSw-RUwiWhngMgGGN5WM253EHpL6mYS=nUXYeTuoa3xucU_osw8ahcscyzdlVRBoN")))}, cpwrPassword = (PlainTextPassword "c\1079842+1;\DC1\SYN\991966\SOH\NULgf\SI0\989947\1022161\ESC_\1097075\NUL\174967=\61651\NAK\40240|L@\NAK\r2\164863lf\145426vj\n\SO\ETX]{&\141119oWRQ{j9\EOTh\99767H\ETX<\32490\1057903\SUBI\1061660\v\143742\179348G\1072907D\SOH\ETB\fS>^Jt'x\1026114\SYNL\SO)%%A\DELa\1085708\&70&Dr\138783]r~\26558\171072uE;U\"\9174|B\SI\39921?\37926:SV\SO6\138937\1105343\1016654\&9K\120623\21523,W]tNS\DC1\EM5\FSfy\1005556\183865Z<\1025907M\161714='+DE3$\23212eg\r\72874\33552ZM|\1076203$h\ENQ_yZ\42474=U\1075320q-D\n\ACK-\18714\&7\178571t\ETX*\t\995728C\DC3\169409SM\1097384\&4qd`<DO\1077566\1053072XyS\ETX\174010l7R\1037611B\DC332&HfD#\1058623\1003349\987957\188365Hs\993731\28905\1020103\1045673\146779zy\NULui&\25155\24212\STXjO\t\GS")}
testObject_CompletePasswordReset_3 :: CompletePasswordReset
testObject_CompletePasswordReset_3 = CompletePasswordReset {cpwrIdent = PasswordResetIdentityKey (PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("icjqww==")))}), cpwrCode = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("HYREZ6toVH4q-zCOKnBn3=ByvGg_HeFsRotvFA-M7mrefwCKUqI_Wnfr8dcndRutaW=axtL0ncRQY5T68Z2ynZUJD6CGi8m18YIxOmddOYk14wty7m56UBGLsLf8Z9xyhd_vP2lKnActmFaHB=4xJLTtyD_IYZTt8Ht6UILPkHJhMeiboCH9FFM6SyHPJ8UqXFrmzoTMeSAzXqLRgo_tHuqz7ZNf-VPI1QzbBqnQsxwTX9dJ4OqdCEWs9iuhaHj4njfeYRe9X-mpqdIC1mgyWARjxBhjn34SZkxd_a7JjKqTHglfRbsyFRyznRXxEWq3rWw_9OvPZ7MVX-eFNn3dNEYHxRRJ5eMFTilDLOkCVjEGFfONZe5jFZXE7OHHmGMTeAY-7Snl1I5IUsysgV-PF9gowSnfMzpVVsOCRXkeoWpn1uDovR4uXoIwt9DkRLX9-H-tNgf2Psr3q=Xby-PPvaZ7y0aGvyWt3I2vFYGZUrs5sy4-VREB-jRbGtGCfXq8p-Llb4oNr7XSkcN98dAaPp3RjYyQZGJI20gC_DFgh14SSrIIw9qeN2E_yW551k5kIMTaR_Y6xWPRaB-1tAuu-6RUthBidWZs-QHHOV9nR2du=q5lrpar8s5d_WLMOXbNzZG8uCBU8")))}, cpwrPassword = (PlainTextPassword "\1044137Scyu\EOTs_\"\41164\33984\1020427\STX\95999U\DLEQ\994854MQ%1qd\179409c\1070300K?\1053609\19689b\nr\t>\SOd.1\1050145CQoDn\1040881W%B\1046372Oy2\DC4\1003354*u4\185959\157493n9\\\1096523x\181088\1028106\53894m\68034o&\7433I\EOT>tO\177544:eL O\EM\1039372\1050105\STX\992802\163860\1080421j\SIE\183652\DC2U\t~%\f\rEma\EM\139657\&9\1062661\58392z*\rx\1002607\1014180\154691\184745\23858Ft(\1045565)z\SI\fo\SO\94969\ENQ\1053958^\1062961\74326c4\EM\160887TJ\a\146573\60461`\RS\158817\146006V\SUB\SO\94772\987493\DEL\EMQ\"?\STX\tz@\ETB\v\CAN\EOT/,\986465>\1046904\1071135JP\1043205\159037K\142997D\1094452\1006392\STX](\1050255'lA8\NUL\1093000'\ENQRH\SO\DC41\EM\136231\RS\1032068\183065$z\ETBI\DC43q[\41251OS\1024855\983120K\GSF\vh\179037C\SYNE^\n\ETB\1067004fI-?\12943Q\1014245\1017815\&0k`D\SICR\ETB\DC4d\1094261\17146\&6\1075500\133020E\SO\ENQeN\STXef9bm\EOT\1008781\74063`fG\121059\EOT\1023105\1055670\STX@=?\1011798?d\NUL\135541\7446\bcG\DEL\CAN/V7\149105\EOT\194783_\1057883\&2\\\a\1064759R4\SUBaS-v6fv\STX\5348\\\143727a\bf2!\1048525@1I\1110505w\SYN\1029016{\1040622=5\1098009`\SO\37864\155108\1051746\ETX\1017725\150361x\FS:\41559\SYN\140977S9\fQ\v,+E\1013651`\182434\186752_\US\t-\DC1\49810-\1097989\172756y@cJm]s;.\ENQlP\1019694uD\143007\nfd\1086040\&6Rn\ETX\ETX\SOI\1076518:k\DLE5L \1016724i\EM\DC2sg\SOHX\74953\SUBN=7R\t\19616$\26925c\96423AvB\1020902\994635\154315~I]~C\ACKL1\ACKA\60476\&3\1028549\ACK\SUB\17333]}h\4895\180183:\RSQppA\151157&z*U\144843R\15535B\r\34597\&0W\fr\1079564\1038580@+\1071211?P\1090645i!*\1092513:\150859w\38399\SYNrMv\30201\ESC\171716\&2]\GS^_>\f\175990Zj~\60842y\988206\24776f\GS\\qS>KkV`\1106473Zry9\SIy\1018288")}
testObject_CompletePasswordReset_4 :: CompletePasswordReset
testObject_CompletePasswordReset_4 = CompletePasswordReset {cpwrIdent = PasswordResetIdentityKey (PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("elaWoGJ0")))}), cpwrCode = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("D20gYa1jrVjt5kctv7pkuPeSB87OI0ioq=3uYaBeHf92bwqcrJYZIJwDcdY6YtScOngRypJu-U0nUK8MKCr6zjE=gIZgBIFqCTs06AkuY4Ybt7JcuX5QQK3owh1INKUvmceJtOF=_td7Ft-P7un=D9TXLqnSJijgW8zvJOrgmu-VQZZGtexSD1TfnMd7ixvQcykwOPdaIRyFAQINiTIdaSNu6gDBaQc6=Rq9v7rHVVK171ePMw7FXG7Fpq677pm_RCqvSiXYp6AEOYUfTsbkwk-tUT7SHssp6y9LOUg4zShN70JA4YxN73PVrWk9MvHlh02u4egH_=Lvuhv5bocL=2Oic5reKlj8uv4iifp-kBjA-obwRm2uYRBc5SCWC7guLRQiyaxWRRIEK3Ma7_uGaTnBJYUcRw_tdpejSLjtIsV4Sh2qG63anPdO-Wie0EEGu3djzjmK8TNdvuDcrKIQE80jnbWUBYBkD")))}, cpwrPassword = (PlainTextPassword "Z)M\119271y\SO~!519\1104020`{P\172093m?$J\CAN@\t'K\DELD(\RS\EOT\1100234\64310{rm\121103\ESCH>.V^Zre\b\DC4z\1087530~A\1078127qd\DC3\1017518\52279\1007354\22435\996128.PA\1029769xb\ACK}\169824*jY%\GS\51215Qy\ah\171337A\148495\ESCL\ETX\NAK?\58112\SO)G\ETX|;*\1070917\rpp\rj\SYN@\r/\SOHI\v1m\1059462O2OU\SO\STX\SOH=Oe\1017215_\985443Z\1020937_^\1020586\n\SO I<\CANe\45212#\ETB8d^\FS\48370\17074 'IU\SOH.\SI\94215\t\25930\&7;\vA\DC4\1000594\SYN\1015210.t\FS\1004775\141564T\SO\ETX\36893\1013822x{\DEL3?:\98960\1044739\1018169g%\1065746|0lhO\181106\DC4\160730ii\70325m\EOTH_\137025\NAK</\150214\155139\&2\159688rD\1025570 W\US\ENQes1\ESC(^\985842\57660\STXD?\SUBO\110969`\94572k\ETB\191161\986373\1063101\USa#\30365\ESCr\28128\EOTM1h{\tG\1084656\&7\1058153\bAE:p%H=\181754\1094029#\veVGf\1038818\13128?6\148777\40511\142734\SIlhO\SO\NAK\SYNn\1036614w^Ec\DLE\1079778\156170_\1040298\1070043\n\1022682uU\171884\NULx\GS\150219pIY\94669I~^\1046293\&9\USH)5J\54331{\DC3\RS\fj(\1016059\SO\1062542p\34934@n\bq\1043321\SUB]\19323ma[']\1033857\186098\CANRs\1058917^\1032374\54659V(bJ#\STX\1013837#Q|GT\SUB|\1081284`\SYNp1R;oj\1106214c0{\NAKR\aWa#\1063808:J\52410}~%\18267\1068836\FSGwQ\STXJ\3488EY\37853#d\1079491fN\RS\EM\r\4230W\NAKT!\1094114\172720\FSlrJ%,\156569\998182\\8J\188412;\177354\US]U\EM~\ACK\1100378\&7`W\\~$?8\126111\176302:&\f\1010665Y\1049485:F\GS\1038902\50159H\1001544/D6s\39248H\1003534T.\ESC\ENQ\DC1aH\48730*'\EOT=zE]\128230\&2\997662Q\1104296\1025409\3449\994627\EM'\"K\17023\1114100\SUBZ\DC3$3O'2/\FSq\73449>\1075827\NUL\1008940L\59328^Lj\11556\STX\1002135a\1106357\28802i!\42641N\DC1s@mjPybp+p\77949\1088785\1099777\SUBN\t\\\SYN,\NAK6\1080457'\67699\135962\47956*\1090068\&8~\985745\&80k\1007220\SI\1050874[b!.'\r<j\US\t\1066994B#9F\DC12\CAN0\"\1055538b\136646\&5\FS8?RS(\53380z\55289\1050684\1096621\1031532\ESCP<K\SYN\fd\NAKICz'\ESCA\\q\43862K&*\10194\SO\ESC{:\"Y`|\3743\1046236pksW\1022000\t6\1084031\v^\DC3oN>Y*\32752$P\DEL5\1066526=3sT\99990?v\1095121\99952$\1002808\185534\118978\STX]j%{1b8SD5\am#\75066\131654\1029703#\1002250$Gug\30448\DC2|\SUBI\143165\DLEIYfx\EOT\168223[\DC3|DN(IgK\125242\993707\95196J#}O<\1048989.\ACK\18163\137435\GSo\NAK[y'\987458Y\20377\USr\ETXvVX\NUL\SUB\n\1091922\DC4\1068982_\t\1030504\53947\&1\1033805\NAK/b\GS\ETB\EOT\1082457u3t\14360\&7\151138\1005766A\n\139736(\44163_bB\1089568.\r'4O\DEL\n \1099926:P\b\n\1005692\1044798\SOHR\\\1009377>\ENQ\FS\994968\ETB<\172887<\SI\ENQRhv~K}\STXL\186413\1099099\1020205|\SUBDVf8+d\97836t\1111368\1105012$_<\r<\ETBIO\158419p\EOT\SOH\194753\1009144\STXO\DEL\SOH*EB\1073737\129379\r\ETX\SI\1064339\&1_\STXF\22042FvF\a\151421;\1083274\\\146044")}
testObject_CompletePasswordReset_5 :: CompletePasswordReset
testObject_CompletePasswordReset_5 = CompletePasswordReset {cpwrIdent = PasswordResetIdentityKey (PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("nQ==")))}), cpwrCode = PasswordResetCode {fromPasswordResetCode = (fromRight undefined (validate ("Rdu=DyICY8ud1GsQL4kzH-g9YYbJ7nkMtV7ksJZfarjktnlx=rdimQo8AAJDlKuoOdCI==")))}, cpwrPassword = (PlainTextPassword "lX@\SO\t7u\33153B\1089119\rdRF\DC1>)j1\STX\1077001\SI1\DC2M\SIu\991943\1045234\182654\&9G[8:\SO\1028769\1031882A\FSFv<%\RSt`)&opV\35128lG \4002\1030087_6{\"\70692\&0\DC2\DLE+4{\69388\43954\1026523\ETB312\ESC\1031658\v\19265\140563Z|}\178091\1050833\184442\&7]yy[\62019\vY7\32665\28463m\3250\1095117\8308D\1105833\DC3Mc\SO\DC2!@7\98674/\29221$[\32727\GS!\15652\ETBj\b4\1005299c\993294\DC3\1089483\1043556J\STXQ\1082024\DC2\DEL~\1111696|\11075\SI\SUBrk\SUB,\16739*h\SI`C\25667\v+n8Ia\1107080\1106882\150624\CANrl\134650\SI\1014676\&0\1091008\DC2\171784T\1014492\RS{B\SO\39066MjId\35173\1079181\DC4\129285\&2\149882\ACK\EM\CANGh\USr\NUL\ETXO^\t\DEL{Pc\33016\1092826u\DC2t\USrOF\a6EZ\1037019g,OJ:\RS2)\1028825O\RSs~-\t\SOH\15413z\1034442|hm\1073201\990272\1029321\&9\ENQ\NUL\1049246\ETB9v`)M\f\33857]$-b9TQCr\GSN8\STX?\1092720\189174\f\ESC \f\63460\1028616\190574d\1004275\&24\r\RS\DEL:,w\DC4\ESCE`\1076337P\v\47016<\170145\1070260{\DC2Q`<u\47284vJ(VDE&)\1048801<\18715.NJ\1052456\DLE$H\1053138D l5\10432 V\46812!\fM\1071643~\"\1078334\FS\STXX\1039193\&2\a\32161\r#\1015557R\1051668\22711Q^=-I\SUB5v<\f+u%\1083933[\NAKj\SOH/?\16678\rz{\168863\b>X\184767GF\n\SI\"\984846\n\DC3\ACKX$%\1089411nF\SOi\DC3SD\158977{ \DC2-\NAK\6832\FS\173268\41504\1017705RpX\1035522\18355\DLE\EOT},Ni\RSGQ\164342uz.\48113\f\1007191PG\1069393\SIfB\25029_l\1020387Y\31581\&8\31766\1051013r~9-R\6413\15544\188859\FS\EOT7")}
