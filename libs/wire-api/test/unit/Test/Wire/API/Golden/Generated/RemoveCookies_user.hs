{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveCookies_user where

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
testObject_RemoveCookies_user_1 :: RemoveCookies
testObject_RemoveCookies_user_1 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "\NAK5\f\SYN;5\1110712N\36563C\4666\1000109i]n\997568R\NAKPh\ENQ\v\1034587pJ\SYN&\DLEw\167840\fZ5\9150\35088\99038\995346\&0v\24168~\1057716\175334jb\9720;\aC\SYN\94208g\1019735M\1084998\DC3\1062351\17401$-\ETX\144777qKT\44903-t=n@\149839\42338\1111714\a\111109\ETX,m\vASnu\111117ZE\aN\r\RSS9\1083070_\1019944;N\1037492n\174551\ACK'Yc\STX\1011735n\127038\\(a\1090265}\DC30\985471\1080158NO\STX\bl\r\DC3qAS\1046634\SO/\1017570\1004856\1033555\1010474\SOH\1054317\1051544It_s#\61408LM@\US\CANgPy\NAK\1034946$\190100\DLE\FSY\167626i\1095141\63485\&9|\1075731\US\45775\1000830\1020037\1070991}d\181130ng\39387\1100379\1009109\RSrWgm\FSs\1109829\SO1\1012960\98379W\SOoN?\CAN|\23293\77838K\SUBK\RS\SI\DC3Q\46065\DC4\1035043H\ESC\1009665\r\182011R\STX\1048441IP2\DEL\190243<8\CANz9R,\1011429Q\1054033t409nz\99780\1083716}q~P\170157)J=QzdO<2\1081794\1032904JnZ\r\CANR\SOH'D\97257Z\NAK\DEL\143822\SYN_qh\DC1CdHe\n\1107208\EM\152897v\1061924\EOT\DC3{\152751!f$\156569{O\SUB\1068841\GS\ACK\DC4?\1084386\STXW\\\RS1(\99883\t)\NAKp\196]&V\GS\1001586\&1E\1107902\DELF\7023{\1012885\1042995F\US\989994j\a0\SO\SOH\SUB\984874\988901\45146mYq[\ACK\FSh\RS+0!\159100Mx\52241\GS7\ESC~\1014323\1023550U`\113817\SO\USR\ETX\STX\ENQc-\185710\ACK*dv\ACK\169693W\1097608\r\GSM\1113726\1114091B\61935\NULt\158232\1083272\94781\DELtE\ENQWl}E4\n\1076260S*\182912r\95591<\SYN\128310[M6q\4776%\USEn$\156232\&1.\44056\191096`\45925P\NUL\73793M\993777\STX(\DC2o\v0'\41375\30158+\1082097d\1019460\50847\7993gxtj>\SO7u\SO\SYN1\1103313\FSmEjY\35960<\vFjy\34848!$n{\27529jI\r\9057A#$2\DC3Z(f\NAK\NAK\1065904\1112451\991961\&1\DC3%_m~^\FS#H\1018191`Ep\\\51310\183217DN\136566\&6u\146600P[\1039725\NAKv/t\1017565\985428Ew\ETX\ACK\1065394\US\1022855SI6y\132716^oD\184073X\65166U\175203ZE\1069439~vH6\1003434!8\50213FB: \185479YC3]w\1024031)\NUL\1021854\&1\158611\CAN\33418[I\2476+\RSukW\128007\DC4D\NAK/u\RS\DLE\r\DC3\41948,\nL>\ENQ\995376\DC1n%:19F\vM\1001382\"VS#\b8\14872\&4\t\NUL2B\tjC00#,`\US\997167\NUL\GSX\131528Vw\179904\1082712\&2Fb\1093282~]rR\22513\CANzC\176752O\186037\97367B?.e-\RS\SO]L\vJ\1088554&;\1078281A\154147zz\ESC\40910obH\auc<PG15\1002867\SOHwV\60200?cJ\1074901Gg\50527\989237s0lCc\25893g \rDG/\74284 8\1016244@K39\fY\r\141146i\38042H{\FSy\1065940yo\DC1t\SYN\156691:G<\f<B]3A0X\1003257 \DC3G\984907E0'}\160236\7611\a\ENQ7\SO9if\fW\1044886Gm6\DC2\1107521RT^\153263VI\41296z\bI\DC1\142286\151680u\1100792&\983311@1\998249=zL\rl\983935\&2\98644\FS~d\DC4A;\1030453~|\n%6z\1096795L\\'6_x]5Cq#"), rmCookiesLabels = [CookieLabel {cookieLabelText = "\SOy"}], rmCookiesIdents = [CookieId {cookieIdNum = 22}]}
testObject_RemoveCookies_user_2 :: RemoveCookies
testObject_RemoveCookies_user_2 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "y-lL4\1032509\DLE\SUB$\113781F\ESC0\66193\142285$\DC3f\127994\&7\1007481\\H>\US\DC1A\DEL\ENQ\173979\NAK\a\STX,\EMC\SUBD\ENQ\24480\FSeZ\RS\DEL\51026E\150471]#\1034208G8\16854.iG)^8o?SY\134777|aO~\DC3\ACK\139956\1034284\GSi5\1107367xKnL/\ETBOl>\"K\1099651r+\NAK\35714w\NAKk,Gv\1085365\14852Ig\1109559\&7\148764G~\1112964M\STX%>=\1015812QP\8278\DLEq\166344_4_\53073~\22816\RSs\1061699\988439|a\ACK\1004322\SOHB\48307\&2\DC2y\rs\ETB\SO}\149272\SYN\1067591\7251=g/\SIKEx7\GS\SO\161588>6g\DEL\66198&M\41921\ACK>\"\1064330u\\}51mi\34188\EOTi?d\1026500(kC)\180920\1083584=\"F\1069044GpL'\113740\1031277fb`+u8o\NUL\1054830\1102736\&6#V j\DC16\191326\1031189\STXw\v\DC4E\ENQ\bF\f9\1090652LH\1008733\USaB\1310V,\1081305y-\43621\1054171f!P\147021f\163388aX*\DC1\" ]ft\988604\\\1030237tf"), rmCookiesLabels = [CookieLabel {cookieLabelText = ""},CookieLabel {cookieLabelText = "*"},CookieLabel {cookieLabelText = ""},CookieLabel {cookieLabelText = ""},CookieLabel {cookieLabelText = ""},CookieLabel {cookieLabelText = ""},CookieLabel {cookieLabelText = "8"},CookieLabel {cookieLabelText = "^"},CookieLabel {cookieLabelText = "}"}], rmCookiesIdents = [CookieId {cookieIdNum = 31}]}
testObject_RemoveCookies_user_3 :: RemoveCookies
testObject_RemoveCookies_user_3 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "[eA\vA\1065594a\1067423Rjp\1108868\ESC\NAK\917626\50470O:\DLE\20334\70070V\EOT\1009472s\188696\1055192*o\DEL*\1097478\1031823S vO\DC1}\SO\FS\DC1\20172\47401\175146\24482y\RS%\177853r-\152368\DEL^Z\f$hn\70198\tQ\51783\1015686u\135194{\15726Hl\SI2\1010453z\fk{%i\1010810RC\ETX\37535\31389j\FS\13544f$hi\1041556\GS,.xU6>>2RT\1082741Z\1094623\&7\140880\GSU\53025\150332&3\1057198M\DC3\1062453\1109765\1034834\SO.B\1060301^|F\92187\21771\ENQq(\ESC5%\1105942h7\1098667:r\1019306%yy\"\18308di\"\29364O{\"\24656_\1056565~\1000469\ETBNVc\154992q\SUB)a\1011637\n2G\1005801\167785n\v{!\FS\775/%\r`\141340\1067667\148628d=\DC4GoWdv\SOH>BQ\30558<\52920\DC3g\SOrw\1002151r?\RSt\ETBS2\26479t\32746N\ENQ\t'\US&1\NAK\SUB\r: c\985550\183867^\ETB\b\25418t\CAN\43119\1085016\63442H\1036558r\ESC\1111256#pFiu\SOH\FS\27100\68004uA\44766\ETX\n\DELD\ACK!\"\SOH\SYNi\EOT\1057427H\2007\143595\77933l\fp\DEL=\US\149455'K\1036688N\ETXY^\US\n2+dj9>O.t\1037675\ETX!8>wY\991175~Pu\1097324I=\DEL9\128719\GS/\174234\93985}b\1021444\4123\29495\v6\12208&\SUB\1027882\1050821\bH\27782\1073111=\186661\DC4=vA\1015425r!P>|\DC3b\21384\aagS\1059995\RSR\SIQ\SIl\\F\DC34i\USp\162488\\J\167073\1037502\170718D\EOTII\1108393\31203\187896^9R\DC2Q\DLE\158705\STXU\25530mT\991575z\FS\1066029\&0\"\183161$\94874<\10145\SOH\v\fB\1066397\DEL|\ENQb\FS.{z\1038007\1005478\SYN:=\DLEU\1046301C\GS\1108644\&0y\ETX\163798!\1094754\r[@\"\RS\SI\164993fN\1043335\DLE\RS}\DC3\1078193\983493\np\1052692^,*\996701\1023322\FSe\STX\SYN7Q\aQ=\1030457\93795\1032495%.\SUBg\STXs\ETB\985421F{p\134066f\126110\36459\1106604.\1107191n\SUB\26143Y\a\a\ETBsVm"), rmCookiesLabels = [], rmCookiesIdents = []}
testObject_RemoveCookies_user_4 :: RemoveCookies
testObject_RemoveCookies_user_4 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "\t\ESC\STXr\141588@\EM\ESC\137149\n\998649\133364\62444I\EOT\66445 a\ESCE\156712\NUL\ESCJ>\EOT\DC1\999734n{a\97638\1060179Y5\39576#\US\118946\989404\133271t\1064639|\40271\n\1013041l\ESC\DC1@\1076459\111033\USJ\ESC=\177792\164909\50507z\RS\167481vH\SOK\68042\1071871b\65871`\GSdw<\SYN\190410\ETXq#\1012261\RS\164870$t\EOTk\DC4LW\1090786\178740\1048691\vGT\\(\SIP}r\57433\DC2\34356\25028\52370\US\129414\US(\f\EOT\1051650y\ESCh\EOTRT\1091663\&9\DC1 \1023022\988514\166532wR\180401|IY]\SOH~\rW\DC3B\DC3y\1043465Z\EOT\\\ni\1058721\1110476\f\1000132G\1077109H`\1035001\STX\r\19025U\1113516R\ACK\1049955:T\1035421#\150511\&6p\167154\EMS\1002951#\46848,g\2489\993726nfL\t\14881\4826krTnG^\DLE\n\SYN\69986\r)Jd\DC3G`\GS\SI:\DELWzz\1085835\b\CAN\DC32\STX\STX]\DLE\1027598\ENQ\38439;\1062118@\95362k$7]\RS\DC1\r2P\n\ENQ\t.@5%\1062583\1058795\SYN\1055359B\\\DC2\1002887\&0+U$\1000694\DC3\ENQ=\b1G\177294a\55187:x\143039\STXI\185458\1018834O\DLE@\FS\FS\167931 \DC4\40116\&8{^\1055348;\1036463x\1075367K\DELP\EM\SYNL\GS\1035506\&1\48587\139384Xmx\1075010d1D\ESC\bk\170241\bJg\983320)h\GSg4}\1036362NH?w1\1090271r\r(D'\SUB$\STX\GS.7q\151630m\36165\15041\1111663x\1107666XsY:\fh\156704\67324FC\t\1104208P\DLEQ\SYN"), rmCookiesLabels = [], rmCookiesIdents = []}
testObject_RemoveCookies_user_5 :: RemoveCookies
testObject_RemoveCookies_user_5 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "R\DC1\GS\SOHi>\1038146\SIAY7\188329\5067\ACK\r\n\SO\DLEw\149314n\ETB\53966N\NUL[\1040810~\1040911\GSYBV\ENQ\8705\1082709\184616[\1070848[~\100472\DC1\SO\10715\39222KQ\1047672\SOHZ/\DLE\DC3\NUL!t\NULpm\\b\12239=:a\38462\EMH\54427\ro\47050\1093703ru"), rmCookiesLabels = [], rmCookiesIdents = [CookieId {cookieIdNum = 2},CookieId {cookieIdNum = 2},CookieId {cookieIdNum = 2}]}
