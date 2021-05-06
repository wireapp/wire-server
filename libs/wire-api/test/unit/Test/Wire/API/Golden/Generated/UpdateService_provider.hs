{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateService_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_UpdateService_provider_1 :: UpdateService
testObject_UpdateService_provider_1 = UpdateService {updateServiceName = Just (Name {fromName = "s\172850\165672&\140109|\176984\1024620\DC1O\NAK~9O\GS\166020\GS@h\1089102>\1048752pWDvv)*\NAKv\119346\ACK\158086\&0\66836\1035372\1080178\STXae\RS0SQ>x\SOH"}), updateServiceSummary = Just (unsafeRange ("B\STXf2\NAK\rw#\6920b.N<\61892O\153588\DEL\SOw+\USqw\n\1049933_\DELh\1071786\STXSSaA\1089635\&1*\132033\f\STXd\1099582\1087201s\1050888\&9NE\125058")), updateServiceDescr = Just (unsafeRange ("\1049212\1077603o/:\STX>9\1054397r\SYN\1039400\DLE \SUBv\1007825\&2\NUL:K\r&G}32\ETB\EM\NAK]\1091607OjM\RS\DLE\1070361\DC3l|:$\31947\145138D\SI%\f\990331\155352~\1099672\&3)r&\1048683\RS\64181E\ETX\1041415qAy\38939\RS]\DLE\RS\1068996\1068105x7\ETX\CANa)\1057258/d\rTb$XQ\1016584x\EOT\30548\SOUs]\1011088(J\1022573\tu\1022669\1001690e\148176\DC3\1038231\1092283\4753\NAK=\181309*\US\b\t'\1018513#ph\180587e\48018\ETX\1003112#\170768\SUB\1095673\137251a\1048310\1088278%\CANgWc\ENQ\24979.\39522.2#l\155501\1034362h\21716 4\35795B\1004426:\145268ti~#j\f\t\1033515h3\133875Dc\1016802Uxp\ETX>\SO;\176727P[nP\DLEe\36672\163257D\39415\1058109\1109944\989372\1068452Z#\1096316<(\992299C_\119592\t\1043785J\STX2\142641\9149\174094\&4*\\\aAkU\992556\&5KyA\1064125\&2~kO\CAN\SYNA\EOTD4A\995728?@5\140015c\183859\135647\&3l\94040h/gwjw5\FSD\SYN\a#,]\DC3\1074817\54621\SOH&\40403\44254\STX\CAN\DC3\CAN\155058\30916b\1066397\188124j\28333\&4-;\CAN\34323\bnj\69638t\RS\1107169j\rI\ETX*n\58994B\1101168-\1011038\166428wR\1062976w\29954\RS\bXA G\1023596X}h\ENQ\52035J\1107887|\fTD2qO\1075093\DC1/\GS\1060882\1038855\&9!\1064582\ACKlp\1089241\1081327\DELV}~\27330")), updateServiceAssets = Just [], updateServiceTags = Just (unsafeRange (fromList [ShoppingTag]))}
testObject_UpdateService_provider_2 :: UpdateService
testObject_UpdateService_provider_2 = UpdateService {updateServiceName = Nothing, updateServiceSummary = Just (unsafeRange ("\RSU\1073836o \153723U\190862\&2._\54557H\\l\DC4\1042298\994578")), updateServiceDescr = Just (unsafeRange ("P\99336#\STXT\1025814\20483?q\1108738\DC42]\1028397M>dg\155317S\US\DLE@\997631\135812\&7\SI\EM\a0\1105645\&1\rK9\SOHZF.F\40814\1056738\"^\1096615Tj\35514\1026896p.(-oo\37114D;\SYNA\DC1X\DC4\ao\1058110\SUB\GS\SIp\\&1\57831*\GS=y\1011045o\NULF\1011682\150093nf\1113049]\45436\46942#6\CAN6\183428\991016\1057864t\ETX)x%\987336\168760\985436\46914\1114005\GS\SIY\172986\t]65#U>\ACK/\148308\173948\DLE/\\6\EMY\a\NAK\DC4}v3\n\3207\\k\57457S[\94075\SUB\ETBALL\GSH>\120201\DC3b%\1091843f\21355\RS\1073409\SOH\1004647l\18980\1059571E*\v\EM\7692\beW^r\171702\&52x*\DC4}uhwg?\1100107\f(~j\CANw3DdG\t\NAK5'\a\aq\1068675\NUL\ETB\1031438&\GS\ETB{\1030632u\188175%:i\NAK\41849\9245\CANr\r\1110633\1033981\&7!;\1087291$\ETXN\1087123^\156188dj\RS\643\&3w<\n<I\78530\NUL{l,\SYN\ETXI\19470IxoD\1003535\59748\EOT\146874S\STX\183074\&8q\DEL\DC18\1006406&\151754Q6\1066224\RSs\r[&\52450yQ\t\14792J\158644\151658w?M\RSF01T%^\1061347lX\DC1\166243\1098452\&9\ETX9h0\1017483\1042022\991319\&0:\1017326\7317\994302\1002867\\-\177010 Pr\1096366\&9\97481Cxc\1002561m\99380\67993\&1n\DC3(\DLE \128124\"\1104765\DC2\bn\40294!H\SI\a\1054721\1112502\1089269\GSGs\42666\SI\134592\r\1015504Y\v>\ETB\CANUbe\1003274\NAK\n+uF?\14463;U\1039265?\1010190er\DC1S\\\EM*o\42071\1017466}>\132296\148001\tG\NAKu\GS{X\70487\n\1100913\"_|PI\EOT\a\f\1004455^\STX\fp\46828\SUB\a\SYN&\1029221\DLE\169996V|\bZA.\1438\1001883rWgZp\GS2+G\ETX\aiE\RSh^\by\180361x\NULRE\DC4w\1027514\ETX\98182\1070363\RS\EOTW#w\5871\RS\1104278\54933\1054813\vE1j/\1108406@\"\DC4\53646\1101882*\1085014%D\FS\1043470\NUL]O\1017648\&3\SOHC\n\SUB\1003678\1090196\aad1\38690dD\\\64103A$;\NUL\f[\1071011\\M\139621uh*d\r\EMqmpG\DC4\ESCk%R\DELv7\DC1\1016517\DLER|{o\DC2*Cn\a\SO\1043684F\1044083\48517\160126\189383\&1Lq\GSJV\64881PP)n~\32487\&6?")), updateServiceAssets = Nothing, updateServiceTags = Just (unsafeRange (fromList [EntertainmentTag]))}
testObject_UpdateService_provider_3 :: UpdateService
testObject_UpdateService_provider_3 = UpdateService {updateServiceName = Just (Name {fromName = "%\7044\fl\fD;F\1058661\1053691\188594aTK'[\\&C*\a\8534/)\NAK\25133?4T\b\bN\"]\DC3\FS\1073196\SUBHok-qjH\STX 6_\1089217#\992455\&1sw\1066204\95078\r\ETB\137335}[b|-*-\23348\44079\ETX\165504\172793p\994909\rWz\EM?M\1032452\US\EM\1024537JX~X,'\11104\EOT\21754\DC4%\RS\SO\194784FK\v\994253\1054756\131324\17546o\30468\14430(j9WHB\50938R"}), updateServiceSummary = Just (unsafeRange ("H01=n+4\SOH\6692\1113036\160636\NAKr\984428\CAN+")), updateServiceDescr = Just (unsafeRange ("#\SOD@b\DC4Io\1096440\152446G\ETB^6A\SIH]\EMcE\63164Q\ACK\t%M+\v\SI)~I\43918Tq@\US\RS\FS\STXMC]I\ETB\19066Mi\DELp\29848\1031265l\92437\28645\1031754{\69767j\ENQw_\99223]E\25671=\1029255]4\ENQ\1069581F\998245O CeC\154822_[O\1106297\1014834\b\45255g`vE\1036069\52294\1033720p8\187109i@\1045038\1057621\992130\SILR(Gs\983325\1080751;!#\ETB\ACK.\DC3\96790Y.\vnJ\120713;:Q9/z:\1037524TD\FS\189324b>\1038611sf\120784\71474w_[;\DC2*\186589m\ETX\DC4|k%\174468Hu\ETX'\24710$oX\ETX\1039502\FS\37720!q3N\995124{\14389\a\NUL\DEL7\1103646S%\1080080d\FS2?wzp\SO\EOT\1068542\b=A")), updateServiceAssets = Just [(ImageAsset "\10842\&4\EM" (Nothing)),(ImageAsset "S" (Just AssetComplete))], updateServiceTags = Just (unsafeRange (fromList [ProductivityTag]))}
testObject_UpdateService_provider_4 :: UpdateService
testObject_UpdateService_provider_4 = UpdateService {updateServiceName = Nothing, updateServiceSummary = Just (unsafeRange ("w?\119066\&0\1030508c\\\160326Y\152981Z\US\DEL\37541.wPNh\NUL\53884G\187308z\SUB|\184168o\ACK\155435F5N<{\DLE*-\1066615\&4'\176227\RS\157507\189682)\119067wK\EOT\984560\US%A\157119\1088771L;o\1032681|\1055370[(b%\ENQ~w\GS\182096\SI\41864iqR\ESC\15394\">Z#\1054004f=T\DC34y]\SUBf*\984699Q\v\23696\1062693hC\SO\12085\147568q6\DC3\1062665")), updateServiceDescr = Just (unsafeRange ("*\168117\ESC--\DC4\NUL-\1052004\1105891\990837p\1103570)\NAK\65576\t.\186012\52153\10441\n\DC4}x:\SI\1000171ly\SUB\ACKGv\CANy\1009978\161732\1105543\DC3a\vhQ\994868vn\"\US\ENQ0!\36754\53055\&7\179466{\t\ACKP\DLE\GS\FS4FI\1072734\1073561\74964\CAN\140851\&6\b\26969x@\US\EOTRx\CAN\1082101k\nM\25798\1046668\t!#.+=\1052164\&3*\r-s\183766XY\DC4S.\1869^\ETX?(\1109849\988658\1032082\SOPe\RSU\n6\1050901y\SO+UkD,n_\178892P\DLEN~Qi2I]Q\SI\139163\1065495\ACK`\139098}\EM]z\1094735'\ACKwHxq\SYNb\DLE\GS\1084398\29921B\78200;\a\GS{0\EM|YM\DELh\21262\SUB\15037V6\SO\1035410x\167834\&0=&\27418LniL \\fDj[\1065042\SUBe9\65920@\FS\1094099\DC4\STX\STXgs(O%\nP\GS\181580\&6F\8364$j\")\44877?\ACKW\100370}peJb?\DC46J\11336Qr\13143\1080814?\EM\SOR\43999a\"\35711\DC4a^)\ENQ\21478R\SOXp \128418r\NUL\FSv\1063744\29979~_g\149824!2\63853\DLE2S\78355\CAN\17330H\10176<\184892\FSW\ETX\n\ESC\r?\FS\998788,\NULp\f\154520\140546/\DC4,\1076907RYHE\127767\rq/\10339U\42877\173144@;d/\7988B\r\120942\1024564\1096920\996264\&9Wq\SI\r\1085078`\b-\vg\1023338\USx$\GSk-\917893\129619\STX}F^:P\4013=\118993$w\"\1099811\1033010\rq\USA1\1048881\STX:4\1008418g^\159732`x\DC4z\151154\&8z\1047086J\1043220Z3e\DC2\188391Cm\NUL2L3\DLEg/a7\145916\1083987<\35126\1019035\r\SO\1063278\11170\v\DLEqS\185493]H\1017322\ETXf;\1103868\v\EOTA\40288w0\SO\1019810J\DC2H\DC1=X8#\1074956\1097070\f\1027396\1081153:Y\SIKVa{2\152843O\CANgk\185206\1080118^\1047219s;rO%\19021\&4e\166558\39811iV\ESC\150722\134118wGO\1071020\r\DC4y+\SYNA\40980\&3~T\1055152W\DC4?\DC2<x49F\r\1031849\&7\STXn\188121\&73\EOT\r\f7\1106142\&7\32732,j\1030036\1081098iuS<6\SYN\27599\EOTm\r6\CAN\1033492\23477v\185350QJ\DC4X\167232t4\USF,7\DLE\bhL\1014587\999033{\1046848tO\147551\&9Ua\fY\v\"9\1036469\1068922\DC2\170284\99420\DC1\b|\SOHI\984252G\99658YM(\36042V>\a\6532\156135\997155/{\\F:\1055728'k\1050357\3916tx>\150749\1085184-\1071909\RSP\70082\SI\146514A\DC3\1110224\&1\179667\55136WB#v\f\28088\ETBA\4824\EOTQ\tcM\EOTH\183337@@\NAK\RS%\NUL0\ESC\987086d\a\28008\GSm\179744\FS\988078\164009\34091\169466.b\50709\180121|S\1103622jA\DC3,\DC1N\152526\&2Q\134010Zq\NULX\SIl;\164506\134219Z\98015F8\ESCa\1100929\"\DC1\141866yb;\\\989247\&8z1^\6934\&1K\1091933\SOH_\f:\159736vW\ETBEB\13056\DC3~\DC3\1104636e")), updateServiceAssets = Just [(ImageAsset ":[" (Just AssetComplete)),(ImageAsset "\ETXF" (Just AssetComplete)),(ImageAsset "2\1141" (Just AssetPreview)),(ImageAsset "\SIO" (Just AssetComplete)),(ImageAsset "i" (Just AssetPreview)),(ImageAsset "^\ESC" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [TravelTag]))}
testObject_UpdateService_provider_5 :: UpdateService
testObject_UpdateService_provider_5 = UpdateService {updateServiceName = Just (Name {fromName = "0/\FS\USh\US(\67154\STXR0V\NULv\58243Sp\188059V\t\"[6\991409\1064142\NUL\1030684L+\143732P\70103~QnN&{6v\1007280\991294\1019817\SOH,}CI\38573"}), updateServiceSummary = Just (unsafeRange ("\ETXl\20673R\RS\DC4\1036915&\71920')9\"\148693\40748/F\50818>{d\GS{<m D\EM\1103550\&3")), updateServiceDescr = Just (unsafeRange ("b\40219+\SOur\33246\RS\ESC\1035285\1041331Y{\SYN\17707:A\v\rt6\SUBQI\66765*E\100802Y3f\n\1065258~?\NUL\DC4\170400\66320Ko\ETXbr\r\r\EOT\US\186614O\ax\99109JevvLKS\1089336\50995ru;il\1040795qn\aaz\1045486A/\EOT!\97746j\60124\ETB\1006629\tRg\1083108x\131093\151785p$a\v1\1041475\52078o\1040244\11495\28351:THx\6931fb\DLE\SUBj+3X\146139\140947\1104714!j\1095994\1102079\RS\SO\147976KLq\61979+\1036065'\SOH.\t4\988128Aj20\1104533tu\49464w\148272\1090729K\1097108\39196M\CAN\32473\31564\ETX\DLEHMn$\150342]~3P\DC3SQ69\STXn\1092353\FSc\ACK\61466]\32613\171957*\NULai\aaliu\GS\180505E|f(\998358g\1016889A6w6\SOH&\SOH0\ETB\"\1008780B`\57807\17970\r\1091954\1008205qxO_\DC2Hn\f\1091708B\ETX\1102797<6\1065767Hu\1018255\140267\&7&~?\1075744\1060512{\GStr\CAN\1211\CAN\1015676\DC2w[c'\FSe\1080513\135094N\1005238fqqR\58684w\b\r\\H5}h?\f/<\EOT\GS\aX$\SOHs\179923]\SYNf\STX\17555R \1049955\EM(t\37857v\1101707B\STX84\DC2\168371\GST\138640\1072046G\1066295\EOT\1086552#\1092438\SYNj\NULOZ\148068cJ|\1049928\1061194\92558K\EOT:\DLE<j\DEL\1028103\SI<\b\983337\1073557[8&=2DB8\119267\a\989036N6\1075114\RS\45697Z*A\v\989492'fp$6^\ETX[")), updateServiceAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset ";\r\151007" (Just AssetPreview)),(ImageAsset " " (Just AssetComplete)),(ImageAsset "l\17662s" (Just AssetComplete))], updateServiceTags = Just (unsafeRange (fromList [MediaTag,SportsTag,TutorialTag]))}
testObject_UpdateService_provider_6 :: UpdateService
testObject_UpdateService_provider_6 = UpdateService {updateServiceName = Just (Name {fromName = "'w:u\US\STXU&F,bmq .\27908y:=_\1038766qv/TK^\"\993167~y\SYN\52448^\ETB'&d\n\NAK\60441\DC4z\v\"\a\97108\DC2\170997i\136563S-w_\RSj\1011073\1082021\1113363*GN>1\STX9\RS\v\t5\SI\1045400\1000816\1022033\94973\DEL1HR\t\7686sY\1022159\\\CANdbJi\FS\1078008QiQ"}), updateServiceSummary = Nothing, updateServiceDescr = Just (unsafeRange ("\DC1Pelw\1007630J\DC4KK='MLc*n75\CAN\DC2u\1089064\EOT\1055477bS\1011021J\186711#1.\RS\SUBn\140901\64800\\M<D%\1106685\&5\RS\v\118804kA?\1099144\SIQ\991169\&77\1026578\1054636\SI0\1070834zP}b.n\FS]\ETX\f\1003766\&6{`\1009289\ACK\ACK^\NULQ\1012854\1084114h\23965N\a\188891\b&B*zM\SI2\145799\998339\SI{\SYNTG\6297\37398\t\EM\1076957.\RS\1092424R8\1029680\ETX7zz0\SYN(8\DC1;\CAN\b\n\47383\169237~&^%\ESC\ACK|3Bc\1113940SmujM\1072185|\181342\RSy\FSY\151371zN\v\24988W\1012605F&(}\1070244\NULk\1067845=~;{\SOHBr\EMRb<\SUBf\23609\ENQ\21474\989886\1095509\44884\&0`\175327Ku\1049654O~*\RS\999173\1023763\189253G`lt=Ni\144761U\"/\70001\1013282 \1000149Y3\EMj\EOTG\1021824\a\13154Q\1002615\1028483B6\1040107\SOyL\t!\162530\990818\169762V)\ETBPs\991783\ACK+</>xB/\985208\&1\DC3\176032?\127011\1081977\&9~N\DC1G\152236(-tO\1094339\CAN>f`\DC19i\US%+4v\14821\159768}TE#7\"@\984318`\136449\993150$@\DLE &\ACKX*h@~\19095\988717\&7\94224y\162287p\1049298t\ESC\140015\1075273h\RSd,y\1008537=0 a=g\35622\1031991\1075324>\139637\EM\"/N/\DC3\1094685W\78736S\b\SOH$\RS+8\ACK\FS3\1028937\&9>[\1013190\1016769\NULg\DEL\992511!*d\33289+\157587\181122\23805\NUL\SYN\v\136041\ACK\STX+\42243; \CAN\t\1037586\&4x\66231!Bh_\1078309!\rP&\53998\986112\CAN\GSN?c W}\EM\160488\11206Y8\161012\DC3\131288\62203\&9^n\177084b\DC1o*RwOq{}m\NAK\ESCmhOnVO\78842wl\917975|+)[4/\DC2\60636i'jC\rX\1006797Bp\1088603>*dp1\44139Yb\1086336A\63789r[\nUFh3\DLEbn\63566\1033269\US&W\1089940\3335\DEL\5839\1085009uw\NUL\186306u\1093250,{iU\US\133686dA\DC2nr\DELW\50456\27722I\66652\SI\SO\1032020x#V\")\988053ZQ\24812\1047399s]\NUL1\DC1i\US\1081811\STXiRw\166810ELJ\NULD*x\995762|\EOT?\146832\994792\72786Wio\1048997U\16132\21482\SYN\1093524[Pc\FS\EOTm\121191j\1099418\1011973\175600-0v\DC1we\ESC\1070856!8o#4jO\1006545\132488\ap\146085&\34775l\1077237.\1004654\&8DU\DC3MP\1064216\US241iv\US\152107\1085888\\\1076641\tBe8\1062803XU8]Hz\\\US]\f\1028108uY)q\a)\CAN,\1064912\1087503>/$E\984269D\NAK\ESC\1042464Um9\rC\68034\63982]8/I\3333\b\bm\ESC\CAN\DC26\\\1063501\nb\STX\ETXN\135896\SYNu<\SUB$4N\1076197\993595\986181*4A\GSa")), updateServiceAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "D" (Just AssetPreview))], updateServiceTags = Nothing}
testObject_UpdateService_provider_7 :: UpdateService
testObject_UpdateService_provider_7 = UpdateService {updateServiceName = Just (Name {fromName = "f\28020G\RS\1026048\4283\SOH}\DLE\DC3\GS=\r\143791W`\FS\ACKH#p\145007cM\62862\&4aW\STXu\1045785i\DLE?Wy"}), updateServiceSummary = Just (unsafeRange ("\SYN\SUB>\"oyi\ENQl\b\RS\NUL(N,\141278\997171mS=RJ\1010624H\1105519|\1046651DT5y.K\\OO\DELk\"ER\DC3\94814 x]\ETBS\CANtX\1014323\30845(\1092699\1105900\1089053)\US\1049598\\GD\986473\983254R?V-\1112433G5(;\DLE8D\1017732*#\168450=$Gh \DLE:S\fCR\177945\162785)l\98486]F\EOT\FSGi\1113445\SOH\20995\b\ESC\NAK\54082\&0rg\SYNvv\FS\1036019\43273")), updateServiceDescr = Just (unsafeRange ("L,\RS-\r#l\GS\EM\1063416\1035677[\DC2=\1064814e]:\RSke\ACK\ESCO\1096418%q|\trT\157922d\16098_J\1066797\134639\DC2zi\48850F\SUB]\1110620\1029411U\1073793\DC4o\STX\172729xr$\SIO\659\a\127084/k\DC1v;!\33427}Tc\1022360\&8a{1?[\1110148]\1042959s\1093977\NAK\ETB\140684{P\NAK0)\180391V\161095\40231\&5h?g\1101700C\1064778\1112375`\t^x\1014960l\1102736\1078550(\142183\158278\ETB\65593\16268FGW\US!\187769$j:\SOH\ENQ<a\1023461\v9\987103\DC4\999014Amp")), updateServiceAssets = Just [(ImageAsset "uI\DLE" (Just AssetPreview)),(ImageAsset "g" (Nothing))], updateServiceTags = Nothing}
testObject_UpdateService_provider_8 :: UpdateService
testObject_UpdateService_provider_8 = UpdateService {updateServiceName = Nothing, updateServiceSummary = Just (unsafeRange ("\1051401~\DC3C1\1103426\1065711\\\152092M\51115\&4\ESC\1025004\&9J\1021337O3\1098757f.V&o&\1048647>\41854\45618")), updateServiceDescr = Just (unsafeRange ("\ETBn\991943^\1094103f]\169584.3*\GSN\f\1023908\45849K\155293'\1089529\nkX;\STX9K|:\42256\65359\1023919\25418S\7279\a>\a_{\92245\1104378\RS1\ACK^V\ESCh\DELj\18578QJ\50390\1082924\US\NAK'\1107361\65226}e\139000O\\\1078728j\174816\NUL=\ve\FSP\1064496/c{\SOH\32254Y\175473\150182oS5R\SO\1020810\1040926\182050s\992339\150357>MD\52200\&9#k\94266~\37292M]*.qP\1087373\157794cz\1028548cvV 9K\1006577\STX\1103483\&5/\181983oevf\1004680\1074329\57416\&6HV\998440\SI\v*$\10316}\ESC\190964\&0')\55198\141151\t{|qk\ACK\187298\191038\ACKXqLhv\tE\ETXnQ)4\161971SR\1086709\&8\147313\DC1\r\FS\479\1029711\SUB55\SO\v\n&\\N\74425\186845q\ETBTv\1008606\1094377;[.51\1079236Ne\ACK\ESC\SYN\1006519$\EM|\1001606y\FS'\189697\1010140(t\DC1.\DLE\SYN\EM\988333NVlD\STX\1060821\1018872\DLE?\ACK\38297(\1049506\&5\47526m_C8H.\60260Z\ACKp\171210!\1023003m\US\DC2.\SI\t\SOq\1042389%:)#u;(ay<\r\1047652OVC\FS\150431A)X\1962\&3-Y0MF\185012`\984613\&7U\1075141\&7\RSi\140794(\23523\1021875\DC3b|\1114030d1\NAKJ`\1009508\1051452H\SYN9\986072\989691X]\1039415\986102X,T\17415\ETX5KT\1073245oG\GS4\b\ENQ\16582?\v\EOTs\18744\138109\bk\184560'\1008490\t\1082262=#\aws{j\1010694v\CANt\SI\US73P\\a\1045684\8519\RS!\\bJ>\tz\135915\1061027\157174\1086046\ENQA\60459\65130\991533\&9\vO5\171379H\DC4moK\155844j\SYN:\FS[4\RS5w\r\1091059\&4\v^\f\50515a2\bI)N\EOT\174927k\US\28803\1095107M\NUL\1102329\1025175(7dvF\NUL\74778\41169\155843\1109745F\178292Q\167249\NAK\STXh\STX92\SO7\1042964WM\f\72131\DELSCZ\1025621?z")), updateServiceAssets = Nothing, updateServiceTags = Just (unsafeRange (fromList [ProductivityTag,RatingTag,VideoTag]))}
testObject_UpdateService_provider_9 :: UpdateService
testObject_UpdateService_provider_9 = UpdateService {updateServiceName = Just (Name {fromName = "\SUB\DC4>t)(Y\a\1081674r\FS\NUL\1046715`\97660S\GS\CANmH8d\ETB%\139045{\1072461\1034674&N\68324=\141624Gh]\\\1053730\1020620\183683n~P\1058445("}), updateServiceSummary = Just (unsafeRange ("\100581db\1008472S\1031428'i\DC3C\1029761%IhF~j\\\DEL\997314\1009102vS\SI\FS\DC2\43431\182917[\1085802>\CAN*\SI}t\157250\"W\1034841\177207\67117gq\98511$\FS\24117\74370w41\70096S\DC4pw\SO\tGd\NAK\DC3\NAKLF!7\ESC>V\ESC\128201t\ESC^z\t\57939X}\1083163e\ETB\1049066\GSj\1108420[P\RS0\a;\DC4]\185968}\NAK\NUL\18506r@&B")), updateServiceDescr = Just (unsafeRange ("i@;^!F\3139v?t\996318*e\nNq!\frM\32474\&1\EM\SO\DC4\1016648Z\984826N]\1018951\&9 DMX\"(\1065279\1033368A\STX\DLEF\ACKi`\33612\ba\120585\27712\f \ETXn\140229}\DLE.\DC2p\95895#\162855s\985482\DC4\DEL\1066903R\US\1047581}\FSL\SUBU]\rD\13601\127188x\37002C3q{\1018772\&6qu\DC1\SUB\991968`35\178194)\189743\57832\165774U%\CAN[Ecxu\178416\21434\35017*d\ETB\132471\33625\1080143wp\\a\70431j=\1059290gY\61695S\144053Zw\DLECc\r\RS\tsNO|\USx\r\1021997?;bf\182007g\149955\137032\SOf\1010960\ACK\SOE\127873~\ENQ\1032013<EGt\42729$k\a]y\176621G\ENQJf18@\141129{.+s\151452\ACKjU\174077\1110650<\SO\45555\38316\1107130\1014569\42438\ACKjUP>A~&t\157041\a#!\1075581VX\1093173S<bh\ETXPEV*s\NUL\GS`\ETXrK\CANEt;)pVP\DC3E\127030M\aJ\1054248DJ\DC1T.\vmcP\a\DC1\992103 nyC\n/\1080824\152762\SO\169635\132814H!J7\v*`h\166475i>\SOHX\ETB\1011019\1002425\NAKB\139468*\SYN!B\146534\SO\128504\v\164562\ACKG\DLEU\fMy\SUBB\999868,\1015851\f`J\ETXK\a\SI\1061671[\176179\DEL\a{\95093H\145172X3u\1083031T\41599t\DC1X*\1010927\DC3\EM@\1101045 \r:\1110501Bl\1089154\ESC]j`Q\52418\ETB*P~OtgV(k1\15745^U\ESCLv\145965Qyp5\136761G\b\EM\vT\ESC\ETX#L\\{#4z\41524\&9i\f]\127765}\SOH\190641H\1097429\np\154946d\178054wH\t\176334\1107631 \43394R\tG\31450\tG'\RS5E_BA\1053464\&3\1057539\&5UWJs+b=\SO\ETB\CANjN\SO\EM\1021363!f\SO^MZ\1066882@\SI\1005452\&6Q\ESC\DC4!\22417>w\1035947$Uje\1077874aI\169185\62943\CANjYg`5\41817!0\158639\GS?&\67722\31846Ck\140595\1027103~u\1107400b\1044784s\40394\ENQ\1072267\\Wp\SOH\SO\NAK\152454!*/\RS`>l\1082901\f\22961\b\DLEaq\\\1026025\DC3\1064944d\28128P.h5l\f\27134\EM_)\1044538\1078201\SI\1048027\&5i\1070162\STXcWP0\131617\1112183\&6\191180b;\1065661\GS\GS\n\177569\168606i-~vv#}G)\NAK$F\EOT0DbT[\33348po\f:\49921\1002678\FS\DLEl+\18804@$T\139386C\1024570\1109681\1011706\DC2\ETX+wQ~'z\CAN\ACK\5755\US\f\1040182\SUBL<\1030997\1012764-_CI31hF\183695P\997762\33517\127277a2\t\1027870\1044075\NAK5f?3fp\1030040A\EOT\SUB\DELQ\"\1012956\NUL\ACKj\a\1026425@8O\62585\1089263t^\151214\"t\ETB\1023023\1022535\184253\110689 e\1030136):[!,\ETB\1014117H\DC3AW\ESC\f(^x\DC3\1067115\149605DmJ#\n\DLEDs\1039161Kp\ACKh8\v\10706\1113456S6\fOE\184338*B\1006574L\1085853LE\ENQ^\1051165+#\EM+v<\1073642?G\1021250\1070150&\SO\149504h0+v\1055543\&2q@x\ACK\1065218= N-h\127973N\1095555){&\39166Ep[J$\v3\GS\1100454\&2\179833<i\US")), updateServiceAssets = Just [(ImageAsset "\41990qz" (Just AssetComplete)),(ImageAsset "X" (Just AssetComplete))], updateServiceTags = Nothing}
testObject_UpdateService_provider_10 :: UpdateService
testObject_UpdateService_provider_10 = UpdateService {updateServiceName = Just (Name {fromName = "C\DC1R\NAKtZ\t,c\994699\985456+\SYN\f%\DC4/NY\72221\rc\1079985kx1!\113787i\32824'\173992G\DC2'\t-\DLE\1011310Q \SO\1022612\&3\1006604'\35305\&8#*\1071691Kin0{\171710\1019744\1041914\147983q\1077899B3"}), updateServiceSummary = Just (unsafeRange ("P\DC1E!{/\US[T\DEL9rxP\SUBGi\1107459\"6N\NUL`\tC@i\DC2s.z\"\DC4=;U<RT-cP\SOH\13328i\EMT2\120830\1078823W:t\NAK\US]@\49617F#Z(\ETB3J[?\15437\EMyfj\STX\985218%\39697\DLE-i\DLE\190810T\SOH\152049\&1\118827\139170\155049\r41\135453\STXv\DC2O")), updateServiceDescr = Just (unsafeRange ("\ETX\993755\1046674k\150885\1036813Z)\194854\v\98264\t;G:[\78632v+\DC2T\NULK\1061466\CAN\v#\DLE4\141471\STX\1058477jo\157922\t\r*\2294|lt\GSF\1086686\DELJK!\1103888\1094578\f\986071\1031179b\1058998nfG!\38154H!Z\1043136}V\27038U\135958\984225\1016614\138273uXs`O\DC12p\989682\EOTS0\181089\NAKU\162449N9\CAN\v]\22886x?\25488_y\ACK\59785m\171366);Q,c\RS\DEL\69971\182602x\DLE2J:qq(jfaZ\41466\169156\154533=\168292ww22\DC3o\DC1\ETB\1026586:e\ESC\aXn\1022507Su\1068999\1010159\EOTy\ETBC(;ZUR0<}f>N\1106755t\\\182022\1106084A:\vZ\60908\48202(\1071824)G^6KXB\vms\53091!h2_e\120946\194749M&\ENQ\1041583T\US\4729B\ESC\68252\t\1007395]y\ESC\EOT\SOB\aMz\CANey6\1012;C\ENQ\"\66631?!u8\61097K\CAN\NAKe\FSZFAV9\1071841\157702J\1088092g7K~vs\DC2<'\US\176383\51753x\20335\&4O\1089268\1106339e{\1072251\ESC\NULG\1043742\1106096\1007972/\1068718lK\1039053B{\1017926NP\EOT\1058668\&0c\168887\164285\DC3TQ\138220lL\166191\1051067U\985651\&62\\k\DELGjEr\191253?KD\1004842\SYNX,\DC2\1072374a_%\113820\SO\1108869\58578\32065\54608\SI0tQr\190609\\Y\98692v\1009408z\DC2>n(T\nW\1075453S7\n+ s\DC4\CAN{/\53491\181027W'@0s\5378\SUBACnA\128356':k\96969\&73\NAKU\ACKKz\1097940\NAKp*LIA\DLE\187721<\b\165309\STX\14630Ea[\153229^\EOT\FSX~:/l4\NAKE_z\ESC\f\NAK\1056810\DC3\"\143930\54442kg\DC40(\184179k'\ETB\132005y!\f\DLE\1006669\120863\182543\&8`\bf\1096078|\1108386m\1026222\15811\SOKq7\64649\FS5)XTB\1097487\RS\DC3\995818X/\77951D\1024266\r\174472B){@X{,\6499\EMH\vu\170194,*\62834\1023175*M\134238x&]\ENQX\78235T\1074149@\SUB/\1094699[`\92559kqn\1016126J\SUB}uT0Cc\1053692A,M\34442\34183$k\155772\1024743\1060241\1046304\b5eq\DEL\fI\ETX\NAK\1096754\99825\998446^\991052em\1002235\1075720\991612D\148998\ETB\ETXf\45629\SYN\132224v\ESCPx\1046623/5\DC4~j\63317\1074657P\349v\1069253v}\SYNT")), updateServiceAssets = Just [(ImageAsset "\1032651" (Nothing)),(ImageAsset "J" (Just AssetPreview)),(ImageAsset "\1097487" (Just AssetComplete)),(ImageAsset "\STX\n" (Just AssetComplete)),(ImageAsset "MfJ" (Just AssetComplete)),(ImageAsset "v" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [DesignTag,EducationTag,ProductivityTag]))}
testObject_UpdateService_provider_11 :: UpdateService
testObject_UpdateService_provider_11 = UpdateService {updateServiceName = Just (Name {fromName = "\1020113\nV\92483\&4\ETX$y\190585)\22232\78073*\5218(O_k"}), updateServiceSummary = Just (unsafeRange ("#!L\175566\DC2\DC1\98436OjAP\1016195\ruP9yR")), updateServiceDescr = Just (unsafeRange ("CH7I\1054522>~c\\\USz\a,\1075140\n\139093S \DC3jh\RSXS:\989660Fk\991742GKf+\990197\ETXc\307\&2`s\1080154\10611N\985221y\164252g\99336EMida\1019201J\1089972#W\n\DLER\v.\1060243Xw\ENQG*To\r(I\1016905\97388\185741\STX\169054\ACK\152512\SIW}\SOH\1110071|y=G\b+\ENQ\28501\\\DEL7\99758>ut\1000507\DLE2`\1016784]\CANA\4014\151902l\FSi\DC1\SO*.aHM\148868c\63802\SOHlfM\SYNI\\\43336\GS]<\t\SOHL\142621\1052045\GS\"x\985891n=Xn\CAN\1023790g;\32502\172301\NAK\1034133{p2k\1095693@\ACK\1001152\a\40169/|\ETB(\DC2\DC2M\166943\1071939>?;4C\131091Z\7848\&6]L\r\1019983&\181288\f5Z5\SO\DC2#\98056\1011521T\186890x6W5qS\988007Q?NM\185437r-r&E9go\179930\&1\ESC]\1060859\1003488\FS~#\t\CANda\141149\v\NAK+cZf\70721\1056233h\NUL\RS\143171r\1083405\ETB61@\FS>\SYN#\ETB")), updateServiceAssets = Just [], updateServiceTags = Nothing}
testObject_UpdateService_provider_12 :: UpdateService
testObject_UpdateService_provider_12 = UpdateService {updateServiceName = Nothing, updateServiceSummary = Just (unsafeRange ("1\174558\1104564v\\\70506\DC3a^\74338\185657#/=\50256\146385~w\48417\185196\127069+\FS\24910y\DEL\73732}|SV|\1013182(y\986803*\bf(YF\162083\ESC\1003294?q\65159?\bP{~\aL'~\139199`\1104302\&9\94595s_T[P\1083900+>rsl\111103\1078685i@r\178540P`\1073706\169643\r\162005?\FS8@,!\162201#o\57711\190640<")), updateServiceDescr = Just (unsafeRange ("*Q\984r\120185R&\DC1\120491\33530\DC2Y1\DLE\1114009\EM\178431\1076873\21431\182541\&08e\984831\120077_\SYN8'X\"J\39242\137889jP4#\FS\1037782T\ETBk\30986*Q\11361\ETX\137591\&9\6943\nG\DC4\DEL6\v\1102835o\DLE\SYNve\"_=\167949\44795`\SOH'\39069\1067429\46201-ot\6147\1080926\ETB#\SOHj\1061272\1068854xOg\1002440_\1060791aD\DC2\1030522Us]\EOT\177805 b(\184976d\SOo\\\1007412-{+o\1014038T\DC1yv1E\RS\1012354u\STX\1060259\1049044U\99884,\vK\US\DEL\37391\DC4\DEL\STX\ESCz\994052D\62452*\"\61556\CAN'2\aJT-\ETX\tC\1113887A>B\US\DC2G\1048218\RS6*wI8h/\160177;\DLEA\160784SIz\1061826\96615P\\\1017940\&4\GSu}\DC2\DC4\9603\&4\60180\SOH\1043510\SUBAEvV\92761\166014h\FS\DLEp\1063024\36076\&8Xz\ESC'\71326]E$qu\EM4CO|ID4e!&c\DELVKW,\a\STX6\1077204\a+]03\2965:r\1047064^\1017970\143405u\NAKi\44114\73448\111096j\rJU\28650j\100976\1083306\1012208\&4\r\92881Uu\145755=H\1054165\&3^\184934j/#ueoq%\DLE92\34297\150618^\45709D\1012452\&7-\146446\1031362\97018N\190887\DLE_\EOT7*\US\184879(\RSw\10427\\\16977Z7\78740,61\1037249\FSO-\DC3\187438\SI^DYQ~\DC2\EOTg*_c\ESC\SOL Z~\1034656p0\CAN.\1068188J\127355\a5vP9S[\1088979\160190Ko*\143923+J\DC3]~\97645\1034640\1004545(\1031694I\bn:M<\f8O cz>[\187437\SO\ETXr\STXh\NUL\DC1\a\v\1013326\1091168dhZe\vq\47834\\'E}\131187WyA\1078606I\20170[J\1066221_\1074378F\29625\DLE\CANlOb(,\DEL6u,A?\1082227E\66639`Q\1099413/C\33085\vy\EOT\133903@\ETB\1080330Uho:5\72742\STX\187908~\ENQ\STXe0\136155YW6\NAK]\1089892T\154669\&21L(\SYNq\146003[\NULP'\CANA\CANL\ACKRrvg\188039\SI0Y\DC3\49352z>\1017208\173182r\1009120z#A+mo\97283p\1048901$'8)m\138323\ENQs#\12103i\1000676\ETB\150455\1088534\&7\134073GX\SO,BT\FS\986171r<:\ACK\1045700\US5y\RS\1110802\&9\DC4\157888\CANl\1063507\SIDjBR+)AdX/0\1055108R\127242SI\1095820Eg\98813\&3\153741S\1069728~\DC3?\1105496\SOTNsY6\177695\1096787u$\n;\1100115\1049879<.R\ETB\1086523\140064\998137%3+>\13808\ACK\166076O$*!<\DLEP\\\993055\DEL\b\vj}\35042S1o\fdmWO&l\EOT\f\145287\63883Do\NUL4}\1053966]ES\1003898i\74593\bI,P\1095771$o\EM?\134130 q\1034815\DLE\63588\SOHjr\NULV\28888+^\1044227c0")), updateServiceAssets = Just [], updateServiceTags = Just (unsafeRange (fromList [EducationTag,ShoppingTag]))}
testObject_UpdateService_provider_13 :: UpdateService
testObject_UpdateService_provider_13 = UpdateService {updateServiceName = Just (Name {fromName = "\DC2f\1095534\2524h\182265Y\STX\176136\b \USK\1023288j\DLE8)\128406\993047\&5\FSV"}), updateServiceSummary = Just (unsafeRange ("}\99225\1109890\150977\t]\DC4\147213wTg\bS3c\SI\83276!5u\1030140\&3\DC1\1061386\156247izS\n\STX^VRZu\DLE*h5W\169014tc~:~\1029532\&2#y:\GS\994698Qz&\ACKl\182783\SI2;\993278\n\EOT~_X\DC1}GH\1057533Pt7Ldq\997622\1005416.k\ETBUQlC<\ETB\"A\SO\1093136'\USpZg.I[w\ESC\132648\RS")), updateServiceDescr = Nothing, updateServiceAssets = Just [(ImageAsset "\6541" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], updateServiceTags = Just (unsafeRange (fromList [SocialTag]))}
testObject_UpdateService_provider_14 :: UpdateService
testObject_UpdateService_provider_14 = UpdateService {updateServiceName = Just (Name {fromName = "a\DC4\DC13uo*\178402\DLE#y=\7123[v(\f\\\SOH\FSO]\US\t\UST\"z\1053418v\173554\NAK\DC4\1065378\170288\134455\43952\DLE\SOH\40271\aGW?\126594\167752\b\998666\1028585\997688\136094OO\ETB\FS\DC1}<\ESCtk\t\r#?\1074612&\1027204w\992134ag(\1013137,d\ETBc,\"\188881\96856U\1008131Kd\61567qR+\1103606\1087857A\185932\RS"}), updateServiceSummary = Just (unsafeRange ("e\1002193G\aq-\DLE\US>7J\53191\tx\162999\NAK\DLEZs")), updateServiceDescr = Nothing, updateServiceAssets = Just [(ImageAsset "\141069\SOHR" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [FinanceTag,FoodDrinkTag,VideoTag]))}
testObject_UpdateService_provider_15 :: UpdateService
testObject_UpdateService_provider_15 = UpdateService {updateServiceName = Nothing, updateServiceSummary = Just (unsafeRange ("~d[f\DLEu\USi*w{\166714")), updateServiceDescr = Just (unsafeRange ("\1073489X\SI\1007008;\EOT\183763\&6\n>Bv\1068400y2a.#\DC1&x^\15786EFZ\168934g\917990\\\179130 @`L\110657\170760\SOT\ETB\\;\SO\CANNa\1072380r\182928\ENQ\nP}\SOHB\98760:k\USO\a(%\f\167134\&0\FSZ\NULMIX\SOp\ETB\DLEcJFI\DC1\SYNE\99649\1080590\DC3\23432l\1087787\1014002\DC2@;\RS{gz0D'=3\1053907\997152\157230\&26Ko^.(\SUB\147821N\1080384>\NULQ\1084552q!\ENQ\1075183l\SUBZ(P\SI\DLE\997378x\153062:\63678,E{u\133079\&1^>f>1\145372\150197PHS)\CANG\f\STXK6vS\ETB\1064148O\1006873\SOH\ETXFk\187258\&3\b\DELa4\180477{9\69872+S/QJuc\991238l`\US\67711\1113160\FSJh\1001413q`\vV*5\f\27854\CANQlCHZ\\'{\54701O\995994(\SI*>\"1z\t\SOH\73443\16789\988498R\ETX\ETXOED\1005855\1071849c>\188169E+\182684\1106249a\EM\1097771\&9_o\167524J\DLET\a\151961AU\19339\SOH\41840)Z\CANU\159003\US\50711\182962\150497\3792\EMk\n\4696ZP\EMV}^\133475\1111057m\t\1098603B)\1005271U\172218\1014177m8/&>BbeMA \FS\ACK\1088060[\1032205C\\m>ETeOP\1102677\n\f\1008530\1020533\STXw\DC1\171024\41395z\b\t\US\97161j\\_d}\1007980\1018693\1035295\41270\1031606\990245F\1010907\1053967\SIz\14209[ue\SUB\1031191h_\FS\vkjkYIV\a>\SO\r\ETXq&=\NULb\v=P\183189a\1077341H\NAKif5Z4&s\63331[\DC2XC\ESC\GSSl\1095889S\24945-\SO\1052669\aAF]`WZ\141704\&0Y5u\31253`C^\vt\28732$\62987Mi\29853z\1072329\rA$^\1020288P\1043788\1067556\STX\1083309(\1058341\100738 \1028670f()Qu\162836\143056\&9\99538\1017657\186604M.Y\171460\n\143786\680\DC2k\26214\185849\US\149564\EOT\1010152b\ENQ\DC2\rz]\SI\NAK\DC31?q5\EMi*\35895D@ \US\1019120\180899I\DEL--\31844m2\1031187[!\SOjKA q\168643]\STX4\14039\172967\&9>cMj\1066471{\1094763\143266\1095421\SUB>N,r\nr\186040on\1007773QQ\tpk\"x\1017950\NAK\ETB\1079116\\\GS\b\1066917\127400\996231\n)b.E \39399W\SYNDJ\1100433\DLE\997798\37773\&3$\52240(e\f(@7\US\DC1\1077076\1069008I\NUL\b\DC4:wl\1009150\155399\a\EOTju\t\1017926Q\SO3u'o`rk]v\n,:VVM;\1020215M\NAK\1064937n\2365w\ETX8U9bC\b6C\985012G\1046994'\170855\ETB*]/\57433\23645&p\138497(\t1W!\CANl\CANN\ETBK[Exk\v{WXz+3X\17865b/g\"T1]\18419xK0J1\SIa^]/\FS\FS9\DC3iSD#\CAN4\DC1TfO\t\ENQ\DEL>:}8Y\SYN\1076277Tx#m\184824\ETX\ENQ%\1084684\aR\n.\DC2\1023654o\DC2\1068364\1000474mu\4959\ETXI\63137\rz3\NAK\61515\1076759\r\FSbD3gJ\1019714h\EOT\ETXC$S.ID\51547\EOT\SUBG b\ESC=W\RS83\68882!+\4797s\DC4&AsX=OSm\ENQ*[\121288,w\ACK\ACKoX|\992535\1108589}4 \1027760&\SYN")), updateServiceAssets = Nothing, updateServiceTags = Nothing}
testObject_UpdateService_provider_16 :: UpdateService
testObject_UpdateService_provider_16 = UpdateService {updateServiceName = Just (Name {fromName = "x5\RS\ESCTWkqU\140033\&0\US\DLEG@%\ESC\19022\",-7\1050900F\1040312\97216\1025696z\1058149\b\SOt\DEL\f\185994\983973^Q\DLEt\1096997h\SOH\37129\985924P\STX-\61575E\61463\1025357;\1000778\f\SYN\v\ETX\70100~/x\RS3$\47910{d\DC3\t\99972+#\1000170pi\SI\1089699R\DLE;"}), updateServiceSummary = Nothing, updateServiceDescr = Just (unsafeRange ("\"a*\1068204g\20013\GS<\1093060\bfN\1038507J\EM8;~)(\ESC\94284\1006416\1011261\17411x\1045829(9qa\DLE\ETX\66043?) qm\137829\1075169+\1069477\SOH\NUL\58179\3989\1009037G\n\41248n|;G\n\998220\SOHR\37700\163804|Y&\aVD\ENQ\1053626x\139539|R|9>\1044416Tc\1085177\139630{\EOT\SOHLs\29104\&6\NUL^2\74306\USnG(:\989222\ENQ#E\\E\ESC\NULYu:\32087(x\v#\38461\&5\1009579\USo\98187c?\170793Ai{h>#P\157344\160205Zh\39581\SO!\SOH[Q\1100885\97653\&40\"5\t\4787\US\19697h(\EOT\ACK\1022338v\STX\100367\1022009\fHk2\1030261=\\ \v.q\64079C\71332|@\SOb\SOH#\DLErOI;\1090894=8\DC4\988166\&6\99184x.p*U\"O\aV:\1017789\1073474T\FS\SI\1021234\&0I6m0\21069\1076393W\1076063=\GS\98756\162761\ACK\1082888}\186442`\1112018*\1094457\26441\110866\&7\142097\GS\188903f\137860\SYN&b\134735\DC2?\ETX\1056799\USB\DLE\1070735.D2!U5\97966u\1010166\&8\FS&=\EOTm!5m;J\43903\1029487nm,\\\186523W-\b\DC2J\1021202?Q\3121cTKC,\9196\DC3Q \1087910=\141786\ng\ACK995\DC4K\DEL<~lc\1090537\RS\1051308\38741ab\DLE\1057135\n\14918'\134558#")), updateServiceAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "\138024\EOT`" (Just AssetComplete)),(ImageAsset "\1108160Rk" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "\SODI" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [FinanceTag,ShoppingTag]))}
testObject_UpdateService_provider_17 :: UpdateService
testObject_UpdateService_provider_17 = UpdateService {updateServiceName = Just (Name {fromName = "!M}\1062771\NAKX\ETX\f_\1006976\b;\95958U\16936_SoT\1074942*7f\DC1.\30852D\1033954\54783n\RS\DC3;1\1078029YH"}), updateServiceSummary = Just (unsafeRange ("G{sq\v\DC4q\180248\t\1067130\128015k\995513\NUL^\RS\1022441CZ\1038742\131510O\ACK/g\ETB.G\917944\"l<K\ny,xZ\r\SYN-j\1112594FVy \SO\1015600\EM\DEL~\984243{\STXm\1009345W\GS7N\134697\&2\1026583ry\1099200#T8\190851`\DLE&\1059259\DC1N\172956e\SUBCfmWQh\1057106S\RS\GS1o=6s \ENQ")), updateServiceDescr = Nothing, updateServiceAssets = Just [(ImageAsset "" (Just AssetComplete))], updateServiceTags = Just (unsafeRange (fromList [EntertainmentTag]))}
testObject_UpdateService_provider_18 :: UpdateService
testObject_UpdateService_provider_18 = UpdateService {updateServiceName = Just (Name {fromName = "O\145041 \1049611\61149*5g\r^MU\1001242\DEL\DLET\DLE\95870<\69958\1054480Q\1041939p^\fCe"}), updateServiceSummary = Just (unsafeRange ("S\DC4\n\6645*\SYN\1045236r.\1021207\EOThaikV7d{\1034199RZ\47154(P\SI\1046278\&0,+1s\f\1072332\t\985714h\ESC<]u\48615l\159137\&9#\STX\SON \NUL\1045496P)\9553-C/\186427\120366`)s%\ETB3\SOH5\b:mVx \1035582|)\fyxH\STXs\32150")), updateServiceDescr = Nothing, updateServiceAssets = Just [(ImageAsset "\RS" (Just AssetPreview)),(ImageAsset "\CAN$a" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [TutorialTag]))}
testObject_UpdateService_provider_19 :: UpdateService
testObject_UpdateService_provider_19 = UpdateService {updateServiceName = Just (Name {fromName = "\1020662y\a\SO\DC2\184935&\183366Q\1090740\GS\1105571KB\1110017\DC2k_w\167062=\1099164z#o!<zZk\1066629q<W}`t)\1098641c\1090439Uf_E\929\1110534ub\ACKQ_\1104935F_5\1048556\991388\30483\ETB\FS\a\133974B\ns\t\1012120c\SYN\1036848yWr&"}), updateServiceSummary = Just (unsafeRange ("\1075386fCD~)\190350\SOH_\EM#(N\141352\NULa\96647\139985\37664")), updateServiceDescr = Just (unsafeRange ("\SYN2q\\\1045125\1103817\185868\SUB\EOTXb\f9>\DEL0\DC17\rp\v!\31347\154381\1067567\1078637\1083717\\\1037398\57360\1035770\30159?KU\146596\917924 j\DC4G\ETX+S\USR\\T\ENQ}\189358\1081662\135205\&0\1102326\r\NAKp\EOT\NAKC\1056169\163595VG\RSC\1027555\&8FRa\ESCM\41703fb\\\ETBO9*\174054\&1$\983140j\159410fD\1106144}\1034153\1009920\DC3GM:\1049228\USU4eGRt\f\11752}[\985029\DC1\58190H~+^\138010APIV`\178534%#D\DEL*>()k\54349&\ETB;\ETB1\EM&\37259K\1005612\983722\135366\&2l\100494\ESC\985131xT-\SOHl~L\168301,\SOH\DC3\1075115H*\985050v\1038202@\SUBH\EM5T8\1014412\33689\ETX-\134574\\\169557\bsg1\999230\&4Vx&\1019705\32920aO'\DC3g=2\DC1W\58014\DEL\\1[\b\fUa\1024833p\27091!d\46383~YX\1054681\SI\148198\1072347a\SUB\ACKc\46339\DC4L\92608\t\999393\1081697g-I\b\172861\&9\1030218\69622\1066914;\SOH\r>\52535\142361\39551\DEL\129651\1037707\1100047\&0\999021U\RS+\1016013h\ETXNpd\66654\SO,tb%\182901;\60719S\GS\SOH\189330fB\135889!\125059\1098337z\1018723\93054Td")), updateServiceAssets = Nothing, updateServiceTags = Nothing}
testObject_UpdateService_provider_20 :: UpdateService
testObject_UpdateService_provider_20 = UpdateService {updateServiceName = Just (Name {fromName = "4\GSf\179205l\STXE@\ENQmJFh\172821Vb\FS:\v\FS\f\SUB\"\SUB\111251\168566gxjB\1073987\a\GSL.\vcEq{\DC2\183845F\148018\a4n\ACK\EM}\999795B\1016453\&6\985555\b\ENQ\94461\ENQ>7\983543\1020036SF\ETB=v"}), updateServiceSummary = Just (unsafeRange ("$!\160720\GS=<._\996200P\ETB\ETX\62618u*\132497j\EM\995008\DEL#\ESCm+(t\7896\162056c\tZ?+ \1057494|b\62167\995559\1003889\DC48*\r\DELE\t!F\167137\1092727t\SYN\DLE\987716\1041885\94885L\ao-\1096852\1040210\DC4_\\u\US\137312\1080924H~(U1@FVXS\69766D5\ACK\ETB\1047176\1040496}|\a8\FSKD")), updateServiceDescr = Just (unsafeRange ("\1051188\tq\60648u\1066592[\1037018W\186215\996594\RSM=M\180269\NAKp*\182479lX.\49241C|\GS\27673mmh)\1032069jl\1020692\64540\NAK+wB\SUB*I{Xn,KTo\984941\1048003E\186\995376\t\44458\1004160Y\66802Aw\USy\64353Vr@\987452\141651\DC3L7\EOT\1069411\ad=B\47324\1051326=:\DLE\1018342\CAN\1016140\1042863\32276\51952c\132511\ETXe\1081647,\SUB\176372|\NAK\US\185761ws*dmV,u\NAK\43007\171637F\NAK5\DC2\bI\EOT\143579\1056918~\49217\1002811\&4^\133512\23148`$;\\\1031232.\FSGnZ'W@/\ETX\1088441\GSLh~@\US;W\ETB\52683Vs\999670\164107\GSV\FS\129323;Mp>Z45G?~\GS\6121O\1064964t\39908W\GS4kY\1068410\ESCSbme&\1034253\1108280b\120914|fBl\72307\t$\US\125115\NUL\120340ZgK\4608\SOHT\1098611MT1E\t\n\194972\161332\987459\DC3\43035\1071268\45957\44649t\154502\RS7x\1046891&\5443/r\1028254i'd\ACK\ACK\162398\EOT}\rv\NAK\GSmzo\NAKO\f{\173142a\157693\&8e(\1007286\1094637\f1u\33957e\181312\156429o\1018373\GS&\36189<\a\DC1eD\STXm\58277N\187446G\45884\SOL\a\1087910\1044741\vvZ\1082991\f6)\1017008\40380R\1027192\a\1026341\1097398-\1058504\aI\165158\1067350\1086474{ANY\20133c\1064242AZ\RS\1029011\917966\169193+[\983981K@\r:I\ENQ\CAN:\1097713\&8(M\"|)y\1055292*`\DC3&X\SUBBj\EOT\18584\CAN\990311\985377\STX%\1035875\57846\DC4G:\26873-\34746\1032239\n\1040142\20374d\171105N\SI\DC2a\991307Fd\NAKC.X\983198\r\172296.\985380\64941\376\SO\50675Y\DC2\34825\24523\CANp\NAKb5\US\DEL9b&\EM%%s[\SUB\17643\188983\1103973\DC3\1101266\DLE\1049221@=\FSE9$S\10619^j\1089004\DC3w',\SYN4,?\NAK\1075329VL\SO\US\1100432\DC3n~\b '\ENQ\1066926E^@\146078\1060006(l\"LK-\100782\"aB\118899a\GS\SYNP\STXy\1013892Us2\995464\1024080\127101\28923\&9\EMG\1016402\SOHS\986129)'\43861|\SOH\129147#r.`q\37012\CAN\SI\1035436\1104357\1046542mbyM+\185868\t\n$#\148935>")), updateServiceAssets = Just [(ImageAsset "3" (Just AssetPreview)),(ImageAsset " ?\a" (Just AssetComplete)),(ImageAsset "\NUL" (Just AssetPreview)),(ImageAsset "\39831\&85" (Nothing))], updateServiceTags = Just (unsafeRange (fromList [FinanceTag]))}
