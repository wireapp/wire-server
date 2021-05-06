{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserUpdate_user where

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
testObject_UserUpdate_user_1 :: UserUpdate
testObject_UserUpdate_user_1 = UserUpdate {uupName = Just (Name {fromName = "\DC2\1082643\1006711\39674\DELbW\60866`]O\318Q\US]\DC2\DC3\SYN\\\43175\1059234\EM\GS\DC3c\100027\1028420&\1027880y\CAN\DC49aaX\ESCQ\SOH;\13687g\"\65432f\58093AQ\1085218\7050~uS;P8v\1038121\69671n\FSz\49204\SOH\US,euXIb\NAK\DC1\1009581, \DLE\FS\170882\184122\1066342;s\1003983\134413\SO\SO\DC1\")\1110378o5\rq7|\61055RZ\1113976\"R8\68640\ENQ\SOH]jx\1090126'@\12109\1030747\DC1>E\FSX\ETBu#UG\ENQg\1093407"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\27975," (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "`" (Nothing)),(ImageAsset ")" (Just AssetComplete)),(ImageAsset "!n" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = -7})}
testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 = UserUpdate {uupName = Just (Name {fromName = "P\ETB'~\1053421&w#\DC2$+\1004289\\\n\r\1104637pw\43856KEy\146809>H\149640$.\NUL\172900\1056806u\SUB\DC4\SYN*\t,ky\4917/ed\166739i\GSB"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 2})}
testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 = UserUpdate {uupName = Just (Name {fromName = "DaFr\1022717\29871\SO|\1018043F\DC2\STX\RS\n\127111\STXq.R$\132281E1\178303N\2260\SOH]R\185306F\1053026\&6U0d\SI7N\t+\DC4U\32765J\178452\SYN"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "Px" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = 3})}
testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 = UserUpdate {uupName = Just (Name {fromName = "\1003990a6\GS\r\175132\DC4\58694\a-K\RSijv\DC4\183576n_];\3493|}iC\ETX+]\994920>\DC4\1050592K\1097388\ETB\1005978_\1078436a\1068335Y{\453\98291\985470N\987681<1\194570\\\24795R\54548\&2_\1113041\b\ENQ\ETXxHP%\b-G\1013829HbW\1012204\SYN'3N8s@:\157344\DEL\1110296\1228\1025669\EM"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "V" (Just AssetComplete)),(ImageAsset "\SYN\n7" (Just AssetComplete)),(ImageAsset "e\av" (Nothing)),(ImageAsset "i" (Just AssetComplete)),(ImageAsset "\1106267" (Nothing)),(ImageAsset "\159848" (Just AssetPreview))], uupAccentId = Nothing}
testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 = UserUpdate {uupName = Just (Name {fromName = "\FSH|0\1000405\172080?\1009679\1103442njU\b\1029608w\1058225\142698\&9v\SYN\n\1087162!\ESC\ESC\ESC{i\a\1000650)\136954#\DC2\DC3|\NUL\ETBtWq7\SO->C\t\166643\&9\8314\1069885\1003045MY\45334\f<\29131O\1023798Q+\vJ3yb\149939\&9\157605E\996421\NAK\1070207ZS\SIkE(\1045829 Bo)\1007416\SYN\169155Eyy\181380n\987111g\1020091]\159874/5X\SI9\FS\101104\DC3\\!ZU\993377A\DC2dgAA\DEL:q&x"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\DC3" (Just AssetComplete)),(ImageAsset "k" (Just AssetComplete)),(ImageAsset "\11599>" (Just AssetPreview)),(ImageAsset "9" (Nothing)),(ImageAsset "\1047283" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = 2})}
testObject_UserUpdate_user_6 :: UserUpdate
testObject_UserUpdate_user_6 = UserUpdate {uupName = Just (Name {fromName = "K\188045%d%B\a\16234=;u\1103629\&8\t&'\52230\"\a.\DC1 X\RS\70805A\1019659=I\SOHi\178721P\1093742 R\64890\70192\SOb\\$\1067023L',$\1026149\1077103\DEL1]\nMf\ESCP\r=\47441UG("}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "2\22413" (Just AssetComplete)),(ImageAsset "\1026509\58966" (Just AssetPreview))], uupAccentId = Nothing}
testObject_UserUpdate_user_7 :: UserUpdate
testObject_UserUpdate_user_7 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\NUL\77939\SI" (Nothing)),(ImageAsset "\994574" (Just AssetComplete)),(ImageAsset "H" (Just AssetPreview)),(ImageAsset "\1062390" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = 5})}
testObject_UserUpdate_user_8 :: UserUpdate
testObject_UserUpdate_user_8 = UserUpdate {uupName = Just (Name {fromName = "v\DC2(\DC3\997894\1038731\ETB\ETB$?z\rC\3905lIfXdP\DC3\1017532,L\ETX\150718\DC2)\USm\161016Q\ACKo2j:I:\SUB|W:\83035=j#3\996015\DC3s\DLE2H\128850\&1H~"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 6})}
testObject_UserUpdate_user_9 :: UserUpdate
testObject_UserUpdate_user_9 = UserUpdate {uupName = Just (Name {fromName = "L<\NUL_RF>\v\ETB|S8jp\150637*1#ly(\995781|v\35531\&5HTPv\1100423|\63169\DC1\1054937:hwU\NUL\ENQ\1066049\180517O\1042279\ENQReQgg\119253Q\176426:S\bZ#\NULF]{ozMEQ1.\142839\133855,\ESCA24\63354V{K\28097C\60549\145829\1056177\GSY:\1110314\184582\1037390\ACK\\Z\ENQ\97028\&7\134367\33496\ETXI\1015664B\60017G\rx\2122P\1101372"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\ETX" (Nothing)),(ImageAsset "\1072075W" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset ",)" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = -8})}
testObject_UserUpdate_user_10 :: UserUpdate
testObject_UserUpdate_user_10 = UserUpdate {uupName = Just (Name {fromName = "7\150648[s+l\31812\36616\132130$#\bY\148909\ESC\23062*\RSX+;\34637UZv\927\RSiJa\23994\1069066\a_\DC1=\DC4}\f}Y\1024819\DC4\ESC}\15548\a\60572$\n\1001700\1029475\1103088\&4iXl\157065q\983697u~9!"}), uupPict = Nothing, uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_11 :: UserUpdate
testObject_UserUpdate_user_11 = UserUpdate {uupName = Just (Name {fromName = "\ETBxW\45593FhP+\bp]\1071268\ETB\996472a\996361W.L\54809\182395\DLE\"t\DLE<+\1071187/\917549`\FS\5223\15716M\173214B\1054030\DC1@\1004055\26818\994276:[\78531[3q\STX\1072291O\EM\v}\b\986347|Q\1058794<\DEL?G^x\EM&hm\1050349\t\DC4R{\1021097\ENQN\CAN\t`\\#\SYNB\DLE\1043130@\1085995\54868?\SOH8|\167504ns_#\1076218\83453\re\RS`u\1084510}N\1006551b{\DC2>e:#lgx\45872\32215nC1.\1107194i"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = -8})}
testObject_UserUpdate_user_12 :: UserUpdate
testObject_UserUpdate_user_12 = UserUpdate {uupName = Just (Name {fromName = "\97936(Z\173937E#\NAK8\a\bF\984814^\93031,\990504\&34\ETXo\v)-C\1012119\30767}=L\"%\147514\DELlLH/#\DC2\NULT~z\SUB{g\178928?w'[`\ENQ%G\1007848\176414h\STX\1100793\DC3\ENQ\27663\&7>\DC2/\5579/\178557~\52097\1108706\180425!\156894#C\EM\1041957\94854\DC1\146251\n(\12792:\1046164n\1064473\1038065\NAK\STX1/Xb\52454]6T\1035335\&3\r\vGO\DC4k\ACK\1087919"}), uupPict = Nothing, uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 6})}
testObject_UserUpdate_user_13 :: UserUpdate
testObject_UserUpdate_user_13 = UserUpdate {uupName = Just (Name {fromName = "\n\991740\&6\1033095o\RS.9\1191\STXwK\10371\1006562\FS\1093845DeAP\DC2\DC2\SO\736\b*A\11440`\120597;\128426h%>{>;\GSXb\128080\DC4\183553\EOT"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "\DLE\1097265" (Nothing)),(ImageAsset "\995381n4" (Nothing)),(ImageAsset "q5" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "W" (Nothing)),(ImageAsset "\1014797\188755" (Just AssetPreview)),(ImageAsset "\6576Z\b" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_user_14 :: UserUpdate
testObject_UserUpdate_user_14 = UserUpdate {uupName = Just (Name {fromName = "[$7\1060857T=~4\27080%-5\1085618_\ru\b \v\FS--"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "\SOHH" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 2})}
testObject_UserUpdate_user_15 :: UserUpdate
testObject_UserUpdate_user_15 = UserUpdate {uupName = Just (Name {fromName = "\1031496r5+>\ACK\187900\27998%\SOH\1089045\ESC\1101118\ACK~u\CANv0\29163Sc|\34303\ETXV\18882P\DC3\t[\GSve\r\GS5j\97889\SYN2~5\985648v7>\f\ETX\1062774\983450\v\134229D6\1018392w=w\DLE\1082184UnNCu\1029120\\m\ENQ\rD\1040651\983128%P2KyxRg\1069756(j<\n\78450k\ACK5K'9@#\1000103\SYN\ACKC\166165\SIn&6Vmm\1088763\983207\NULU"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "z\1092877`" (Just AssetPreview)),(ImageAsset "\\\179743h" (Just AssetComplete)),(ImageAsset "\1010347" (Just AssetComplete)),(ImageAsset "r\1066052\ETX" (Just AssetComplete)),(ImageAsset "\120103" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 4})}
testObject_UserUpdate_user_16 :: UserUpdate
testObject_UserUpdate_user_16 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "tf" (Just AssetPreview)),(ImageAsset "F\9539\v" (Just AssetComplete)),(ImageAsset "\SOH" (Just AssetComplete)),(ImageAsset "S\1062426" (Just AssetComplete)),(ImageAsset "= \1088945" (Just AssetComplete))], uupAccentId = Nothing}
testObject_UserUpdate_user_17 :: UserUpdate
testObject_UserUpdate_user_17 = UserUpdate {uupName = Just (Name {fromName = "n%[U\155348\RS;y'~58\1105522<~[\61819\fk\ETXe$@'0Z\tx}\1072082a?Ta!b\991133\b\8005\ENQ\1079277\t\ACK\69608g^\SO+\DC3[>9shAu\"\1109147-\146598\&5v\SOH|eN5.\167895(UR8\1014327k\20019\&1\1024838\143931\29041\&7\1021593I\SIf9\1106955\100044\1082282u$0\51061GO\173487\DLE\94509\1019407\48273z\1102450Y%"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = 7})}
testObject_UserUpdate_user_18 :: UserUpdate
testObject_UserUpdate_user_18 = UserUpdate {uupName = Nothing, uupPict = Nothing, uupAssets = Just [(ImageAsset "q\";" (Nothing))], uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_19 :: UserUpdate
testObject_UserUpdate_user_19 = UserUpdate {uupName = Just (Name {fromName = "\ETX^iV\EM\GS\1110985\1061860W#sTF\174693jE\47540J\GS\CAN\190127;\"\157155\74561\98156h@\1063806\ACK\b\185987\94802\&5GR_\CAN\191291\1007771\151569\1102353I\DC2j0\nc\1082186}\EM<_\1521\&5&B\DLE\993372\54314\140945nVl\DC1\US#9('t\170179w\1070124>\DEL8\33922O\NAK\SOHyg,?a\1000464t#\983875\DC3~m'\SOHN\1060872\154371uo4\NAK\DLE\DEL\ESCOE\184221"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\55157\1100352" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "yP\1094283" (Just AssetComplete)),(ImageAsset "h\1012222]" (Just AssetPreview)),(ImageAsset ">\1111441\10962" (Just AssetPreview)),(ImageAsset "l" (Just AssetComplete))], uupAccentId = Just (ColourId {fromColourId = 1})}
testObject_UserUpdate_user_20 :: UserUpdate
testObject_UserUpdate_user_20 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 2})}
