{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewBotResponse_provider where

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
testObject_NewBotResponse_provider_1 :: NewBotResponse
testObject_NewBotResponse_provider_1 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "t:"}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "\1039158\169147\\2\178577P2\tGq*$\73811awy^\1014598\EOT5\ETB8E'Xbe_\EOTx\5582O\138237p\132273/yN\138555n\1006255D9y\FS\29748\NAK\SO]\186949\GSJ\1074122\RS\r^3\SOH\1094117\SI\1057026v\"mJ\1007380i\1059180\&2]]\1021593Us[/\1042776\184319\STXj\1008241\&7v,2d+\1110716mp\SO\US\SYNq+\STXv\RS%\1094677\1080932/l\NAK\US\1027321}\994363\ty0}2\1006510,\4927\CAN\113778"}), rsNewBotColour = Just (ColourId {fromColourId = 5}), rsNewBotAssets = Just [(ImageAsset "\1114053" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "e1:" (Just AssetComplete)),(ImageAsset "\120631e" (Just AssetPreview))]}
testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("`|\43492")), rsNewBotName = Just (Name {fromName = ")\180780u)fL\30694x\39316\&0C\68881\145036^\59738\1062909\SOH\21100sI\SIp9\1109079XB?\"\1050241)tL\ENQna\10574|8W\FS\995784\SOHQ\8541C\DEL\NAK\ACK\SYN0\1080806\155956]D\1083533zB\a\SUB\DC3#\178479~OPU8\12460t\rV0d\1109597\ETB\63245QQ/\DC3&=N\187090\CANl#?*<\SUB\ENQ?@*<7E"}), rsNewBotColour = Just (ColourId {fromColourId = -6}), rsNewBotAssets = Just []}
testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\RS\EMU")), rsNewBotName = Just (Name {fromName = "W\RS Y\97081I\988140:k\1093157\45350;Y\189583\5691\DC45\STX@J\1054258c\EM\DC3I$\153180ep-~k0\b\41600\35518\r3p\160645\CANN\\\EM#B\1016304\16051\a(\990827\DC4\1099091\30474\&5\DC2dH0\985936\b6^\ACK5Vo"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\129072" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "\DC1\1020045P" (Nothing)),(ImageAsset "oM" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))]}
testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = -3}), rsNewBotAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "p\RSk" (Just AssetComplete))]}
testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("4j")), rsNewBotName = Just (Name {fromName = "\169046|a\v\1094259K\ENQvh\SI\180129\ACK\SI.\1102333[\r\137417\131198y\38626\155176[X\RSM\36530\&7Q\b%\1063145\FS\997567 7\NULn0"}), rsNewBotColour = Just (ColourId {fromColourId = 0}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_6 :: NewBotResponse
testObject_NewBotResponse_provider_6 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "d"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\160673"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "M"}], rsNewBotLastPrekey = (lastPrekey ("\45559")), rsNewBotName = Nothing, rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_7 :: NewBotResponse
testObject_NewBotResponse_provider_7 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\1091471\GS\DC4_")), rsNewBotName = Just (Name {fromName = "H5~\SUBY\ENQ\SOHP\DC1kLf\DC3! \66836\RS\ACK,6k\149216\vE)\NUL\t;\169397\NUL\187753\NAKy[\SYNfwF\1093402,7)\1006362\\\78622pr\a\177648@\STX\3772\FSq\138337ym\DC4j\DC2\24627\1113359\SOHei\ACKbPT5\11992|I\142948\&2\\gi\134489QJ\DC1\f\ACK\125099\CAN\1047159 \1071930@t\"#Vq>\10547V\FS\100046"}), rsNewBotColour = Just (ColourId {fromColourId = 1}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_8 :: NewBotResponse
testObject_NewBotResponse_provider_8 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "E"}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = ";\SOH?\29967l,\f\1040028\&2\ENQ.\ACK\EM:\984706&\1051448@\74914\1012374\CANN\1097244\1042314<@\DC3_\DC3V`3Omc|m\19411\EM\t\1047715\148516e>\t\1077145\ETB\14840\1058226\157256@y"}), rsNewBotColour = Just (ColourId {fromColourId = -3}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "a" (Just AssetComplete)),(ImageAsset "l" (Nothing)),(ImageAsset "y@" (Nothing))]}
testObject_NewBotResponse_provider_9 :: NewBotResponse
testObject_NewBotResponse_provider_9 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("!e")), rsNewBotName = Just (Name {fromName = "\EM\64306\1008157\179906,2\994203\1033387^S(>\1098189mkFKY\12905\ENQ\\\n#lNbE]]\5551\v\t;\96517v\DC3u\SId|t_W\a\70113p\RS#\DC4\5798T:*\RSs\125133M\1020462{P\1039316\144438\ESC\v\1008185\ETX\1039313\177270Ac\ACK\1043090\989362}YC\1034279HP\\e\1028121Z\FS\121042\1046326^R<j\fX\998996/ \tu\n/\9364\1096684\174498\55072dP\1047845C\1046329D"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "uIj" (Nothing)),(ImageAsset "v" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "6\ETB1" (Just AssetComplete)),(ImageAsset "\1110044" (Nothing)),(ImageAsset "\EOTo" (Just AssetComplete))]}
testObject_NewBotResponse_provider_10 :: NewBotResponse
testObject_NewBotResponse_provider_10 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("`}~Y\25019 ")), rsNewBotName = Just (Name {fromName = "\DC3=\CAN\DC2La?\SI\ETB\ft1L2h7y\8518$!\SYN{!\DC1#\NUL7\GS\1026839\t"}), rsNewBotColour = Just (ColourId {fromColourId = 0}), rsNewBotAssets = Just []}
testObject_NewBotResponse_provider_11 :: NewBotResponse
testObject_NewBotResponse_provider_11 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1099121"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\1029299C1M")), rsNewBotName = Just (Name {fromName = "#\GS\29434o>{\DC3\1095544\ESCzB[\"*\ETBN\EOTS!\181425r\6705\a\DLE*v\GS\EOTo&b\165101k\146396S\ESCO\9870+g#=\DC2\999221\17078,\63425\1087475k\\\1037988\&5\1102272@\141679\1103727\119259G\DC4+-=\f,@M7\\\166374r\NAK\EOT\93803N 3\9502:\EOT\b#j\150512U\DC1\DC3\USYVA\141903\&3_\1030957\SI\SI5\r\78220f\1065833\bl\n3%nql-\1029020p#_9>3U\152650\a\DC1\v\1017246Dl"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\1095675" (Just AssetComplete)),(ImageAsset "{\\5" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset ">U" (Just AssetPreview)),(ImageAsset "\153841" (Just AssetComplete)),(ImageAsset "\r" (Nothing))]}
testObject_NewBotResponse_provider_12 :: NewBotResponse
testObject_NewBotResponse_provider_12 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("Bw\1066216#\1097786?")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 1}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_13 :: NewBotResponse
testObject_NewBotResponse_provider_13 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1056781\\d"}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "5n\155130d\EOTVk\vC\1063632\ACK/1\SYNM^\nA\1113811\176617\DC1Mf\\\1104989S5G\991401su\EOT\ETByW\EOT\1039930\1082850J\151314,'P\10140\&9\917628w\NAK\DLE\34967!\142447!;O'(\64594t\1040337kb\SUBZ\1038255\GS'_|Y\29533e\132420:\FSV~\ETB\r\EM\\,\1333x\ac-`zP)\USLvg.Y\NUL\a\1113527"}), rsNewBotColour = Just (ColourId {fromColourId = 3}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_14 :: NewBotResponse
testObject_NewBotResponse_provider_14 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "h"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\51450"}], rsNewBotLastPrekey = (lastPrekey ("\984824\988817\1020902")), rsNewBotName = Just (Name {fromName = "\b.Rg"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\1023939.\152497" (Just AssetPreview)),(ImageAsset "KCU" (Just AssetPreview))]}
testObject_NewBotResponse_provider_15 :: NewBotResponse
testObject_NewBotResponse_provider_15 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("$")), rsNewBotName = Just (Name {fromName = "<,e\1081406\DC2\SO\DC3\\{=\SO\71184l\1054511f(g\110680]\42368\137593,Kl\166009\SUB\ACKb8\1042865\DLE\NAK"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_16 :: NewBotResponse
testObject_NewBotResponse_provider_16 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "^."}], rsNewBotLastPrekey = (lastPrekey ("\1082165\1070276Yv")), rsNewBotName = Just (Name {fromName = "0\SO\1087013\1094455\EM\DEL'j0\DC4\1037007\53152|B\SO\165050M\163829B.\1075243,"}), rsNewBotColour = Just (ColourId {fromColourId = -5}), rsNewBotAssets = Just [(ImageAsset "\175650)\134901" (Nothing)),(ImageAsset "Z\9223\1003011" (Just AssetComplete)),(ImageAsset "\58538R\EM" (Nothing))]}
testObject_NewBotResponse_provider_17 :: NewBotResponse
testObject_NewBotResponse_provider_17 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("@i\147902/\DC2")), rsNewBotName = Just (Name {fromName = "j\DC4\STX:u\STX\n\RSD\1107083g\SUB0"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "'" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "~" (Just AssetPreview)),(ImageAsset "R@" (Nothing)),(ImageAsset "" (Just AssetComplete))]}
testObject_NewBotResponse_provider_18 :: NewBotResponse
testObject_NewBotResponse_provider_18 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("2\9656SR-B")), rsNewBotName = Just (Name {fromName = "r\27336=[K\1036036|\FS\1026918UBVTkb\1091100\1025666\57346P7y\DC1\ENQ\24252s\62627\191259(\36971R\16487E\SI\1053391S\EM\STXy.J\1057780\&5\9401Q6N\1044070!\SOHB=\ETX\1056479f\163450\SO(\1090145\DC1 !dx\1110269j\160023\ETX\USj\v\n\173951bwZQk\ETX:k\SOHz\ENQ\CANC"}), rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "\1061544\1068101" (Just AssetPreview)),(ImageAsset "\ENQ\1070755" (Just AssetComplete))]}
testObject_NewBotResponse_provider_19 :: NewBotResponse
testObject_NewBotResponse_provider_19 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ";}\1092101"}], rsNewBotLastPrekey = (lastPrekey ("\181589\ESCZmP")), rsNewBotName = Just (Name {fromName = "\1093749|\37889\988189\&1Bdh\DLE\US``z)\150323l\CAN\1087397\78249O\1010380\fm_\b;r\18183t\1000114a4\DLE\175322W\1052578\&4\rV%^j\1085773E\62219_7<R]CW\1074445,\aK*n\1011415\32965\1044671\1060787\12349\120138\1113869\t\DC3A\1102883\39368b:\ETX(\63353X\144642\EM'\1099921k\69787/\DC3s\5192>\DEL\ACK\182459\41183\aS$R?q\DC3.n7O"}), rsNewBotColour = Just (ColourId {fromColourId = -1}), rsNewBotAssets = Just [(ImageAsset "\984542" (Just AssetPreview))]}
testObject_NewBotResponse_provider_20 :: NewBotResponse
testObject_NewBotResponse_provider_20 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("W\50378\&9\DC2:")), rsNewBotName = Just (Name {fromName = "-Ef\SIsPNgA\ESC5;|\94954b\1072785\1082818\FS\141977]`\USZX\US\ESCs\48902\DC1H\41918%^n\1039547\&4O|\\\1707\95753IRQ1~\CAN+\15609\DELw\1108525\US@}v\bv7\SYN\GS\US\1111340\1046478\DC3Fq\ACK\1010825\1045153\1032369`dgq(bs\1071100\\%R\ETB"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
