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
testObject_NewBotResponse_provider_1 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\20740")), rsNewBotName = Just (Name {fromName = "?A$\1026787\1074677D\1109804\1062600@\DC1^\v'1\rz5$\100631Js=i\rhH\ENQRo\1046494$T\CAN){Kh&58?\1047970)H\183772#\1095378W"}), rsNewBotColour = Just (ColourId {fromColourId = 7}), rsNewBotAssets = Just [(ImageAsset "6" (Just AssetComplete)),(ImageAsset "" (Nothing))]}
testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "k"}], rsNewBotLastPrekey = (lastPrekey ("i\136034\143854U")), rsNewBotName = Just (Name {fromName = "\bM\1027442\US\1005136\&2Flqz\1015697\1410}\vu\1025669t>%C\137267\1071860^L\f\"*\DC2\\\1072107\59706\ACK,R\t!tp\1097806\1053755N"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("7")), rsNewBotName = Just (Name {fromName = "\135021l\1032979\27401\r\157673\DC3{r\1010221l]`\50368M;\50474\159775=s\1049063n\994281p\179259\ACK\DC1;O\CANqX\1022908:>k7\v\18702n\EOT~q\SIa<Kwa;H"}), rsNewBotColour = Just (ColourId {fromColourId = 6}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "u\SUB>"}), rsNewBotColour = Just (ColourId {fromColourId = -1}), rsNewBotAssets = Just [(ImageAsset "s\v" (Nothing)),(ImageAsset "/I" (Just AssetComplete)),(ImageAsset "?" (Nothing)),(ImageAsset "\ACKY" (Nothing)),(ImageAsset "\ESC}" (Just AssetComplete)),(ImageAsset "82" (Just AssetPreview))]}
testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\33300"}], rsNewBotLastPrekey = (lastPrekey ("3\24675")), rsNewBotName = Just (Name {fromName = "\CAN\1011928\160538w&9\DC4U%\GS\190561\997347~^\n\71203\SUBq\150361^l\ESCt\DC4Z&y@5ZM\1107670hn\1004400z\999290\3860\11815\NUL\GS\EM\39925gS7=\1049819`q\1073530\SO\SO\DELw!hK3\986841\62069x\1061114\159798( \1089996\133554 LJ\986246\DC1\120887,Hi\72304a\61505qM\1035602\SO-|TO\f\SUB\DC2;\993938\DEL$P\190188t/\171154\1111050M\STX\96648 DW2\1100356t?\SOHhM"}), rsNewBotColour = Nothing, rsNewBotAssets = Just []}
testObject_NewBotResponse_provider_6 :: NewBotResponse
testObject_NewBotResponse_provider_6 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\1049214%nv")), rsNewBotName = Just (Name {fromName = "6GdA\97489K\19114\186721\184401\1045156(\ESCoj\DC29T\189367\191153\"\CAN\v)S\42139\1104758\STX\DC4\NULM\1058610hohM\97146\1104936DS4`^p\992246c*A\1106803O1\CAN\US4\SUB\ACK \1102583\US`AA"}), rsNewBotColour = Just (ColourId {fromColourId = -3}), rsNewBotAssets = Just [(ImageAsset "X" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "\\\176019" (Just AssetPreview)),(ImageAsset "\1100703" (Just AssetPreview)),(ImageAsset "j" (Just AssetComplete))]}
testObject_NewBotResponse_provider_7 :: NewBotResponse
testObject_NewBotResponse_provider_7 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "3"}], rsNewBotLastPrekey = (lastPrekey ("\1076256")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 4}), rsNewBotAssets = Just [(ImageAsset "exg" (Nothing)),(ImageAsset "\161630\ETX" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "n" (Just AssetComplete)),(ImageAsset "\1025039?W" (Just AssetComplete))]}
testObject_NewBotResponse_provider_8 :: NewBotResponse
testObject_NewBotResponse_provider_8 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "1"}], rsNewBotLastPrekey = (lastPrekey ("\1063829\&5z")), rsNewBotName = Just (Name {fromName = "F+\DC33n,\99662D)\171854\1003330YQh?\1076518\ENQ\ENQI0m\137619Bo\132111U\1020680s\990251\139100\&4\1056780\&9^\703\fx\DLE\t\n$E\DC2\96006\59778X\100586\r\154228lW\1070720\71264Y\SIQ\1110336g@\DC4\16296A_\1049718\141640\STX{\997260YW\1112724W#N\41414_NX#\NAKU\RS]K\145868\1396\1014827Q1\1005939\&0"}), rsNewBotColour = Just (ColourId {fromColourId = 4}), rsNewBotAssets = Just [(ImageAsset "\DC3" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))]}
testObject_NewBotResponse_provider_9 :: NewBotResponse
testObject_NewBotResponse_provider_9 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("i\"\EOT<-\1065969")), rsNewBotName = Just (Name {fromName = "\EOT]m/z\1062790\40792su\DC2bkT|@|NO,\1043346h-[fN`^%L\1002097l\NAK"}), rsNewBotColour = Just (ColourId {fromColourId = -2}), rsNewBotAssets = Just [(ImageAsset "F" (Just AssetComplete)),(ImageAsset "\987188" (Nothing)),(ImageAsset "\1108039" (Just AssetComplete)),(ImageAsset "q.\ACK" (Just AssetComplete)),(ImageAsset "" (Nothing))]}
testObject_NewBotResponse_provider_10 :: NewBotResponse
testObject_NewBotResponse_provider_10 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "\CAN\ACKb?b\SOHK\189014\95355D\16482\1111915<(#a\164962S{C\b\1107623-\SUB\179223\151635&[)j\ESC\DC3Lt\60229}E"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\SO" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))]}
testObject_NewBotResponse_provider_11 :: NewBotResponse
testObject_NewBotResponse_provider_11 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("Y")), rsNewBotName = Just (Name {fromName = "\EM\46258\27300`-\70156u\189220G%0\166497s\48222\45188trY\DLE*Fm\ETX4\47012s\1113741Uh\4336\&6w\1071718\1070573\165926\DC2<\16024\1112241\SUBd\STXp?\1051617@\US\NAK\FS;\137464G-J0\179095\44775\&3R\SI=\1082410Zw\\\121466\\5\15306\r\1062242\1058159\&6z\1052370j\987294vQj;^0\STX$/\GS-\a\999322x\1103399(H\ah\1023537\STX"}), rsNewBotColour = Just (ColourId {fromColourId = 8}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_12 :: NewBotResponse
testObject_NewBotResponse_provider_12 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\1109436\rw\RS")), rsNewBotName = Just (Name {fromName = "x\NUL\NUL@|\ETXN\991346\\oa\13284\DC3\a\182503\&7tn\tm\NAKw2hzT=zkdfP\ETX\1064932\SYN-a\1054257\ETBs\1003749%\ACKE\120711\995272F<\37433P\DELL7\vk9\DC3\54306P\1021310\DC37q\SI*+P\991892\&3\175012>\78593\984150_\\\141303\984490\9449\CANgp\1097239\155104\&8\ACK\1109035%l\ENQ5{\992347- p#`Q\149178\98190\96148\&0\CAN\STX6/\1081384\1019065H\1242\984343\f"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "\2337\10279" (Just AssetComplete))]}
testObject_NewBotResponse_provider_13 :: NewBotResponse
testObject_NewBotResponse_provider_13 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("f\SI\CANi\ETX")), rsNewBotName = Just (Name {fromName = "\19395\987450\1098052)\DELq?-\1030269\r`tM\49258=m`Z\52019q9\44102~d\SIy\DC2Ko@\"\FS\NAKt_\DC3l\t\SUB-\1004117N^\DEL>iS\DLE\1052568W\DC4D*t7\DC2\SO\180949RFF\1001697\&5/%\92468k\DC1*]!\DC3\DC1\1080005P\74889D!"}), rsNewBotColour = Just (ColourId {fromColourId = 2}), rsNewBotAssets = Just [(ImageAsset "W" (Just AssetComplete)),(ImageAsset "\993181$" (Just AssetPreview))]}
testObject_NewBotResponse_provider_14 :: NewBotResponse
testObject_NewBotResponse_provider_14 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1103881"}], rsNewBotLastPrekey = (lastPrekey ("Z\34820\EM\1110702")), rsNewBotName = Just (Name {fromName = "Xb!<\1083710G5c\b\1099319)\f\f\ETX\bM~`\1077269\1074202\1041032X\1022533J\50993\95977\DEL6\DLE\SUBP J\142451j+\53363\GS\172982\ESCh\\T\DLEOJ2]8\ACKi\99641\1113878b\189157Ss"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\151745\1103259\14316" (Just AssetPreview)),(ImageAsset "\158566" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))]}
testObject_NewBotResponse_provider_15 :: NewBotResponse
testObject_NewBotResponse_provider_15 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1063528"}], rsNewBotLastPrekey = (lastPrekey ("\1104591")), rsNewBotName = Just (Name {fromName = "\18365\187161\1086619aQ\1043135\EM\SYNY\SIdT5l`Z[\1046530\156417_\DC2\3542hD"}), rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "\30677&\ETX" (Just AssetPreview)),(ImageAsset "v" (Nothing)),(ImageAsset "\985537e9" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "Ii" (Just AssetPreview))]}
testObject_NewBotResponse_provider_16 :: NewBotResponse
testObject_NewBotResponse_provider_16 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\1026525\16999e\996084\b")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 4}), rsNewBotAssets = Just [(ImageAsset "\75018D" (Just AssetComplete)),(ImageAsset "\995832\1070516" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))]}
testObject_NewBotResponse_provider_17 :: NewBotResponse
testObject_NewBotResponse_provider_17 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "bT\184566"}], rsNewBotLastPrekey = (lastPrekey ("m]j\1094856(")), rsNewBotName = Just (Name {fromName = "\USEVk\152349\162991\119838\990291N:\1020577,\v^{\1007067:P\DLE\153597H\DC2z\SI\1083854'6\STXY\b\EOT\43660\1060220\DEL\1008037\NULGfA8\1032743B\1105476:D\1025635\\8\STX\GSde\GS\ru;p\DC477(UV\171827:\1047551U\989637_$SE\SUB\158390\DLE\DC3\CAN\SOH\STX\1054496>_\1038390\917788\n\RS\1020689\&6\11429\bQu\65232\98358\150233\DC1\GS"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_18 :: NewBotResponse
testObject_NewBotResponse_provider_18 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("+")), rsNewBotName = Just (Name {fromName = "\SOHc|\147572\ACK\48457lm\EOTx3f\160113j\1108303q\EM]\1093711wf&\1021931\&3\US:Q3+.\b\1106646\1033562\rMxw\41460@'GR\1066426w\10962&B\f(\32804+\1101536tM\27121C\167026\181330\ETX\164753\147667I%\151606\t\ETB\135530t?\147684\1081129y\\,$"}), rsNewBotColour = Just (ColourId {fromColourId = 2}), rsNewBotAssets = Just [(ImageAsset "o" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "i\160924" (Just AssetComplete))]}
testObject_NewBotResponse_provider_19 :: NewBotResponse
testObject_NewBotResponse_provider_19 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("i\173732(")), rsNewBotName = Just (Name {fromName = "6\45174\54295\1003884eI\SO\ETX\31169\1007372"}), rsNewBotColour = Just (ColourId {fromColourId = 8}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "L" (Just AssetComplete)),(ImageAsset "v" (Just AssetComplete))]}
testObject_NewBotResponse_provider_20 :: NewBotResponse
testObject_NewBotResponse_provider_20 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("8\1016863\128802")), rsNewBotName = Just (Name {fromName = "2\1007581\1396\1095189 \1095169C\111228s`*ll4i\994298\5669'5"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
