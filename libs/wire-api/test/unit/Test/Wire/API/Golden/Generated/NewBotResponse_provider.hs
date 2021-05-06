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
testObject_NewBotResponse_provider_1 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "W"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\167767\1070532\44552")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 8}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\ACK\FSr\1078424")), rsNewBotName = Just (Name {fromName = "Z3J\153331l\78757S zb/\ESCO\SI=\123627\&1h\15587Q\1090052\1040787"}), rsNewBotColour = Just (ColourId {fromColourId = -6}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "\SYN\1056570" (Nothing)),(ImageAsset "6\1055349\ETX" (Nothing)),(ImageAsset "\139244<A" (Nothing))]}
testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\ENQ")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 3}), rsNewBotAssets = Just []}
testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("\169445~D\171367")), rsNewBotName = Just (Name {fromName = "\1111557[\\:.6C\DLE`\147954\995482\SOHQ`ckT(\31134IO2y\DELo\101086x`\29024~\"\1104631\62056\14603\19810Q\179303Of\51501w.\FS\SUB\170108\a\STXC=\191093\1096979\1065987\1111981|\1026600#S\182312\150466$\188047\&6\27685Znn\n\SOH:\1003735 Ax"}), rsNewBotColour = Just (ColourId {fromColourId = -8}), rsNewBotAssets = Just [(ImageAsset "8\ENQg" (Nothing)),(ImageAsset "?" (Nothing)),(ImageAsset "\SI" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "q>" (Just AssetComplete)),(ImageAsset "P\1013532" (Just AssetComplete))]}
testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "z"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "Z"}], rsNewBotLastPrekey = (lastPrekey ("\\$")), rsNewBotName = Just (Name {fromName = "o;\160594no6\DC2\tzt{F0K_\DC43~i\14916\1059217[=\ETXt\183283d\37383M;OM-um\993739:\DEL\999953\70838\ETB,k\154166\1084678\DLEKc(\aUR\43284\1092248\&43Pt(\UScD\EM\1053097i!7:\DC4\176392\SYN]"}), rsNewBotColour = Just (ColourId {fromColourId = 7}), rsNewBotAssets = Just [(ImageAsset "-" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "/\aO" (Just AssetPreview)),(ImageAsset "\1049283\DC4" (Just AssetComplete)),(ImageAsset "v.[" (Just AssetComplete))]}
testObject_NewBotResponse_provider_6 :: NewBotResponse
testObject_NewBotResponse_provider_6 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "|"}], rsNewBotLastPrekey = (lastPrekey ("FY")), rsNewBotName = Just (Name {fromName = "\992232N\2553w\65754\152130rx\157595\ENQw\988323\SOHX\181222!('Lz5z\1032502\1055429'\SI_\1020407\&5S8\ESCC{v\td\\&{\184803\ETXj\1111541r\1100007\1022869\169488\re'F$"}), rsNewBotColour = Just (ColourId {fromColourId = -6}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview))]}
testObject_NewBotResponse_provider_7 :: NewBotResponse
testObject_NewBotResponse_provider_7 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "\DELV3\ETB}A\v\NAKA\1081778\SOHb,R\1031888!\"\ETX\128047\9607\1058297\SYNyJS\1022341\ff\t\EOT\CANRn,_R\DC4\FS/\ENQr\1009101\US2\1087677}\rP6WM/9\1072381\53299\&9\ETBp<)\95267j\1003498v\EMk\SIzH\1110400\1096783[\132645~UB\DC1K4#$9\18274.zt?\fnZ\995973-\SIc\GS_DXa\EMf\NUL\1049925\98094{\ab0ODl\a\1095839\b)\1052449_kd\NAK\1068521"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_8 :: NewBotResponse
testObject_NewBotResponse_provider_8 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\6453"}], rsNewBotLastPrekey = (lastPrekey ("\1033242\NUL\CANx\150047")), rsNewBotName = Just (Name {fromName = "\ETX)V<mOd\184127wC\SOR\63400SZ\30420\DLE*\RS\16274\38154+=-\1101459X\149255}\1029634\DLE\GS{\1095107pI?A\a\1095665\&4XCo#"}), rsNewBotColour = Just (ColourId {fromColourId = -1}), rsNewBotAssets = Just [(ImageAsset "\128024," (Just AssetPreview)),(ImageAsset "\ESCG" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "\CAN\146151~" (Just AssetComplete)),(ImageAsset "6" (Just AssetPreview))]}
testObject_NewBotResponse_provider_9 :: NewBotResponse
testObject_NewBotResponse_provider_9 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("i\NUL,\1005033D\1048465")), rsNewBotName = Just (Name {fromName = "^\66637PaiZ\1047728nQ\172336\1087608+?Q\ESC\"[h\1068967zW\US\18486;EhJ\149951\136716\v\62901B\987003h"}), rsNewBotColour = Just (ColourId {fromColourId = -2}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview))]}
testObject_NewBotResponse_provider_10 :: NewBotResponse
testObject_NewBotResponse_provider_10 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\96456"}], rsNewBotLastPrekey = (lastPrekey ("\1068117\1071191\SO(0\1079441")), rsNewBotName = Just (Name {fromName = "X\ACK`c?A\1013303\72244\1076690&}A\SO'I~\SOH\1003057P\1111937m\74884\146984G\DC1q\991385@LkF\1043230\188779s=5@\SYN\SOH\DC2RC\120917\1011947a~1RrS\144085\ACKqht&\142281M6\992890\1046610\984100 s\DC3\f3\141183B\50921s'\1001135\1049501h\DC4\RS"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "" (Nothing)),(ImageAsset "%c<" (Just AssetComplete)),(ImageAsset "\19536|" (Nothing)),(ImageAsset "*\DLE\149858" (Just AssetPreview))]}
testObject_NewBotResponse_provider_11 :: NewBotResponse
testObject_NewBotResponse_provider_11 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\SYN"}], rsNewBotLastPrekey = (lastPrekey ("^")), rsNewBotName = Just (Name {fromName = "\1071115\&0E{0\DC4F\7238\&5\1062866ix5jW\SYN\72286\984388cy\6240x$plfA`e\1105141Cxn/\181354,4~\131498\143425\28052p.\DC3\1084nJ\191373\NUL\32208yK<\USB\990538\DLEb\14389*l%~\97966\24260\172663\&7[$\4763\62763\DLEn\2562\1083504p/WC\150591r$\43152\&8\1111547v\DEL\993587A\SIbNq\1106929u"}), rsNewBotColour = Just (ColourId {fromColourId = 4}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_12 :: NewBotResponse
testObject_NewBotResponse_provider_12 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\GS"}], rsNewBotLastPrekey = (lastPrekey ("\STX\DLE\187777\&8*")), rsNewBotName = Just (Name {fromName = "u\129377=\EM\23988\USR8k\65293$\ETB\5196\63685\&8\DEL$JicU1\16200\187918\1014866\1018067\157985\24867\&5Kq\1005414\DEL\1076403\100451D0NN\GSd\STX\174786\NAKr\ACKQ<L\1049943E9\137485\164381\USa1\10225:.T\ETX\1036592\24517\96478\134735\119821r;*\US\GSEFi\2704\993314\FS%\1076337;[\164686\CAN\SO0\ENQ,q\ETB\1299=Q)\1000598"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "\173246O" (Just AssetPreview)),(ImageAsset "]\1078357" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "\ETB" (Just AssetComplete))]}
testObject_NewBotResponse_provider_13 :: NewBotResponse
testObject_NewBotResponse_provider_13 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1049356"}], rsNewBotLastPrekey = (lastPrekey (" \ETX:")), rsNewBotName = Just (Name {fromName = "\1058911OtB`#1\bI]OP\181738\984880gh\vU\1007067#^n\53451^\162592q\a\1076253\171282\133577\1488\DLE\25687d\165312`r\NUL<_za>3\n\bb\1069460F\157008\173514Z-*V:F\US\1014276\ETB^\1102873Z\FS\94452\97722vZ}\RSE}3'9\1007158:\STXv\10641vx2}\1080598\EM\1476\50980\&53%\174587\1066766VJ\ESC\52015XE"}), rsNewBotColour = Just (ColourId {fromColourId = -7}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_14 :: NewBotResponse
testObject_NewBotResponse_provider_14 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("'G")), rsNewBotName = Nothing, rsNewBotColour = Just (ColourId {fromColourId = 2}), rsNewBotAssets = Just [(ImageAsset "" (Just AssetPreview)),(ImageAsset "\1095241" (Just AssetComplete)),(ImageAsset ":" (Just AssetComplete))]}
testObject_NewBotResponse_provider_15 :: NewBotResponse
testObject_NewBotResponse_provider_15 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("z\SOH\1066446S")), rsNewBotName = Just (Name {fromName = "\984814A\1054670|\998764\159931\&8l\RSG\STX\SYNIioMr>\153228\&8\62463\&7\r\FS\t5!&z--R\DLEY\SYN\1090973\DC2\1090475b\CAN]M\188761\1106726Z7-\185922$\ESC}!\SYN/S]\1061625\\\DC1\SIT\SOR\\\aC\1080351\&9\ACK#Q\1078089?r^\174873\997242\1070273\1059957,\CANW\993467\SUB=\168127H\1077607s\FSm&\EMs\DC1O1\DC3\995400W\30629\\\GSYB\ACK\a3lZ"}), rsNewBotColour = Just (ColourId {fromColourId = 8}), rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_16 :: NewBotResponse
testObject_NewBotResponse_provider_16 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\1051512\DC3\US\DC3")), rsNewBotName = Just (Name {fromName = "Q' \CAN,F\29440%ST\1043444J\128247\1064317`c1\1082409m2\SUB\v/C\ESC?\42380(&\96572I!lQ\a\NAK6nK\1099235\8226\&0N\SO_w"}), rsNewBotColour = Just (ColourId {fromColourId = 0}), rsNewBotAssets = Just [(ImageAsset "\135255\v" (Just AssetComplete)),(ImageAsset "\131300\FS" (Just AssetComplete)),(ImageAsset "\195062SB" (Just AssetPreview)),(ImageAsset "g>" (Just AssetComplete)),(ImageAsset "f" (Just AssetPreview))]}
testObject_NewBotResponse_provider_17 :: NewBotResponse
testObject_NewBotResponse_provider_17 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("v\184642G")), rsNewBotName = Just (Name {fromName = "\1055850>\1080152x%M7=\SI\70348\182743S\EMIh\1008990UCFj\167736\ENQGr2\ESC\18479h\1106261\1023491m>\1111882pg<\1018593\1043181\&4\1045177c\ENQWC\1098949\1019871\1010503\ESC\FS<\DC1,\EOT9e\ENQf\SI\DC3KJ7\t W\f\180851G3J\DC2\ETB\917782\US\1106926\f\STX\ACK7\1049358\&5rX\43616k\33819\1084734hY\1030715\1023628C"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "_\"" (Just AssetPreview))]}
testObject_NewBotResponse_provider_18 :: NewBotResponse
testObject_NewBotResponse_provider_18 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("\SOH")), rsNewBotName = Just (Name {fromName = "\":\DC2X\NAK\59100m\DC3v%s\NAK\DC1\96844\134428\aFk\DC14z#\t=\54357{.\42791z \SYN\b\19677\FS\141455kL6\1038734&*\170695}\ENQ(\NAKc\\\1070916$V\DC2b\62326\SUB`\1033049;\23281\119606\1052645:\1090095\18029EZ"}), rsNewBotColour = Nothing, rsNewBotAssets = Just [(ImageAsset "k\1100130." (Nothing))]}
testObject_NewBotResponse_provider_19 :: NewBotResponse
testObject_NewBotResponse_provider_19 = NewBotResponse {rsNewBotPrekeys = [], rsNewBotLastPrekey = (lastPrekey ("1\US\DC1G")), rsNewBotName = Just (Name {fromName = "\1081627\1049736]0\DC1~\SO-j\5198h\RSK#]\183017\EOT\1072823i'1\a\28183\994941%-\STX\CAN{\n\187462 \NUL1\119104)F+M\1014569\ETBG,IRt@\SOQ3\36870\ETXQ\n\155801\1038323u\CANl\983452\1056806\1076316\STXoN\ty\1040627Q\69987\DC1\n?)\46230[\32450A\bN\CAN\nr\1103446\1074338\1106315/\44371p}\EOT\1078725B\"G\11974\1079162\1004712d\DC3QB\FS\41522\983108?\998440Dr\DLE\172588e\1004625\ESC\1033956i\988341"}), rsNewBotColour = Nothing, rsNewBotAssets = Nothing}
testObject_NewBotResponse_provider_20 :: NewBotResponse
testObject_NewBotResponse_provider_20 = NewBotResponse {rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\ETX"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], rsNewBotLastPrekey = (lastPrekey ("")), rsNewBotName = Just (Name {fromName = "\27786vW`8QBJ\1077968\rm'$\1035598\173394\SO\ETB\183133\STXC\NUL\129335!\NAK\1069703\f\EOTx+\ESCT\RS\SUBc1=[\ACK\DC1\RS,K?\42517~f\1058740\&4A\57581\\\"\168443\991583Ik\EMK/HKf$dx:\1031648\72821\990883O\1065365\DEL<8&fV\63550\SI)wm\r\12689\DC1YRM\NAKTW?L}w\26450\185981\DC4\145792\1016816A\1072507|x\\\ACK\1017707\RS\985266 pP\RS\189077\1031985Gh\132767\64538\118916\1082517\b\152066\159239"}), rsNewBotColour = Just (ColourId {fromColourId = -5}), rsNewBotAssets = Nothing}
