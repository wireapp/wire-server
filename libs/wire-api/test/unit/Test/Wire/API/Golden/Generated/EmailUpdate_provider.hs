{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_provider where

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
testObject_EmailUpdate_provider_1 :: EmailUpdate
testObject_EmailUpdate_provider_1 = EmailUpdate {euEmail = Email {emailLocal = "w1\SOHJ\ETXD\ENQ\SYN\1011756", emailDomain = "\ESCqS3,\138056sJ#b;6\1069704I"}}
testObject_EmailUpdate_provider_2 :: EmailUpdate
testObject_EmailUpdate_provider_2 = EmailUpdate {euEmail = Email {emailLocal = "\EOT\23899\52491\1012105\f.M", emailDomain = "\45348=~0?v3j\168164]\SI\tUe>:\r\DC4\1059608[^\DC1J\36216O\96307\1104843V\64761"}}
testObject_EmailUpdate_provider_3 :: EmailUpdate
testObject_EmailUpdate_provider_3 = EmailUpdate {euEmail = Email {emailLocal = "\GS6>\ACK2+\DC1?4\5379\16882h\1024411\&9Q\a", emailDomain = "\1043232VSm\17581q\f\1019974f(/Ke7\74507\1084742\a;;"}}
testObject_EmailUpdate_provider_4 :: EmailUpdate
testObject_EmailUpdate_provider_4 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "\1100103y\1023733\10308M%\1081523"}}
testObject_EmailUpdate_provider_5 :: EmailUpdate
testObject_EmailUpdate_provider_5 = EmailUpdate {euEmail = Email {emailLocal = "\GS\a?#P\1066318T,k\DC3mW", emailDomain = "\179000\174208vm\SYN\37856\145569n\EMfc_\RS\r\1045238-k\RS`\"^\1050341\57455"}}
testObject_EmailUpdate_provider_6 :: EmailUpdate
testObject_EmailUpdate_provider_6 = EmailUpdate {euEmail = Email {emailLocal = "\SOH2*\SO\1032816\t [\26708\1075564\NULvhO\128743z\ETXmC!=", emailDomain = "vEA]K\987568Y\NAKpa\29309\ETB\GS\1048515\DC3E\997269\998724\187747pq\156057\1060328"}}
testObject_EmailUpdate_provider_7 :: EmailUpdate
testObject_EmailUpdate_provider_7 = EmailUpdate {euEmail = Email {emailLocal = "X\17635q(", emailDomain = "?~D\1052997\1062054#\r\66628il\1098141\&2\GS\1105658\999842!\1013313Qx$';C\DLEz\46888"}}
testObject_EmailUpdate_provider_8 :: EmailUpdate
testObject_EmailUpdate_provider_8 = EmailUpdate {euEmail = Email {emailLocal = "\1100055a\145558\71719{\180138\149687J;\92365Q\EOT{%\189276\67602C\987934\1068451\1043027 &", emailDomain = "}/\ETX\163114\1088751"}}
testObject_EmailUpdate_provider_9 :: EmailUpdate
testObject_EmailUpdate_provider_9 = EmailUpdate {euEmail = Email {emailLocal = "{\fzO=\ESC\CANl3f\t\40239(U\a8Y$\136290\27941'\r\SUBk\SYNt", emailDomain = "Y\STXt9"}}
testObject_EmailUpdate_provider_10 :: EmailUpdate
testObject_EmailUpdate_provider_10 = EmailUpdate {euEmail = Email {emailLocal = "\996875\&9\46080\EM\7448\bNY\FSP", emailDomain = "\t\95095\DC4%\72792\4894\bD\1058809C"}}
testObject_EmailUpdate_provider_11 :: EmailUpdate
testObject_EmailUpdate_provider_11 = EmailUpdate {euEmail = Email {emailLocal = "|-&\a}\139574o\188444\&1Pi-l%Nc\160174", emailDomain = "}\989157\1013141\SIgw!U{2\1112958f\1109963"}}
testObject_EmailUpdate_provider_12 :: EmailUpdate
testObject_EmailUpdate_provider_12 = EmailUpdate {euEmail = Email {emailLocal = "\\\FS3nG\48528}9\46318\49890\44621\1083133\&6-\1093840\ACKI\"\49110\161989", emailDomain = "\47262\&5\1111502\150129#/\182596+\18928\t5w\1009459\DLE\FS\27314\&0\998583<7\11255m3"}}
testObject_EmailUpdate_provider_13 :: EmailUpdate
testObject_EmailUpdate_provider_13 = EmailUpdate {euEmail = Email {emailLocal = "\180430\&72_>un \GShyK+{9\SUB^\SI\CANK\US", emailDomain = "\1103804\&80\1032728\\\1041770w\NUL0\SOH/[\EOT,\134108~\50892\EOT\24588\1067691\1109195\ENQ\167385_\ESC"}}
testObject_EmailUpdate_provider_14 :: EmailUpdate
testObject_EmailUpdate_provider_14 = EmailUpdate {euEmail = Email {emailLocal = "\1080071_\97154\166691\1065595g\a", emailDomain = "`\b7-6\46501C5q\SO\ESC\"\167555"}}
testObject_EmailUpdate_provider_15 :: EmailUpdate
testObject_EmailUpdate_provider_15 = EmailUpdate {euEmail = Email {emailLocal = "Q*i\1021769FB\ETXp", emailDomain = "\STX8m=y\1042355\37502\STXJ\1057688\EOT\140943\\V.)_\t\1076664/X7"}}
testObject_EmailUpdate_provider_16 :: EmailUpdate
testObject_EmailUpdate_provider_16 = EmailUpdate {euEmail = Email {emailLocal = "[\1091675Gf+\39774f\EM\986974\EOTf\az3\1085426\SI\ENQ\\M\1028668vBj", emailDomain = "\ETX\1083162nBrY;\144678\bd\141397w\1040613j/?\DC2^6\1110413"}}
testObject_EmailUpdate_provider_17 :: EmailUpdate
testObject_EmailUpdate_provider_17 = EmailUpdate {euEmail = Email {emailLocal = "H\148532\&4\16062\t+\45129\nnqP_\60394", emailDomain = "\1062621>e\1000248 _7'G\77971\&2\120721b\12095Q\DC4\EMgt="}}
testObject_EmailUpdate_provider_18 :: EmailUpdate
testObject_EmailUpdate_provider_18 = EmailUpdate {euEmail = Email {emailLocal = "\a:;\97094\US\DC4W#\167708e_/N\ACKg\SYN\ar}&\ETBo\7991\179011<!1lp", emailDomain = "\tT3zg\DC4(5v\58187O;.\1097432n\41246\174099h}#\FS~\167522[\SYN"}}
testObject_EmailUpdate_provider_19 :: EmailUpdate
testObject_EmailUpdate_provider_19 = EmailUpdate {euEmail = Email {emailLocal = ".\1082173<\1087004o1bs\CAN\160156\133867\142580\f\1088412_^A>:B\6948?q?\FS\SOF\1081778\EOT\34739", emailDomain = "\68435\DC2Z\1091170v\1063468$\1056973\1102747uq\STX\ETBIME"}}
testObject_EmailUpdate_provider_20 :: EmailUpdate
testObject_EmailUpdate_provider_20 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "'\138516\FS\DC4IA~N\r%\1026240\1018703O\STXXa]\1012625"}}
