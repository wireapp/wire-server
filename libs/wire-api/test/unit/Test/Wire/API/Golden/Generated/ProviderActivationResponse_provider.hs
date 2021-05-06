{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ProviderActivationResponse_provider where

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
testObject_ProviderActivationResponse_provider_1 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = ">\178601`,\989204X9Zb\1077142#_W", emailDomain = "j\7214Sc\SYN1~S\1067109"}}
testObject_ProviderActivationResponse_provider_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\131991Z\FSHN\1068389\a", emailDomain = "\1091893(v\v}[;\ETXB\"Bfi"}}
testObject_ProviderActivationResponse_provider_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\v\ETB&\USz\f\1047924\66656\bD\1069937\SI\154325&\EOT\992012\93045\SI\DLE\157471\STX\985484(Y\172024r|C", emailDomain = "\ak\993320\36737"}}
testObject_ProviderActivationResponse_provider_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1077651\GS\RS\1083001N", emailDomain = "\171771\SI\SIy8\a\SOH:\a"}}
testObject_ProviderActivationResponse_provider_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "rx5\US\DELYL\SOH6\\\1053366D\1105903\r=\EOT%\f", emailDomain = "x,\DC3=f^\ENQ"}}
testObject_ProviderActivationResponse_provider_6 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_6 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\"\146522iP\159450JP\78413\EOT\37451\42222\USAa\1096767\ESC;\141410\64569#\1037534=\v", emailDomain = "\45148g\1094752\28376\t \1052357\\\DLE\ACKX\1086472oX\133138UIn\"\992328"}}
testObject_ProviderActivationResponse_provider_7 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_7 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\143926i\1074755e\1075021G\ESC\1058264\&1[\DC2]\DC4\ACK|\16508\&9\SO(0(\n\21762", emailDomain = "9\b"}}
testObject_ProviderActivationResponse_provider_8 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_8 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "=>7U\EMIu\1036177L\51878?\1102353Zq", emailDomain = "*5\"\1093077\171918m\SIe\1100793V^PB\1069559\1028467b\1059808\RS\EOT<\988897"}}
testObject_ProviderActivationResponse_provider_9 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_9 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\20835j\ESCi\185724", emailDomain = "K^{\34923\144300Q\147756"}}
testObject_ProviderActivationResponse_provider_10 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_10 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "[59TH\61274>-\DC1Qf\1058352", emailDomain = "\1005256k,Q\29117d{I_\t\b"}}
testObject_ProviderActivationResponse_provider_11 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_11 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "0\1095955WA9", emailDomain = "8A\NUL\RSb\191364[\1095476)\1041609Q=\181515fHc\13173\1043216\7494\97621\v"}}
testObject_ProviderActivationResponse_provider_12 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_12 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "jK\1085258\16246\EMs\US7IQ\143368\ETX\FSm", emailDomain = "ub\1015918\1093261\ETBF\38494\ETX\n!B+um \US\DC2\vx7T\1075165$Ch\SUB\156306K"}}
testObject_ProviderActivationResponse_provider_13 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_13 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "2\1087521Mw\\\992773.SS\SUBW%\988911", emailDomain = ""}}
testObject_ProviderActivationResponse_provider_14 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_14 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\NAK\SUBwT", emailDomain = "i"}}
testObject_ProviderActivationResponse_provider_15 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_15 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\t4\190593*u\CAN\ESC\169318b2;\DLEj\n\1049275\48902", emailDomain = "/\150788r|\1035480zK\RS\149102v8ck\DC4\DLEY\DC1i\1088396mY\1038711\DC30"}}
testObject_ProviderActivationResponse_provider_16 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_16 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\DEL", emailDomain = "\1028601jf!^&J:S\tB\STX\13104K"}}
testObject_ProviderActivationResponse_provider_17 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_17 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1059197=", emailDomain = "T\1034206g\58760\163596i\DC1*&+e\r!Q#Q\FS\ETXa\4957N\EOT\v\ETXwo\29173\1076439"}}
testObject_ProviderActivationResponse_provider_18 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_18 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\DC4z\DC1svfA", emailDomain = "7\DC3.6\1070394\1018294\27177p0\1089629k_9\5095d\1037217B\1024362_\DLE"}}
testObject_ProviderActivationResponse_provider_19 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_19 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "`\DLEqg\1084841R\20321\SI5\tn\1083392\t\993366(\1097892v\140638\1027200\133978\&1{#,", emailDomain = "\v\b\988325n\46180\SYN\131752\1002385g\995305\ACK?\1102118\baLP\DC3\DC1#\b"}}
testObject_ProviderActivationResponse_provider_20 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_20 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "s-\ESC", emailDomain = "o\1045546\163081\SUB\97504\181418\24294\83227\148584y\142412|\1039846\r8\187385\43985\70810H<\EM)\ACK^\STXN;"}}
