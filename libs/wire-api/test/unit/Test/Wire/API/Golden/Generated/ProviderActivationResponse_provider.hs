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
testObject_ProviderActivationResponse_provider_1 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\1028212W(pR\156714\135505G+\CAN\128391\1085437\&3\1012064T\97042[*\CAN\ACK-a>R\t`d\DC2", emailDomain = "A\174406X\1099840O0&\16660\EOTb\1106563\993773\1064056OV\5876*:"}}
testObject_ProviderActivationResponse_provider_2 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_2 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "g\21227\nbM5,\989315", emailDomain = "v\ETBP\134605Z\149136Sh\RS\NAK\989861j7&u\\\a4|\a\vIw]?"}}
testObject_ProviderActivationResponse_provider_3 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_3 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "+c\175855\ACKy\NAK\1069870\990743p", emailDomain = "7^^G\tW\SUBI\SI\DC1\984659d\nr"}}
testObject_ProviderActivationResponse_provider_4 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_4 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = ">\NAK\DC1r[,M\1074185*h\37637c~\DC1\138416", emailDomain = "\r\16551\129426\992549DR+\n\1014542\tQk\RS"}}
testObject_ProviderActivationResponse_provider_5 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_5 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\ACK\49970OdQ", emailDomain = "9d\1041587^\ESC>\35024\v\1102205vf\fW\a<wK#\58742\1084172\DC4_"}}
testObject_ProviderActivationResponse_provider_6 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_6 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "*\CAN)\96628\CAND\ENQMoM'", emailDomain = "\FS\1062545\a"}}
testObject_ProviderActivationResponse_provider_7 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_7 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\39680\1026963\67843E\62221\98509\27431\189871\1035835\83226\1028970]\DELZ.\2085{xc\23255p\1026764\39229\n\1018293:\SIgu>", emailDomain = "s\36155g\EM\1095312\"\DC3\133514\169099O"}}
testObject_ProviderActivationResponse_provider_8 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_8 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "Wo>\177253,V\"\SUB", emailDomain = "h&f\FS6hny4\1104289G4\987885\t\176851\149323\RSN>\SI9Yfrb\163862"}}
testObject_ProviderActivationResponse_provider_9 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_9 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "F6\1098744\SOH\995187q\1002290ls\157633Nyp!\993673O\DC2u=J%P\FS?\1084963", emailDomain = "/l2W!\SIVL%x"}}
testObject_ProviderActivationResponse_provider_10 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_10 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "{L\158586w\NUL\1003253", emailDomain = "6)\9727M%C\163353Y\RS\DC21,2[K\141235\SI\\\\fUr\49046\1102738\&0\141548s]\1101966\169947"}}
testObject_ProviderActivationResponse_provider_11 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_11 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "__3Kk\158751e\100651\&9\127758", emailDomain = "hH\EM6q"}}
testObject_ProviderActivationResponse_provider_12 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_12 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "w", emailDomain = "\FS\ETBA|\NAKX2%\190261\44594W\b\6837"}}
testObject_ProviderActivationResponse_provider_13 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_13 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "i\159476l4\1008277\SYN\998600\46723P\51801;2T", emailDomain = "*\1082949m\DLEDH\NULN2"}}
testObject_ProviderActivationResponse_provider_14 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_14 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "G\1021212", emailDomain = "\1096857\1087656H>B\"\US\153\73943\RSoU\189795\68360\151258n"}}
testObject_ProviderActivationResponse_provider_15 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_15 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\SUB${\185520\&8y\1037030d\167123\991011\NUL\r*\145909S\1086716\1048857zsxf\1073748jw\23377\SOH\180720d", emailDomain = "\EMZ\1015125\186668Kj8k]A\USt\ETB"}}
testObject_ProviderActivationResponse_provider_16 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_16 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "", emailDomain = ""}}
testObject_ProviderActivationResponse_provider_17 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_17 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\ETB\1019233.\SOH_WD9[\"\FS\"j\SO\127256\1079423", emailDomain = "\997267\ESC\1037366|7\989466\rn5#Wb`79Y\1003690\ESC\tF\182611\138820\SOH^\NAK)"}}
testObject_ProviderActivationResponse_provider_18 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_18 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "x$\154765:\1046354\83289\1046031.%\1015635l\190480C\DC2u\SI", emailDomain = "\ESCan\69398a\1059060"}}
testObject_ProviderActivationResponse_provider_19 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_19 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "L!\NULA\1039900\1016846qw\97035\1078553\1045949\rewH\f", emailDomain = "lg\1037379\1104212\&59\NAKF#\189956\STX\t\1100359["}}
testObject_ProviderActivationResponse_provider_20 :: ProviderActivationResponse
testObject_ProviderActivationResponse_provider_20 = ProviderActivationResponse {activatedProviderIdentity = Email {emailLocal = "\DC4_]%", emailDomain = "\1028245:\10627\RS\994618X\1082266n\ETB\EM\51524zC\16395G\993834;gX"}}
