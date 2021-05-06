{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ProviderProfile_provider where

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
testObject_ProviderProfile_provider_1 :: ProviderProfile
testObject_ProviderProfile_provider_1 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000200000000"))), providerName = Name {fromName = "&\CANc\1105118:\119337\991530WT\37683\1064208d\ESC0A/9\1033109!\ETB\1043161uA\119134\1104720n\GSUV8Y\ETXJ\USu\988653\&2~m]\129577\\\27503~_K\US\987770\&1k\1028147\1107450\1071000\62537~g\989451\n:\985250\SOH\1090138\ACKG\1066329\t8\13502EV%?PzB\STXA\5196q\1065956,\t\133116\984419\37679\SOH\1056576\1095793eO\CAN\1098623\150722s]o\165758NK\SOZ\ACK\ETX#\1078133\FSxQ:/\n\182359\1037335\1080382\141038>\SOxFB"}, providerEmail = Email {emailLocal = "s\DC3\ACKg&", emailDomain = "c?1\bQ<"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_2 :: ProviderProfile
testObject_ProviderProfile_provider_2 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), providerName = Name {fromName = "\v#\US?W\1047699\13326\EMCR\141213>\ENQ\EOTBn\b\ETB\181524\30774\&9}=\1035560\"}\ACKGjl$\167578\SUB@^\134886\&5\\o\a2(9|yr\US\SO\NUL\149548_XE\ENQ\54780\985089`f4\CAN\r\1012282$M\137261Ha{b/\DC1\21719\GSU2Ah\vG2q{\11616\50776hL\153802i\149514h\12894lg?n\1102709o\SOHcj[8,\2712#\1112309\b\1107824\61420"}, providerEmail = Email {emailLocal = "J\157935\ESC\1021350\US", emailDomain = ".K;P-J"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\995948-h@"})
testObject_ProviderProfile_provider_3 :: ProviderProfile
testObject_ProviderProfile_provider_3 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000004"))), providerName = Name {fromName = "7\1112370B\b \12230ru\25837Hrt`\1017296CT8#\1015297WL%[\144206K\1034754l\a<d/%\a\1091297\96373\SUB`\28967\"\RS\1087356 \\%|![:\CAN\1050577p\1110545Tv\aO\n\167836\1050646\FS\RS\SOHa\\)\1105896\1025820\\U\35123)\SUB\EMO\30839<?m\181833\&8D/\a\12846\998980 p=*7h\992555\162491\1005060B{\100785\993585\ETB\40453\189923y|k_\GS"}, providerEmail = Email {emailLocal = "\DC3x\1083141\987669", emailDomain = "\a\""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\166816\ETB02P"})
testObject_ProviderProfile_provider_4 :: ProviderProfile
testObject_ProviderProfile_provider_4 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000008"))), providerName = Name {fromName = "=Pr8\30774q\141815_"}, providerEmail = Email {emailLocal = "Q\ACK\DC2", emailDomain = "\1020798}\SI"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\50294\93012N"})
testObject_ProviderProfile_provider_5 :: ProviderProfile
testObject_ProviderProfile_provider_5 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000700000001"))), providerName = Name {fromName = "f\97746WT/?1$9\147994\1097787\DC1f\57934}{Vi9\DLE\190226C\SUB\ETB\RS\1032457\n;Gs\STXVm\ESCLL^\CAN>\30122of\1067341\149495\DLEL?\19870\94932\1054614\&3a\STX[\SOH\SOH\1064031\158173Vr\t\ACK\ACKE4\SUBOg\a#\GS>!\61210*7\DLE\131669\1035422;qND\tg6\DC2\11811]\STX\173107\148703 ;\142060A\18996i\DC1\CAN"}, providerEmail = Email {emailLocal = "HhS9", emailDomain = "Q\DLE&"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
