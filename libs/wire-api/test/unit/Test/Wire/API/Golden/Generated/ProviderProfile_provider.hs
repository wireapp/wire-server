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
testObject_ProviderProfile_1 :: ProviderProfile
testObject_ProviderProfile_1 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000003"))), providerName = Name {fromName = "\145363j\143279.'\DELF\12818\184089\t\1094823\an\ak\1072576\DC4\185021JA\DC4\EOT=XYp\8879X=\1025379\SOH\176200*\ENQXo\1051880\US\NAKLl}m\48302\SYN\\UHeTZ\ETB]\STXr 8\1072122O\139568\186928d(\EOT\ACK\1028734)\1014192d^_\\!Q\1095817\1092739\153961P\63054\DC2\136870V?\ACK?F\DC2?\NAK\29863"}, providerEmail = Email {emailLocal = ">", emailDomain = "G\fQ"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "F"})
testObject_ProviderProfile_2 :: ProviderProfile
testObject_ProviderProfile_2 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000400000005"))), providerName = Name {fromName = ")|\fF\1089534\2285\SOHv7\9282N\SYN\7474\DC3\NUL\19039 8\1109770\58580bN\ESC\DC2l\1031727\SYN\52686\DC33v9"}, providerEmail = Email {emailLocal = "*)\168477}^G", emailDomain = "y#"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "f"})
testObject_ProviderProfile_3 :: ProviderProfile
testObject_ProviderProfile_3 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000800000008"))), providerName = Name {fromName = "aZmy\1068247jY\133167G]\1018123#u\US\CAN\NAK\r\70059\1039684\61845N\1010763\33002=Z\183843\&6\13552\1054792\992394\nR"}, providerEmail = Email {emailLocal = "\ESC,/\66246", emailDomain = "\18815kd\1105688"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "s[m"})
testObject_ProviderProfile_4 :: ProviderProfile
testObject_ProviderProfile_4 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000700000008"))), providerName = Name {fromName = "\22225\DLE\32114k\ETB;\1010864c\v\147560mSg\1017885\USM!\SUB\f\SOK\ENQz]!V\SYNI\1092350\ACKI\ETB>@R\DC1ja\132012\&15\65907\1061172\&4t\1109210\EOT.\NAKb\1103695\1025589pO#[\ETX\DC3h*\ESC"}, providerEmail = Email {emailLocal = "W_2>^\1035325", emailDomain = "\ESC>N\127066\1020989u"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_5 :: ProviderProfile
testObject_ProviderProfile_5 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000700000003"))), providerName = Name {fromName = "\t\n=\6201\&1a\nE\24656\1057281\r\1065532\992364\4503\997546euS\1072446x"}, providerEmail = Email {emailLocal = "j\EOT^", emailDomain = "`7q\133904v\EOT"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\175229eX{\EM"})
