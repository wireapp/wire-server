{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Provider_provider where

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
testObject_Provider_provider_1 :: Provider
testObject_Provider_provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000005"))), providerName = Name {fromName = "\1089382\1029151\1088448"}, providerEmail = Email {emailLocal = "YBq\DC2H5", emailDomain = "r\DC19"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "a\190033\SI\65359"}
testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), providerName = Name {fromName = "}C*0^]\STX\GSd&\SYNLu41WKf`P\128043@$\138922J kNu*"}, providerEmail = Email {emailLocal = "5\1029665;P%", emailDomain = "k8%\ETBg"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "U\1025188\NAK\ENQ%"}
testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000500000001"))), providerName = Name {fromName = "\32817\1098249-(\1074010\65365\1059335S\EM3(T\994090-\1089603\1083759#\SO\120806\US\66468\v\1073824\DC1t0d\49020\&5B)\1099031e\173588\EOT\119184\1025694\&5\36038\&9\SO<:!\vw\180564\STX\FS\1041205\t8\DLE/\1034079\&5\1041539\NULXy'rZ\23485\991291Vf%X\nY"}, providerEmail = Email {emailLocal = "", emailDomain = "\ACK'"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\984243D\92294\177163"}
testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000005"))), providerName = Name {fromName = "eDE\DC4g{<\42404\NUL\179040iko\1003823\&0+6_\SUB #`c\1047561u{\98832\NUL\183630\92580Xo=\1015598<\1109164\1043943\DC2i@\1092815]>\43950\120383"}, providerEmail = Email {emailLocal = "s", emailDomain = "N\1109583"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "jK"}
testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000600000000"))), providerName = Name {fromName = "\"\NUL\1067380\n\v3\DC1b\26051Ca0S\1056391B:q\139764)Gk\ESCRi\njI\14032\1028640\1107470\16321\36635`\"\1082194Q\NUL\ACK#g<LU\173575\US8yd\ENQ\DLE:\183339\SO\1074152!"}, providerEmail = Email {emailLocal = "\DELK+", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "_Rh"}
