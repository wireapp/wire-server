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
testObject_ProviderProfile_provider_1 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000300000007"))), providerName = Name {fromName = "y\128589\&85\9501q\996755hZ\37540\1011760rSF\DC2IwW\v\DC2v.\DC2"}, providerEmail = Email {emailLocal = "\6094", emailDomain = "\SO\RS+\1014094"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "4\v\\\1372\1046235"})
testObject_ProviderProfile_provider_2 :: ProviderProfile
testObject_ProviderProfile_provider_2 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000500000003"))), providerName = Name {fromName = "?\DC2&\f\1002392\&4\a\6592\1082969g\1090458\SYNN\1037371\1091901\&3\GSrC\120119\1069245\158361K'\999133\144555\rU-\995117\42175?{tG\ETB\1056436\994304;G8\tD\992186@\RS4M\\\1045282\991485\1012756\NAK\49319e?\DC1`"}, providerEmail = Email {emailLocal = "/\96977X\ETB\142001^", emailDomain = "'n{{t<"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\46284\nJ\1004279"})
testObject_ProviderProfile_provider_3 :: ProviderProfile
testObject_ProviderProfile_provider_3 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000800000003"))), providerName = Name {fromName = "[\GSV\141614\&9D\v\DC3\191136\&0\1102771\vNS\177967di\1068627\nVz\EOT X]6V*!\ENQ\1006014yZ^\EOT\1036974i2Z4m!\\Ds\1056325\1078673\CANw#\NUL\GS\814k\ETB\1065451\64573f\STX\12332\STX=E,Z\1094913 c\1019673\1008197"}, providerEmail = Email {emailLocal = "", emailDomain = "-\trvI "}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "'\173440"})
testObject_ProviderProfile_provider_4 :: ProviderProfile
testObject_ProviderProfile_provider_4 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000500000004"))), providerName = Name {fromName = "84\n\58716f\NUL`\1044181Q`\EM"}, providerEmail = Email {emailLocal = "/,", emailDomain = ",ph5\1081146"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "}-7Bu"})
testObject_ProviderProfile_provider_5 :: ProviderProfile
testObject_ProviderProfile_provider_5 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000700000000"))), providerName = Name {fromName = "\f\1093520\99988OM\167975\20267[\ETX\12951~\SYNl\35353\2673\&4B\DEL\1031749\174332w<DGsM\SYN\1106018\1092617s\CAN\ACK+#\SUB\CAN\67359\152986kQ\1078891\1072399\ETBj\29332W%8Hk\EOTX!=Z\1055533J\147265\51052\1009241\NAK:?\1073759P\SI\SIG\1109885UpOBsq\1089977>\1100897P\53192\33762F\1105867;\t\140326{\1105625qB^g"}, providerEmail = Email {emailLocal = "\ESCr\1031287!", emailDomain = "\f\"\DC1\NUL+\EOT"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
