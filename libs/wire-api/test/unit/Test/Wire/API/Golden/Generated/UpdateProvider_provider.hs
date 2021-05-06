{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where

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
testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "\98037`->\vv\EMz\12240ba\1008066\\9\1047285\1035501a\1011010\127818\SYN\26840~\SUB\48493"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\25651\40384S\1038934v*R~]"}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Just (Name {fromName = "+\DC40}5*SO_H?1Fc\CANU\28313\1084873Fk\48458\DC46"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1072455\1070888enR,'"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "\13195RYRy"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\NAKa\1094520\67657e\1027286U\fP"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "IE\1003886\CAN\74507$"}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "Bo\997876\&7\1084383ZfN.S\1003725\127870\RS\1040962\19990+8\1013229\1024267 \FS\DC1\DEL|$f2]\142566\ESC\SIg\128462M]\14986\DC38W)C\1047712\1016023kc\ETB\1051509\137692s:\DC2\1064230x@\ENQ(\46408\170580\53462\1105980\&0\1040885K\1038400J:Sl{mc&\40409\&5\\\CAN"}), updateProviderUrl = Nothing, updateProviderDescr = Just "6K~\34850"}
