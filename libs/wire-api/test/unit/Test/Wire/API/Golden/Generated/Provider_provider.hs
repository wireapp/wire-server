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
testObject_Provider_1 :: Provider
testObject_Provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000100000005"))), providerName = Name {fromName = "\EOTAN\137177*9uC\1084697\127058u \46318]Jf],8\DC4k"}, providerEmail = Email {emailLocal = "\1011743O", emailDomain = "Lh"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "gl\f|"}
testObject_Provider_2 :: Provider
testObject_Provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000500000003"))), providerName = Name {fromName = "\DC3NK7s\SO\DC3\1018603,?\1099653\ETB\67393\146669T7\ETB\10942\n\37381\1038284t=T\n\120058q \1039758CY\69406\a\170186"}, providerEmail = Email {emailLocal = "vX\SYN]\155278\995434", emailDomain = "\fK\DLE"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_3 :: Provider
testObject_Provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002"))), providerName = Name {fromName = "\ACK\EM\SUB\DC1k{"}, providerEmail = Email {emailLocal = "3(", emailDomain = "\2286\1030859\DLEHc"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\SUB"}
testObject_Provider_4 :: Provider
testObject_Provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000700000000"))), providerName = Name {fromName = "tPliJ+\156773t\1001466r2QQW\1074643\190127rbj\1020305F\EOT\1079170hFb\\/4;5{+))\a\FS=Y\128921Ex\ESCQA\RSIZ\f\135234\1096283Y\DC4\992410\1074184\EOT\1005913\74769\9466hJz"}, providerEmail = Email {emailLocal = "4G\GS\1085717", emailDomain = "\162111\RS\b"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_5 :: Provider
testObject_Provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000700000005"))), providerName = Name {fromName = "\153239Vh\1003794E\63435^P\v\1048439]uD\64480I\142388}Y2\ESC?\DC4]r&%\1095746vi\NUL3(I\EMx|\ESCN[.b'\1092819u?\168521\1010587\1021697AA\nw\SOJ)!1\36419\US\DC3\SIQ(wJ\170353\150273m[\ETX\NULs~\SYN\DC2\61061\78467"}, providerEmail = Email {emailLocal = "\ESC", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "p\ENQ&"}
