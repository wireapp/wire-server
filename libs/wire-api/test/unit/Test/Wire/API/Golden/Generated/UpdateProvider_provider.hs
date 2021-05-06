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
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\20732#\1011428\&8\RS"}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Just (Name {fromName = "qf\NULI<b\53955A\1057144x\EOTe\1032258\&7\ACKTs%\"\1039353\ETB\45786\1069003Ea\168927 4\1054680\1109885:3\RSdGZ6u\DLE%\SO;\1039596m:\994966\&8`\58120)\1027457\EM\DC1\143989>]DD\EM\DC4\178381fU\176736\SUBK\NAK\1067897\US\SOY\CANY2\EM\SIoj\1097289\1019765\GS\EOTw-H\NAK\\\SI\SOH`T\EOTw\ETX\172167I"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\62856"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "^-\n\52348l\1113189\1112383F\33084I\1015573Or\1089211?$\983299\1102887\21585G<hJ}&"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\173380\RStJ\DEL"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Just (Name {fromName = "\4584\140050\NUL;}\DC4h\ENQ(\EOT\f\DEL%\DC1\984701/m8\150728\1003598#vNv4qY/\58766~\NUL`h-\ESCYf(~_\1057952\&1XB\SOHE\DC43Ogml.G4\EOT\1101487<\994060\190086"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\161602.\EM\136267\27404\&3\SYN"}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "#X\STX\b\1006413\NUL;[O\44552#\ETX\46004\v9\187814\NAKRm\1010810\1103214}\US\fM\STXBQH=\40110k'_\179990]bL\ACK\ENQS\ENQ\EM\188267\1106723\ETBw\1058939\n\170735\DC4\159505:`\1112568Z\1096140\DC3\23344\19925\1060118\36864\1103891\1055706G\15429"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
