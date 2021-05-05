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
testObject_UpdateProvider_1 :: UpdateProvider
testObject_UpdateProvider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "\r?a\1112856'<g\ETX@l\163269\178387\FS\1101674\141499\STX\1048099\131327\1041689\&5b\45977(`\ESCq%R\v\97530\1102257\173697\985470&\78850\STX\152777\1105869*B\14409$=\163931\1070274\US~qF\ACKOOvIL\1028132F+^\1066765\1080186Uo\145326P1\191260\&2T\72806M\DC2\"@M\1040889= X\16208\DEL(\ENQ@o\DEL\27348k9!\RS%DIL`2\DC2S\155079\52518\1109062%\65590\&9\1055846o{\27954\ETB\1006941\SI"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_2 :: UpdateProvider
testObject_UpdateProvider_2 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "l\DC3\NAK"}
testObject_UpdateProvider_3 :: UpdateProvider
testObject_UpdateProvider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "\175633u\996877MGxg\176997\SYN\35582\1082820 \61667G.\1009640"}), updateProviderUrl = Nothing, updateProviderDescr = Just ".\n"}
testObject_UpdateProvider_4 :: UpdateProvider
testObject_UpdateProvider_4 = UpdateProvider {updateProviderName = Just (Name {fromName = "$\174349\1112693\RSF\1067211.BoJ\ESCG\996485\143731(X{\182042\1041481\DC4\172550\EM\DC3.>`\1054718o"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_5 :: UpdateProvider
testObject_UpdateProvider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "yuY~{\r^\1047410>l\"A,y?i\GS+\53700\&7!\60175\f\ETX\SO19D\DC1\94701[\1067191Qd\NAK\29142\&9\171915D\rP2\155420\STX/\DC3/I\1105036\DC4u\59011\FSM\986007)\NUL\1016113\CANp\SOH\ETX\\\FS?\FS+\DC3\1088814\SIK/\127791&&\1023801\35724uw2;jSG3b8&\1057418\\'\NUL`$\1099022W\139334|\159174C6x\EOT^w\187302\ENQ$z<_O,, "}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "pX\US"}
