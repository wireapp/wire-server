{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ViewLegalHoldService_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_ViewLegalHoldService_team_1 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_1 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_2 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_2 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("lQ=="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_3 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_3 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000004"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("5w=="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_4 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_4 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_5 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_5 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_6 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_6 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_7 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_7 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_8 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_8 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_9 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_9 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_10 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_10 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_11 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_11 = ViewLegalHoldServiceDisabled
testObject_ViewLegalHoldService_team_12 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_12 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000001"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate (""))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_13 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_13 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000001"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("uHvV"))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_14 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_14 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000004"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("2rnH"))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_15 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_15 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000004"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate (""))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})
testObject_ViewLegalHoldService_team_16 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_16 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_17 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_17 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_18 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_18 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_19 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_19 = ViewLegalHoldServiceNotConfigured
testObject_ViewLegalHoldService_team_20 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_20 = ViewLegalHoldServiceNotConfigured
