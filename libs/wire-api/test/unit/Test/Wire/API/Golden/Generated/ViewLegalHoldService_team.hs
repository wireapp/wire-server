{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.ViewLegalHoldService_team where

import Data.Coerce (coerce)
import Data.Id (Id (Id))
import Data.Misc
  ( Fingerprint (Fingerprint, fingerprintBytes),
    HttpsUrl (HttpsUrl),
  )
import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Maybe (Just, Nothing),
    fromJust,
    fromRight,
    undefined,
  )
import URI.ByteString
  ( Authority
      ( Authority,
        authorityHost,
        authorityPort,
        authorityUserInfo
      ),
    Host (Host, hostBS),
    Query (Query, queryPairs),
    Scheme (Scheme, schemeBS),
    URIRef
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
  )
import Wire.API.Provider.Service
  ( ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM),
    ServiceToken (ServiceToken),
  )
import Wire.API.Team.LegalHold
  ( ViewLegalHoldService (..),
    ViewLegalHoldServiceInfo
      ( ViewLegalHoldServiceInfo,
        viewLegalHoldServiceAuthToken,
        viewLegalHoldServiceFingerprint,
        viewLegalHoldServiceKey,
        viewLegalHoldServiceTeam,
        viewLegalHoldServiceUrl
      ),
  )

testObject_ViewLegalHoldService_team_1 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_1 = ViewLegalHoldServiceNotConfigured

testObject_ViewLegalHoldService_team_2 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_2 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_3 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_3 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000004"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate (""))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_4 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_4 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_5 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_5 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_6 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_6 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_7 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_7 = ViewLegalHoldServiceNotConfigured

testObject_ViewLegalHoldService_team_8 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_8 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000000"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("aLE="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_9 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_9 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_10 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_10 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_11 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_11 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_12 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_12 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000001"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("L5xw"))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_13 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_13 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("B-k="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_14 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_14 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000000"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("SjY8Ng=="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_15 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_15 = ViewLegalHoldServiceNotConfigured

testObject_ViewLegalHoldService_team_16 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_16 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000003"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("8A=="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_17 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_17 = ViewLegalHoldService (ViewLegalHoldServiceInfo {viewLegalHoldServiceTeam = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000004"))), viewLegalHoldServiceUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, viewLegalHoldServiceFingerprint = Fingerprint {fingerprintBytes = "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"}, viewLegalHoldServiceAuthToken = ServiceToken (fromRight undefined (validate ("MdCZQA=="))), viewLegalHoldServiceKey = ServiceKeyPEM {unServiceKeyPEM = PEM {pemName = "PUBLIC KEY", pemHeader = [], pemContent = "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"}}})

testObject_ViewLegalHoldService_team_18 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_18 = ViewLegalHoldServiceNotConfigured

testObject_ViewLegalHoldService_team_19 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_19 = ViewLegalHoldServiceDisabled

testObject_ViewLegalHoldService_team_20 :: ViewLegalHoldService
testObject_ViewLegalHoldService_team_20 = ViewLegalHoldServiceNotConfigured
