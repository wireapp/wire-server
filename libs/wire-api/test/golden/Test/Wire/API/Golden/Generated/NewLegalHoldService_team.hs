{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.NewLegalHoldService_team where

import Data.Coerce (coerce)
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
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
import Wire.API.Provider.Service (ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM), ServiceToken (ServiceToken))
import Wire.API.Team.LegalHold (NewLegalHoldService (..))

testObject_NewLegalHoldService_team_1 :: NewLegalHoldService
testObject_NewLegalHoldService_team_1 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate ""))
    }

testObject_NewLegalHoldService_team_2 :: NewLegalHoldService
testObject_NewLegalHoldService_team_2 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "PikG5xThDw=="))
    }

testObject_NewLegalHoldService_team_3 :: NewLegalHoldService
testObject_NewLegalHoldService_team_3 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "kw2r-cjra-U0"))
    }

testObject_NewLegalHoldService_team_4 :: NewLegalHoldService
testObject_NewLegalHoldService_team_4 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate ""))
    }

testObject_NewLegalHoldService_team_5 :: NewLegalHoldService
testObject_NewLegalHoldService_team_5 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "T9f1d6R3gA=="))
    }

testObject_NewLegalHoldService_team_6 :: NewLegalHoldService
testObject_NewLegalHoldService_team_6 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "U40hTxpN_1Q="))
    }

testObject_NewLegalHoldService_team_7 :: NewLegalHoldService
testObject_NewLegalHoldService_team_7 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "vn4="))
    }

testObject_NewLegalHoldService_team_8 :: NewLegalHoldService
testObject_NewLegalHoldService_team_8 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "iRi6OJIRXA=="))
    }

testObject_NewLegalHoldService_team_9 :: NewLegalHoldService
testObject_NewLegalHoldService_team_9 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "lxyPY187BbY="))
    }

testObject_NewLegalHoldService_team_10 :: NewLegalHoldService
testObject_NewLegalHoldService_team_10 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "Ckn983B62A=="))
    }

testObject_NewLegalHoldService_team_11 :: NewLegalHoldService
testObject_NewLegalHoldService_team_11 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "zZKENRzO"))
    }

testObject_NewLegalHoldService_team_12 :: NewLegalHoldService
testObject_NewLegalHoldService_team_12 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "uE7fSSldCrg="))
    }

testObject_NewLegalHoldService_team_13 :: NewLegalHoldService
testObject_NewLegalHoldService_team_13 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "JyGtSwecCC0="))
    }

testObject_NewLegalHoldService_team_14 :: NewLegalHoldService
testObject_NewLegalHoldService_team_14 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "9QSn1j95"))
    }

testObject_NewLegalHoldService_team_15 :: NewLegalHoldService
testObject_NewLegalHoldService_team_15 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "oxBexKer"))
    }

testObject_NewLegalHoldService_team_16 :: NewLegalHoldService
testObject_NewLegalHoldService_team_16 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "hQ=="))
    }

testObject_NewLegalHoldService_team_17 :: NewLegalHoldService
testObject_NewLegalHoldService_team_17 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "XukS"))
    }

testObject_NewLegalHoldService_team_18 :: NewLegalHoldService
testObject_NewLegalHoldService_team_18 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "CjZs8l8RosNB"))
    }

testObject_NewLegalHoldService_team_19 :: NewLegalHoldService
testObject_NewLegalHoldService_team_19 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "hR_8Tg=="))
    }

testObject_NewLegalHoldService_team_20 :: NewLegalHoldService
testObject_NewLegalHoldService_team_20 =
  NewLegalHoldService
    { newLegalHoldServiceUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      newLegalHoldServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newLegalHoldServiceToken = ServiceToken (fromRight undefined (validate "BnEGJ3V1"))
    }
