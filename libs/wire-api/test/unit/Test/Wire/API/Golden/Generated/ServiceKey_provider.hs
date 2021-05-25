{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.ServiceKey_provider where

import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Wire.API.Provider.Service
  ( ServiceKey (..),
    ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM),
    ServiceKeyType (RsaServiceKey),
  )

testObject_ServiceKey_provider_1 :: ServiceKey
testObject_ServiceKey_provider_1 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -12,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_2 :: ServiceKey
testObject_ServiceKey_provider_2 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -15,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_3 :: ServiceKey
testObject_ServiceKey_provider_3 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -4,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_4 :: ServiceKey
testObject_ServiceKey_provider_4 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -25,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_5 :: ServiceKey
testObject_ServiceKey_provider_5 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 1,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_6 :: ServiceKey
testObject_ServiceKey_provider_6 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -2,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_7 :: ServiceKey
testObject_ServiceKey_provider_7 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -1,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_8 :: ServiceKey
testObject_ServiceKey_provider_8 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -27,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_9 :: ServiceKey
testObject_ServiceKey_provider_9 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 4,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_10 :: ServiceKey
testObject_ServiceKey_provider_10 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -10,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_11 :: ServiceKey
testObject_ServiceKey_provider_11 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 21,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_12 :: ServiceKey
testObject_ServiceKey_provider_12 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -26,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_13 :: ServiceKey
testObject_ServiceKey_provider_13 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -3,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_14 :: ServiceKey
testObject_ServiceKey_provider_14 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -31,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_15 :: ServiceKey
testObject_ServiceKey_provider_15 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 5,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_16 :: ServiceKey
testObject_ServiceKey_provider_16 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -20,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_17 :: ServiceKey
testObject_ServiceKey_provider_17 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 6,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_18 :: ServiceKey
testObject_ServiceKey_provider_18 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = -9,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_19 :: ServiceKey
testObject_ServiceKey_provider_19 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 31,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }

testObject_ServiceKey_provider_20 :: ServiceKey
testObject_ServiceKey_provider_20 =
  ServiceKey
    { serviceKeyType = RsaServiceKey,
      serviceKeySize = 17,
      serviceKeyPEM =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          }
    }
