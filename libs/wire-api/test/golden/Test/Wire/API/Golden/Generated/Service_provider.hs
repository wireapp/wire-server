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

module Test.Wire.API.Golden.Generated.Service_provider where

import Data.Coerce (coerce)
import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust, fromRight, undefined)
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
import Wire.API.Asset
import Wire.API.Provider
  ( ServiceTag
      ( AudioTag,
        BusinessTag,
        DesignTag,
        EducationTag,
        EntertainmentTag,
        FinanceTag,
        FitnessTag,
        FoodDrinkTag,
        LifestyleTag,
        MediaTag,
        MedicalTag,
        MoviesTag,
        PollTag,
        ProductivityTag,
        QuizTag,
        ShoppingTag,
        SportsTag,
        TravelTag,
        TutorialTag,
        WeatherTag
      ),
    ServiceToken (ServiceToken),
  )
import Wire.API.Provider.Service
  ( Service (..),
    ServiceKey
      ( ServiceKey,
        serviceKeyPEM,
        serviceKeySize,
        serviceKeyType
      ),
    ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM),
    ServiceKeyType (RsaServiceKey),
  )
import Wire.API.User.Profile (Asset (ImageAsset), AssetSize (AssetComplete, AssetPreview), Name (Name, fromName))

testObject_Service_provider_1 :: Service
testObject_Service_provider_1 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002")),
      serviceName =
        Name
          { fromName =
              "a$AAr<\SO)@\EMWcke\NAK\27561\EM\1081263\1078831\&8F\65470\1084107~(IA:\1073579\SI8\ETB\983106\GS*!\1090054aa\GSW3\SO\ETB-L.Ze\1107546\ETB\991481*<\RSTC6\NAKC\1047680vE "
          },
      serviceSummary = "y",
      serviceDescr = "z\DC4",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "RA==")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [BusinessTag, FitnessTag, SportsTag],
      serviceEnabled = False
    }

testObject_Service_provider_2 :: Service
testObject_Service_provider_2 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000")),
      serviceName =
        Name
          { fromName =
              "\986677t&\DEL\ETB\1096223\15177n\152311\EOT\999638\154843S~!\159464;\SO\r\1043691\1070330\38260Sm\1064451\150365\179328xm+~@sgR\162693n\38312?\SI \1022292\&6\1047112\NUL\1011983p\1074060\66014'my?1\993668\EM%|\SOH\1029489\137666\&7\189896;S\r\167820(7\95484\DEL\1086178 x\145898\1053897|WI\8451\161149\US\183680F6\181053MpL>\153307\RS"
          },
      serviceSummary = "J@",
      serviceDescr = "\1106272\49264",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      serviceTags = fromList [],
      serviceEnabled = True
    }

testObject_Service_provider_3 :: Service
testObject_Service_provider_3 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")),
      serviceName =
        Name
          { fromName =
              "\a\ETB;\DEL\77916\42287\SI\EM<{wD\41777\DC4\b\144513GG\51961=H-\14837:\SO\998930\&0`\v\1049512\179792=FWj\ESC\1061141\5083\SOHC\1057567G\13270JDqUp\nL\995501\1085115rx\ESCg\RS5\140245h*\DEL\1038468}\60422\FSdi;Q]\983968\DLE\SYNa\1095926\1013683\EMnb`\35174\n\ACK~3;@]~\992427P@l+\73803\98146l%*\97177{n\STXx@4\1043667\11619)\GS\US<ev\1028592\&9=\992375["
          },
      serviceSummary = "i\1035555",
      serviceDescr = "x",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "9JE=")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
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
                  },
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [],
      serviceEnabled = True
    }

testObject_Service_provider_4 :: Service
testObject_Service_provider_4 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")),
      serviceName =
        Name
          { fromName =
              "OR\SUB6h\1037096\38886^\1089666\10147\1090718\b4N\CANS7\1101323\161132NgKC\8731\44129TAskw}\983872Ig[\1104052%\CAN=\DC4\1099124\141813;OL\alE\131930c*nT\STX\SI"
          },
      serviceSummary = "g\1053866L",
      serviceDescr = "\984789Vf",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "ZGU=")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      serviceTags = fromList [MediaTag],
      serviceEnabled = False
    }

testObject_Service_provider_5 :: Service
testObject_Service_provider_5 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001")),
      serviceName =
        Name
          { fromName =
              "\984497\&5'!)\SO\SUB\DC4\30162\"q\60295\1008738dCS9\1083464Jv[t\DC1\EOT\rXP\v=_\999900=@\171860I\1014318\EOTJ\"\31797C{;\175805\136555\178201Do\DC3\1039496\v+S\DELz\SOHv@\EOT\1043868\agR\153215u+aw\1864}0B~\ACK\22009\&8PT[\EMF"
          },
      serviceSummary = "D\b\r",
      serviceDescr = "\170752",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate "hQ=="))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [LifestyleTag, TutorialTag],
      serviceEnabled = False
    }

testObject_Service_provider_6 :: Service
testObject_Service_provider_6 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")),
      serviceName = Name {fromName = "\v\4282X\74919B\GS\ACK\165012s)\vq\1050183(\60982\f{\ETB~0\1028236L\131764"},
      serviceSummary = "V",
      serviceDescr = "\1021802",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "jK0=")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)
        ],
      serviceTags = fromList [FinanceTag, FitnessTag, MoviesTag],
      serviceEnabled = True
    }

testObject_Service_provider_7 :: Service
testObject_Service_provider_7 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")),
      serviceName =
        Name
          { fromName =
              "\a\984302\990888^*%\1054640\120662;+(\STX3I\SOH;4\1088788q2\1110997\&5U\EOT\1070358C\"\b\146025x\175239=%[\1083040adK!1\8764\139906\131141Y\EOTX\1026494\&3\ACK\989996\t\SOB\ENQPV[bb\fW\"\vO\144921\ENQ2=;S0w`\991315\28410\DC4V/_\37547\f\139472\SI\f-XM\184742q\EM9\DC3\SUB\1080283\1083657\ENQ9"
          },
      serviceSummary = ")S",
      serviceDescr = "\1024306",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "Csg=")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      serviceTags = fromList [MoviesTag],
      serviceEnabled = True
    }

testObject_Service_provider_8 :: Service
testObject_Service_provider_8 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")),
      serviceName =
        Name
          { fromName = "n\984687\&5\SOH\1064780\US36\60630\1078569\SOH\1105307y\53734\SO\ENQ={Hu9\RS/\a\141400\1058385"
          },
      serviceSummary = "\NAKMV",
      serviceDescr = "\rJ",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "ow==")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
                  },
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
                  },
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
                  },
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [],
      serviceEnabled = True
    }

testObject_Service_provider_9 :: Service
testObject_Service_provider_9 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001")),
      serviceName =
        Name
          { fromName =
              "\DEL\SI!sC(\190179\1106895oh,\22340\993985\64492\SUB\993324~!\ETX \185466\&9\rH\165025c<\DC3\NUL\v\DLE\985645\SO\DC4j"
          },
      serviceSummary = "",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate ""))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [LifestyleTag],
      serviceEnabled = False
    }

testObject_Service_provider_10 :: Service
testObject_Service_provider_10 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
      serviceName =
        Name
          { fromName =
              "\1078057\&0\1003719\DC2AhO)ZzoZ{\EOT\1054980z\US\1085764`JJsBj#\74399\SOH$r\DC3P\EOT@#l\1079163\aw\28403\n\71878N\111345m?X\DC4\1280#0QKKdPiH%N-1*\ENQ/D\28532\187364\DC4&\fg\182064\135880vY2c<\1008478A\20467\1094915z\v\1036294;\SOHg;\US@\ENQ\1017693\990588{ks`UW\NULv\175534\GS.\1043351\156732/\151590\37755\EOT\CAN/q\a"
          },
      serviceSummary = "",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate "ZQ=="))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [MediaTag, PollTag],
      serviceEnabled = True
    }

testObject_Service_provider_11 :: Service
testObject_Service_provider_11 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")),
      serviceName =
        Name
          { fromName =
              "s\DEL\1087345\54223\ENQ1\fm\10986\&2\1003890\&3bpJld\1035085\ETB^\SI\1066272+\EOT*&^r\NUL\35508\&7Xf'\92301\154216%\ESCt\46118$h\99512manS\94292\a\DELVV\t\DC4BYCy\1094348\188875s\138039Q\NUL\DLE\71862\te\f}"
          },
      serviceSummary = "KD^",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "Ros=")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      serviceTags = fromList [],
      serviceEnabled = False
    }

testObject_Service_provider_12 :: Service
testObject_Service_provider_12 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
      serviceName = Name {fromName = "1\\$r~WnIAG\142833\999062fG%)4m\EOT\SO\133652X\ETB4~"},
      serviceSummary = "\1054517",
      serviceDescr = "+N",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate ""))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
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
                  },
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
                  },
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
              ]
          ),
      serviceAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)
        ],
      serviceTags = fromList [MedicalTag, TravelTag, WeatherTag],
      serviceEnabled = False
    }

testObject_Service_provider_13 :: Service
testObject_Service_provider_13 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
      serviceName =
        Name
          { fromName =
              "\DLEdy>kx\1092189\&4f\ESC*\996554y5Lp?I+p\DC4\7442\vq\1089351|\v7CIg\SO\NUL/\1034482H\995785\FS%t\1114103\&0\92492\fp@^\1036059\&0M}"
          },
      serviceSummary = "R0m",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ServiceToken (fromRight undefined (validate "")), ServiceToken (fromRight undefined (validate ""))]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      serviceTags = fromList [EducationTag, MoviesTag, ShoppingTag],
      serviceEnabled = False
    }

testObject_Service_provider_14 :: Service
testObject_Service_provider_14 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")),
      serviceName =
        Name
          { fromName =
              "Q}\a9\SUB(\1074586\SI\1051871\FS~v\v\188951\f\ETBp\NUL\176142\29270L\1111738Z\1016959\101066AR0\12875O\178051\&1\SYN\SOHmF\DC2\SO\1102403)\STX=3Rq:G\DLE\1072012\1096405\1074990.\ETBi\RS\USU\aH\NUL\35252Pd 9I\SOHtLf@aZ\26516]7hO5\a"
          },
      serviceSummary = "\ETX\988661",
      serviceDescr = ")R",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "Pw==")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
                ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      serviceTags = fromList [],
      serviceEnabled = True
    }

testObject_Service_provider_15 :: Service
testObject_Service_provider_15 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000")),
      serviceName =
        Name
          { fromName =
              "X\1102103 \CAN\SUB\172460\a'YNs\ETX\DEL%\41222(\1057933\1065981\t\SYN\SO\1038510\"u\11829\&7\EOTW\174511\bqO8W|T\DEL$Wtw\NUL\998525tI\1080803\1107028qg\51115<$\185171WtXD>O\1019163W\991041PZo.\ENQ\1094359\CANh\180249\EMQV\71088\&7\"2_\68222\186698!\147181\DC1inq\FSiJ)_\173911;\1063486"
          },
      serviceSummary = "",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate "yA=="))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)
        ],
      serviceTags = fromList [DesignTag, LifestyleTag, QuizTag],
      serviceEnabled = True
    }

testObject_Service_provider_16 :: Service
testObject_Service_provider_16 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
      serviceName =
        Name
          { fromName =
              "\DC1\142187\vG0(H\CAN\170870\1031410\NAKBtQZ1S*Kk~\1066479\NULo\165805o\83139\1001418@Y\92323(}\ENQk\v\1107294\ETXnx\f\EMW\b1r\1001015x :r\1005941\156747E7\1029198\1034548[PU\f\FSz\t\vx<\1016214I0)(0"
          },
      serviceSummary = "?`x",
      serviceDescr = "",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate ""))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
                  { serviceKeyType = RsaServiceKey,
                    serviceKeySize = 0,
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
                  },
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
              ]
          ),
      serviceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      serviceTags = fromList [PollTag],
      serviceEnabled = False
    }

testObject_Service_provider_17 :: Service
testObject_Service_provider_17 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001")),
      serviceName =
        Name
          { fromName =
              "0\DC3)NU\178509\rr\CANc\DELYpN1\EOT\99678\78448V\27789\STXQl\EOT\1062138\SOH\1026234KtA##j\NAKQ\5342k\1008810Z(\EM|\NAKga\1107351n\STX'"
          },
      serviceSummary = "\993604",
      serviceDescr = "\183483",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "BA==")),
                ServiceToken (fromRight undefined (validate "Fm4="))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [AudioTag, EntertainmentTag, MedicalTag],
      serviceEnabled = True
    }

testObject_Service_provider_18 :: Service
testObject_Service_provider_18 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")),
      serviceName =
        Name
          { fromName =
              "\ACK\f\1105185nw\34463$\15116\1056570\&6\1036390\1040667OS` H/&y?\22347\3405\&6\1067677\120086\DC2e\1034847w)z.g\SYN\1015294\987174y@\ETB\1013029H\NULM^\FS[B3$\1076423S8\SYN\1099942N\163866Vk/\72227\SOH"
          },
      serviceSummary = "|n",
      serviceDescr = "\1103538am",
      serviceUrl =
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
      serviceTokens =
        List1
          ( NonEmpty.fromList
              [ ServiceToken (fromRight undefined (validate "5jM=")),
                ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
                  },
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
                  },
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
                  },
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [FoodDrinkTag],
      serviceEnabled = False
    }

testObject_Service_provider_19 :: Service
testObject_Service_provider_19 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
      serviceName = Name {fromName = ";<\46080\1015531"},
      serviceSummary = "",
      serviceDescr = "PSG",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate ""))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [FitnessTag, FoodDrinkTag, ProductivityTag],
      serviceEnabled = True
    }

testObject_Service_provider_20 :: Service
testObject_Service_provider_20 =
  Service
    { serviceId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")),
      serviceName =
        Name
          { fromName =
              "$\1002351\22460z\SO$\a\DELiC$z)\179392\98452\NULw=0OU\1093405\STX\1010026\ACK\1036592(Hi\1095230\1061030L{Q\"](%\100179L4k\DELs\1094674\148727\10477tf\995803\58846\STX\6153C\110985@\1092300P\168283\1064868\1037943U\1033961)eH7\30812\&7\ACK1neF \DC1j\1102586\NUL\12437\&1):\71305>Qt\ESC,\\n\26131D\SO6J[\157196T\b\SI\178153HO4\SOH[\SI7\1012969*7\GSH\FSl"
          },
      serviceSummary = "p\RS",
      serviceDescr = "\vZK",
      serviceUrl =
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
      serviceTokens = List1 (NonEmpty.fromList [ServiceToken (fromRight undefined (validate ""))]),
      serviceKeys =
        List1
          ( NonEmpty.fromList
              [ ServiceKey
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
              ]
          ),
      serviceAssets = [],
      serviceTags = fromList [],
      serviceEnabled = True
    }
