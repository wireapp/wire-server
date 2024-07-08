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

module Test.Wire.API.Golden.Generated.RTCConfiguration_user where

import Control.Lens
import Data.Coerce
import Data.List.NonEmpty
import Data.Misc
import Data.Text.Ascii
import Data.Time
import Imports
import URI.ByteString
import Wire.API.Call.Config

testObject_RTCConfiguration_user_1 :: RTCConfiguration
testObject_RTCConfiguration_user_1 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostName "a-c") (read "0") (Just TransportTCP)
            :| [ turnURI SchemeTurns (TurnHostIp (IpAddr (read "102.223.53.51"))) (read "1") Nothing,
                 turnURI SchemeTurns (TurnHostName "123") (read "0") Nothing
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "y"
            & tuVersion .~ 0
            & tuKeyindex .~ 2
            & tuT .~ '\990111'
        )
        (fromRight undefined (validate "KA=="))
        :| [ rtcIceServer
               (turnURI SchemeTurns (TurnHostIp (IpAddr (read "11.115.71.116"))) (read "0") (Just TransportTCP) :| [])
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "g9l"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 1
                   & tuT .~ 'F'
               )
               (fromRight undefined (validate "vg==")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "64.166.247.200"))) (read "1") (Just TransportUDP)
                   :| [ turnURI SchemeTurns (TurnHostIp (IpAddr (read "169.32.10.117"))) (read "1") (Just TransportTCP),
                        turnURI SchemeTurns (TurnHostIp (IpAddr (read "146.223.237.161"))) (read "0") (Just TransportTCP)
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "vkw"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 2
                   & tuT .~ 'O'
               )
               (fromRight undefined (validate "1Q==")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "229.74.72.234"))) (read "1") Nothing
                   :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "200.55.82.144"))) (read "0") (Just TransportTCP),
                        turnURI SchemeTurns (TurnHostIp (IpAddr (read "30.151.133.158"))) (read "1") (Just TransportTCP)
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "qv"
                   & tuVersion .~ 0
                   & tuKeyindex .~ 2
                   & tuT .~ 'F'
               )
               (fromRight undefined (validate "/w==")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)
                   :| [turnURI SchemeTurn (TurnHostIp (IpAddr (read "212.204.103.144"))) (read "1") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "b"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 1
                   & tuT .~ '\40387'
               )
               (fromRight undefined (validate "TQ==")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "36.138.227.130"))) (read "0") Nothing
                   :| [turnURI SchemeTurns (TurnHostName "a-c") (read "0") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 3.000000000000) "1j"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 1
                   & tuT .~ '6'
               )
               (fromRight undefined (validate "1CM=")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "39.3.236.143"))) (read "0") (Just TransportUDP)
                   :| [turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 0.000000000000) "v"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 0
                   & tuT .~ 'D'
               )
               (fromRight undefined (validate "xVY=")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "126.131.239.218"))) (read "0") (Just TransportUDP)
                   :| [ turnURI SchemeTurns (TurnHostIp (IpAddr (read "189.135.181.33"))) (read "0") (Just TransportUDP),
                        turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 3.000000000000) "i3"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 1
                   & tuT .~ '\DLE'
               )
               (fromRight undefined (validate "9g=="))
           ]
    )
    Nothing
    2
    Nothing
    Nothing

testObject_RTCConfiguration_user_2 :: RTCConfiguration
testObject_RTCConfiguration_user_2 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "225.27.138.155"))) (read "0") (Just TransportUDP)
            :| [ turnURI SchemeTurns (TurnHostIp (IpAddr (read "226.235.88.44"))) (read "0") (Just TransportUDP),
                 turnURI SchemeTurns (TurnHostIp (IpAddr (read "235.195.120.46"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 3.000000000000) "i3u"
            & tuVersion .~ 0
            & tuKeyindex .~ 1
            & tuT .~ 'I'
        )
        (fromRight undefined (validate "2w=="))
        :| [ rtcIceServer
               (turnURI SchemeTurn (TurnHostName "a-c") (read "1") Nothing :| [])
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "x"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 1
                   & tuT .~ 'z'
               )
               (fromRight undefined (validate "VA==")),
             rtcIceServer
               (turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") Nothing :| [])
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "2"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 0
                   & tuT .~ '('
               )
               (fromRight undefined (validate "4A==")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostName "007.com") (read "0") (Just TransportUDP)
                   :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "82.115.0.150"))) (read "1") (Just TransportTCP),
                        turnURI SchemeTurns (TurnHostIp (IpAddr (read "172.9.22.21"))) (read "0") (Just TransportUDP)
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "vp"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 0
                   & tuT .~ '\DC2'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "37.46.50.11"))) (read "0") (Just TransportTCP)
                   :| [turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "4h4"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 0
                   & tuT .~ '\1100995'
               )
               (fromRight undefined (validate "Mw==")),
             rtcIceServer
               (turnURI SchemeTurn (TurnHostName "123") (read "1") (Just TransportTCP) :| [])
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "c9l"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 2
                   & tuT .~ 'w'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               (turnURI SchemeTurn (TurnHostIp (IpAddr (read "137.180.116.174"))) (read "0") (Just TransportUDP) :| [])
               ( turnUsername (secondsToNominalDiffTime 3.000000000000) "h8e"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 0
                   & tuT .~ '\1070826'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostName "007.com") (read "1") (Just TransportTCP)
                   :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "148.207.99.149"))) (read "0") Nothing,
                        turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP),
                        turnURI SchemeTurn (TurnHostIp (IpAddr (read "102.41.143.12"))) (read "0") Nothing
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "cr"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 1
                   & tuT .~ '\v'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               (turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP) :| [])
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "ol0"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 2
                   & tuT .~ '.'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostName "a-c") (read "0") (Just TransportUDP)
                   :| [turnURI SchemeTurns (TurnHostName "123") (read "0") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "a"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 0
                   & tuT .~ '"'
               )
               (fromRight undefined (validate ""))
           ]
    )
    ( Just
        ( sftServer
            ( coerce
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
                  }
            )
            :| [ sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   )
               ]
        )
    )
    4
    Nothing
    Nothing

testObject_RTCConfiguration_user_3 :: RTCConfiguration
testObject_RTCConfiguration_user_3 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurn (TurnHostName "a-c") (read "0") (Just TransportTCP)
            :| [ turnURI SchemeTurns (TurnHostIp (IpAddr (read "44.242.178.2"))) (read "1") (Just TransportUDP),
                 turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 0.000000000000) "m2s"
            & tuVersion .~ 2
            & tuKeyindex .~ 0
            & tuT .~ '\f'
        )
        (fromRight undefined (validate ""))
        :| [ rtcIceServer
               (turnURI SchemeTurn (TurnHostIp (IpAddr (read "113.127.226.211"))) (read "1") Nothing :| [])
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "2b"
                   & tuVersion .~ 0
                   & tuKeyindex .~ 0
                   & tuT .~ '\37292'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)
                   :| [turnURI SchemeTurn (TurnHostIp (IpAddr (read "222.209.199.151"))) (read "0") (Just TransportUDP)]
               )
               ( turnUsername (secondsToNominalDiffTime 0.000000000000) "w"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 0
                   & tuT .~ '-'
               )
               (fromRight undefined (validate "Sjk=")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "33.214.122.255"))) (read "0") (Just TransportUDP)
                   :| [turnURI SchemeTurns (TurnHostName "007.com") (read "1") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "py"
                   & tuVersion .~ 1
                   & tuKeyindex .~ 0
                   & tuT .~ '#'
               )
               (fromRight undefined (validate "awA=")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "72.84.227.18"))) (read "0") (Just TransportUDP)
                   :| [turnURI SchemeTurn (TurnHostName "007.com") (read "0") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "l1f"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 0
                   & tuT .~ '$'
               )
               (fromRight undefined (validate "jw==")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "75.117.151.157"))) (read "1") (Just TransportUDP)
                   :| [ turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP),
                        turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP),
                        turnURI SchemeTurn (TurnHostName "007.com") (read "0") (Just TransportTCP)
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 2.000000000000) "kke"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 0
                   & tuT .~ '{'
               )
               (fromRight undefined (validate "hQ==")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostName "123") (read "1") Nothing
                   :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "53.242.117.37"))) (read "1") Nothing,
                        turnURI SchemeTurn (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") Nothing
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 0.000000000000) "8"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 0
                   & tuT .~ 'Z'
               )
               (fromRight undefined (validate "mHw=")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "148.8.193.103"))) (read "1") Nothing
                   :| [turnURI SchemeTurns (TurnHostName "host.name") (read "0") (Just TransportUDP)]
               )
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "shf"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 1
                   & tuT .~ '^'
               )
               (fromRight undefined (validate "")),
             rtcIceServer
               ( turnURI SchemeTurns (TurnHostName "host.name") (read "1") (Just TransportTCP)
                   :| [turnURI SchemeTurn (TurnHostIp (IpAddr (read "159.246.220.178"))) (read "1") (Just TransportTCP)]
               )
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "x5"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 0
                   & tuT .~ 'd'
               )
               (fromRight undefined (validate "FU0=")),
             rtcIceServer
               (turnURI SchemeTurns (TurnHostName "007.com") (read "0") (Just TransportTCP) :| [])
               ( turnUsername (secondsToNominalDiffTime 4.000000000000) "v"
                   & tuVersion .~ 2
                   & tuKeyindex .~ 2
                   & tuT .~ 'q'
               )
               (fromRight undefined (validate "1Q==")),
             rtcIceServer
               ( turnURI SchemeTurn (TurnHostIp (IpAddr (read "243.183.34.181"))) (read "1") (Just TransportUDP)
                   :| [ turnURI SchemeTurns (TurnHostName "123") (read "0") Nothing,
                        turnURI SchemeTurns (TurnHostName "host.name") (read "0") (Just TransportTCP),
                        turnURI SchemeTurns (TurnHostName "123") (read "0") Nothing
                      ]
               )
               ( turnUsername (secondsToNominalDiffTime 1.000000000000) "8"
                   & tuVersion .~ 3
                   & tuKeyindex .~ 1
                   & tuT .~ '\b'
               )
               (fromRight undefined (validate ""))
           ]
    )
    ( Just
        ( sftServer
            ( coerce
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
                  }
            )
            :| []
        )
    )
    9
    Nothing
    Nothing

testObject_RTCConfiguration_user_4 :: RTCConfiguration
testObject_RTCConfiguration_user_4 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") Nothing
            :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP),
                 turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "tj"
            & tuVersion .~ 0
            & tuKeyindex .~ 0
            & tuT .~ '\1011805'
        )
        (fromRight undefined (validate ""))
        :| []
    )
    ( Just
        ( sftServer
            ( coerce
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
                  }
            )
            :| [ sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   ),
                 sftServer
                   ( coerce
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
                         }
                   )
               ]
        )
    )
    2
    Nothing
    Nothing

testObject_RTCConfiguration_user_5 :: RTCConfiguration
testObject_RTCConfiguration_user_5 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") Nothing
            :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP),
                 turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "tj"
            & tuVersion .~ 0
            & tuKeyindex .~ 0
            & tuT .~ '\1011805'
        )
        (fromRight undefined (validate ""))
        :| []
    )
    ( Just
        ( sftServer
            ( coerce
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
                  }
            )
            :| []
        )
    )
    2
    Nothing
    Nothing

testObject_RTCConfiguration_user_6 :: RTCConfiguration
testObject_RTCConfiguration_user_6 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") Nothing
            :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP),
                 turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "tj"
            & tuVersion .~ 0
            & tuKeyindex .~ 0
            & tuT .~ '\1011805'
        )
        (fromRight undefined (validate ""))
        :| []
    )
    Nothing
    2
    Nothing
    Nothing

testObject_RTCConfiguration_user_7 :: RTCConfiguration
testObject_RTCConfiguration_user_7 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") Nothing
            :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP),
                 turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "tj"
            & tuVersion .~ 0
            & tuKeyindex .~ 0
            & tuT .~ '\1011805'
        )
        (fromRight undefined (validate ""))
        :| []
    )
    Nothing
    2
    ( Just
        [ authSFTServer
            ( sftServer
                ( coerce
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
                      }
                )
            )
            (mkSFTUsername False (secondsToNominalDiffTime 12) "username")
            "credential"
        ]
    )
    Nothing

testObject_RTCConfiguration_user_8 :: RTCConfiguration
testObject_RTCConfiguration_user_8 =
  rtcConfiguration
    ( rtcIceServer
        ( turnURI SchemeTurns (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") Nothing
            :| [ turnURI SchemeTurn (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP),
                 turnURI SchemeTurns (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP),
                 turnURI SchemeTurn (TurnHostName "host.name") (read "1") (Just TransportTCP)
               ]
        )
        ( turnUsername (secondsToNominalDiffTime 2.000000000000) "tj"
            & tuVersion .~ 0
            & tuKeyindex .~ 0
            & tuT .~ '\1011805'
        )
        (fromRight undefined (validate ""))
        :| []
    )
    Nothing
    2
    Nothing
    (Just True)
