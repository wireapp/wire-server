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
module Test.Wire.API.Golden.Generated.RTCConfiguration_user where

import Control.Lens ((.~))
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc (HttpsUrl (HttpsUrl), IpAddr (IpAddr))
import Data.Text.Ascii (AsciiChars (validate))
import Data.Time (secondsToNominalDiffTime)
import Imports (Maybe (Just, Nothing), fromRight, read, undefined, (&))
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
import Wire.API.Call.Config
  ( RTCConfiguration,
    Scheme (SchemeTurn, SchemeTurns),
    Transport (TransportTCP, TransportUDP),
    TurnHost (TurnHostIp, TurnHostName),
    rtcConfiguration,
    rtcIceServer,
    sftServer,
    tuKeyindex,
    tuT,
    tuVersion,
    turnURI,
    turnUsername,
  )

testObject_RTCConfiguration_user_1 :: RTCConfiguration
testObject_RTCConfiguration_user_1 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP))
                :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "102.223.53.51"))) (read "1") (Nothing)),
                     (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("y") & tuVersion .~ (0) & tuKeyindex .~ (2)
                  & tuT .~ ('\990111')
              )
            )
            ((fromRight undefined (validate ("KA=="))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "11.115.71.116"))) (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("g9l") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('F')
                     )
                   )
                   ((fromRight undefined (validate ("vg=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "64.166.247.200"))) (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "169.32.10.117"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "146.223.237.161"))) (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("vkw") & tuVersion .~ (2) & tuKeyindex .~ (2)
                         & tuT .~ ('O')
                     )
                   )
                   ((fromRight undefined (validate ("1Q=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "229.74.72.234"))) (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "200.55.82.144"))) (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "30.151.133.158"))) (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("qv") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('F')
                     )
                   )
                   ((fromRight undefined (validate ("/w=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP))
                       :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "212.204.103.144"))) (read "1") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("b") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('\40387')
                     )
                   )
                   ((fromRight undefined (validate ("TQ=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "36.138.227.130"))) (read "0") (Nothing))
                       :| [(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("1j") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('6')
                     )
                   )
                   ((fromRight undefined (validate ("1CM="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "39.3.236.143"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("v") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('D')
                     )
                   )
                   ((fromRight undefined (validate ("xVY="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "126.131.239.218"))) (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "189.135.181.33"))) (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("i3") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('\DLE')
                     )
                   )
                   ((fromRight undefined (validate ("9g=="))))
               )
             ]
      )
      (Nothing)
      (2)
  )

testObject_RTCConfiguration_user_2 :: RTCConfiguration
testObject_RTCConfiguration_user_2 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "225.27.138.155"))) (read "0") (Just TransportUDP))
                :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "226.235.88.44"))) (read "0") (Just TransportUDP)),
                     (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "235.195.120.46"))) (read "0") (Just TransportTCP)),
                     (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("i3u") & tuVersion .~ (0) & tuKeyindex .~ (1)
                  & tuT .~ ('I')
              )
            )
            ((fromRight undefined (validate ("2w=="))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("x") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('z')
                     )
                   )
                   ((fromRight undefined (validate ("VA=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("2") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('(')
                     )
                   )
                   ((fromRight undefined (validate ("4A=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "82.115.0.150"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "172.9.22.21"))) (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("vp") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('\DC2')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "37.46.50.11"))) (read "0") (Just TransportTCP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("4h4") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('\1100995')
                     )
                   )
                   ((fromRight undefined (validate ("Mw=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("c9l") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('w')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "137.180.116.174"))) (read "0") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("h8e") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('\1070826')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "148.207.99.149"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "102.41.143.12"))) (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("cr") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('\v')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("ol0") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('.')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("a") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('"')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (4)
  )

testObject_RTCConfiguration_user_3 :: RTCConfiguration
testObject_RTCConfiguration_user_3 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP))
                :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "44.242.178.2"))) (read "1") (Just TransportUDP)),
                     (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("m2s") & tuVersion .~ (2) & tuKeyindex .~ (0)
                  & tuT .~ ('\f')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "113.127.226.211"))) (read "1") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("2b") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('\37292')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "222.209.199.151"))) (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("w") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('-')
                     )
                   )
                   ((fromRight undefined (validate ("Sjk="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "33.214.122.255"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("py") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('#')
                     )
                   )
                   ((fromRight undefined (validate ("awA="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "72.84.227.18"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("l1f") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('$')
                     )
                   )
                   ((fromRight undefined (validate ("jw=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "75.117.151.157"))) (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("kke") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('{')
                     )
                   )
                   ((fromRight undefined (validate ("hQ=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "53.242.117.37"))) (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("8") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('Z')
                     )
                   )
                   ((fromRight undefined (validate ("mHw="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "148.8.193.103"))) (read "1") (Nothing))
                       :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("shf") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('^')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP))
                       :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "159.246.220.178"))) (read "1") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("x5") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('d')
                     )
                   )
                   ((fromRight undefined (validate ("FU0="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("v") & tuVersion .~ (2) & tuKeyindex .~ (2)
                         & tuT .~ ('q')
                     )
                   )
                   ((fromRight undefined (validate ("1Q=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "243.183.34.181"))) (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("8") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('\b')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (9)
  )

testObject_RTCConfiguration_user_4 :: RTCConfiguration
testObject_RTCConfiguration_user_4 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "248.187.155.126"))) (read "1") (Nothing))
                :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "166.155.90.230"))) (read "0") (Just TransportTCP)),
                     (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
                     (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("tj") & tuVersion .~ (0) & tuKeyindex .~ (0)
                  & tuT .~ ('\1011805')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| []
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (2)
  )

testObject_RTCConfiguration_user_5 :: RTCConfiguration
testObject_RTCConfiguration_user_5 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))
                :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing)),
                     (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "80.25.165.101"))) (read "1") (Just TransportTCP)),
                     (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("5m7") & tuVersion .~ (2) & tuKeyindex .~ (0)
                  & tuT .~ ('{')
              )
            )
            ((fromRight undefined (validate ("dvI="))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("x") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('\1002612')
                     )
                   )
                   ((fromRight undefined (validate ("cCU="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("r5n") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\DC2')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "73.195.120.125"))) (read "1") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("8f") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('\DEL')
                     )
                   )
                   ((fromRight undefined (validate ("Mw=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "134.245.76.176"))) (read "0") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("e1") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('m')
                     )
                   )
                   ((fromRight undefined (validate ("W/E="))))
               )
             ]
      )
      (Nothing)
      (12)
  )

testObject_RTCConfiguration_user_6 :: RTCConfiguration
testObject_RTCConfiguration_user_6 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "149.161.183.205"))) (read "0") (Nothing))
                :| [ (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
                     (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),
                     (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "92.128.67.225"))) (read "0") (Just TransportTCP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("de") & tuVersion .~ (1) & tuKeyindex .~ (1)
                  & tuT .~ ('Q')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "118.170.44.54"))) (read "0") (Just TransportTCP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("bi") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('\33797')
                     )
                   )
                   ((fromRight undefined (validate ("N2I="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (26)
  )

testObject_RTCConfiguration_user_7 :: RTCConfiguration
testObject_RTCConfiguration_user_7 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "44.1.165.236"))) (read "1") (Nothing)) :| [])
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("a6m") & tuVersion .~ (0) & tuKeyindex .~ (2)
                  & tuT .~ ('H')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "11.96.91.17"))) (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("lp") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('e')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "13.23.57.118"))) (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("8et") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('&')
                     )
                   )
                   ((fromRight undefined (validate ("7w=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("7ap") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('\DC1')
                     )
                   )
                   ((fromRight undefined (validate ("0Ow="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "135.209.223.40"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "173.139.89.251"))) (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("g9") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('X')
                     )
                   )
                   ((fromRight undefined (validate ("ag=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("xlk") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('\ENQ')
                     )
                   )
                   ((fromRight undefined (validate ("ZQ=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("h") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('B')
                     )
                   )
                   ((fromRight undefined (validate ("Tw=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("x") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('F')
                     )
                   )
                   ((fromRight undefined (validate ("yQ=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "99.124.6.72"))) (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("b") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('l')
                     )
                   )
                   ((fromRight undefined (validate ("vWc="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "86.184.243.74"))) (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "244.0.87.83"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "59.44.234.164"))) (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("178") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('2')
                     )
                   )
                   ((fromRight undefined (validate ("wQ=="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (28)
  )

testObject_RTCConfiguration_user_8 :: RTCConfiguration
testObject_RTCConfiguration_user_8 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP))
                :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "117.96.79.180"))) (read "1") (Just TransportUDP)),
                     (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("ky") & tuVersion .~ (0) & tuKeyindex .~ (1)
                  & tuT .~ ('\7470')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "248.107.4.38"))) (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("2e") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('V')
                     )
                   )
                   ((fromRight undefined (validate ("2g=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "129.183.147.71"))) (read "1") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("ae3") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('+')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("081") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('~')
                     )
                   )
                   ((fromRight undefined (validate ("gnU="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                 ]
          )
      )
      (28)
  )

testObject_RTCConfiguration_user_9 :: RTCConfiguration
testObject_RTCConfiguration_user_9 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP)),
                     (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("1h") & tuVersion .~ (1) & tuKeyindex .~ (2)
                  & tuT .~ ('\111235')
              )
            )
            ((fromRight undefined (validate ("X9k="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "22.129.108.184"))) (read "1") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("5ku") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\31409')
                     )
                   )
                   ((fromRight undefined (validate ("og=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("ar") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\SOH')
                     )
                   )
                   ((fromRight undefined (validate ("ow=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "164.254.14.80"))) (read "1") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("hc") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('\DC4')
                     )
                   )
                   ((fromRight undefined (validate ("tNQ="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "171.181.144.124"))) (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("vq") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('\DC2')
                     )
                   )
                   ((fromRight undefined (validate ("7Ng="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing))
                       :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "94.219.124.35"))) (read "1") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("zc") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('c')
                     )
                   )
                   ((fromRight undefined (validate ("Qw=="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (13)
  )

testObject_RTCConfiguration_user_10 :: RTCConfiguration
testObject_RTCConfiguration_user_10 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing))
                :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing))]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("ux") & tuVersion .~ (3) & tuKeyindex .~ (0)
                  & tuT .~ ('.')
              )
            )
            ((fromRight undefined (validate ("Zh8="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("k") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('\f')
                     )
                   )
                   ((fromRight undefined (validate ("Cg=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("9wq") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('H')
                     )
                   )
                   ((fromRight undefined (validate ("Huw="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (24)
  )

testObject_RTCConfiguration_user_11 :: RTCConfiguration
testObject_RTCConfiguration_user_11 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "180.204.175.154"))) (read "0") (Just TransportTCP))
                :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "243.122.21.187"))) (read "0") (Nothing)),
                     (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Nothing))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("8") & tuVersion .~ (0) & tuKeyindex .~ (0)
                  & tuT .~ ('\1110856')
              )
            )
            ((fromRight undefined (validate ("9Uc="))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "140.96.58.16"))) (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("v3") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('H')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "50.111.81.130"))) (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("h") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('\a')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("z2") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('6')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("qv") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('\45518')
                     )
                   )
                   ((fromRight undefined (validate ("kA=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "141.124.238.104"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "220.161.164.44"))) (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("q") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('\1017549')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (23)
  )

testObject_RTCConfiguration_user_12 :: RTCConfiguration
testObject_RTCConfiguration_user_12 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP))
                :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "145.53.168.127"))) (read "1") (Just TransportUDP))]
            )
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("og") & tuVersion .~ (1) & tuKeyindex .~ (2)
                  & tuT .~ ('"')
              )
            )
            ((fromRight undefined (validate ("H9Y="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("3s") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('~')
                     )
                   )
                   ((fromRight undefined (validate ("74w="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "214.42.57.20"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("yyd") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('`')
                     )
                   )
                   ((fromRight undefined (validate ("WaE="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (19)
  )

testObject_RTCConfiguration_user_13 :: RTCConfiguration
testObject_RTCConfiguration_user_13 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP)) :| [])
            ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("5c") & tuVersion .~ (1) & tuKeyindex .~ (2)
                  & tuT .~ ('\FS')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "241.7.138.168"))) (read "0") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("zac") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('f')
                     )
                   )
                   ((fromRight undefined (validate ("9Uk="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "12.24.56.118"))) (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("1vf") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('\b')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("wt") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('X')
                     )
                   )
                   ((fromRight undefined (validate ("Cpk="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| []
          )
      )
      (18)
  )

testObject_RTCConfiguration_user_14 :: RTCConfiguration
testObject_RTCConfiguration_user_14 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "9.44.249.238"))) (read "1") (Just TransportUDP)) :| [])
            ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("0") & tuVersion .~ (0) & tuKeyindex .~ (1)
                  & tuT .~ ('\ACK')
              )
            )
            ((fromRight undefined (validate (""))))
        )
          :| [ ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("wp") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('5')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "20.214.247.113"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("te") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('"')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "34.111.158.108"))) (read "1") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("ew") & tuVersion .~ (2) & tuKeyindex .~ (2)
                         & tuT .~ ('\DC2')
                     )
                   )
                   ((fromRight undefined (validate ("gg=="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                 ]
          )
      )
      (29)
  )

testObject_RTCConfiguration_user_15 :: RTCConfiguration
testObject_RTCConfiguration_user_15 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "16.13.64.137"))) (read "0") (Nothing))
                :| [ (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP)),
                     (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("5") & tuVersion .~ (3) & tuKeyindex .~ (0)
                  & tuT .~ ('\133622')
              )
            )
            ((fromRight undefined (validate ("gg=="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
                       :| [(turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "4.21.222.180"))) (read "1") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("6") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('?')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "124.52.236.64"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("zxk") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('\STX')
                     )
                   )
                   ((fromRight undefined (validate ("7XQ="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "82.201.209.41"))) (read "1") (Just TransportTCP))
                       :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "207.47.251.117"))) (read "1") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("wq") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('\STX')
                     )
                   )
                   ((fromRight undefined (validate ("xm4="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "81.160.103.101"))) (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("gh7") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('\STX')
                     )
                   )
                   ((fromRight undefined (validate ("FA=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "47.173.209.140"))) (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "83.54.196.51"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("h") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('\44193')
                     )
                   )
                   ((fromRight undefined (validate ("Ukk="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "68.110.78.163"))) (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("j") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('\151303')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "159.210.176.232"))) (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "133.116.58.206"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "195.3.162.153"))) (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("wl2") & tuVersion .~ (2) & tuKeyindex .~ (2)
                         & tuT .~ ('R')
                     )
                   )
                   ((fromRight undefined (validate ("4BI="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "109.108.175.177"))) (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("asq") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('\991437')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "216.186.129.183"))) (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("1t") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ (')')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "43.82.5.88"))) (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "148.225.106.7"))) (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("t") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('j')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                 ]
          )
      )
      (19)
  )

testObject_RTCConfiguration_user_16 :: RTCConfiguration
testObject_RTCConfiguration_user_16 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))
                :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportUDP)),
                     (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "130.9.240.157"))) (read "0") (Just TransportUDP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("a") & tuVersion .~ (0) & tuKeyindex .~ (1)
                  & tuT .~ ('8')
              )
            )
            ((fromRight undefined (validate ("wA=="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Nothing))
                       :| [(turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("z") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('\155559')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "16.76.164.202"))) (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("mt") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('L')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("f") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\SO')
                     )
                   )
                   ((fromRight undefined (validate ("2A=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("0c") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('$')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("url") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('N')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "171.81.187.236"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("eeq") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('\164609')
                     )
                   )
                   ((fromRight undefined (validate ("OFI="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("s") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('H')
                     )
                   )
                   ((fromRight undefined (validate ("jQ=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("5") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('|')
                     )
                   )
                   ((fromRight undefined (validate ("lBw="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "242.239.169.126"))) (read "0") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("q") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('#')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (24)
  )

testObject_RTCConfiguration_user_17 :: RTCConfiguration
testObject_RTCConfiguration_user_17 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP))
                :| [ (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportTCP)),
                     (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("jme") & tuVersion .~ (1) & tuKeyindex .~ (2)
                  & tuT .~ ('E')
              )
            )
            ((fromRight undefined (validate ("sB8="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("r") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('#')
                     )
                   )
                   ((fromRight undefined (validate ("w+c="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "79.55.124.204"))) (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("jn") & tuVersion .~ (3) & tuKeyindex .~ (2)
                         & tuT .~ ('B')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("vp") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('\165749')
                     )
                   )
                   ((fromRight undefined (validate ("Ug=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("c7") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ (' ')
                     )
                   )
                   ((fromRight undefined (validate ("Sg=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "254.2.228.149"))) (read "0") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "131.78.13.4"))) (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("kbh") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('o')
                     )
                   )
                   ((fromRight undefined (validate ("KA=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "92.191.110.233"))) (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("b2") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('|')
                     )
                   )
                   ((fromRight undefined (validate ("og=="))))
               )
             ]
      )
      (Nothing)
      (21)
  )

testObject_RTCConfiguration_user_18 :: RTCConfiguration
testObject_RTCConfiguration_user_18 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing)) :| [])
            ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("q") & tuVersion .~ (1) & tuKeyindex .~ (0)
                  & tuT .~ ('A')
              )
            )
            ((fromRight undefined (validate ("a0c="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "220.101.0.43"))) (read "0") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("s88") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('w')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Nothing))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("sl") & tuVersion .~ (0) & tuKeyindex .~ (0)
                         & tuT .~ ('Y')
                     )
                   )
                   ((fromRight undefined (validate ("dg=="))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("c") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('\EM')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "246.153.120.56"))) (read "0") (Nothing))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("lkz") & tuVersion .~ (3) & tuKeyindex .~ (0)
                         & tuT .~ ('h')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "206.249.148.39"))) (read "1") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "8.107.15.99"))) (read "1") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("i") & tuVersion .~ (1) & tuKeyindex .~ (0)
                         & tuT .~ ('\38977')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ((turnURI (SchemeTurn) (TurnHostName "123") (read "0") (Just TransportUDP)) :| [])
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("7q5") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('}')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "187.61.112.23"))) (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "15.140.51.24"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "16.22.166.21"))) (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("y7") & tuVersion .~ (0) & tuKeyindex .~ (2)
                         & tuT .~ ('u')
                     )
                   )
                   ((fromRight undefined (validate ("AhY="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                   ),
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
                 ]
          )
      )
      (26)
  )

testObject_RTCConfiguration_user_19 :: RTCConfiguration
testObject_RTCConfiguration_user_19 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ((turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP)) :| [])
            ( ( turnUsername (secondsToNominalDiffTime (4.000000000000)) ("ku") & tuVersion .~ (3) & tuKeyindex .~ (2)
                  & tuT .~ ('\\')
              )
            )
            ((fromRight undefined (validate ("XA=="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                       :| [(turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("72s") & tuVersion .~ (2) & tuKeyindex .~ (1)
                         & tuT .~ ('\1019253')
                     )
                   )
                   ((fromRight undefined (validate ("9Q=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "227.193.165.231"))) (read "1") (Just TransportTCP))
                       :| [(turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "190.161.166.241"))) (read "0") (Just TransportUDP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("lis") & tuVersion .~ (1) & tuKeyindex .~ (2)
                         & tuT .~ ('\1001654')
                     )
                   )
                   ((fromRight undefined (validate ("Fg=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))
                       :| [(turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (0.000000000000)) ("kf3") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('(')
                     )
                   )
                   ((fromRight undefined (validate ("uw=="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "128.94.55.116"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("0") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('\DEL')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Nothing)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "149.38.204.147"))) (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("4u") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\176458')
                     )
                   )
                   ((fromRight undefined (validate ("Rf8="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing))
                       :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "227.57.188.192"))) (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "180.125.230.159"))) (read "1") (Nothing))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("fx") & tuVersion .~ (0) & tuKeyindex .~ (1)
                         & tuT .~ ('\EOT')
                     )
                   )
                   ((fromRight undefined (validate ("ubQ="))))
               )
             ]
      )
      ( Just
          ( ( sftServer
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
              :| [ ( sftServer
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
                 ]
          )
      )
      (20)
  )

testObject_RTCConfiguration_user_20 :: RTCConfiguration
testObject_RTCConfiguration_user_20 =
  ( rtcConfiguration
      ( ( rtcIceServer
            ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP))
                :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "47.244.178.201"))) (read "1") (Just TransportUDP)),
                     (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "184.150.97.196"))) (read "0") (Nothing))
                   ]
            )
            ( ( turnUsername (secondsToNominalDiffTime (2.000000000000)) ("w5") & tuVersion .~ (2) & tuKeyindex .~ (0)
                  & tuT .~ ('\178252')
              )
            )
            ((fromRight undefined (validate ("NQ=="))))
        )
          :| [ ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "175.240.38.214"))) (read "0") (Nothing)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "216.115.207.5"))) (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("q") & tuVersion .~ (1) & tuKeyindex .~ (1)
                         & tuT .~ ('@')
                     )
                   )
                   ((fromRight undefined (validate (""))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "231.15.14.20"))) (read "1") (Just TransportUDP)),
                            (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "214.144.196.143"))) (read "1") (Just TransportTCP)),
                            (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "106.250.39.172"))) (read "1") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("js") & tuVersion .~ (3) & tuKeyindex .~ (1)
                         & tuT .~ ('.')
                     )
                   )
                   ((fromRight undefined (validate ("fjs="))))
               ),
               ( rtcIceServer
                   ( (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))
                       :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "239.7.100.134"))) (read "0") (Just TransportUDP)),
                            (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))
                          ]
                   )
                   ( ( turnUsername (secondsToNominalDiffTime (1.000000000000)) ("jq") & tuVersion .~ (2) & tuKeyindex .~ (0)
                         & tuT .~ ('q')
                     )
                   )
                   ((fromRight undefined (validate ("NA=="))))
               )
             ]
      )
      (Nothing)
      (13)
  )
