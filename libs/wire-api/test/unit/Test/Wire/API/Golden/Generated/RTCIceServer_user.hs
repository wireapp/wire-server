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
module Test.Wire.API.Golden.Generated.RTCIceServer_user where

import Control.Lens ((.~))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc (IpAddr (IpAddr))
import Data.Text.Ascii (AsciiChars (validate))
import Data.Time (secondsToNominalDiffTime)
import Imports (Maybe (Just, Nothing), fromRight, read, undefined, (&))
import Wire.API.Call.Config
  ( RTCIceServer,
    Scheme (SchemeTurn, SchemeTurns),
    Transport (TransportTCP, TransportUDP),
    TurnHost (TurnHostIp, TurnHostName),
    rtcIceServer,
    tuKeyindex,
    tuT,
    tuVersion,
    turnURI,
    turnUsername,
  )

testObject_RTCIceServer_user_1 :: RTCIceServer
testObject_RTCIceServer_user_1 =
  ( rtcIceServer
      ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "118.129.179.126"))) (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "161.156.122.7"))) (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "125.103.68.5"))) (read "1") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (38.000000000000)) ("6vgzfba") & tuVersion .~ (4) & tuKeyindex .~ (24)
            & tuT .~ ('\DC1')
        )
      )
      ((fromRight undefined (validate ("ZtBPgUaUYg=="))))
  )

testObject_RTCIceServer_user_2 :: RTCIceServer
testObject_RTCIceServer_user_2 =
  ( rtcIceServer
      ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "108.37.81.160"))) (read "0") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "147.240.166.49"))) (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "242.214.187.48"))) (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "228.51.14.158"))) (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("a8kdffu4") & tuVersion .~ (5) & tuKeyindex .~ (24)
            & tuT .~ ('\SOH')
        )
      )
      ((fromRight undefined (validate ("d1VUzpxZ3TeM"))))
  )

testObject_RTCIceServer_user_3 :: RTCIceServer
testObject_RTCIceServer_user_3 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "72.203.31.79"))) (read "2") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (133.000000000000)) ("l9mr") & tuVersion .~ (9) & tuKeyindex .~ (16)
            & tuT .~ ('^')
        )
      )
      ((fromRight undefined (validate ("jw=="))))
  )

testObject_RTCIceServer_user_4 :: RTCIceServer
testObject_RTCIceServer_user_4 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "37.223.75.78"))) (read "1") (Just TransportUDP))
          :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Nothing))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (90.000000000000)) ("rcz") & tuVersion .~ (3) & tuKeyindex .~ (12)
            & tuT .~ (')')
        )
      )
      ((fromRight undefined (validate ("jHf3PRvglQ=="))))
  )

testObject_RTCIceServer_user_5 :: RTCIceServer
testObject_RTCIceServer_user_5 =
  ( rtcIceServer
      ( (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "103.196.217.16"))) (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (178.000000000000)) ("gwihax") & tuVersion .~ (0) & tuKeyindex .~ (30)
            & tuT .~ ('r')
        )
      )
      ((fromRight undefined (validate ("9+ems/6Fv0M="))))
  )

testObject_RTCIceServer_user_6 :: RTCIceServer
testObject_RTCIceServer_user_6 =
  ( rtcIceServer
      ((turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "196.170.89.170"))) (read "1") (Just TransportUDP)) :| [])
      ( ( turnUsername (secondsToNominalDiffTime (231.000000000000)) ("z2j8s7exhz") & tuVersion .~ (9) & tuKeyindex .~ (4)
            & tuT .~ ('b')
        )
      )
      ((fromRight undefined (validate ("b76sRlk="))))
  )

testObject_RTCIceServer_user_7 :: RTCIceServer
testObject_RTCIceServer_user_7 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "007.com") (read "0") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "123") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (14.000000000000)) ("f4x") & tuVersion .~ (4) & tuKeyindex .~ (2)
            & tuT .~ ('\1006244')
        )
      )
      ((fromRight undefined (validate ("ft8iuA=="))))
  )

testObject_RTCIceServer_user_8 :: RTCIceServer
testObject_RTCIceServer_user_8 =
  ( rtcIceServer
      ( (turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "89.93.38.11"))) (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "83.231.206.233"))) (read "1") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "57.223.15.28"))) (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "85.102.29.140"))) (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "95.11.213.169"))) (read "1") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (121.000000000000)) ("wbfglz") & tuVersion .~ (6) & tuKeyindex .~ (18)
            & tuT .~ ('\132671')
        )
      )
      ((fromRight undefined (validate ("6eRp29c2znw/"))))
  )

testObject_RTCIceServer_user_9 :: RTCIceServer
testObject_RTCIceServer_user_9 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "248.142.46.235"))) (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "12.224.132.41"))) (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "234.126.43.235"))) (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "221.57.91.255"))) (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "007.com") (read "1") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (137.000000000000)) ("9jwt1") & tuVersion .~ (10) & tuKeyindex .~ (31)
            & tuT .~ ('0')
        )
      )
      ((fromRight undefined (validate ("nzZx"))))
  )

testObject_RTCIceServer_user_10 :: RTCIceServer
testObject_RTCIceServer_user_10 =
  ( rtcIceServer
      ((turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Nothing)) :| [])
      ( ( turnUsername (secondsToNominalDiffTime (225.000000000000)) ("ywvp0wy") & tuVersion .~ (5) & tuKeyindex .~ (17)
            & tuT .~ ('\1091223')
        )
      )
      ((fromRight undefined (validate ("BaDPJOg="))))
  )

testObject_RTCIceServer_user_11 :: RTCIceServer
testObject_RTCIceServer_user_11 =
  ( rtcIceServer
      ((turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)) :| [])
      ( ( turnUsername (secondsToNominalDiffTime (82.000000000000)) ("um7wu") & tuVersion .~ (10) & tuKeyindex .~ (2)
            & tuT .~ ('\1061785')
        )
      )
      ((fromRight undefined (validate ("+DJTWlqizw=="))))
  )

testObject_RTCIceServer_user_12 :: RTCIceServer
testObject_RTCIceServer_user_12 =
  ( rtcIceServer
      ( (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "195.42.42.234"))) (read "0") (Nothing))
          :| [ (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (195.000000000000)) ("50buvxyh") & tuVersion .~ (7) & tuKeyindex .~ (8)
            & tuT .~ ('6')
        )
      )
      ((fromRight undefined (validate ("Bhm/vUAzRQ=="))))
  )

testObject_RTCIceServer_user_13 :: RTCIceServer
testObject_RTCIceServer_user_13 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "159.6.46.79"))) (read "1") (Nothing))
          :| [ (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "235.192.126.115"))) (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "007.com") (read "1") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (98.000000000000)) ("dur1kszw") & tuVersion .~ (10) & tuKeyindex .~ (17)
            & tuT .~ ('\176036')
        )
      )
      ((fromRight undefined (validate ("y0JeDGJm"))))
  )

testObject_RTCIceServer_user_14 :: RTCIceServer
testObject_RTCIceServer_user_14 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "33.104.116.89"))) (read "0") (Just TransportTCP))
          :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "132.193.82.132"))) (read "2") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "78.0.56.210"))) (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (129.000000000000)) ("5tjy") & tuVersion .~ (0) & tuKeyindex .~ (4)
            & tuT .~ ('/')
        )
      )
      ((fromRight undefined (validate ("06HqENnz"))))
  )

testObject_RTCIceServer_user_15 :: RTCIceServer
testObject_RTCIceServer_user_15 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "24.131.95.46"))) (read "0") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "72.121.208.204"))) (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "127.13.25.233"))) (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "123") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "240.77.56.253"))) (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "48.254.19.119"))) (read "2") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (238.000000000000)) ("ws") & tuVersion .~ (1) & tuKeyindex .~ (27)
            & tuT .~ ('v')
        )
      )
      ((fromRight undefined (validate ("uxE="))))
  )

testObject_RTCIceServer_user_16 :: RTCIceServer
testObject_RTCIceServer_user_16 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "143.97.234.91"))) (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "175.19.223.168"))) (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "007.com") (read "2") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (54.000000000000)) ("d0s3a") & tuVersion .~ (4) & tuKeyindex .~ (11)
            & tuT .~ ('\53466')
        )
      )
      ((fromRight undefined (validate ("Mw=="))))
  )

testObject_RTCIceServer_user_17 :: RTCIceServer
testObject_RTCIceServer_user_17 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "123") (read "2") (Just TransportTCP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "169.70.61.106"))) (read "0") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "2") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "40.231.247.70"))) (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "2") (Nothing)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "164.220.43.156"))) (read "1") (Nothing))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (81.000000000000)) ("5nxg") & tuVersion .~ (8) & tuKeyindex .~ (30)
            & tuT .~ ('}')
        )
      )
      ((fromRight undefined (validate ("f8wvNtPhU94A"))))
  )

testObject_RTCIceServer_user_18 :: RTCIceServer
testObject_RTCIceServer_user_18 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Nothing))
          :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "host.name") (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "2") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "184.63.122.24"))) (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Nothing)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "117.242.195.170"))) (read "0") (Just TransportTCP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (24.000000000000)) ("jjx53c65i") & tuVersion .~ (7) & tuKeyindex .~ (5)
            & tuT .~ ('\\')
        )
      )
      ((fromRight undefined (validate ("mQ=="))))
  )

testObject_RTCIceServer_user_19 :: RTCIceServer
testObject_RTCIceServer_user_19 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostName "host.name") (read "0") (Just TransportUDP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "007.com") (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "host.name") (read "1") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostName "123") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "169.100.145.5"))) (read "0") (Just TransportTCP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "221.45.182.44"))) (read "2") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostName "a-c") (read "1") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (249.000000000000)) ("a7hqal0n") & tuVersion .~ (5) & tuKeyindex .~ (29)
            & tuT .~ ('\51751')
        )
      )
      ((fromRight undefined (validate ("eA5FI9S6Ow=="))))
  )

testObject_RTCIceServer_user_20 :: RTCIceServer
testObject_RTCIceServer_user_20 =
  ( rtcIceServer
      ( (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "119.72.79.226"))) (read "2") (Just TransportUDP))
          :| [ (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP)),
               (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "165.153.82.29"))) (read "2") (Just TransportTCP)),
               (turnURI (SchemeTurns) (TurnHostName "a-c") (read "1") (Just TransportUDP)),
               (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "233.158.218.205"))) (read "2") (Just TransportUDP))
             ]
      )
      ( ( turnUsername (secondsToNominalDiffTime (119.000000000000)) ("hdgi3apt") & tuVersion .~ (3) & tuKeyindex .~ (30)
            & tuT .~ ('&')
        )
      )
      ((fromRight undefined (validate ("o3sXZAOB"))))
  )
