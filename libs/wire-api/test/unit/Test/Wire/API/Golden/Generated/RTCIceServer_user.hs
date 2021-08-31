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
          :| []
      )
      ( ( turnUsername (secondsToNominalDiffTime (3.000000000000)) ("a8kdffu4") & tuVersion .~ (5) & tuKeyindex .~ (24)
            & tuT .~ ('\SOH')
        )
      )
      ((fromRight undefined (validate ("d1VUzpxZ3TeM"))))
  )
