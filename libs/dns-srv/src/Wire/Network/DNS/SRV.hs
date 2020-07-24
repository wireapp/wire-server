-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Network.DNS.SRV where

import Data.List.NonEmpty
import Imports
import Network.DNS (DNSError, Domain)

data SrvEntry = SrvEntry
  { srvPriority :: !Word16,
    srvWeight :: !Word16,
    srvTarget :: !SrvTarget
  }
  deriving (Eq, Show)

data SrvTarget = SrvTarget
  { -- | the hostname on which the service is offered
    srvTargetDomain :: !Domain,
    -- | the port on which the service is offered
    srvTargetPort :: !Word16
  }
  deriving (Eq, Show)

data SrvResponse
  = SrvNotAvailable
  | SrvAvailable (NonEmpty SrvEntry)
  | SrvResponseError DNSError
  deriving (Eq, Show)

interpretResponse :: Either DNSError [(Word16, Word16, Word16, Domain)] -> SrvResponse
interpretResponse = \case
  Left err -> SrvResponseError err
  Right [] -> SrvNotAvailable
  Right [(_, _, _, ".")] -> SrvNotAvailable -- According to RFC2782
  Right (r : rs) -> SrvAvailable $ fmap toSrvEntry (r :| rs)
  where
    toSrvEntry :: (Word16, Word16, Word16, Domain) -> SrvEntry
    toSrvEntry (prio, weight, port, domain) = SrvEntry prio weight (SrvTarget domain port)
