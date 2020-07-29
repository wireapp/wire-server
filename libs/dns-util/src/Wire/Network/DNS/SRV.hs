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

toSrvEntry :: (Word16, Word16, Word16, Domain) -> SrvEntry
toSrvEntry (prio, weight, port, domain) = SrvEntry prio weight (SrvTarget domain port)

-- FUTUREWORK: maybe improve sorting algorithm here? (with respect to performance and code style)
--
-- This function orders the SRV result in accordance with RFC
-- 2782. It sorts the SRV results in order of priority, and then
-- uses a random process to order the records with the same
-- priority based on their weight.
--
-- Taken from http://hackage.haskell.org/package/pontarius-xmpp (BSD3 licence) and refactored.
orderSrvResult :: [SrvEntry] -> IO [SrvEntry]
orderSrvResult =
  -- Order the result set by priority.
  sortBy (comparing srvPriority)
    -- Group elements in sublists based on their priority.
    -- The result type is `[[(Word16, Word16, Word16, Domain)]]' (nested list).
    >>> groupBy ((==) `on` srvPriority)
    -- For each sublist, put records with a weight of zero first.
    >>> map (uncurry (++) . partition ((== 0) . srvWeight))
    -- Order each sublist.
    >>> mapM orderSublist
    -- Concatenate the results.
    >>> fmap concat
  where
    orderSublist :: [SrvEntry] -> IO [SrvEntry]
    orderSublist [] = return []
    orderSublist sublist = do
      -- Compute the running sum, as well as the total sum of the sublist.
      -- Add the running sum to the SRV tuples.
      let (total, sublistWithRunning) =
            mapAccumL (\acc srv -> let acc' = acc + srvWeight srv in (acc', (srv, acc'))) 0 sublist
      -- Choose a random number between 0 and the total sum (inclusive).
      randomNumber <- randomRIO (0, total)
      -- Select the first record with its running sum greater
      -- than or equal to the random number.
      let (beginning, (firstSrv, _), end) =
            case break (\(_, running) -> randomNumber <= running) sublistWithRunning of
              (b, (c : e)) -> (b, c, e)
              _ -> error "orderSrvResult: no record with running sum greater than random number"
      -- Remove the running total number from the remaining elements.
      let remainingSrvs = map (\(srv, _) -> srv) (concat [beginning, end])
      -- Repeat the ordering procedure on the remaining elements.
      rest <- orderSublist remainingSrvs
      return $ firstSrv : rest
