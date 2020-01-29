module Network.Federation.Util.Internal where

import Imports
import Network.DNS.Resolver
import Network.DNS.Types
import Network.DNS
import System.Random
import Data.Text.Encoding (encodeUtf8)

-- Given a prefix (e.g. _wire) and a domain (e.g. wire.com),
-- provides a list of A(AAA) names and port numbers upon a successful
-- DNS-SRV request, or `Nothing' if the DNS-SRV request failed.
-- Modified version inspired from http://hackage.haskell.org/package/pontarius-xmpp
srvLookup' :: Text -> Text -> ResolvSeed -> IO (Maybe [(Domain, Word16)])
srvLookup' prefix realm resolvSeed = withResolver resolvSeed $ \resolver -> do
    srvResult <- lookupSRV resolver $ encodeUtf8 $ prefix <> "._tcp." <> realm <> "."
    case srvResult of
        Right [] -> return Nothing
        Right [(_, _, _, ".")] -> return Nothing -- explicit "not available"
        Right srvResult' -> do
            -- Get [(Domain, PortNumber)] of SRV request, if any.
            -- Sorts the records based on the priority value.
            orderedSrvResult <- orderSrvResult srvResult'
            return $ Just $ fmap (\(_, _, port, domain) -> (domain, port)) orderedSrvResult
        -- The service is not available at this domain.
        Left _ -> return Nothing

-- FUTUREWORK: maybe improve sorting algorithm here?
--
-- This function orders the SRV result in accordance with RFC
-- 2782. It sorts the SRV results in order of priority, and then
-- uses a random process to order the records with the same
-- priority based on their weight.
--
-- Taken from http://hackage.haskell.org/package/pontarius-xmpp (BSD licence)
orderSrvResult :: [(Word16, Word16, Word16, Domain)] -> IO [(Word16, Word16, Word16, Domain)]
orderSrvResult srvResult = do
    -- Order the result set by priority.
    let srvResult' = sortBy (comparing (\(priority, _, _, _) -> priority)) srvResult
    -- Group elements in sublists based on their priority. The
    -- type is `[[(Word16, Word16, Word16, Domain)]]'.
    let srvResult'' = groupBy (\(priority, _, _, _) (priority', _, _, _) -> priority == priority') srvResult' :: [[(Word16, Word16, Word16, Domain)]]
    -- For each sublist, put records with a weight of zero first.
    let srvResult''' = map (\sublist -> let (a, b) = partition (\(_, weight, _, _) -> weight == 0) sublist in concat [a, b]) srvResult''
    -- Order each sublist.
    srvResult'''' <- mapM orderSublist srvResult'''
    -- Concatenate the results.
    return $ concat srvResult''''
  where
    orderSublist :: [(Word16, Word16, Word16, Domain)] -> IO [(Word16, Word16, Word16, Domain)]
    orderSublist [] = return []
    orderSublist sublist = do
        -- Compute the running sum, as well as the total sum of
        -- the sublist. Add the running sum to the SRV tuples.
        let (total, sublist') = mapAccumL (\total' (priority, weight, port, domain) -> (total' + weight, (priority, weight, port, domain, total' + weight))) 0 sublist
        -- Choose a random number between 0 and the total sum
        -- (inclusive).
        randomNumber <- randomRIO (0, total)
        -- Select the first record with its running sum greater
        -- than or equal to the random number.
        let (beginning, ((priority, weight, port, domain, _):end)) = break (\(_, _, _, _, running) -> randomNumber <= running) sublist'
        -- Remove the running total number from the remaining
        -- elements.
        let sublist'' = map (\(priority', weight', port', domain', _) -> (priority', weight', port', domain')) (concat [beginning, end])
        -- Repeat the ordering procedure on the remaining
        -- elements.
        rest <- orderSublist sublist''
        return $ ((priority, weight, port, domain):rest)
