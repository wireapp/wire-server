module Brig.User.API.Handle (getHandleInfo, filterHandleResults) where

import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import Brig.App (settings, viewFederationDomain)
import qualified Brig.Data.User as Data
import qualified Brig.Federation.Client as Federation
import Brig.Options (searchSameTeamOnly)
import Control.Lens (view)
import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Qualified (Qualified (..))
import Imports
import qualified System.Logger.Class as Log
import qualified Wire.API.User as Public

-- FUTUREWORK: use 'runMaybeT' to simplify this.
getHandleInfo :: UserId -> Qualified Handle -> Handler (Maybe Public.UserProfile)
getHandleInfo self handle = do
  domain <- viewFederationDomain
  if qDomain handle == domain
    then getLocalHandleInfo domain
    else getRemoteHandleInfo
  where
    getLocalHandleInfo domain = do
      Log.info $ Log.msg $ Log.val "getHandleInfo - local lookup"
      maybeOwnerId <- lift $ API.lookupHandle (qUnqualified handle)
      case maybeOwnerId of
        Nothing -> return Nothing
        Just ownerId -> do
          ownerProfile <- lift $ API.lookupProfile self (Qualified ownerId domain)
          owner <- filterHandleResults self (maybeToList ownerProfile)
          return $ listToMaybe owner
    getRemoteHandleInfo = do
      Log.info $ Log.msg (Log.val "getHandleInfo - remote lookup") Log.~~ Log.field "domain" (show (qDomain handle))
      Federation.getUserHandleInfo handle

-- | Checks search permissions and filters accordingly
filterHandleResults :: UserId -> [Public.UserProfile] -> Handler [Public.UserProfile]
filterHandleResults searchingUser us = do
  sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
  if sameTeamSearchOnly
    then do
      fromTeam <- lift $ Data.lookupUserTeam searchingUser
      return $ case fromTeam of
        Just team -> filter (\x -> Public.profileTeam x == Just team) us
        Nothing -> us
    else return us
