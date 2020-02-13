-- | Ownership of unique user handles.
module Brig.User.Handle
  ( claimHandle,
    freeHandle,
    lookupHandle,
    glimpseHandle,
  )
where

import Brig.App
import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import Brig.Types.Common
import Brig.Types.User
import Brig.Unique
import Cassandra
import Data.Id
import Imports

claimHandle :: User -> Handle -> AppIO Bool
claimHandle u h = do
  owner <- lookupHandle h
  case owner of
    Just u' | userId u /= u' -> return False
    _ -> do
      env <- ask
      let key = "@" <> fromHandle h
      claimed <- withClaim (userId u) key (30 # Minute)
        $ runAppT env
        $ do
          -- Record ownership
          retry x5 $ write handleInsert (params Quorum (h, userId u))
          -- Update profile
          User.updateHandle (userId u) h
          -- Free old handle (if it changed)
          for_ (mfilter (/= h) (userHandle u)) $
            freeHandle u
      return (isJust claimed)

-- | Free a 'Handle', making it available to be claimed again.
freeHandle :: User -> Handle -> AppIO ()
freeHandle u h = do
  retry x5 $ write handleDelete (params Quorum (Identity h))
  let key = "@" <> fromHandle h
  deleteClaim (userId u) key (30 # Minute)

-- | Lookup the current owner of a 'Handle'.
lookupHandle :: Handle -> AppIO (Maybe UserId)
lookupHandle h =
  join . fmap runIdentity
    <$> retry x1 (query1 handleSelect (params Quorum (Identity h)))

-- | A weaker version of 'lookupHandle' that trades availability
-- (and potentially speed) for the possibility of returning stale data.
glimpseHandle :: Handle -> AppIO (Maybe UserId)
glimpseHandle h =
  join . fmap runIdentity
    <$> retry x1 (query1 handleSelect (params One (Identity h)))

--------------------------------------------------------------------------------
-- Queries

handleInsert :: PrepQuery W (Handle, UserId) ()
handleInsert = "INSERT INTO user_handle (handle, user) VALUES (?, ?)"

handleSelect :: PrepQuery R (Identity Handle) (Identity (Maybe UserId))
handleSelect = "SELECT user FROM user_handle WHERE handle = ?"

handleDelete :: PrepQuery W (Identity Handle) ()
handleDelete = "DELETE FROM user_handle WHERE handle = ?"
