module Brig.API.Util where

import Brig.API.Handler
import Brig.Types
import Control.Monad
import Data.Id
import Data.Maybe
import Imports

import qualified Brig.Data.User as Data

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> Handler [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
    selfTeam <- lift $ Data.lookupUserTeam self
    return $ case selfTeam of
        Just team -> filter (\x -> profileTeam x == Just team) us
        Nothing   -> us
