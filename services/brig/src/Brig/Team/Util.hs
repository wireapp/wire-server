module Brig.Team.Util where

import Brig.App
import Control.Lens
import Data.Id
import Galley.Types.Teams

import qualified Brig.IO.Intra as Intra

isOnlyTeamOwner :: UserId -> AppIO Bool
isOnlyTeamOwner uid = do
    contacts <- Intra.getTeamContacts uid
    return $ case contacts of
        Just mems | isOnlyOwner uid (mems^.teamMembers) -> True
        _                                               -> False
