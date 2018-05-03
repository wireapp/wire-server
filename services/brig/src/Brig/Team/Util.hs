module Brig.Team.Util where  -- TODO: remove this module and move contents to Brig.IO.Intra?

import Brig.App
import Control.Lens
import Data.Id
import Galley.Types.Teams

import qualified Brig.IO.Intra as Intra

-- | True iff the user is member of a team *and* the only owner of that team.
isOnlyTeamOwner :: UserId -> AppIO Bool
isOnlyTeamOwner uid = maybe False (not . any ((/= uid) . (^. userId))) <$> Intra.getTeamOwners uid
