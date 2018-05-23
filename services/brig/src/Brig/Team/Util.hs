{-# LANGUAGE ViewPatterns #-}

module Brig.Team.Util where  -- TODO: remove this module and move contents to Brig.IO.Intra?

import Brig.App
import Control.Lens
import Data.Id
import Galley.Types.Teams

import qualified Brig.IO.Intra as Intra
import qualified Data.Set as Set

data IsOnlyTeamOwner = IsOnlyTeamOwner | IsOneOfManyTeamOwners | IsNotTeamOwner | NoTeamOwnersAreLeft
  deriving (Eq, Show, Bounded, Enum)

-- | A team owner is a team member with full permissions *and* an email address.
isOnlyTeamOwner :: UserId -> TeamId -> AppIO IsOnlyTeamOwner
isOnlyTeamOwner uid tid = isOnlyTeamOwner' uid . fmap (^. userId) <$> Intra.getTeamOwners tid

isOnlyTeamOwner' :: UserId -> [UserId] -> IsOnlyTeamOwner
isOnlyTeamOwner' _ [] = NoTeamOwnersAreLeft
isOnlyTeamOwner' uid (Set.fromList -> owners)
    | uid `Set.notMember` owners       = IsNotTeamOwner
    | Set.null (Set.delete uid owners) = IsOnlyTeamOwner
    | otherwise                        = IsOneOfManyTeamOwners
