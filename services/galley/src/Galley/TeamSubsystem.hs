module Galley.TeamSubsystem where

import Galley.Effects.TeamStore (TeamStore)
import Galley.Effects.TeamStore qualified as E
import Imports
import Polysemy
import Wire.API.Team.HardTruncationLimit
import Wire.TeamSubsystem

-- This interpreter exists so galley code doesn't end up depending on
-- GalleyAPIAccess, while it is possible to implement that, it'd add unnecesary
-- HTTP calls for tiny things.
--
-- When we actually implement TeamSubsystem this can move to wire-subsystem.
-- Moving this to wire-subsystem before that would be too much work as the Store
-- effects in galley are not as thin as we're doing them in wire-subsystems.
-- They also depend on entire galley env.
interpretTeamSubsystem :: (Member TeamStore r) => InterpreterFor TeamSubsystem r
interpretTeamSubsystem = interpret $ \case
  InternalGetTeamMember userId teamId -> E.getTeamMember teamId userId
  InternalGetTeamMembers teamId maxResults ->
    E.getTeamMembersWithLimit teamId $ fromMaybe hardTruncationLimitRange maxResults
