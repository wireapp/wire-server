module Galley.API.Teams.Export (getUserRecord) where

import Control.Lens ((^.))
import Galley.Effects
import Galley.Effects.BrigAccess
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Export
import Wire.API.Team.Member

getUserRecord ::
  ( Member (ErrorS TeamMemberNotFound) r,
    Member BrigAccess r
  ) =>
  TeamMember ->
  Sem r TeamExportUser
getUserRecord member = do
  let uid = member ^. userId
  export <- getUserExportData uid >>= noteS @TeamMemberNotFound
  -- TODO
  pure export
