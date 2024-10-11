module Galley.API.Teams.Export (getUserRecord) where

import Control.Lens (view, (^.))
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.SparAccess qualified as Spar
import Imports
import Polysemy
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.Export
import Wire.API.Team.Member
import Wire.API.User (ScimUserInfo (suiCreatedOn), User (..))

getUserRecord ::
  ( Member (ErrorS TeamMemberNotFound) r,
    Member BrigAccess r,
    Member Spar.SparAccess r
  ) =>
  TeamMember ->
  Sem r TeamExportUser
getUserRecord member = do
  let uid = member ^. userId
  export <- getUserExportData uid >>= noteS @TeamMemberNotFound
  mCreatedOn <- do
    let mFromInvitation = snd <$> member ^. invitation
    case mFromInvitation of
      Just ts -> pure $ Just ts
      Nothing -> do
        -- TODO: make this a single user query
        suis <- Spar.lookupScimUserInfos [uid]
        pure $ listToMaybe suis >>= suiCreatedOn
  -- TODO: optimize!
  let mInviterId = fst <$> member ^. invitation
  users <- getUsers (maybeToList mInviterId)
  let invitedBy = listToMaybe users >>= userHandle
  pure
    export
      { tExportInvitedBy = invitedBy,
        tExportRole = permissionsRole . view permissions $ member,
        tExportCreatedOn = mCreatedOn
      }
