module Wire.MockInterpreters.GalleyAPIAccess where

import Control.Lens (to, (^.))
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Role (Role (RoleOwner))
import Wire.GalleyAPIAccess

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Maybe TeamMember ->
  -- | what to return when calling GetAllFeatureConfigsForUser
  AllFeatureConfigs ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess mMember configs = interpret $ \case
  GetTeamMember _ _ -> pure mMember
  GetAllFeatureConfigsForUser _ -> pure configs
  MemberIsTeamOwner _ uid ->
    pure $ maybe False requestedUserIsOwner mMember
    where
      requestedUserIsOwner m = isSameUser m && isOwner m
      isSameUser m = m ^. userId == uid
      isOwner m = m ^. permissions . to permissionsRole == Just RoleOwner
  _ -> error "uninterpreted effect: GalleyAPIAccess"
