module Wire.MockInterpreters.GalleyAPIAccess where

import Control.Lens (to, (^.))
import Data.Id
import Data.Map qualified as Map
import Data.Proxy
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.GalleyAPIAccess

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Map TeamId [TeamMember] ->
  -- | what to return when calling GetAllTeamFeaturesForUser
  AllTeamFeatures ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess teams configs = interpret $ \case
  CreateSelfConv _ -> error "CreateSelfConv not implemented in miniGalleyAPIAccess"
  GetConv _ _ -> error "GetConv not implemented in miniGalleyAPIAccess"
  GetTeamConv {} -> error "GetTeamConv not implemented in miniGalleyAPIAccess"
  NewClient _ _ -> error "NewClient not implemented in miniGalleyAPIAccess"
  CheckUserCanJoinTeam _ -> pure Nothing
  AddTeamMember {} -> error "AddTeamMember not implemented in miniGalleyAPIAccess"
  CreateTeam {} -> error "CreateTeam not implemented in miniGalleyAPIAccess"
  GetTeamMember uid tid -> pure $ getTeamMemberImpl teams uid tid
  GetTeamMembers _ -> error "GetTeamMembers not implemented in miniGalleyAPIAccess"
  GetTeamId _ -> error "GetTeamId not implemented in miniGalleyAPIAccess"
  GetTeam _ -> error "GetTeam not implemented in miniGalleyAPIAccess"
  GetTeamName _ -> error "GetTeamName not implemented in miniGalleyAPIAccess"
  GetTeamLegalHoldStatus _ -> error "GetTeamLegalHoldStatus not implemented in miniGalleyAPIAccess"
  GetUserLegalholdStatus _ _ -> error "GetUserLegalholdStatus not implemented in miniGalleyAPIAccess"
  GetTeamSearchVisibility _ -> error "GetTeamSearchVisibility not implemented in miniGalleyAPIAccess"
  ChangeTeamStatus {} -> error "ChangeTeamStatus not implemented in miniGalleyAPIAccess"
  MemberIsTeamOwner tid uid ->
    pure $ memberIsTeamOwnerImpl teams tid uid
  GetAllTeamFeaturesForUser _ -> pure configs
  GetFeatureConfigForTeam tid -> pure $ getFeatureConfigForTeamImpl configs tid
  GetVerificationCodeEnabled _ -> error "GetVerificationCodeEnabled not implemented in miniGalleyAPIAccess"
  GetExposeInvitationURLsToTeamAdmin _ -> pure ShowInvitationUrl
  IsMLSOne2OneEstablished _ _ -> error "IsMLSOne2OneEstablished not implemented in miniGalleyAPIAccess"
  UnblockConversation {} -> error "UnblockConversation not implemented in miniGalleyAPIAccess"
  GetEJPDConvInfo _ -> error "GetEJPDConvInfo not implemented in miniGalleyAPIAccess"
  GetTeamAdmins tid -> pure $ newTeamMemberList (maybe [] (filter (\tm -> isAdminOrOwner (tm ^. permissions))) $ Map.lookup tid teams) ListComplete

getFeatureConfigForTeamImpl :: forall feature. (IsFeatureConfig feature) => AllTeamFeatures -> TeamId -> LockableFeature feature
getFeatureConfigForTeamImpl allfeatures _ = npProject' (Proxy @(feature)) allfeatures

memberIsTeamOwnerImpl :: Map TeamId [TeamMember] -> TeamId -> UserId -> Bool
memberIsTeamOwnerImpl teams tid uid =
  case getTeamMemberImpl teams uid tid of
    Nothing -> False
    Just mem -> mem ^. userId == uid && mem ^. permissions . to isAdminOrOwner

getTeamMemberImpl :: Map TeamId [TeamMember] -> UserId -> TeamId -> Maybe TeamMember
getTeamMemberImpl teams uid tid = do
  allMembers <- Map.lookup tid teams
  find (\m -> m ^. userId == uid) allMembers
