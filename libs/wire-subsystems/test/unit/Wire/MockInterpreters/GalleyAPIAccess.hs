module Wire.MockInterpreters.GalleyAPIAccess where

import Data.Id
import Data.Proxy
import Imports
import Polysemy
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.GalleyAPIAccess

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Maybe TeamMember ->
  -- | what to return when calling GetAllTeamFeaturesForUser
  AllTeamFeatures ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess member configs = interpret $ \case
  CreateSelfConv _ -> error "CreateSelfConv not implemented in miniGalleyAPIAccess"
  GetConv _ _ -> error "GetConv not implemented in miniGalleyAPIAccess"
  GetTeamConv _ _ _ -> error "GetTeamConv not implemented in miniGalleyAPIAccess"
  NewClient _ _ -> error "NewClient not implemented in miniGalleyAPIAccess"
  CheckUserCanJoinTeam _ -> error "CheckUserCanJoinTeam not implemented in miniGalleyAPIAccess"
  AddTeamMember _ _ _ _ -> error "AddTeamMember not implemented in miniGalleyAPIAccess"
  CreateTeam _ _ _ -> error "CreateTeam not implemented in miniGalleyAPIAccess"
  GetTeamMember _ _ -> pure member
  GetTeamMembers _ -> error "GetTeamMembers not implemented in miniGalleyAPIAccess"
  GetTeamId _ -> error "GetTeamId not implemented in miniGalleyAPIAccess"
  GetTeam _ -> error "GetTeam not implemented in miniGalleyAPIAccess"
  GetTeamName _ -> error "GetTeamName not implemented in miniGalleyAPIAccess"
  GetTeamLegalHoldStatus _ -> error "GetTeamLegalHoldStatus not implemented in miniGalleyAPIAccess"
  GetUserLegalholdStatus _ _ -> error "GetUserLegalholdStatus not implemented in miniGalleyAPIAccess"
  GetTeamSearchVisibility _ -> error "GetTeamSearchVisibility not implemented in miniGalleyAPIAccess"
  ChangeTeamStatus _ _ _ -> error "ChangeTeamStatus not implemented in miniGalleyAPIAccess"
  MemberIsTeamOwner _ _ -> error "MemberIsTeamOwner not implemented in miniGalleyAPIAccess"
  GetAllTeamFeaturesForUser _ -> pure configs
  GetFeatureConfigForTeam tid -> pure $ getFeatureConfigForTeamImpl configs tid
  GetVerificationCodeEnabled _ -> error "GetVerificationCodeEnabled not implemented in miniGalleyAPIAccess"
  GetExposeInvitationURLsToTeamAdmin _ -> error "GetExposeInvitationURLsToTeamAdmin not implemented in miniGalleyAPIAccess"
  IsMLSOne2OneEstablished _ _ -> error "IsMLSOne2OneEstablished not implemented in miniGalleyAPIAccess"
  UnblockConversation _ _ _ -> error "UnblockConversation not implemented in miniGalleyAPIAccess"
  GetEJPDConvInfo _ -> error "GetEJPDConvInfo not implemented in miniGalleyAPIAccess"

getFeatureConfigForTeamImpl :: forall feature. (IsFeatureConfig feature) => AllTeamFeatures -> TeamId -> LockableFeature feature
getFeatureConfigForTeamImpl allfeatures _ = npProject' (Proxy @(feature)) allfeatures
