module Wire.IdPSubsystem.Interpreter (interpretIdPSubsystem) where

import Control.Lens
import Data.HavePendingInvitations
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.User (User, userId, userManagedBy, userSSOId)
import Wire.API.User.IdentityProvider qualified as IP
import Wire.API.User.Profile (ManagedBy (..))
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.IdPConfigStore
import Wire.IdPSubsystem

-- TODO: Use RateLimit effect for rate limiting
interpretIdPSubsystem ::
  ( Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member IdPConfigStore r
  ) =>
  InterpreterFor IdPSubsystem r
interpretIdPSubsystem = interpret $ \case
  GetSsoCodeByEmail mbHost email -> do
    -- TODO: Rate limiting
    -- TODO: Disable on cloud
    users <- getUsersByVariousKeys [] [] [email] NoPendingInvitations
    mbIdPId <- case users of
      [user] -> do
        -- Ensure: the user was created via SCIM or SSO
        if not (isScimOrSsoUser user)
          then pure Nothing
          else do
            mbTeam <- getTeamId (userId user)
            case mbTeam of
              Just team -> do
                idps <- getConfigsByTeam team
                case mbHost of
                  Just host -> pure $ findIdPByDomain host idps
                  Nothing -> pure Nothing
              Nothing -> pure Nothing
      _ -> pure Nothing
    pure mbIdPId

isScimOrSsoUser :: User -> Bool
isScimOrSsoUser user =
  userManagedBy user == ManagedByScim && isJust (userSSOId user)

findIdPByDomain :: Text -> [IP.IdP] -> Maybe SAML.IdPId
findIdPByDomain host idps = do
  idp <- find (\idp -> (idp ^. SAML.idpExtraInfo . IP.domain) == Just host) idps
  pure $ idp ^. SAML.idpId
