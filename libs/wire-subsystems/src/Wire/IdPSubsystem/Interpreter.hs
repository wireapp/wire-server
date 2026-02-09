module Wire.IdPSubsystem.Interpreter
  ( interpretIdPSubsystem,
  )
where

import Control.Lens
import Data.Domain (domainText)
import Data.HavePendingInvitations (HavePendingInvitations (..))
import Data.Id
import Data.Qualified
import Data.Text qualified as Text
import Imports
import Polysemy
import Polysemy.Error
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Wire.API.User
import Wire.API.User.IdentityProvider qualified as IP
import Wire.BrigAPIAccess
import Wire.GalleyAPIAccess
import Wire.IdPConfigStore
import Wire.IdPSubsystem
import Wire.Sem.Logger (Logger)
import Wire.Sem.Logger qualified as Logger

-- TODO: Use RateLimit effect for rate limiting
interpretIdPSubsystem ::
  ( Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member IdPConfigStore r,
    Member (Error IdPSubsystemError) r,
    Member (Logger (Log.Msg -> Log.Msg)) r
  ) =>
  Bool ->
  InterpreterFor IdPSubsystem r
interpretIdPSubsystem enableIdPByEmailDiscovery = interpret $ \case
  GetSsoCodeByEmail mbHost email -> do
    if not enableIdPByEmailDiscovery
      then pure Nothing
      else do
        -- TODO: Rate limiting
        users <- getUsersByVariousKeys [] [] [email] NoPendingInvitations
        case users of
          [] -> pure Nothing
          [user] -> do
            if not (isScimOrSsoUser user)
              then pure Nothing
              else do
                mbTeam <- getTeamId (userId user)
                case mbTeam of
                  Just team -> do
                    idps <- getConfigsByTeam team
                    pure $ selectIdP mbHost idps
                  Nothing -> pure Nothing
          tooMaybeUsers -> do
            Logger.warn $
              Log.msg @Text "Multiple users found for email address in getSsoCodeByEmail"
                . Log.field "email" (fromEmail email)
                . Log.field "user_ids" (Text.intercalate ", " (map (userIdToText . userQualifiedId) tooMaybeUsers))
            throw InconsistentUsers

userIdToText :: Qualified UserId -> Text
userIdToText uid = idToText (qUnqualified uid) <> "@" <> domainText (qDomain uid)

selectIdP :: Maybe Text -> [IP.IdP] -> Maybe SAML.IdPId
selectIdP mbHost idps = case idps of
  -- No IdPs: no match
  [] -> Nothing
  -- Exactly one IdP: always return it
  [idp] -> pure (idp ^. SAML.idpId)
  -- Multiple IdPs: find by domain if host provided
  _idps' -> case mbHost of
    Just host -> findIdPByDomain host idps
    Nothing -> Nothing

isScimOrSsoUser :: User -> Bool
isScimOrSsoUser user =
  userManagedBy user == ManagedByScim && isJust (userSSOId user)

findIdPByDomain :: Text -> [IP.IdP] -> Maybe SAML.IdPId
findIdPByDomain host idps = do
  idp <- find (\idp -> (idp ^. SAML.idpExtraInfo . IP.domain) == Just host) idps
  pure $ idp ^. SAML.idpId
