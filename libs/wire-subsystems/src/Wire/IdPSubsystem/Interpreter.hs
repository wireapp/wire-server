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
import Data.UUID qualified as UUID
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
  GetSsoCodeByEmail mbHost email -> getSsoCodeByEmailImpl enableIdPByEmailDiscovery mbHost email

getSsoCodeByEmailImpl ::
  ( Member (Logger (Log.Msg -> Log.Msg)) r,
    Member (Error IdPSubsystemError) r,
    Member BrigAPIAccess r,
    Member GalleyAPIAccess r,
    Member IdPConfigStore r
  ) =>
  Bool -> Maybe Text -> EmailAddress -> Sem r (Maybe SAML.IdPId)
getSsoCodeByEmailImpl enableIdPByEmailDiscovery mbHost email =
  do
    if not enableIdPByEmailDiscovery
      then pure Nothing
      else do
        users <- getUsersByVariousKeys [] [] [email] NoPendingInvitations
        case users of
          [] -> pure Nothing
          [user] -> do
            if isScimOrSsoUser user
              then do
                mbTeam <- getTeamId (userId user)
                case mbTeam of
                  Just team -> do
                    idps <- getConfigsByTeam team
                    selectIdP idps
                  Nothing -> pure Nothing
              else pure Nothing
          tooManyUsers -> do
            Logger.warn $
              Log.msg @Text "Multiple users found for email address in getSsoCodeByEmail"
                . Log.field "email" (fromEmail email)
                . Log.field "user_ids" (Text.intercalate ", " (map (userIdToText . userQualifiedId) tooManyUsers))
            throw InconsistentUsers
  where
    userIdToText :: Qualified UserId -> Text
    userIdToText uid = idToText (qUnqualified uid) <> "@" <> domainText (qDomain uid)

    selectIdP :: (Member (Logger (Log.Msg -> Log.Msg)) r) => [IP.IdP] -> Sem r (Maybe SAML.IdPId)
    selectIdP idps = case idps of
      -- No IdPs: no match
      [] -> pure Nothing
      -- Exactly one IdP: always return it
      [idp] -> (pure . pure) (idp ^. SAML.idpId)
      -- Multiple IdPs: find by domain if host provided
      _idps' -> findIdPByDomain idps

    isScimOrSsoUser :: User -> Bool
    isScimOrSsoUser user =
      userManagedBy user == ManagedByScim && isJust (userSSOId user)

    findIdPByDomain :: (Member (Logger (Log.Msg -> Log.Msg)) r) => [IP.IdP] -> Sem r (Maybe SAML.IdPId)
    findIdPByDomain idps = do
      let matches :: [SAML.IdPId] =
            SAML._idpId
              <$> filter (\idp -> (idp ^. SAML.idpExtraInfo . IP.domain) == mbHost) idps
      when (length matches > 1) $
        Logger.warn $
          Log.msg @Text "Found more than one IdP config for domain"
            . Log.field "domain" (fromMaybe "None" mbHost)
            . Log.field "idpIds" (intercalate "," $ (UUID.toString . SAML.fromIdPId) <$> matches)
      pure $ listToMaybe matches
