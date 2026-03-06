module Wire.IdPSubsystem.Interpreter
  ( interpretIdPSubsystem,
    IdPSubsystemError (..),
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

data IdPSubsystemError
  = -- | We found multiple users, but expected only one
    InconsistentUsers
  deriving (Show, Eq)

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

-- | Get the right IdP for a domain for a SCIM user identified by their email address.
--
-- To understand this, it is important to know that `IdP` can optionally carry
-- a host domain in its meta-data. There can be only one IdP with a specific
-- domain per team. So, to look the matching IdP for a (email, domain) tuple
-- up, we:
--
--  - Check that the feature is enabled (it's disabled by default)
--  - Get the `User` from brig
--  - Ensure that it is a SCIM user
--  - Get their team
--  - Lookup the team's IdP matching the domain
--
--  If any of these steps fail, we return `Nothing` - except when the team has
--  only exactly one IdP configured; then we consider this as default.
--
--  In case we find more than one user for an email address, we throw an
--  exception as this "should never happen".
--
--
-- __Design Notes__
--
-- The RFC (/Default SSO flow for team by host domain/) states that the lookup
-- of the EmailAddress should be done by a lookup in spar's database. However,
-- this cannot be simply done: Cassandra doesn't allow queries on random
-- columns, only on primary keys. Issuing full table scans (`ALLOW FILTERING`)
-- could cause bad performace characteristics. Working around that limitation
-- with e.g. additional lookup tables would lead to massive data migrations.
--
-- So, we refer to brig's impression of the user here and got - according to
-- the tests - all known cases covered.
--
-- We query for activated (`NoPendingInvitations`) users, because those may
-- actually log in.
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
