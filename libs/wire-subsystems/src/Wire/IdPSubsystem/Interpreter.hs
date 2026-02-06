module Wire.IdPSubsystem.Interpreter (interpretIdPSubsystem) where

import Imports
import Polysemy
import Wire.IdPSubsystem

-- TODO: Use RateLimit effect for rate limiting
interpretIdPSubsystem :: InterpreterFor IdPSubsystem r
interpretIdPSubsystem = interpret $ \case
  GetSsoCodeByEmail mbHost email -> do
    -- TODO: Rate limiting
    -- TODO: Disable on cloud
    -- TODO: The email belongs to a team -> lookup team
    -- TODO: Lookup IdP for domain
    -- TODO: Ensure: the user was created via SCIM or SSO (this means the user is managed_by: scim (and also has sso_id: {tenant: …, subject: …} properties))
    undefined
