module Wire.IdPSubsystem.Interpreter (interpretIdPSubsystem) where

import Imports
import Polysemy
import Wire.IdPSubsystem

-- TODO: Use RateLimit effect for rate limiting
interpretIdPSubsystem :: InterpreterFor IdPSubsystem r
interpretIdPSubsystem = interpret $ \case
  GetSsoCodeByEmail mbHost email -> undefined
