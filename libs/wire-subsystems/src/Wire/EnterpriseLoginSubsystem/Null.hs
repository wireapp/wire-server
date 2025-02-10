module Wire.EnterpriseLoginSubsystem.Null (runEnterpriseLoginSubsystemNoConfig) where

import Imports
import Polysemy
import Polysemy.Error
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error

runEnterpriseLoginSubsystemNoConfig ::
  (Member (Error EnterpriseLoginSubsystemError) r) =>
  InterpreterFor EnterpriseLoginSubsystem r
runEnterpriseLoginSubsystemNoConfig = interpret $ \case
  GetDomainRegistration _domain -> pure Nothing
  _ -> throw EnterpriseLoginSubsystemNotEnabled
