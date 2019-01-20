{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CargoHold.Error where

import Imports
import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Network.AWS             as AWS

data Error where
    GeneralError :: (Show e, AWS.AsError e) => e -> Error
    SigningError :: RSA.Error -> Error

deriving instance Show     Error
deriving instance Typeable Error

instance Exception Error
