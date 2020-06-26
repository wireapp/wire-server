{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module CargoHold.Error where

import qualified Crypto.PubKey.RSA.Types as RSA
import Imports
import qualified Network.AWS as AWS

data Error where
  GeneralError :: (Show e, AWS.AsError e) => e -> Error
  SigningError :: RSA.Error -> Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error
