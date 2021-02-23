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

module Federator.Util
  ( federateWith,
  )
where

import Control.Lens (view)
import Data.Domain (Domain)
import Federator.Options
import Imports
import Polysemy (Members, Sem)
import qualified Polysemy.Reader as Polysemy

federateWith :: Members '[Polysemy.Reader RunSettings] r => Domain -> Sem r Bool
federateWith targetDomain = do
  strategy <- view federationStrategy <$> Polysemy.ask
  pure $ case strategy of
    AllowAll -> True
    AllowList (AllowedDomains domains) -> targetDomain `elem` domains
