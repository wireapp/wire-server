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

module Spar.Sem.Logger
  ( module Spar.Sem.Logger,
    SAML.Level (..),
  )
where

import Imports hiding (log)
import Polysemy
import qualified SAML2.WebSSO as SAML

data Logger msg m a where
  Log :: SAML.Level -> msg -> Logger msg m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''Logger

mapLogger ::
  forall msg msg' r a.
  Member (Logger msg') r =>
  (msg -> msg') ->
  Sem (Logger msg ': r) a ->
  Sem r a
mapLogger f = interpret $ \case
  Log lvl msg -> log lvl $ f msg

trace :: Member (Logger msg) r => msg -> Sem r ()
trace = log SAML.Trace

debug :: Member (Logger msg) r => msg -> Sem r ()
debug = log SAML.Debug

info :: Member (Logger msg) r => msg -> Sem r ()
info = log SAML.Info

warn :: Member (Logger msg) r => msg -> Sem r ()
warn = log SAML.Warn

err :: Member (Logger msg) r => msg -> Sem r ()
err = log SAML.Error

fatal :: Member (Logger msg) r => msg -> Sem r ()
fatal = log SAML.Fatal
