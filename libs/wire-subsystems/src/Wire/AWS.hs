-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.AWS where

import Amazonka (Env, runResourceT)
import Amazonka.Core.Lens.Internal qualified as AWS
import Amazonka.Send as AWS
import Amazonka.Types qualified as AWS
import Control.Lens
import Imports
import Network.HTTP.Client
import Polysemy
import Polysemy.Input

sendCatch ::
  ( Member (Input Amazonka.Env) r,
    Member (Embed IO) r,
    AWS.AWSRequest req,
    Typeable req,
    Typeable (AWS.AWSResponse req)
  ) =>
  req ->
  Sem r (Either AWS.Error (AWS.AWSResponse req))
sendCatch req = do
  env <- input
  embed . AWS.trying AWS._Error . runResourceT . AWS.send env $ req

canRetry :: Either AWS.Error a -> Bool
canRetry (Right _) = False
canRetry (Left e) = case e of
  AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> True
  AWS.ServiceError se | se ^. AWS.serviceError_code == AWS.ErrorCode "RequestThrottled" -> True
  _ -> False
