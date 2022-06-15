-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.API
  ( sitemap,
  )
where

import Brig.API.Handler (Handler)
import qualified Brig.API.Internal as Internal
import qualified Brig.API.Public as Public
import Brig.API.Types
import Brig.Sem.ActivationKeyStore
import Brig.Sem.ActivationSupply
import Brig.Sem.BudgetStore
import Brig.Sem.GalleyAccess
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import Brig.Sem.PasswordResetSupply (PasswordResetSupply)
import Brig.Sem.Twilio
import Brig.Sem.UniqueClaimsStore
import Brig.Sem.UserHandleStore
import Brig.Sem.UserKeyStore
import Brig.Sem.UserQuery
import Brig.Sem.VerificationCodeStore
import Data.Qualified
import qualified Data.Swagger.Build.Api as Doc
import Network.Wai.Routing (Routes)
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import qualified Polysemy.TinyLog as P
import qualified Ropes.Twilio as Twilio

sitemap ::
  Members
    '[ ActivationKeyStore,
       ActivationSupply,
       Async,
       BudgetStore,
       Error ReAuthError,
       Error Twilio.ErrorResponse,
       GalleyAccess,
       Input (Local ()),
       P.TinyLog,
       PasswordResetStore,
       PasswordResetSupply,
       Race,
       Resource,
       Twilio,
       UniqueClaimsStore,
       UserHandleStore,
       UserKeyStore,
       UserQuery,
       VerificationCodeStore
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
sitemap = do
  Public.sitemap
  Public.apiDocs
  Internal.sitemap
