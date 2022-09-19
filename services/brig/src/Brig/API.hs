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
import Brig.Effects.ActivationKeyStore
import Brig.Effects.ActivationSupply
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BudgetStore
import Brig.Effects.CodeStore
import Brig.Effects.GalleyAccess
import Brig.Effects.GundeckAccess (GundeckAccess)
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PasswordResetSupply (PasswordResetSupply)
import Brig.Effects.Twilio
import Brig.Effects.UniqueClaimsStore
import Brig.Effects.UserHandleStore
import Brig.Effects.UserKeyStore
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserQuery
import Brig.Effects.VerificationCodeStore
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
  forall r p.
  Members
    '[ ActivationKeyStore,
       ActivationSupply,
       Async,
       BlacklistStore,
       BlacklistPhonePrefixStore,
       BudgetStore,
       CodeStore,
       Error ReAuthError,
       Error Twilio.ErrorResponse,
       GalleyAccess,
       GundeckAccess,
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
       UserPendingActivationStore p,
       UserQuery,
       VerificationCodeStore
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
sitemap = do
  Public.sitemap
  Public.apiDocs
  Internal.sitemap
