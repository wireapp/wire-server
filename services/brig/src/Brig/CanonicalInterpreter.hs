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

module Brig.CanonicalInterpreter
  ( BrigCanonicalEffects,
    runBrigToIO,
  )
where

import Bilge.IO
import Brig.API.Types (ReAuthError)
import Brig.App
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import qualified Brig.Options as Opt
import Brig.Sem.ActivationKeyStore (ActivationKeyStore)
import Brig.Sem.ActivationKeyStore.Cassandra
import Brig.Sem.ActivationSupply (ActivationSupply)
import Brig.Sem.ActivationSupply.IO
import Brig.Sem.BudgetStore (BudgetStore)
import Brig.Sem.BudgetStore.Cassandra
import Brig.Sem.CodeStore (CodeStore)
import Brig.Sem.CodeStore.Cassandra (codeStoreToCassandra)
import Brig.Sem.Common
import Brig.Sem.GalleyAccess (GalleyAccess)
import Brig.Sem.GalleyAccess.Http
import Brig.Sem.GundeckAccess (GundeckAccess)
import Brig.Sem.GundeckAccess.Http (gundeckAccessToHttp)
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import Brig.Sem.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Sem.PasswordResetSupply (PasswordResetSupply)
import Brig.Sem.PasswordResetSupply.IO
import Brig.Sem.Twilio (Twilio)
import Brig.Sem.Twilio.IO
import Brig.Sem.UniqueClaimsStore (UniqueClaimsStore)
import Brig.Sem.UniqueClaimsStore.Cassandra
import Brig.Sem.UserHandleStore (UserHandleStore)
import Brig.Sem.UserHandleStore.Cassandra
import Brig.Sem.UserKeyStore (UserKeyStore)
import Brig.Sem.UserKeyStore.Cassandra
import Brig.Sem.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Sem.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.Sem.UserQuery (UserQuery)
import Brig.Sem.UserQuery.Cassandra
import Brig.Sem.VerificationCodeStore (VerificationCodeStore)
import Brig.Sem.VerificationCodeStore.Cassandra
import Cassandra
import qualified Cassandra as Cas
import Control.Lens (view, (^.))
import Data.Qualified
import Data.String.Conversions
import Imports
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Async (Async, asyncToIO)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Conc.Interpreter.Race
import qualified Polysemy.Error as P
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIO)
import qualified Polysemy.TinyLog as P
import qualified Ropes.Twilio as Twilio
import Wire.Sem.Error
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO
import Wire.Sem.Paging.Cassandra (InternalPaging)

type BrigCanonicalEffects =
  '[ BlacklistPhonePrefixStore,
     BlacklistStore,
     VerificationCodeStore,
     UserKeyStore,
     UserHandleStore,
     Twilio,
     ActivationKeyStore,
     ActivationSupply,
     UniqueClaimsStore,
     GalleyAccess,
     GundeckAccess,
     Embed HttpClientIO,
     UserQuery,
     PasswordResetStore,
     UserPendingActivationStore InternalPaging,
     Now,
     PasswordResetSupply,
     CodeStore,
     BudgetStore,
     P.TinyLog,
     Input (Local ()),
     Async,
     Race,
     Resource,
     Embed Cas.Client,
     P.Error Twilio.ErrorResponse,
     P.Error ReAuthError,
     Embed IO,
     Final IO
   ]

runBrigToIO :: Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) =
  runFinal
    . embedToFinal
    . interpretWaiErrorToException
    . interpretErrorToException twilioToWai
    . interpretClientToIO (e ^. casClient)
    . resourceToIO
    . interpretRace
    . asyncToIO
    . runInputConst (toLocalUnsafe (e ^. settings & Opt.setFederationDomain) ())
    . loggerToTinyLogReqId (view requestId e) (view applog e)
    . budgetStoreToCassandra @Cas.Client
    . codeStoreToCassandra @Cas.Client
    . passwordResetSupplyToIO
    . nowToIOAction (e ^. currentTime)
    . userPendingActivationStoreToCassandra
    . passwordResetStoreToCodeStore
    . userQueryToCassandra @Cas.Client
    . interpretHttpToIO e
    . gundeckAccessToHttp @HttpClientIO (e ^. gundeck)
    . galleyAccessToHttp @HttpClientIO (e ^. galley)
    . uniqueClaimsStoreToCassandra @Cas.Client
    . activationSupplyToIO
    . activationKeyStoreToCassandra @Cas.Client
    . twilioToIO
    . userHandleStoreToCassandra @Cas.Client
    . userKeyStoreToCassandra @Cas.Client
    . verificationCodeStoreToCassandra @Cas.Client
    . interpretBlacklistStoreToCassandra @Cas.Client
    . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
    $ runReaderT ma e

interpretHttpToIO ::
  Member (Final IO) r =>
  Env ->
  Sem (Embed HttpClientIO ': r) a ->
  Sem r a
interpretHttpToIO e = interpret $ \case
  Embed action -> embedFinal @IO $ do
    let ctx = e ^. casClient
        manager = e ^. httpManager
    runClient ctx
      . runHttpT manager
      . flip runReaderT e
      . runHttpClientIO
      $ action

twilioToWai :: Twilio.ErrorResponse -> Wai.Error
twilioToWai e =
  Wai.Error
    { Wai.code = Status (Twilio.errStatus e) "",
      Wai.label = "twilio-error",
      Wai.message = cs . Twilio.errMessage $ e,
      Wai.errorData = Nothing
    }
