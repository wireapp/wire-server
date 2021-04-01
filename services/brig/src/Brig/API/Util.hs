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

module Brig.API.Util
  ( fetchUserIdentity,
    lookupProfilesMaybeFilterSameTeamOnly,
    lookupSelfProfile,
    logInvitationCode,
    validateHandle,
    logEmail,
    ZAuthServant,
    InternalAuth
  )
where

import qualified Brig.API.Error as Error
import Brig.API.Handler
import Brig.API.Types
import Brig.App (AppIO)
import qualified Brig.Data.User as Data
import Brig.Types
import Brig.Types.Intra (accountUser)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except (throwE)
import Data.Handle (Handle, parseHandle)
import Data.Id
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text.Ascii (AsciiText (toText))
import Imports
import System.Logger (Msg)
import qualified System.Logger as Log
import Util.Logging (sha256String)
import Servant.Swagger (HasSwagger(..))
import Servant hiding (Handler)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Swagger (SecurityScheme(..), HasSecurityDefinitions (..), HasSecurity (..), SecurityRequirement(..), SecuritySchemeType (..), ApiKeyParams(..), ApiKeyLocation (..))
import Control.Lens ((<>~))

-- | This type exists for the special 'HasSwagger' and 'HasServer' instances. It
-- shows the "Authorization" header in the swagger docs, but expects the
-- "Z-Auth" header in the server. This helps keep the swagger docs usable
-- through nginz.
data ZAuthServant

type InternalAuth = Header' '[Servant.Required, Servant.Strict] "Z-User" UserId

instance HasSwagger api => HasSwagger (ZAuthServant :> api) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & securityDefinitions <>~ InsOrdHashMap.singleton "ZAuth" secScheme
      & security <>~ [SecurityRequirement $ InsOrdHashMap.singleton "ZAuth" []]
    where
      secScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader),
            _securitySchemeDescription = Just "Must be a token retrieved by calling 'POST /login' or 'POST /access'. It must be presented in this format: 'Bearer \\<token\\>'."
          }

instance
  ( HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    HasServer api ctx
  ) =>
  HasServer (ZAuthServant :> api) ctx
  where
  type ServerT (ZAuthServant :> api) m = ServerT (InternalAuth :> api) m

  route _ = Servant.route (Proxy @(InternalAuth :> api))
  hoistServerWithContext _ pc nt s =
    Servant.hoistServerWithContext (Proxy @(InternalAuth :> api)) pc nt s

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> Handler [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
  selfTeam <- lift $ Data.lookupUserTeam self
  return $ case selfTeam of
    Just team -> filter (\x -> profileTeam x == Just team) us
    Nothing -> us

fetchUserIdentity :: UserId -> AppIO (Maybe UserIdentity)
fetchUserIdentity uid =
  lookupSelfProfile uid
    >>= maybe
      (throwM $ UserProfileNotFound uid)
      (return . userIdentity . selfUser)

-- | Obtain a profile for a user as he can see himself.
lookupSelfProfile :: UserId -> AppIO (Maybe SelfProfile)
lookupSelfProfile = fmap (fmap mk) . Data.lookupAccount
  where
    mk a = SelfProfile (accountUser a)

validateHandle :: Text -> Handler Handle
validateHandle = maybe (throwE (Error.StdError Error.invalidHandle)) return . parseHandle

logEmail :: Email -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . cs . show $ email)

logInvitationCode :: InvitationCode -> (Msg -> Msg)
logInvitationCode code = Log.field "invitation_code" (toText $ fromInvitationCode code)
