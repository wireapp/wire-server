{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Client functions for interacting with the Galley API.
module Spar.Intra.Galley where

import Bilge
import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Id (TeamId)
import Data.String.Conversions
import Galley.Types.Teams
import Galley.Types.Teams.SSO
import Imports
import Network.HTTP.Types.Method
import Spar.Error

----------------------------------------------------------------------

parseResponse :: (FromJSON a, MonadError SparError m) => Response (Maybe LBS) -> m a
parseResponse resp = do
  bdy <- maybe (throwSpar SparNoBodyInGalleyResponse) pure $ responseBody resp
  either (throwSpar . SparCouldNotParseGalleyResponse . cs) pure $ eitherDecode' bdy

----------------------------------------------------------------------

class Monad m => MonadSparToGalley m where
  call :: (Request -> Request) -> m (Response (Maybe LBS))

instance MonadSparToGalley m => MonadSparToGalley (ReaderT r m) where
  call = lift . call

-- | Get all members of a team.
getTeamMembers ::
  (HasCallStack, MonadError SparError m, MonadSparToGalley m) =>
  TeamId ->
  m [TeamMember]
getTeamMembers tid = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . paths ["i", "teams", toByteString' tid, "members"]
  unless (statusCode resp == 200) $
    throwSpar (SparGalleyError "Could not retrieve team members")
  (^. teamMembers) <$> parseResponse @TeamMemberList resp

assertSSOEnabled ::
  (HasCallStack, MonadError SparError m, MonadSparToGalley m) =>
  TeamId ->
  m ()
assertSSOEnabled tid = do
  resp :: Response (Maybe LBS) <-
    call $
      method GET
        . paths ["i", "teams", toByteString' tid, "features", "sso"]
  unless (statusCode resp == 200) $
    throwSpar (SparGalleyError "Could not retrieve SSO config")
  SSOTeamConfig status <- parseResponse resp
  unless (status == SSOEnabled) $
    throwSpar SparSSODisabled
