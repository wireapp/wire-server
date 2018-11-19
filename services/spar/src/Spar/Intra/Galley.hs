{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Spar talking to Galley.
module Spar.Intra.Galley where

import Imports
import Bilge
import Galley.Types.Teams
import Control.Monad.Except
import Control.Lens
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Conversion
import Data.Id (TeamId)
import Data.String.Conversions
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
getTeamMembers
  :: (HasCallStack, MonadError SparError m, MonadSparToGalley m)
  => TeamId -> m [TeamMember]
getTeamMembers tid = do
  resp :: Response (Maybe LBS) <- call
    $ method GET
    . paths ["i", "teams", toByteString' tid, "members"]
  unless (statusCode resp == 200) $
    throwSpar (SparGalleyError "Could not retrieve team members")
  (^. teamMembers) <$> parseResponse @TeamMemberList resp
