{-# LANGUAGE TemplateHaskell #-}

module Wire.LegalHoldStore where

import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Id
import Data.LegalHold
import Data.Misc
import Imports
import Network.HTTP.Client qualified as Http
import Polysemy
import Wire.API.Provider.Service
import Wire.API.Team.LegalHold.Internal
import Wire.API.User.Client.Prekey

data LegalHoldStore m a where
  CreateSettings :: LegalHoldService -> LegalHoldStore m ()
  GetSettings :: TeamId -> LegalHoldStore m (Maybe LegalHoldService)
  RemoveSettings :: TeamId -> LegalHoldStore m ()
  InsertPendingPrekeys :: UserId -> [Prekey] -> LegalHoldStore m ()
  SelectPendingPrekeys :: UserId -> LegalHoldStore m (Maybe ([Prekey], LastPrekey))
  DropPendingPrekeys :: UserId -> LegalHoldStore m ()
  SetUserLegalHoldStatus :: TeamId -> UserId -> UserLegalHoldStatus -> LegalHoldStore m ()
  SetTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m ()
  UnsetTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m ()
  IsTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m Bool
  MakeVerifiedRequestFreshManager ::
    Fingerprint Rsa ->
    HttpsUrl ->
    (Http.Request -> Http.Request) ->
    LegalHoldStore m (Http.Response LC8.ByteString)
  MakeVerifiedRequest ::
    Fingerprint Rsa ->
    HttpsUrl ->
    (Http.Request -> Http.Request) ->
    LegalHoldStore m (Http.Response LC8.ByteString)
  ValidateServiceKey :: ServiceKeyPEM -> LegalHoldStore m (Maybe (ServiceKey, Fingerprint Rsa))

makeSem ''LegalHoldStore
