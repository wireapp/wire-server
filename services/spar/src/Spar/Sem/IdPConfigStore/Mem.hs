{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.IdPConfigStore.Mem (idPToMem, TypedState) where

import Control.Lens ((.~), (^.))
import Data.Id (TeamId)
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.IdPConfigStore (IdPConfigStore (..), Replaced (..), Replacing (..))
import Wire.API.User.IdentityProvider (IdPHandle (IdPHandle))
import qualified Wire.API.User.IdentityProvider as IP
import qualified Wire.API.User.IdentityProvider as SAML

type TypedState = Map SAML.IdPId IP.IdP

idPToMem ::
  forall r a.
  Sem (IdPConfigStore ': r) a ->
  Sem r (TypedState, a)
idPToMem = evState . evEff
  where
    evState :: Sem (State TypedState : r) a -> Sem r (TypedState, a)
    evState = runState mempty

    evEff :: Sem (IdPConfigStore ': r) a -> Sem (State TypedState ': r) a
    evEff = reinterpret @_ @(State TypedState) $ \case
      InsertConfig iw ->
        modify' (insertConfig iw)
      NewHandle _tid ->
        -- Same handle for all IdPs is good enough, for now
        pure $ IdPHandle "IdP 1"
      GetConfig i ->
        gets (getConfig i)
      GetIdPByIssuerV1Maybe issuer ->
        gets (getIdByIssuerWithoutTeamMaybe issuer)
      GetIdPByIssuerV1 issuer ->
        gets (getIdByIssuerWithoutTeam issuer)
      GetIdPByIssuerV2Maybe issuer tid ->
        gets (getIdByIssuerWithTeamMaybe issuer tid)
      GetIdPByIssuerV2 issuer tid ->
        gets (getIdByIssuerWithTeam issuer tid)
      GetConfigsByTeam team ->
        gets (getConfigsByTeam team)
      DeleteConfig idp ->
        modify' (deleteConfig idp)
      SetReplacedBy (Replaced replaced) (Replacing replacing) ->
        modify' (updateReplacedBy (Just replacing) replaced <$>)
      ClearReplacedBy (Replaced replaced) ->
        modify' (updateReplacedBy Nothing replaced <$>)
      DeleteIssuer issuer _tid -> modify' (deleteIssuer issuer)

insertConfig :: IP.IdP -> TypedState -> TypedState
insertConfig iw =
  M.insert (iw ^. SAML.idpId) iw
    . M.filter
      ( \iw' ->
          (iw' ^. SAML.idpMetadata . SAML.edIssuer /= iw ^. SAML.idpMetadata . SAML.edIssuer)
            || (iw' ^. SAML.idpExtraInfo . IP.wiTeam /= iw ^. SAML.idpExtraInfo . IP.wiTeam)
      )

getConfig :: SAML.IdPId -> TypedState -> IP.IdP
getConfig idpId mp = fromMaybe (error "idp not found") $ M.lookup idpId mp

getIdByIssuerWithoutTeam :: SAML.Issuer -> TypedState -> SAML.IdP
getIdByIssuerWithoutTeam issuer mp = fromMaybe (error "idp not found") $ getIdByIssuerWithoutTeamMaybe issuer mp

getIdByIssuerWithoutTeamMaybe :: SAML.Issuer -> TypedState -> Maybe SAML.IdP
getIdByIssuerWithoutTeamMaybe iss mp =
  case filter (\idp -> idp ^. SAML.idpMetadata . SAML.edIssuer == iss) $ M.elems mp of
    [] -> Nothing
    [a] -> Just a
    _ : _ : _ -> error "impossible"

getIdByIssuerWithTeam :: SAML.Issuer -> TeamId -> TypedState -> SAML.IdP
getIdByIssuerWithTeam issuer tid mp = fromMaybe (error "idp not found") $ getIdByIssuerWithTeamMaybe issuer tid mp

getIdByIssuerWithTeamMaybe :: SAML.Issuer -> TeamId -> TypedState -> Maybe SAML.IdP
getIdByIssuerWithTeamMaybe iss team mp =
  case filter fl $ M.elems mp of
    [] -> Nothing
    [a] -> Just a
    (_ : _ : _) ->
      -- (StoreConfig doesn't let this happen)
      error "GetIdByIssuerWithTeam: impossible"
  where
    fl :: IP.IdP -> Bool
    fl idp =
      idp ^. SAML.idpMetadata . SAML.edIssuer == iss
        && idp ^. SAML.idpExtraInfo . IP.wiTeam == team

getConfigsByTeam :: TeamId -> TypedState -> [IP.IdP]
getConfigsByTeam team =
  filter fl . M.elems
  where
    fl :: IP.IdP -> Bool
    fl idp = idp ^. SAML.idpExtraInfo . IP.wiTeam == team

deleteConfig :: IP.IdP -> TypedState -> TypedState
deleteConfig idp =
  M.filter fl
  where
    fl :: IP.IdP -> Bool
    fl idp' = idp' ^. SAML.idpId /= idp ^. SAML.idpId

updateReplacedBy :: Maybe SAML.IdPId -> SAML.IdPId -> IP.IdP -> IP.IdP
updateReplacedBy mbReplacing replaced idp =
  idp
    & if idp ^. SAML.idpId == replaced
      then SAML.idpExtraInfo . IP.wiReplacedBy .~ mbReplacing
      else id

deleteIssuer :: SAML.Issuer -> TypedState -> TypedState
deleteIssuer = const id
