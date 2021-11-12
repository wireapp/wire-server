{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.IdP.Mem (idPToMem, TypedState) where

import Control.Lens ((.~), (^.))
import Data.Id (TeamId)
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import qualified Spar.Sem.IdP as Eff
import qualified Wire.API.User.IdentityProvider as IP

type TypedState = Map SAML.IdPId IP.IdP

idPToMem ::
  forall r a.
  Sem (Eff.IdP ': r) a ->
  Sem r (TypedState, a)
idPToMem = evState . evEff
  where
    evState :: Sem (State TypedState : r) a -> Sem r (TypedState, a)
    evState = runState mempty

    evEff :: Sem (Eff.IdP ': r) a -> Sem (State TypedState ': r) a
    evEff = reinterpret @_ @(State TypedState) $ \case
      Eff.StoreConfig iw ->
        modify' (storeConfig iw)
      Eff.GetConfig i ->
        gets (getConfig i)
      Eff.GetIdByIssuerWithoutTeam iss ->
        gets (getIdByIssuerWithoutTeam iss)
      Eff.GetIdByIssuerWithTeam iss team ->
        gets (getIdByIssuerWithTeam iss team)
      Eff.GetConfigsByTeam team ->
        gets (getConfigsByTeam team)
      Eff.DeleteConfig idp ->
        modify' (deleteConfig idp)
      Eff.SetReplacedBy (Eff.Replaced replaced) (Eff.Replacing replacing) ->
        modify' (updateReplacedBy (Just replacing) replaced <$>)
      Eff.ClearReplacedBy (Eff.Replaced replaced) ->
        modify' (updateReplacedBy Nothing replaced <$>)

storeConfig :: IP.IdP -> TypedState -> TypedState
storeConfig iw =
  M.insert (iw ^. SAML.idpId) iw
    . M.filter
      ( \iw' ->
          or
            [ iw' ^. SAML.idpMetadata . SAML.edIssuer /= iw ^. SAML.idpMetadata . SAML.edIssuer,
              iw' ^. SAML.idpExtraInfo . IP.wiTeam /= iw ^. SAML.idpExtraInfo . IP.wiTeam
            ]
      )

getConfig :: SAML.IdPId -> TypedState -> Maybe IP.IdP
getConfig = M.lookup

getIdByIssuerWithoutTeam :: SAML.Issuer -> TypedState -> Eff.GetIdPResult SAML.IdPId
getIdByIssuerWithoutTeam iss mp =
  case filter (\idp -> idp ^. SAML.idpMetadata . SAML.edIssuer == iss) $ M.elems mp of
    [] -> Eff.GetIdPNotFound
    [a] -> Eff.GetIdPFound (a ^. SAML.idpId)
    as@(_ : _ : _) -> Eff.GetIdPNonUnique ((^. SAML.idpId) <$> as)

getIdByIssuerWithTeam :: SAML.Issuer -> TeamId -> TypedState -> Maybe SAML.IdPId
getIdByIssuerWithTeam iss team mp =
  case filter fl $ M.elems mp of
    [] -> Nothing
    [a] -> Just (a ^. SAML.idpId)
    (_ : _ : _) ->
      -- (Eff.StoreConfig doesn't let this happen)
      error "Eff.GetIdByIssuerWithTeam: impossible"
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
