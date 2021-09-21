{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.IdP.Mem (idPToMem) where

import Control.Exception (assert)
import Control.Lens ((%~), (.~), (^.), _1, _2)
import Data.Id (TeamId)
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import qualified Spar.Sem.IdP as Eff
import qualified Wire.API.User.IdentityProvider as IP

type IS = (TypedState, RawState)

type TypedState = Map SAML.IdPId IP.IdP

type RawState = Map SAML.IdPId Text

idPToMem ::
  forall r a.
  Sem (Eff.IdP ': r) a ->
  Sem r a
idPToMem = evState . evEff
  where
    evState :: Sem (State IS : r) a -> Sem r a
    evState = evalState mempty

    evEff :: Sem (Eff.IdP ': r) a -> Sem (State IS ': r) a
    evEff = reinterpret @_ @(State IS) $ \case
      Eff.StoreConfig iw ->
        modify' (_1 %~ storeConfig iw)
      Eff.GetConfig i ->
        gets (getConfig i . (^. _1))
      Eff.GetIdByIssuerWithoutTeam iss ->
        gets (getIdByIssuerWithoutTeam iss . (^. _1))
      Eff.GetIdByIssuerWithTeam iss team ->
        gets (getIdByIssuerWithTeam iss team . (^. _1))
      Eff.GetConfigsByTeam team ->
        gets (getConfigsByTeam team . (^. _1))
      Eff.DeleteConfig i iss team ->
        modify' (_1 %~ deleteConfig i iss team)
      Eff.SetReplacedBy (Eff.Replaced replaced) (Eff.Replacing replacing) ->
        modify' (_1 %~ ((updateReplacedBy (Just replacing) replaced) <$>))
      Eff.ClearReplacedBy (Eff.Replaced replaced) ->
        modify' (_1 %~ ((updateReplacedBy Nothing replaced) <$>))
      Eff.StoreRawMetadata i txt ->
        modify (_2 %~ storeRawMetadata i txt)
      Eff.GetRawMetadata i ->
        gets (getRawMetadata i . (^. _2))
      Eff.DeleteRawMetadata i ->
        modify (_2 %~ deleteRawMetadata i)

storeConfig :: IP.IdP -> TypedState -> TypedState
storeConfig iw =
  M.filter
    ( \iw' ->
        or
          [ iw' ^. SAML.idpMetadata . SAML.edIssuer /= iw ^. SAML.idpMetadata . SAML.edIssuer,
            iw' ^. SAML.idpExtraInfo . IP.wiTeam /= iw ^. SAML.idpExtraInfo . IP.wiTeam
          ]
    )
    . M.insert (iw ^. SAML.idpId) iw

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

deleteConfig :: SAML.IdPId -> SAML.Issuer -> TeamId -> TypedState -> TypedState
deleteConfig i iss team =
  M.filter fl
  where
    fl :: IP.IdP -> Bool
    fl idp =
      assert -- calling this function with inconsistent values will crash hard.
        ( idp ^. SAML.idpMetadata . SAML.edIssuer == iss
            && idp ^. SAML.idpExtraInfo . IP.wiTeam == team
        )
        (idp ^. SAML.idpId /= i)

updateReplacedBy :: Maybe SAML.IdPId -> SAML.IdPId -> IP.IdP -> IP.IdP
updateReplacedBy mbReplacing replaced idp =
  idp
    & if idp ^. SAML.idpId == replaced
      then SAML.idpExtraInfo . IP.wiReplacedBy .~ mbReplacing
      else id

storeRawMetadata :: SAML.IdPId -> Text -> RawState -> RawState
storeRawMetadata = M.insert

getRawMetadata :: SAML.IdPId -> RawState -> Maybe Text
getRawMetadata = M.lookup

deleteRawMetadata :: SAML.IdPId -> RawState -> RawState
deleteRawMetadata idpid = M.filterWithKey (\idpid' _ -> idpid' /= idpid)
