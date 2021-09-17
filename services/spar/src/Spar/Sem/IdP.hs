module Spar.Sem.IdP where

import Data.Id
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import qualified Wire.API.User.IdentityProvider as IP

data GetIdPResult a
  = GetIdPFound a
  | GetIdPNotFound
  | -- | IdPId has been found, but no IdPConfig matching that Id.  (Database
    --   inconsistency or race condition.)
    GetIdPDanglingId SAML.IdPId
  | -- | You were looking for an idp by just providing issuer, not teamid, and `issuer_idp_v2`
    --   has more than one entry (for different teams).
    GetIdPNonUnique [SAML.IdPId]
  | -- | An IdP was found, but it lives in another team than the one you were looking for.
    --   This should be handled similarly to NotFound in most cases.
    GetIdPWrongTeam SAML.IdPId
  deriving (Eq, Show)

newtype Replaced = Replaced SAML.IdPId

newtype Replacing = Replacing SAML.IdPId


data IdP m a where
  StoreConfig              :: IP.IdP -> IdP m ()
  GetConfig                :: SAML.IdPId -> IdP m (Maybe IP.IdP)
  GetIdByIssuerWithoutTeam :: SAML.Issuer -> IdP m (GetIdPResult SAML.IdPId)
  GetIdByIssuerWithTeam    :: SAML.Issuer -> TeamId -> IdP m (Maybe SAML.IdPId)
  GetConfigsByTeam         :: TeamId -> IdP m [IP.IdP]
  DeleteConfig             :: SAML.IdPId -> SAML.Issuer -> TeamId -> IdP m ()

  SetReplacedBy            :: Replaced -> Replacing -> IdP m ()
  ClearReplacedBy          :: Replaced -> IdP m ()

-- TODO(sandy): maybe this wants  to be a separate effect
-- data Metadata  m a wher       e
  StoreRawMetadata         :: SAML.IdPId -> Text -> IdP m ()
  GetRawMetadata           :: SAML.IdPId -> IdP m (Maybe Text)
  DeleteRawMetadata        :: SAML.IdPId -> IdP m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''IdP
