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
  deriving (Eq, Show, Generic)

newtype Replaced = Replaced SAML.IdPId
  deriving (Eq, Ord, Show)

newtype Replacing = Replacing SAML.IdPId
  deriving (Eq, Ord, Show)

data IdP m a where
  StoreConfig :: IP.IdP -> IdP m ()
  GetConfig :: SAML.IdPId -> IdP m (Maybe IP.IdP)
  GetIdByIssuerWithoutTeam :: SAML.Issuer -> IdP m (GetIdPResult SAML.IdPId)
  GetIdByIssuerWithTeam :: SAML.Issuer -> TeamId -> IdP m (Maybe SAML.IdPId)
  GetConfigsByTeam :: TeamId -> IdP m [IP.IdP]
  DeleteConfig :: IP.IdP -> IdP m ()
  -- affects _wiReplacedBy in GetConfig
  SetReplacedBy :: Replaced -> Replacing -> IdP m ()
  ClearReplacedBy :: Replaced -> IdP m ()
  -- TODO(sandy): maybe this wants  to be a separate effect
  -- data Metadata  m a where
  StoreRawMetadata :: SAML.IdPId -> Text -> IdP m ()
  GetRawMetadata :: SAML.IdPId -> IdP m (Maybe Text)
  DeleteRawMetadata :: SAML.IdPId -> IdP m ()

deriving stock instance Show (IdP m a)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''IdP
