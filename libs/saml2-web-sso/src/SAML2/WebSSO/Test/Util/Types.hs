module SAML2.WebSSO.Test.Util.Types where

import Control.Concurrent.MVar
import Control.Lens
import Data.X509 as X509
import SAML2.WebSSO
import SAML2.WebSSO.API.Example
import Text.XML.DSig as SAML

type CtxV = MVar Ctx

data Ctx = Ctx
  { _ctxNow :: Time,
    _ctxConfig :: Config,
    _ctxIdPs :: [(IdPConfig_, SampleIdP)],
    _ctxAssertionStore :: AssertionStore,
    _ctxRequestStore :: RequestStore
  }
  deriving (Eq, Show)

data SampleIdP = SampleIdP
  { sampleIdPMetadata :: IdPMetadata,
    sampleIdPSignPrivCreds :: SignPrivCreds,
    sampleIdPSignCreds :: SignCreds,
    sampleIdPSignedCertificate :: SignedCertificate
  }
  deriving (Eq, Show)

makeLenses ''Ctx
