module Wire.API.EnterpriseLogin where

import Data.Id
import Data.Misc
import qualified SAML2.WebSSO as SAML
import Imports

data DomainRedirect
  = None
  | Locked
  | SSO SAML.IdPId
  | Backend HttpsUrl
  | NoRegistration
  | PreAuthorized

data TeamInvite
  = Allowed
  | NotAllowed
  | Team TeamId

newtype DnsVerificationToken = DnsVerificationToken { unDnsVerificationToken :: Text }
