module Wire.API.EnterpriseLogin where

import Data.Id
import Data.Misc

data DomainRedirect
  = None
  | Locked
  | SSO
  | Backend HttpsUrl
  | NoRegistration
  | PreAuthorized

data TeamInvite
  = Allowed
  | NotAllowed
  | Team TeamId

data DnsVerificationToken = DnsVerificationToken
