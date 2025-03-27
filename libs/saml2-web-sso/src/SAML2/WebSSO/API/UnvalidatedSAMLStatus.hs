-- | This is a partial implementation of Web SSO using the HTTP Post Binding [2/3.5].
--
-- The default API offers 3 end-points: one for retrieving the 'AuthnRequest' in a redirect to the
-- IdP; one for delivering the 'AuthnResponse' that will re-direct to some fixed landing page; and
-- one for retrieving the SP's metadata.
--
-- There are other scenarios, e.g. all resources on the page could be guarded with an authentication
-- check and redirect the client to the IdP, and make sure that the client lands on the initally
-- requested resource after successful authentication.  With the building blocks provided by this
-- module, it should be straight-forward to implement all of these scenarios.
--
-- This module works best if imported qualified.
--
module SAML2.WebSSO.API.UnvalidatedSAMLStatus
  ( UnvalidatedSAMLStatus,
    mkUnvalidatedSAMLStatus,
    eqUnvalidatedSAMLStatus,
  )
where

import SAML2.WebSSO.Types

-- | As the SAML standard requires taking the unsigned status value into account, we return it
-- here.  This would be bad if we relied on it *exclusively*, but since we also require that
-- there is a least one signed `Assertion`, and that this assertion also grants access, the
-- only way this can conceivably be exploited is by locking people out, rather than breaching
-- access.  So we err in favor of compatibility here, rather than of reason.
--
-- But this newtype at least makes it explicit what we're doing, so no future me can decide to
-- skip all the *actual* tests, since checking the status is good enough.
newtype UnvalidatedSAMLStatus = UnvalidatedSAMLStatus Status
  deriving (Eq, Show)

mkUnvalidatedSAMLStatus :: Status -> UnvalidatedSAMLStatus
mkUnvalidatedSAMLStatus = UnvalidatedSAMLStatus

eqUnvalidatedSAMLStatus :: UnvalidatedSAMLStatus -> Status -> Bool
eqUnvalidatedSAMLStatus (UnvalidatedSAMLStatus s) t = s == t
