

-- NOTE: we should model all services in one effect tree, and then see which interface parts
-- can be put into a separate service.  the separation between brig and galley has stopped
-- making sense with the introduction of teams.


data UserAccount m a where
  -- crud + validation + authorization/access control

  createUserAccount ::
  -- team-owner-email
  -- team-member-invitation
  -- team-member-scim
  -- team-member-saml (deprecated)
  -- personal-user-email ?
  -- personal-user-phone ?
  --
  -- auth:
  --   saml
  --   oauth (sci-fi)
  --   password/zauth
  --
  -- teamrole:
  --   owner, admin, member, external, ...
  --
  -- TODO: how do email and saml differ categorically?  (identity vs. auth mechanism)

  readUserAccount ::
  -- by userid
  -- by email
  -- by handle
  -- by ...

  updateUserAccount ::
  -- same keys as read, but with a delta to the UserAccount record as extra argument
  -- alternative: have many actions that update individual aspects

  deleteUserAccount ::
  -- same keys as read
