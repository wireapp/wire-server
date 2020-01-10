{-# LANGUAGE AllowAmbiguousTypes #-}
module Web.Scim.Schema.UserTypes where

import Web.Scim.Schema.Schema (Schema)

-- | Configurable parts of 'User'.
class UserTypes tag where
  -- | User ID type.
  type UserId tag
  -- | Extra data carried with each 'User'.
  type UserExtra tag
  -- | Schemas supported by the 'User' for filtering and patching.
  --
  -- This must include User20, this is not checked.
  supportedSchemas :: [Schema]
