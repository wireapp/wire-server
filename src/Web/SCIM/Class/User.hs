{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ConstraintKinds   #-}

module Web.SCIM.Class.User
    ( UserDB (..)
    , StoredUser
    , UpdateError (..)
    ) where

import Web.SCIM.Schema.User
import Web.SCIM.Schema.Meta
import Web.SCIM.Schema.ListResponse

type StoredUser = WithMeta (WithId User)

data UpdateError = NonExisting
                 | Mutability


-- TODO: parameterize UserId
class UserDB m where
  list     :: m (ListResponse StoredUser)
  get      :: UserId -> m (Maybe StoredUser)
  create   :: User -> m StoredUser
  update   :: UserId -> User -> m (Either UpdateError StoredUser)
  patch    :: UserId -> m StoredUser
  delete   :: UserId -> m Bool
  getMeta  :: m Meta
