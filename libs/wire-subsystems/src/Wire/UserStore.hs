{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.UserStore where

import Data.Default
import Data.Handle
import Data.Id
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.User
import Wire.Arbitrary
import Wire.StoredUser

data StoredUserHandleUpdate = MkStoredUserHandleUpdate
  { old :: Maybe Handle,
    new :: Handle
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform StoredUserHandleUpdate

-- TODO: remove handle from this record; add operation UpdateHandle; make everything
-- symmetrical to the UserSubsystem operation.  nowait: replace StoredUserUpdate with
-- UserProfileUpdate.  and maybe that type should reside here, not in UserSubsystem.
data StoredUserUpdate = MkStoredUserUpdate
  { name :: Maybe Name,
    pict :: Maybe Pict,
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale,
    handle :: Maybe StoredUserHandleUpdate,
    supportedProtocols :: Maybe (Set BaseProtocolTag)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform StoredUserUpdate

instance Default StoredUserUpdate where
  def = MkStoredUserUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data StoredUserUpdateError = StoredUserUpdateHandleExists

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)
  UpdateUserEither :: UserId -> StoredUserUpdate -> UserStore m (Either StoredUserUpdateError ())
  DeleteUser :: User -> UserStore m ()
  -- | this operation looks up a handle but may not give you stale data
  --   it is potentially slower and less resilient than 'GlimpseHandle'
  LookupHandle :: Handle -> UserStore m (Maybe UserId)
  -- | the interpretation for 'LookupHandle' and 'GlimpseHandle'
  --   may differ in terms of how consistent they are, if that
  --   matters for the interpretation, this operation may give you stale data
  --   but is faster and more resilient
  GlimpseHandle :: Handle -> UserStore m (Maybe UserId)

makeSem ''UserStore

updateUser ::
  (Member UserStore r, Member (Error StoredUserUpdateError) r) =>
  UserId ->
  StoredUserUpdate ->
  Sem r ()
updateUser uid update = either throw pure =<< updateUserEither uid update

updateUserHandle ::
  (Member UserStore r, Member (Error StoredUserUpdateError) r) =>
  UserId ->
  StoredUserHandleUpdate ->
  Sem r ()
updateUserHandle uid upd = updateUser uid (def {handle = Just upd})
