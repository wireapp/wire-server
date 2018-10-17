module Web.SCIM.Handler
  ( SCIMHandler
  , throwSCIM
  , fromSCIMHandler
  ) where

import Control.Monad.Except
import Web.SCIM.Schema.Error

-- | Handler type for SCIM. All errors will be thrown via 'ExceptT'.
type SCIMHandler m = ExceptT SCIMError m

-- | Throw a 'SCIMError'.
throwSCIM :: Monad m => SCIMError -> SCIMHandler m a
throwSCIM = throwError

-- | A natural transformation for Servant handlers. To use it, you need to
-- provide a way to throw errors in the underlying @m@ monad.
--
-- We can't use something like 'MonadError' to throw errors in @m@ because
-- 'MonadError' allows only one type of errors per monad and @m@ might have
-- one already.
--
-- You can either do something custom for 'SCIMError', or use
-- 'scimToServantErr'.
fromSCIMHandler
  :: Monad m
  => (forall a. SCIMError -> m a)
  -> (forall a. SCIMHandler m a -> m a)
fromSCIMHandler fromError = either fromError pure <=< runExceptT
