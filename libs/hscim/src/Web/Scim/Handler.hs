module Web.Scim.Handler
  ( ScimHandler,
    throwScim,
    fromScimHandler,
  )
where

import Control.Monad.Except
import Web.Scim.Schema.Error

-- | Handler type for SCIM. All errors will be thrown via 'ExceptT'.
type ScimHandler m = ExceptT ScimError m

-- | Throw a 'ScimError'.
throwScim :: Monad m => ScimError -> ScimHandler m a
throwScim = throwError

-- | A natural transformation for Servant handlers. To use it, you need to
-- provide a way to throw errors in the underlying @m@ monad.
--
-- We can't use something like 'MonadError' to throw errors in @m@ because
-- 'MonadError' allows only one type of errors per monad and @m@ might have
-- one already.
--
-- You can either do something custom for 'ScimError', or use
-- 'scimToServantErr'.
fromScimHandler ::
  Monad m =>
  (forall a. ScimError -> m a) ->
  (forall a. ScimHandler m a -> m a)
fromScimHandler fromError = either fromError pure <=< runExceptT
