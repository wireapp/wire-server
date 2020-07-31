{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Data.ZAuth.Validation
  ( Env,
    Validate,
    mkEnv,
    runValidate,
    Failure (..),
    validate,
    validateUser,
    validateAccess,
    validateBot,
    validateProvider,
    check,
  )
where

import Control.Lens
import Control.Monad.Except
import qualified Data.ByteString as Strict
import Data.ByteString.Conversion
import Data.Time.Clock.POSIX
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.ZAuth.Token
import Imports
import Sodium.Crypto.Sign (PublicKey, Signature, verifyWith)

data Failure
  = -- | The token signature is incorrect.
    Falsified
  | -- | The token is expired.
    Expired
  | -- | Invalid token.
    Invalid
  | -- | This operation is unsupported on this token type
    Unsupported
  deriving (Eq, Show)

instance Exception Failure

newtype Env = Env
  {verifyFns :: Vector (Signature -> Strict.ByteString -> IO Bool)}

newtype Validate a = Validate
  { valid :: ExceptT Failure (ReaderT Env IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError Failure
    )

mkEnv :: PublicKey -> [PublicKey] -> Env
mkEnv k kk = Env $ Vec.fromList (map verifyWith (k : kk))

runValidate :: MonadIO m => Env -> Validate a -> m (Either Failure a)
runValidate v m = liftIO $ runReaderT (runExceptT (valid m)) v

validateUser :: ByteString -> Validate (Token User)
validateUser t = maybe (throwError Invalid) check (fromByteString t)

validateAccess :: ByteString -> Validate (Token Access)
validateAccess t = maybe (throwError Invalid) check (fromByteString t)

validateBot :: ByteString -> Validate (Token Bot)
validateBot t = maybe (throwError Invalid) check (fromByteString t)

validateProvider :: ByteString -> Validate (Token Provider)
validateProvider t = maybe (throwError Invalid) check (fromByteString t)

-----------------------------------------------------------------------------
-- User & Access Validation
--
-- It is not allowed to only have a user, but no access token for
-- validation purposes.

validate ::
  -- | assumed to be a 'Token User'
  Maybe ByteString ->
  -- | assumed to be a 'Token Access'
  Maybe ByteString ->
  Validate (Token Access)
validate Nothing Nothing = throwError Invalid
validate (Just _) Nothing = throwError Invalid
validate Nothing (Just t) = validateAccess t
validate (Just c) (Just t) = do
  u <- maybe (throwError Invalid) return (fromByteString c)
  a <- maybe (throwError Invalid) return (fromByteString t)
  void $ check u
  void $ check a
  unless (u ^. body . user == a ^. body . userId) $
    throwError Invalid
  return a

check :: ToByteString a => Token a -> Validate (Token a)
check t = do
  ff <- Validate $ lift $ asks verifyFns
  let dat = toByteString' $ writeData (t ^. header) (t ^. body)
  let k = t ^. header . key
  when (k < 1 || k > Vec.length ff) $
    throwError Invalid
  ok <- liftIO $ (ff ! (k - 1)) (t ^. signature) dat
  unless ok $
    throwError Falsified
  isExpired <-
    if t ^. header . time == -1
      then return False
      else (t ^. header . time <) <$> now
  when isExpired $
    throwError Expired
  return t

now :: (Functor m, MonadIO m) => m Integer
now = floor <$> liftIO getPOSIXTime
