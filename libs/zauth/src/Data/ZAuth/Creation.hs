{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Data.ZAuth.Creation
  ( -- * Types
    Create,
    Env,

    -- * Initialisation
    mkEnv,
    runCreate,

    -- * Specific
    accessToken,
    accessToken1,
    userToken,
    sessionToken,
    botToken,
    providerToken,
    legalHoldAccessToken,
    legalHoldAccessToken1,
    legalHoldUserToken,

    -- * Generic
    newToken,
    renewToken,
  )
where

import Control.Lens hiding (withIndex)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.ByteString qualified as Strict
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX
import Data.UUID
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vec
import Data.ZAuth.Token hiding (signature)
import Imports
import Sodium.Crypto.Sign
import System.Random.MWC

data Env = Env
  { keyIdx :: Int,
    zSign :: Vector (Strict.ByteString -> IO Signature),
    randGen :: GenIO
  }

newtype Create a = Create
  { zauth :: ReaderT Env IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch
    )

tokenVersion :: Int
tokenVersion = 1

mkEnv :: SecretKey -> [SecretKey] -> IO Env
mkEnv k kk = Env 1 (Vec.fromList $ map signature (k : kk)) <$> liftIO createSystemRandom

runCreate :: Env -> Int -> Create a -> IO a
runCreate z k m = do
  when (k < 1 || k > Vec.length (zSign z)) $
    error "runCreate: Key index out of range."
  runReaderT (zauth m) (z {keyIdx = k})

userToken :: Integer -> UUID -> Maybe Text -> Word32 -> Create (Token User)
userToken dur usr cli rnd = do
  d <- expiry dur
  newToken d U Nothing (mkUser usr cli rnd)

sessionToken :: Integer -> UUID -> Maybe Text -> Word32 -> Create (Token User)
sessionToken dur usr cli rnd = do
  d <- expiry dur
  newToken d U (Just S) (mkUser usr cli rnd)

-- | Create an access token taking a duration, userId, clientId and a (random)
-- number that can be used as connection identifier
accessToken :: Integer -> UUID -> Maybe Text -> Word64 -> Create (Token Access)
accessToken dur usr cid con = do
  d <- expiry dur
  newToken d A Nothing (mkAccess usr cid con)

-- | Create an access token taking a duration, userId and clientId.
-- Similar to 'accessToken', except that the connection identifier is randomly
-- generated.
accessToken1 :: Integer -> UUID -> Maybe Text -> Create (Token Access)
accessToken1 dur usr cid = do
  g <- Create $ asks randGen
  d <- liftIO $ asGenIO (uniform :: GenIO -> IO Word64) g
  accessToken dur usr cid d

legalHoldUserToken :: Integer -> UUID -> Maybe Text -> Word32 -> Create (Token LegalHoldUser)
legalHoldUserToken dur usr cli rnd = do
  d <- expiry dur
  newToken d LU Nothing (mkLegalHoldUser usr cli rnd)

-- | Create a legal hold access token taking a duration, userId, clientId and a
-- (random) number that can be used as connection identifier
legalHoldAccessToken ::
  Integer ->
  UUID ->
  Maybe Text ->
  Word64 ->
  Create (Token LegalHoldAccess)
legalHoldAccessToken dur usr cid con = do
  d <- expiry dur
  newToken d LA Nothing (mkLegalHoldAccess usr cid con)

-- | Create a legal hold access token taking a duration, userId. Similar to 'legalHoldAccessToken', except that the connection identifier is randomly generated.
legalHoldAccessToken1 :: Integer -> UUID -> Maybe Text -> Create (Token LegalHoldAccess)
legalHoldAccessToken1 dur usr cid = do
  g <- Create $ asks randGen
  d <- liftIO $ asGenIO (uniform :: GenIO -> IO Word64) g
  legalHoldAccessToken dur usr cid d

botToken :: UUID -> UUID -> UUID -> Create (Token Bot)
botToken pid bid cnv = newToken (-1) B Nothing (mkBot pid bid cnv)

providerToken :: Integer -> UUID -> Create (Token Provider)
providerToken dur pid = do
  d <- expiry dur
  newToken d P Nothing (mkProvider pid)

renewToken :: (ToByteString a) => Integer -> Header -> a -> Create (Token a)
renewToken dur hdr bdy = do
  d <- expiry dur
  newToken d (hdr ^. typ) (hdr ^. tag) bdy

newToken :: (ToByteString a) => POSIXTime -> Type -> Maybe Tag -> a -> Create (Token a)
newToken ti ty ta a = do
  k <- Create $ asks keyIdx
  let h = mkHeader tokenVersion k (floor ti) ty ta
  s <- signToken h a
  pure $ mkToken s h a

-----------------------------------------------------------------------------
-- Internal

signToken :: (ToByteString a) => Header -> a -> Create Signature
signToken h a = Create $ do
  f <- (! (h ^. key - 1)) <$> asks zSign
  liftIO . f . toStrict . toLazyByteString $ writeData h a

expiry :: (MonadIO m) => Integer -> m POSIXTime
expiry d = (fromInteger d +) <$> liftIO getPOSIXTime
