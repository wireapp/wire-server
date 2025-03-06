{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
    ZAuthCreation,
    Env,

    -- * Initialisation
    mkEnv,
    interpretZAuthCreation,
    -- runCreate,

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

import Control.Lens
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX
import Data.UUID
import Data.ZAuth.Token hiding (signature)
import Imports
import Polysemy
import Polysemy.Input
import Sodium.Crypto.Sign
import System.Random.MWC

data ZAuthCreation m a where
  NewToken :: (ToByteString b) => POSIXTime -> Type -> Maybe Tag -> b -> ZAuthCreation m (Token b)
  RenewToken :: (ToByteString a) => Integer -> Header -> a -> ZAuthCreation m (Token a)
  UserToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token (User ActualUser))
  SessionToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token (User ActualUser))
  AccessToken :: Integer -> UUID -> Maybe Text -> Word64 -> ZAuthCreation m (Token (Access ActualUser))
  AccessToken1 :: Integer -> UUID -> Maybe Text -> ZAuthCreation m (Token (Access ActualUser))
  LegalHoldUserToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token (User LHUser))
  LegalHoldAccessToken :: Integer -> UUID -> Maybe Text -> Word64 -> ZAuthCreation m (Token (Access LHUser))
  LegalHoldAccessToken1 :: Integer -> UUID -> Maybe Text -> ZAuthCreation m (Token (Access LHUser))
  BotToken :: UUID -> UUID -> UUID -> ZAuthCreation m (Token Bot)
  ProviderToken :: Integer -> UUID -> ZAuthCreation m (Token Provider)

makeSem ''ZAuthCreation

data Env = Env
  { keyIdx :: Int,
    key :: SecretKey,
    randGen :: GenIO
  }

tokenVersion :: Int
tokenVersion = 1

mkEnv :: Int -> SecretKey -> [SecretKey] -> IO Env
mkEnv i k kk = do
  let keys = k : kk
      signingKey =
        if i > 0 && i <= length keys
          then keys !! (i - 1)
          else error "keyIndex out of range"
  Env i signingKey <$> liftIO createSystemRandom

interpretZAuthCreation :: (Member (Embed IO) r) => Env -> InterpreterFor ZAuthCreation r
interpretZAuthCreation env =
  runInputConst env . interpretZAuthCreationInput . raiseUnder

interpretZAuthCreationInput :: (Member (Embed IO) r, Member (Input Env) r) => InterpreterFor ZAuthCreation r
interpretZAuthCreationInput = interpret $ \case
  NewToken tokenTime tokenTyp mTag a ->
    newTokenImpl tokenTime tokenTyp mTag a
  RenewToken dur hdr bdy ->
    renewTokenImpl dur hdr bdy
  UserToken dur usr cli rnd ->
    userTokenImpl dur usr cli rnd
  SessionToken dur usr cli rnd ->
    sessionTokenImpl dur usr cli rnd
  AccessToken dur usr cid con ->
    accessTokenImpl dur usr cid con
  AccessToken1 dur usr cid ->
    accessToken1Impl dur usr cid
  LegalHoldUserToken dur usr cli rnd ->
    legalHoldUserTokenImpl dur usr cli rnd
  LegalHoldAccessToken dur usr cid con ->
    legalHoldAccessTokenImpl dur usr cid con
  LegalHoldAccessToken1 dur usr cid ->
    legalHoldAccessToken1Impl dur usr cid
  BotToken pid bid cnv ->
    botTokenImpl pid bid cnv
  ProviderToken dur pid ->
    providerTokenImpl dur pid

userTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word32 -> Sem r (Token (User ActualUser))
userTokenImpl dur usr cli rnd = do
  d <- expiry dur
  newTokenImpl d U Nothing (mkUser usr cli rnd)

sessionTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word32 -> Sem r (Token (User ActualUser))
sessionTokenImpl dur usr cli rnd = do
  d <- expiry dur
  newTokenImpl d U (Just S) (mkUser usr cli rnd)

newConnId :: (Member (Input Env) r, Member (Embed IO) r) => Sem r Word64
newConnId = do
  env <- input
  embed $ asGenIO (uniform :: GenIO -> IO Word64) env.randGen

-- | Create an access token taking a duration, userId, clientId and a (random)
-- number that can be used as connection identifier
accessTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word64 -> Sem r (Token (Access ActualUser))
accessTokenImpl dur usr cid con = do
  d <- expiry dur
  newTokenImpl d A Nothing (mkAccess usr cid con)

-- | Create an access token taking a duration, userId and clientId.
-- Similar to 'accessToken', except that the connection identifier is randomly
-- generated.
accessToken1Impl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Sem r (Token (Access ActualUser))
accessToken1Impl dur usr cid = do
  d <- newConnId
  accessTokenImpl dur usr cid d

legalHoldUserTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word32 -> Sem r (Token (User LHUser))
legalHoldUserTokenImpl dur usr cli rnd = do
  d <- expiry dur
  newTokenImpl d LU Nothing (mkUser usr cli rnd)

-- | Create a legal hold access token taking a duration, userId, clientId and a
-- (random) number that can be used as connection identifier
legalHoldAccessTokenImpl ::
  (Member (Input Env) r, Member (Embed IO) r) =>
  Integer ->
  UUID ->
  Maybe Text ->
  Word64 ->
  Sem r (Token (Access LHUser))
legalHoldAccessTokenImpl dur usr cid con = do
  d <- expiry dur
  newTokenImpl d LA Nothing (mkAccess usr cid con)

-- | Create a legal hold access token taking a duration, userId. Similar to 'legalHoldAccessToken', except that the connection identifier is randomly generated.
legalHoldAccessToken1Impl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Sem r (Token (Access LHUser))
legalHoldAccessToken1Impl dur usr cid = do
  d <- newConnId
  legalHoldAccessTokenImpl dur usr cid d

botTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => UUID -> UUID -> UUID -> Sem r (Token Bot)
botTokenImpl pid bid cnv = newTokenImpl (-1) B Nothing (mkBot pid bid cnv)

providerTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Sem r (Token Provider)
providerTokenImpl dur pid = do
  d <- expiry dur
  newTokenImpl d P Nothing (mkProvider pid)

newTokenImpl :: (ToByteString a, Member (Embed IO) r, Member (Input Env) r) => POSIXTime -> Type -> Maybe Tag -> a -> Sem r (Token a)
newTokenImpl tokenTime tokenTyp mTag a = do
  env <- input
  let h = mkHeader tokenVersion env.keyIdx (floor tokenTime) tokenTyp mTag
  s <- embed $ signToken env h a
  pure $ mkToken s h a

renewTokenImpl :: (ToByteString a, Member (Input Env) r, Member (Embed IO) r) => Integer -> Header -> a -> Sem r (Token a)
renewTokenImpl dur hdr bdy = do
  d <- expiry dur
  newTokenImpl d (hdr ^. typ) (hdr ^. tag) bdy

-----------------------------------------------------------------------------
-- Internal

signToken :: (ToByteString a) => Env -> Header -> a -> IO Signature
signToken e h a = do
  liftIO . signature e.key . toStrict . toLazyByteString $ writeData h a

expiry :: (MonadIO m) => Integer -> m POSIXTime
expiry d = (fromInteger d +) <$> liftIO getPOSIXTime
