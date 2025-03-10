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

    -- * Specific

    -- accessToken,
    -- accessToken1,
    -- userToken,
    -- sessionToken,
    -- botToken,
    -- providerToken,

    -- * Generic
    newToken,
    renewToken,
  )
where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX
import Data.ZAuth.Token hiding (signature)
import Imports
import Polysemy
import Polysemy.Input
import Sodium.Crypto.Sign

-- data ZAuthCreation m a where
--   NewToken :: (ToByteString b) => POSIXTime -> Type -> Maybe Tag -> b -> ZAuthCreation m (Token b)
--   RenewToken :: (ToByteString a) => Integer -> Header -> a -> ZAuthCreation m (Token a)
--   UserToken :: (KnownUserTokenType t) => Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token (User t))
--   SessionToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token (User ActualUser))
--   AccessToken :: (KnownUserTokenType t) => Integer -> UUID -> Maybe Text -> Word64 -> ZAuthCreation m (Token (Access t))
--   AccessToken1 :: (KnownUserTokenType t) => Integer -> UUID -> Maybe Text -> ZAuthCreation m (Token (Access t))
--   BotToken :: UUID -> UUID -> UUID -> ZAuthCreation m (Token Bot)
--   ProviderToken :: Integer -> UUID -> ZAuthCreation m (Token Provider)

data ZAuthCreation m a where
  NewToken :: (ToByteString (Body t), KnownType t) => POSIXTime -> Maybe Tag -> Body t -> ZAuthCreation m (Token t)
  RenewToken :: (ToByteString (Body t), KnownType t) => Integer -> Header t -> Body t -> ZAuthCreation m (Token t)

-- UserToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token U)
-- SessionToken :: Integer -> UUID -> Maybe Text -> Word32 -> ZAuthCreation m (Token U)
-- AccessToken :: Integer -> UUID -> Maybe Text -> Word64 -> ZAuthCreation m (Token A)
-- AccessToken1 :: Integer -> UUID -> Maybe Text -> ZAuthCreation m (Token A)
-- BotToken :: UUID -> UUID -> UUID -> ZAuthCreation m (Token B)
-- ProviderToken :: Integer -> UUID -> ZAuthCreation m (Token P)

makeSem ''ZAuthCreation

data Env = Env
  { keyIdx :: Int,
    key :: SecretKey
    -- randGen :: GenIO
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
  pure $ Env i signingKey -- <$> liftIO createSystemRandom

interpretZAuthCreation :: (Member (Embed IO) r) => Env -> InterpreterFor ZAuthCreation r
interpretZAuthCreation env =
  runInputConst env . interpretZAuthCreationInput . raiseUnder

interpretZAuthCreationInput :: (Member (Embed IO) r, Member (Input Env) r) => InterpreterFor ZAuthCreation r
interpretZAuthCreationInput = interpret $ \case
  NewToken tokenTime mTag body -> newTokenImpl tokenTime mTag body
  RenewToken dur hdr bdy -> renewTokenImpl dur hdr bdy

-- -- TODO: This should work for LU too
-- userTokenImpl :: forall r. (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word32 -> Sem r (Token U)
-- userTokenImpl dur usr cli rnd = do
--   d <- expiry dur
--   newTokenImpl d Nothing (User usr cli rnd)

-- sessionTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word32 -> Sem r (Token U)
-- sessionTokenImpl dur usr cli rnd = do
--   d <- expiry dur
--   newTokenImpl d (Just S) (User usr cli rnd)

-- newConnId :: (Member (Input Env) r, Member (Embed IO) r) => Sem r Word64
-- newConnId = do
--   env <- input
--   embed $ asGenIO (uniform :: GenIO -> IO Word64) env.randGen

-- -- | Create an access token taking a duration, userId, clientId and a (random)
-- -- number that can be used as connection identifier
-- -- TODO: This should work for LA too
-- accessTokenImpl :: forall r. (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Word64 -> Sem r (Token A)
-- accessTokenImpl dur usr cid con = do
--   d <- expiry dur
--   newTokenImpl d Nothing (Access usr cid con)

-- -- | Create an access token taking a duration, userId and clientId.
-- -- Similar to 'accessToken', except that the connection identifier is randomly
-- -- generated.
-- -- TODO: This should work for LA too
-- accessToken1Impl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Maybe Text -> Sem r (Token A)
-- accessToken1Impl dur usr cid = do
--   d <- newConnId
--   accessTokenImpl dur usr cid d

-- botTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => UUID -> UUID -> UUID -> Sem r (Token B)
-- botTokenImpl pid bid cnv = newTokenImpl (-1) Nothing (Bot pid bid cnv)

-- providerTokenImpl :: (Member (Input Env) r, Member (Embed IO) r) => Integer -> UUID -> Sem r (Token P)
-- providerTokenImpl dur pid = do
--   d <- expiry dur
--   newTokenImpl d Nothing (Provider pid)

newTokenImpl :: (Member (Embed IO) r, Member (Input Env) r, ToByteString (Body t), KnownType t) => POSIXTime -> Maybe Tag -> Body t -> Sem r (Token t)
newTokenImpl tokenTime mTag a = do
  env <- input
  let h = Header tokenVersion env.keyIdx (floor tokenTime) mTag
  s <- embed $ signToken env h a
  pure $ Token s h a

renewTokenImpl :: (Member (Input Env) r, Member (Embed IO) r, KnownType t, ToByteString (Body t)) => Integer -> Header t -> Body t -> Sem r (Token t)
renewTokenImpl dur hdr bdy = do
  d <- expiry dur
  newTokenImpl d (hdr.tag) bdy

-----------------------------------------------------------------------------
-- Internal

signToken :: (ToByteString (Body t), KnownType t) => Env -> Header t -> Body t -> IO Signature
signToken e h a = do
  liftIO . signature e.key . toStrict . toLazyByteString $ writeData h a

expiry :: (MonadIO m) => Integer -> m POSIXTime
expiry d = (fromInteger d +) <$> liftIO getPOSIXTime
