{-# LANGUAGE DeepSubsumption #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Effects.SFTStore.Cassandra
  ( interpretSFTStoreToCassandra,
    interpretSFTStoreToConstant,
  )
where

import Brig.Effects.SFTStore
import Cassandra
import Cassandra.Settings
import Control.Error
import Control.Lens ((^.))
import Data.Id
import Data.Text.Ascii
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Internal.Tactics
import Wire.API.Call.Config

interpretSFTStoreToCassandra ::
  forall r a.
  (Member (Embed Client) r) =>
  Sem (SFTStore ': r) a ->
  Sem r a
interpretSFTStoreToCassandra =
  interpretH $
    liftT . embed @Client . \case
      SftStoreCredential u c username credential ttl -> storeCredential u c username credential ttl
      SftGetCredential u c -> getCredential u c

interpretSFTStoreToConstant ::
  forall r a.
  Sem (SFTStore ': r) a ->
  Sem r a
interpretSFTStoreToConstant =
  interpretH $
    liftT . \case
      SftStoreCredential _u _c _username _credential _ttl -> pure True
      SftGetCredential _u _c -> pure $ Just (mkSFTUsername 12 "username", "credential")

checkTransSuccess :: [Row] -> Bool
checkTransSuccess [] = False
checkTransSuccess (row : _) = fromMaybe False . hush $ fromRow 0 row

storeCredential :: MonadClient m => UserId -> ClientId -> SFTUsername -> AsciiBase64 -> Int32 -> m Bool
storeCredential u c username credential ttl =
  checkTransSuccess
    <$> retry
      x5
      ( trans
          addCredential
          ( params
              LocalQuorum
              ( u,
                c,
                floor $ nominalDiffTimeToSeconds (username ^. suExpiresAt),
                fromIntegral (username ^. suVersion),
                fromIntegral (username ^. suKeyindex),
                username ^. suS,
                username ^. suRandom,
                credential,
                ttl
              )
          )
            { serialConsistency = Just LocalSerialConsistency
            }
      )
  where
    addCredential :: PrepQuery W (UserId, ClientId, Integer, Int64, Int64, Bool, Text, AsciiBase64, Int32) Row
    addCredential = "INSERT INTO sft_credential (user, client, expiry, version, key_index, s, random, credential) VALUES (?, ?, ?, ?, ?, ?, ?, ?) IF NOT EXISTS USING TTL ?"

getCredential ::
  MonadClient m =>
  UserId ->
  ClientId ->
  m (Maybe (SFTUsername, AsciiBase64))
getCredential u c = mergeColumns <$$> retry x1 (query1 q (params LocalQuorum (u, c)))
  where
    q :: PrepQuery R (UserId, ClientId) (Integer, Int64, Int64, Bool, Text, AsciiBase64)
    q = "SELECT expiry, version, key_index, s, random, credential FROM sft_credential WHERE user = ? AND client = ? LIMIT 1"
    mergeColumns (expiry, version, index, s, rand, credential) =
      ( SFTUsername
          (fromInteger expiry)
          (fromIntegral version)
          (fromIntegral index)
          s
          rand,
        credential
      )
