{-# LANGUAGE OverloadedStrings #-}

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

-- | Common operations and data types in simulations.
module Network.Wire.Simulations
  ( -- * Conversation Setup
    prepareConv,

    -- * Messages
    BotMessage (..),
    mkAssetMsg,
    mkTextMsg,
    AssetInfo,
    assetInfoKey,
    assetInfoToken,
    assetInfoKeys,

    -- * Assertions
    requireAssetMsg,
    requireTextMsg,
    assertNoClientMismatch,
    assertClientMissing,

    -- * Re-exports
    encode,
    decode,
  )
where

import Control.Lens ((^.))
import Control.Monad.Catch
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Id (ConvId, UserId, makeIdOpaque)
import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Imports
import Network.Wire.Bot
import Network.Wire.Bot.Assert
import Network.Wire.Bot.Crypto
import Network.Wire.Client.API.Asset
import Network.Wire.Client.API.Conversation
import Network.Wire.Client.API.User hiding (Asset (..))

--------------------------------------------------------------------------------
-- Conversation Setup

-- | Set up a conversation between a given list of bots. The first bot will
-- connect to everybody else and then create a conversation.
prepareConv :: [Bot] -> BotNet ConvId
prepareConv [] = error "prepareConv: at least two bots required"
prepareConv [_] = error "prepareConv: at least two bots required"
prepareConv [a, b] = do
  connectIfNeeded a b
  conv <- (>>= ucConvId) <$> runBotSession a (getConnection (botId b))
  requireMaybe conv $
    "Missing 1-1 conversation between: "
      <> Text.concat (Text.pack . show . botId <$> [a, b])
prepareConv (a : bs) = do
  mapM_ (connectIfNeeded a) bs
  let bIds = map botId bs
  conv <- cnvId <$> runBotSession a (createConv bIds Nothing)
  assertConvCreated conv a bs
  return conv

-- | Make sure that there is a connection between two bots.
connectIfNeeded :: Bot -> Bot -> BotNet ()
connectIfNeeded = go 6 -- six turns should be enough
  where
    -- Make steps towards a successful connection by alternating turns
    -- (first one side takes a step towards a connection, then another,
    -- etc). If we make more than N turns, we give up.
    go :: Int -> Bot -> Bot -> BotNet ()
    go 0 _ _ = return ()
    go n a b = do
      connected <- runBotSession a $ do
        s <- fmap ucStatus <$> getConnection (botId b)
        case s of
          -- If no connection: initiate one
          Nothing -> do
            void $ connectTo (ConnectionRequest (makeIdOpaque (botId b)) (fromMaybe "" (botEmail a)) (Message "Hi there!"))
            assertConnectRequested a b
            return False
          -- If there's a pending connection to us: accept it
          Just Pending -> do
            void $ updateConnection (botId b) (ConnectionUpdate Accepted)
            assertConnectAccepted a b
            return True
          -- If we have sent a request, we can't do anything
          Just Sent -> return False
          -- In case of any other status, we pretend it's good
          _ -> return True
      unless connected (go (n - 1) b a)

--------------------------------------------------------------------------------
-- Messages

data BotMessage
  = BotAssetMessage AssetInfo
  | BotTextMessage Text
  deriving (Eq, Show)

instance Serialize BotMessage where
  put (BotTextMessage m) = putWord8 1 >> putByteString (Text.encodeUtf8 m)
  put (BotAssetMessage i) = putWord8 2 >> put i

  get = do
    t <- getWord8
    case t of
      1 -> do
        bs <- remaining >>= getByteString
        either
          (fail . show)
          (return . BotTextMessage)
          (Text.decodeUtf8' bs)
      2 -> BotAssetMessage <$> get
      _ -> fail $ "Unexpected message type: " ++ show t

data AssetInfo
  = AssetInfo
      { assetInfoKey :: !AssetKey,
        assetInfoToken :: !(Maybe AssetToken),
        assetInfoKeys :: !SymmetricKeys
      }
  deriving (Eq, Show)

instance Serialize AssetInfo where
  put (AssetInfo key tok keys) = do
    let k = toByteString' key
    putWord16be (fromIntegral (BS.length k))
    putByteString k
    let t = maybe "" toByteString' tok
    putWord16be (fromIntegral (BS.length t))
    putByteString t
    put keys

  get = do
    klen <- getWord16be
    kbs <- getByteString (fromIntegral klen)
    k <-
      maybe
        (fail "Invalid asset key")
        return
        (fromByteString kbs)
    tlen <- getWord16be
    t <-
      if tlen == 0
        then return Nothing
        else do
          tbs <- getByteString (fromIntegral tlen)
          maybe
            (fail "Invalid asset token")
            (return . Just)
            (fromByteString tbs)
    AssetInfo k t <$> get

mkAssetMsg :: Asset -> SymmetricKeys -> BotMessage
mkAssetMsg a = BotAssetMessage . AssetInfo (a ^. assetKey) (a ^. assetToken)

mkTextMsg :: Text -> BotMessage
mkTextMsg = BotTextMessage

--------------------------------------------------------------------------------
-- Assertions

requireAssetMsg :: MonadThrow m => ByteString -> m AssetInfo
requireAssetMsg bs = do
  m <- requireMessage bs
  case m of
    BotAssetMessage info -> return info
    x -> throwM $ RequirementFailed ("Unexpected message: " <> Text.pack (show x))

requireTextMsg :: MonadThrow m => ByteString -> m Text
requireTextMsg bs = do
  m <- requireMessage bs
  case m of
    BotTextMessage t -> return t
    x -> throwM $ RequirementFailed ("Unexpected message: " <> Text.pack (show x))

requireMessage :: MonadThrow m => ByteString -> m BotMessage
requireMessage = requireRight . decode

assertNoClientMismatch ::
  HasCallStack =>
  ClientMismatch ->
  BotSession ()
assertNoClientMismatch cm = do
  assertEqual (UserClients Map.empty) (missingClients cm) "Missing Clients"
  assertEqual (UserClients Map.empty) (redundantClients cm) "Redundant Clients"
  assertEqual (UserClients Map.empty) (deletedClients cm) "Deleted Clients"

assertClientMissing ::
  HasCallStack =>
  UserId ->
  BotClient ->
  ClientMismatch ->
  BotSession ()
assertClientMissing u d cm =
  assertEqual
    (UserClients (Map.singleton (makeIdOpaque u) (Set.singleton $ botClientId d)))
    (missingClients cm)
    "Missing Clients"
