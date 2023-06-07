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

module Galley.API.MLS.Util where

import Control.Comonad
import Data.Id
import Data.Qualified
import qualified Data.Text as T
import Data.Time.Clock
import Debug.Trace (traceM)
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Data.Types
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.SubConversationStore
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Polysemy.Resource (Resource, bracket)
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as TinyLog
import qualified System.Logger as Log
import Text.Printf (printf)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation

getLocalConvForUser ::
  ( Member (ErrorS 'ConvNotFound) r,
    Member ConversationStore r,
    Member MemberStore r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  Sem r Data.Conversation
getLocalConvForUser qusr lcnv = do
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

  -- check that sender is part of conversation
  isMember' <-
    foldQualified
      lcnv
      ( fmap isJust
          . getLocalMember (convId conv)
          . tUnqualified
      )
      (fmap isJust . getRemoteMember (convId conv))
      qusr
  unless isMember' $ throwS @'ConvNotFound

  pure conv

getPendingBackendRemoveProposals ::
  ( Member ProposalStore r,
    Member TinyLog r
  ) =>
  GroupId ->
  Epoch ->
  Sem r [LeafIndex]
getPendingBackendRemoveProposals gid epoch = do
  proposals <- getAllPendingProposals gid epoch
  catMaybes
    <$> for
      proposals
      ( \case
          (Just ProposalOriginBackend, proposal) -> case value proposal of
            RemoveProposal i -> pure (Just i)
            _ -> pure Nothing
          (Just ProposalOriginClient, _) -> pure Nothing
          (Nothing, _) -> do
            TinyLog.warn $ Log.msg ("found pending proposal without origin, ignoring" :: ByteString)
            pure Nothing
      )

withCommitLock ::
  forall r a.
  ( Members
      '[ Resource,
         ConversationStore,
         ErrorS 'MLSStaleMessage,
         SubConversationStore
       ]
      r
  ) =>
  Local ConvOrSubConvId ->
  GroupId ->
  Epoch ->
  Sem r a ->
  Sem r a
withCommitLock lConvOrSubId gid epoch action =
  bracket
    ( acquireCommitLock gid epoch ttl >>= \lockAcquired ->
        when (lockAcquired == NotAcquired) $
          throwS @'MLSStaleMessage
    )
    (const $ releaseCommitLock gid epoch)
    $ \_ -> do
      actualEpoch <-
        fromMaybe (Epoch 0) <$> case tUnqualified lConvOrSubId of
          Conv cnv -> getConversationEpoch cnv
          SubConv cnv sub -> getSubConversationEpoch cnv sub
      unless (actualEpoch == epoch) $ throwS @'MLSStaleMessage
      action
  where
    ttl = fromIntegral (600 :: Int) -- 10 minutes

getConvFromGroupId :: Member (Error MLSProtocolError) r => GroupId -> Sem r (Qualified ConvOrSubConvId)
getConvFromGroupId = either (throw . mlsProtocolError . T.pack) (pure . fst) . groupIdToConv

timedTrace :: Members '[Input UTCTime] r => String -> Sem r a -> Sem r a
timedTrace label action = do
  tBefore <- input @UTCTime
  res <- action
  tAfter <- input @UTCTime
  let x = fromIntegral (truncate @_ @Int (nominalDiffTimeToSeconds (diffUTCTime tAfter tBefore) * 1000)) / (1000.0 :: Double)
  let msg :: String = printf "TRACE %s (%.1f)" label x
  traceM msg
  pure res
