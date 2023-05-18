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

module Galley.API.MLS.Commit.InternalCommit (processInternalCommit) where

import Control.Comonad
import Control.Error.Util (hush)
import Control.Lens (forOf_, preview)
import Control.Lens.Extras (is)
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.MLS.Commit.Core
import Galley.API.MLS.Conversation
import Galley.API.MLS.Proposal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Types.Conversations.Members
import Galley.Types.UserList
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import qualified Wire.API.MLS.Proposal as Proposal
import Wire.API.MLS.SubConversation
import Wire.API.Unreachable
import Wire.API.User.Client

processInternalCommit ::
  forall r.
  ( HasProposalEffects r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSCommitMissingReferences) r,
    Member (ErrorS 'MLSSelfRemovalNotAllowed) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member SubConversationStore r,
    Member Resource r
  ) =>
  ClientIdentity ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  Epoch ->
  ProposalAction ->
  Commit ->
  Sem r ([LocalConversationUpdate], [ClientIdentity], FailedToProcess)
processInternalCommit senderIdentity con lConvOrSub epoch action commit = do
  let convOrSub = tUnqualified lConvOrSub
      qusr = cidQualifiedUser senderIdentity
      cm = convOrSub.members
      ss = csSignatureScheme (cnvmlsCipherSuite convOrSub.meta)
      newUserClients = Map.assocs (paAdd action)

  -- check all pending proposals are referenced in the commit
  allPendingProposals <- getAllPendingProposalRefs (cnvmlsGroupId convOrSub.meta) epoch
  let referencedProposals = Set.fromList $ mapMaybe (\x -> preview Proposal._Ref x) commit.proposals
  unless (all (`Set.member` referencedProposals) allPendingProposals) $
    throwS @'MLSCommitMissingReferences

  withCommitLock (fmap (.id) lConvOrSub) (cnvmlsGroupId convOrSub.meta) epoch $ do
    -- no client can be directly added to a subconversation
    when (is _SubConv convOrSub && any ((senderIdentity /=) . fst) (cmAssocs (paAdd action))) $
      throw (mlsProtocolError "Add proposals in subconversations are not supported")

    (events, addedClients, failedAddComponent, failedToProcess) <-
      if convOrSub.migrationState == MLSMigrationMLS
        then do
          -- Note [client removal]
          -- We support two types of removals:
          --  1. when a user is removed from a group, all their clients have to be removed
          --  2. when a client is deleted, that particular client (but not necessarily
          --     other clients of the same user) has to be removed.
          --
          -- Type 2 requires no special processing on the backend, so here we filter
          -- out all removals of that type, so that further checks and processing can
          -- be applied only to type 1 removals.
          --
          -- Furthermore, subconversation clients can be removed arbitrarily, so this
          -- processing is only necessary for main conversations. In the
          -- subconversation case, an empty list is returned.
          membersToRemove <- case convOrSub of
            SubConv _ _ -> pure []
            Conv _ -> mapMaybe hush <$$> for (Map.assocs (paRemove action)) $
              \(qtarget, Map.keysSet -> clients) -> runError @() $ do
                let clientsInConv = Map.keysSet (Map.findWithDefault mempty qtarget cm)
                let removedClients = Set.intersection clients clientsInConv

                -- ignore user if none of their clients are being removed
                when (Set.null removedClients) $ throw ()

                -- return error if the user is trying to remove themself
                when (cidQualifiedUser senderIdentity == qtarget) $
                  throwS @'MLSSelfRemovalNotAllowed

                -- FUTUREWORK: add tests against this situation for conv v subconv
                when (removedClients /= clientsInConv) $ do
                  -- FUTUREWORK: turn this error into a proper response
                  throwS @'MLSClientMismatch

                pure qtarget

          -- for each user, we compare their clients with the ones being added to the conversation
          failedAddFetching <- fmap catMaybes . forM newUserClients $
            \(qtarget, newclients) -> case Map.lookup qtarget cm of
              -- user is already present, skip check in this case
              Just _ -> do
                -- new user
                pure Nothing
              Nothing -> do
                -- final set of clients in the conversation
                let clients = Map.keysSet (newclients <> Map.findWithDefault mempty qtarget cm)
                -- get list of mls clients from Brig (local or remote)
                getClientInfo lConvOrSub qtarget ss >>= \case
                  Left _e -> pure (Just qtarget)
                  Right clientInfo -> do
                    let allClients = Set.map ciId clientInfo
                    let allMLSClients = Set.map ciId (Set.filter ciMLS clientInfo)
                    -- We check the following condition:
                    --   allMLSClients ⊆ clients ⊆ allClients
                    -- i.e.
                    -- - if a client has at least 1 key package, it has to be added
                    -- - if a client is being added, it has to still exist
                    --
                    -- The reason why we can't simply check that clients == allMLSClients is
                    -- that a client with no remaining key packages might be added by a user
                    -- who just fetched its last key package.
                    unless
                      ( Set.isSubsetOf allMLSClients clients
                          && Set.isSubsetOf clients allClients
                      )
                      $ do
                        -- FUTUREWORK: turn this error into a proper response
                        throwS @'MLSClientMismatch
                    pure Nothing

          -- remove users from the conversation and send events
          removeEvents <-
            foldMap
              (removeMembers qusr con lConvOrSub)
              (nonEmpty membersToRemove)

          -- if this is a new subconversation, call `on-new-remote-conversation` on all
          -- the remote backends involved in the main conversation
          forOf_ _SubConv convOrSub $ \(mlsConv, subConv) -> do
            when (cnvmlsEpoch (scMLSData subConv) == Epoch 0) $ do
              let remoteDomains =
                    Set.fromList
                      ( map
                          (void . rmId)
                          (mcRemoteMembers mlsConv)
                      )
              let nrc =
                    NewRemoteSubConversation
                      { nrscConvId = mcId mlsConv,
                        nrscSubConvId = scSubConvId subConv,
                        nrscMlsData = scMLSData subConv
                      }
              runFederatedConcurrently_ (toList remoteDomains) $ \_ -> do
                void $ fedClient @'Galley @"on-new-remote-subconversation" nrc
          -- add users to the conversation and send events
          addEvents <-
            foldMap (addMembers qusr con lConvOrSub)
              . nonEmpty
              . filter (\u -> u `notElem` failedAddFetching)
              . map fst
              $ newUserClients
          let failedAdding =
                ulAll lConvOrSub . uncurry ulDiff . both (toUserList lConvOrSub) $
                  ( fst <$> newUserClients,
                    foldMap (onlyJoining . lcuEvent) . fst $ addEvents
                  )
              failedAddComponent = failedAddFetching <> failedAdding
          let failedToProcess =
                failedToAdd failedAddComponent
                  <> snd addEvents
                  <> snd removeEvents
              addedClients =
                cmIdentities $
                  Map.filterWithKey (\k _ -> k `notElem` failedAddComponent) $
                    paAdd action
          pure (fst addEvents <> fst removeEvents, addedClients, failedAddComponent, failedToProcess)
        else pure ([], mempty, mempty, mempty)

    -- Remove clients from the conversation state. This includes client removals
    -- of all types (see Note [client removal]).
    for_ (Map.assocs (paRemove action)) $ \(qtarget, clients) -> do
      removeMLSClients (cnvmlsGroupId convOrSub.meta) qtarget (Map.keysSet clients)

    -- if this is a new subconversation, call `on-new-remote-conversation` on all
    -- the remote backends involved in the main conversation
    forOf_ _SubConv convOrSub $ \(mlsConv, subConv) -> do
      when (cnvmlsEpoch (scMLSData subConv) == Epoch 0) $ do
        let remoteDomains =
              Set.fromList
                ( map
                    (void . rmId)
                    (mcRemoteMembers mlsConv)
                )
        let nrc =
              NewRemoteSubConversation
                { nrscConvId = mcId mlsConv,
                  nrscSubConvId = scSubConvId subConv,
                  nrscMlsData = scMLSData subConv
                }
        runFederatedConcurrently_ (toList remoteDomains) $ \_ -> do
          void $ fedClient @'Galley @"on-new-remote-subconversation" nrc

    let newUserClientsFiltered =
          filter
            (\(u, _) -> u `notElem` failedAddComponent)
            newUserClients
    -- add clients to the conversation state
    for_ newUserClientsFiltered $ \(qtarget, newClients) -> do
      addMLSClients (cnvmlsGroupId convOrSub.meta) qtarget (Set.fromList (Map.assocs newClients))

    -- increment epoch number
    for_ lConvOrSub incrementEpoch

    pure (events, addedClients, failedToProcess)
  where
    onlyJoining :: Event -> [Qualified UserId]
    onlyJoining (evtData -> EdMembersJoin ms) = smQualifiedId <$> mMembers ms
    onlyJoining _ = []

addMembers ::
  HasProposalActionEffects r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r ([LocalConversationUpdate], FailedToProcess)
addMembers qusr con lConvOrSub users = case tUnqualified lConvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lConvOrSub (mcConv mlsConv)
    -- FUTUREWORK: update key package ref mapping to reflect conversation membership
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap (first pure)
          . updateLocalConversationUnchecked @'ConversationJoinTag lconv qusr con
          . flip ConversationJoin roleNameWireMember
      )
      . nonEmpty
      . filter (flip Set.notMember (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure ([], mempty)

removeMembers ::
  HasProposalActionEffects r =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r ([LocalConversationUpdate], FailedToProcess)
removeMembers qusr con lConvOrSub users = case tUnqualified lConvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lConvOrSub (mcConv mlsConv)
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap (first pure)
          . updateLocalConversationUnchecked @'ConversationRemoveMembersTag lconv qusr con
      )
      . nonEmpty
      . filter (flip Set.member (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure ([], mempty)

handleNoChanges :: Monoid a => Sem (Error NoChanges ': r) a -> Sem r a
handleNoChanges = fmap fold . runError

existingLocalMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingLocalMembers lconv =
  (Set.fromList . map (fmap lmId . tUntagged)) (traverse convLocalMembers lconv)

existingRemoteMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingRemoteMembers lconv =
  Set.fromList . map (tUntagged . rmId) . convRemoteMembers . tUnqualified $
    lconv

existingMembers :: Local Data.Conversation -> Set (Qualified UserId)
existingMembers lconv = existingLocalMembers lconv <> existingRemoteMembers lconv
