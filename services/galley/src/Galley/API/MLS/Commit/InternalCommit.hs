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
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Codensity
import Data.Default
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Commit.Core
import Galley.API.MLS.Conversation
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.One2One
import Galley.API.MLS.Proposal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import Galley.Data.Conversation.Types qualified as Data
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.SubConversationStore
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.LeaveReason
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.Proposal qualified as Proposal
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
    Member (ErrorS 'MLSIdentityMismatch) r,
    Member (ErrorS 'MissingLegalholdConsent) r,
    Member (ErrorS 'GroupIdVersionNotSupported) r,
    Member SubConversationStore r,
    Member Resource r,
    Member Random r,
    Member (ErrorS MLSInvalidLeafNodeSignature) r
  ) =>
  SenderIdentity ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  CipherSuiteTag ->
  Bool ->
  Epoch ->
  ProposalAction ->
  Commit ->
  Codensity (Sem r) [LocalConversationUpdate]
processInternalCommit senderIdentity con lConvOrSub ciphersuite ciphersuiteUpdate epoch action commit = do
  let convOrSub = tUnqualified lConvOrSub
      qusr = cidQualifiedUser senderIdentity.client
      cm = convOrSub.members
      newUserClients = Map.assocs (paAdd action)

  -- check that all pending proposals are referenced in the commit
  allPendingProposals <-
    lift $
      getAllPendingProposalRefs (cnvmlsGroupId convOrSub.mlsMeta) epoch
  let referencedProposals = Set.fromList $ mapMaybe (\x -> preview Proposal._Ref x) commit.proposals
  unless (all (`Set.member` referencedProposals) allPendingProposals) $
    lift $
      throwS @'MLSCommitMissingReferences

  -- check update path
  lift $ traverse_ (checkUpdatePath lConvOrSub senderIdentity ciphersuite) commit.path

  withCommitLock (fmap (.id) lConvOrSub) (cnvmlsGroupId convOrSub.mlsMeta) epoch
  lift $ do
    -- no client can be directly added to a subconversation
    when (is _SubConv convOrSub && any ((senderIdentity.client /=) . fst) (cmAssocs (paAdd action))) $
      throw (mlsProtocolError "Add proposals in subconversations are not supported")

    events <-
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
                when (cidQualifiedUser senderIdentity.client == qtarget) $
                  throwS @'MLSSelfRemovalNotAllowed

                -- FUTUREWORK: add tests against this situation for conv v subconv
                when (removedClients /= clientsInConv) $ do
                  -- FUTUREWORK: turn this error into a proper response
                  throwS @'MLSClientMismatch

                pure qtarget

          -- For each user, we compare their clients with the ones being added
          -- to the conversation, and return a list of users for of which we
          -- were unable to get a list of MLS-capable clients.
          --
          -- Again, for subconversations there is no need to check anything
          -- here, so we simply return the empty list.
          failedAddFetching <- case convOrSub.id of
            SubConv _ _ -> pure []
            Conv _ ->
              fmap catMaybes . forM newUserClients $
                \(qtarget, newclients) -> do
                  -- get list of mls clients from Brig (local or remote)
                  mClientData <-
                    fmap mkClientData . hush
                      <$> runError @FederationError (getClientInfo lConvOrSub qtarget ciphersuite)
                  unreachable <- case (mClientData, Map.lookup qtarget cm) of
                    -- user is already present, skip check in this case
                    (_, Just existingClients) -> do
                      -- make sure none of the new clients already exist in the group
                      when
                        ( any
                            (`Map.member` existingClients)
                            (Map.keys newclients)
                        )
                        $ throw
                          (mlsProtocolError "Cannot add a client that is already part of the group")
                      pure False
                    (Nothing, Nothing) -> pure True
                    (Just clientData, Nothing) -> do
                      -- final set of clients in the conversation
                      let clients =
                            Map.keysSet
                              ( fmap fst newclients
                                  <> Map.findWithDefault mempty qtarget cm
                              )

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
                        ( Set.isSubsetOf clientData.allMLSClients clients
                            && Set.isSubsetOf clients clientData.allClients
                        )
                        $
                        -- FUTUREWORK: turn this error into a proper response
                        throwS @'MLSClientMismatch

                      pure False

                  -- Check that new leaf nodes are using the registered signature keys.
                  for_ mClientData $ \clientData ->
                    for_ (Map.assocs newclients) $ \(cid, (_, mKp)) ->
                      checkSignatureKey (fmap (.leafNode) mKp) (Map.lookup cid clientData.infoMap)

                  pure $ guard unreachable $> qtarget
          for_
            (unreachableFromList failedAddFetching)
            (throw . unreachableUsersToUnreachableBackends)

          -- Some types of conversations are created lazily on the first
          -- commit. We do that here, with the commit lock held, but before
          -- applying changes to the member list.
          case convOrSub.id of
            SubConv cnv sub | epoch == Epoch 0 -> do
              -- create subconversation if it doesn't exist
              msub' <- getSubConversation cnv sub
              when (isNothing msub') $
                void $
                  createSubConversation
                    cnv
                    sub
                    convOrSub.mlsMeta.cnvmlsGroupId
              pure []
            Conv _
              | convOrSub.meta.cnvmType == One2OneConv
                  && epoch == Epoch 0 -> do
                  -- create 1-1 conversation with the users as members, set
                  -- epoch to 0 for now, it will be incremented later
                  let senderUser = cidQualifiedUser senderIdentity.client
                      mlsConv = fmap (.conv) lConvOrSub
                      lconv = fmap mcConv mlsConv
                  conv <- case filter ((/= senderUser) . fst) newUserClients of
                    [(otherUser, _)] ->
                      createMLSOne2OneConversation
                        senderUser
                        otherUser
                        mlsConv
                    _ ->
                      throw
                        ( mlsProtocolError
                            "The first commit in a 1-1 conversation should add exactly 1 other user"
                        )
                  -- notify otherUser about being added to this 1-1 conversation
                  let bm = convBotsAndMembers conv
                  members <-
                    note
                      ( InternalErrorWithDescription
                          "Unexpected empty member list in MLS 1-1 conversation"
                      )
                      $ nonEmpty (bmQualifiedMembers lconv bm)
                  update <-
                    notifyConversationAction
                      SConversationJoinTag
                      senderUser
                      False
                      con
                      lconv
                      bm
                      ConversationJoin
                        { users = members,
                          role = roleNameWireMember,
                          joinType = def
                        }
                      def
                  pure [update]
            SubConv _ _ -> pure []
            Conv _ -> do
              -- remove users from the conversation and send events
              removeEvents <-
                foldMap
                  (removeMembers qusr con lConvOrSub)
                  (nonEmpty membersToRemove)

              -- add users to the conversation and send events
              addEvents <-
                foldMap (addMembers qusr con lConvOrSub)
                  . nonEmpty
                  . map fst
                  $ newUserClients
              pure (addEvents <> removeEvents)
        else pure []

    -- Remove clients from the conversation state. This includes client removals
    -- of all types (see Note [client removal]).
    for_ (Map.assocs (paRemove action)) $ \(qtarget, clients) -> do
      removeMLSClients (cnvmlsGroupId convOrSub.mlsMeta) qtarget (Map.keysSet clients)

    -- add clients to the conversation state
    for_ newUserClients $ \(qtarget, newClients) -> do
      addMLSClients (cnvmlsGroupId convOrSub.mlsMeta) qtarget $
        Set.fromList [(cid, idx) | (cid, (idx, _)) <- Map.assocs newClients]

    -- set cipher suite
    when ciphersuiteUpdate $ case convOrSub.id of
      Conv cid -> setConversationCipherSuite cid ciphersuite
      SubConv cid sub -> setSubConversationCipherSuite cid sub ciphersuite

    -- increment epoch number
    for_ lConvOrSub incrementEpoch

    pure events

data ClientData = ClientData
  { allClients :: Set (ClientId),
    allMLSClients :: Set (ClientId),
    infoMap :: Map ClientId ByteString
  }

mkClientData :: Set ClientInfo -> ClientData
mkClientData clientInfo =
  ClientData
    { allClients = Set.map (.clientId) clientInfo,
      allMLSClients =
        Set.map
          (.clientId)
          (Set.filter (.hasKeyPackages) clientInfo),
      infoMap =
        Map.fromList
          [ (info.clientId, key)
            | info <- toList clientInfo,
              key <- toList info.mlsSignatureKey
          ]
    }

addMembers ::
  (HasProposalActionEffects r) =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r [LocalConversationUpdate]
addMembers qusr con lConvOrSub users = case tUnqualified lConvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lConvOrSub (mcConv mlsConv)
    -- FUTUREWORK: update key package ref mapping to reflect conversation membership
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap pure
          . updateLocalConversationUnchecked @'ConversationJoinTag lconv qusr con
          . (\uids -> ConversationJoin uids roleNameWireMember def)
      )
      . nonEmpty
      . filter (flip Set.notMember (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure []

removeMembers ::
  (HasProposalActionEffects r) =>
  Qualified UserId ->
  Maybe ConnId ->
  Local ConvOrSubConv ->
  NonEmpty (Qualified UserId) ->
  Sem r [LocalConversationUpdate]
removeMembers qusr con lConvOrSub users = case tUnqualified lConvOrSub of
  Conv mlsConv -> do
    let lconv = qualifyAs lConvOrSub (mcConv mlsConv)
    foldMap
      ( handleNoChanges
          . handleMLSProposalFailures @ProposalErrors
          . fmap pure
          . updateLocalConversationUnchecked @'ConversationRemoveMembersTag lconv qusr con
          . flip ConversationRemoveMembers EdReasonRemoved
      )
      . nonEmpty
      . filter (flip Set.member (existingMembers lconv))
      . toList
      $ users
  SubConv _ _ -> pure []

handleNoChanges :: (Monoid a) => Sem (Error NoChanges ': r) a -> Sem r a
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
