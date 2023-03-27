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

module Galley.API.MLS.SubConversation
  ( getSubConversation,
    getLocalSubConversation,
    deleteSubConversation,
    deleteLocalSubConversation,
    getSubConversationGroupInfo,
    getSubConversationGroupInfoFromLocalConv,
    leaveSubConversation,
    HasLeaveSubConversationEffects,
    LeaveSubConversationStaticErrors,
    leaveLocalSubConversation,
    MLSGetSubConvStaticErrors,
    MLSDeleteSubConvStaticErrors,
  )
where

import Control.Arrow
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Time.Clock
import Galley.API.MLS
import Galley.API.MLS.Conversation
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.Removal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.App (Env)
import qualified Galley.Data.Conversation as Data
import Galley.Data.Conversation.Types
import Galley.Effects
import Galley.Effects.FederatorAccess
import qualified Galley.Effects.FederatorAccess as Eff
import qualified Galley.Effects.MemberStore as Eff
import qualified Galley.Effects.SubConversationStore as Eff
import Galley.Effects.SubConversationSupply (SubConversationSupply)
import qualified Galley.Effects.SubConversationSupply as Eff
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.SubConversation

type MLSGetSubConvStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'ConvAccessDenied,
     ErrorS 'MLSSubConvUnsupportedConvType
   ]

getSubConversation ::
  ( Members
      '[ SubConversationStore,
         ConversationStore,
         ErrorS 'ConvNotFound,
         ErrorS 'ConvAccessDenied,
         ErrorS 'MLSSubConvUnsupportedConvType,
         Error FederationError,
         FederatorAccess
       ]
      r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getSubConversation lusr qconv sconv = do
  foldQualified
    lusr
    (\lcnv -> getLocalSubConversation (tUntagged lusr) lcnv sconv)
    (\rcnv -> getRemoteSubConversation lusr rcnv sconv)
    qconv

getLocalSubConversation ::
  Members
    '[ SubConversationStore,
       ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
       ErrorS 'MLSSubConvUnsupportedConvType
     ]
    r =>
  Qualified UserId ->
  Local ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getLocalSubConversation qusr lconv sconv = do
  c <- getConversationAndCheckMembership qusr lconv

  unless (Data.convType c == RegularConv) $
    throwS @'MLSSubConvUnsupportedConvType

  msub <- Eff.getSubConversation (tUnqualified lconv) sconv
  sub <- case msub of
    Nothing -> do
      mlsMeta <- noteS @'ConvNotFound (mlsMetadata c)
      -- deriving this detemernistically to prevent race condition between
      -- multiple threads creating the subconversation
      let groupId = initialGroupId lconv sconv
          epoch = Epoch 0
          suite = cnvmlsCipherSuite mlsMeta
      Eff.createSubConversation (tUnqualified lconv) sconv suite epoch groupId Nothing
      Eff.setGroupIdForSubConversation groupId (tUntagged lconv) sconv
      let sub =
            SubConversation
              { scParentConvId = tUnqualified lconv,
                scSubConvId = sconv,
                scMLSData =
                  ConversationMLSData
                    { cnvmlsGroupId = groupId,
                      cnvmlsEpoch = epoch,
                      cnvmlsEpochTimestamp = Nothing,
                      cnvmlsCipherSuite = suite
                    },
                scMembers = mkClientMap [],
                scIndexMap = mempty -- TODO
              }
      pure sub
    Just sub -> pure sub
  pure (toPublicSubConv (tUntagged (qualifyAs lconv sub)))

getRemoteSubConversation ::
  forall r.
  ( Members
      '[ ErrorS 'ConvNotFound,
         ErrorS 'ConvAccessDenied,
         ErrorS 'MLSSubConvUnsupportedConvType,
         FederatorAccess
       ]
      r,
    RethrowErrors MLSGetSubConvStaticErrors r
  ) =>
  Local UserId ->
  Remote ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getRemoteSubConversation lusr rcnv sconv = do
  res <- runFederated rcnv $ do
    fedClient @'Galley @"get-sub-conversation" $
      GetSubConversationsRequest
        { gsreqUser = tUnqualified lusr,
          gsreqConv = tUnqualified rcnv,
          gsreqSubConv = sconv
        }
  case res of
    GetSubConversationsResponseError e ->
      rethrowErrors @MLSGetSubConvStaticErrors @r e
    GetSubConversationsResponseSuccess subconv ->
      pure subconv

getSubConversationGroupInfo ::
  ( Members
      '[ ConversationStore,
         Error FederationError,
         FederatorAccess,
         Input Env,
         MemberStore,
         SubConversationStore
       ]
      r,
    Members MLSGroupInfoStaticErrors r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  Sem r OpaquePublicGroupState
getSubConversationGroupInfo lusr qcnvId subconv = do
  assertMLSEnabled
  foldQualified
    lusr
    (getSubConversationGroupInfoFromLocalConv (tUntagged lusr) subconv)
    (getGroupInfoFromRemoteConv lusr . fmap (flip SubConv subconv))
    qcnvId

getSubConversationGroupInfoFromLocalConv ::
  Members
    '[ ConversationStore,
       SubConversationStore,
       MemberStore
     ]
    r =>
  Members MLSGroupInfoStaticErrors r =>
  Qualified UserId ->
  SubConvId ->
  Local ConvId ->
  Sem r OpaquePublicGroupState
getSubConversationGroupInfoFromLocalConv qusr subConvId lcnvId = do
  void $ getLocalConvForUser qusr lcnvId
  Eff.getSubConversationPublicGroupState (tUnqualified lcnvId) subConvId
    >>= noteS @'MLSMissingGroupInfo

type MLSDeleteSubConvStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvNotFound,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSStaleMessage
   ]

deleteSubConversation ::
  ( Members
      '[ ConversationStore,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSNotEnabled,
         ErrorS 'MLSStaleMessage,
         Error FederationError,
         FederatorAccess,
         Input Env,
         MemberStore,
         Resource,
         SubConversationStore,
         SubConversationSupply
       ]
      r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  DeleteSubConversationRequest ->
  Sem r ()
deleteSubConversation lusr qconv sconv dsc =
  foldQualified
    lusr
    (\lcnv -> deleteLocalSubConversation (tUntagged lusr) lcnv sconv dsc)
    (\rcnv -> deleteRemoteSubConversation lusr rcnv sconv dsc)
    qconv

deleteLocalSubConversation ::
  ( Members
      '[ ConversationStore,
         ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSNotEnabled,
         ErrorS 'MLSStaleMessage,
         FederatorAccess,
         Input Env,
         MemberStore,
         Resource,
         SubConversationStore,
         SubConversationSupply,
         SubConversationSupply
       ]
      r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  SubConvId ->
  DeleteSubConversationRequest ->
  Sem r ()
deleteLocalSubConversation qusr lcnvId scnvId dsc = do
  assertMLSEnabled
  let cnvId = tUnqualified lcnvId
  cnv <- getConversationAndCheckMembership qusr lcnvId
  cs <- cnvmlsCipherSuite <$> noteS @'ConvNotFound (mlsMetadata cnv)
  (mlsData, oldGid) <- withCommitLock (dscGroupId dsc) (dscEpoch dsc) $ do
    sconv <-
      Eff.getSubConversation cnvId scnvId
        >>= noteS @'ConvNotFound
    let (gid, epoch) = (cnvmlsGroupId &&& cnvmlsEpoch) (scMLSData sconv)
    unless (dscGroupId dsc == gid) $ throwS @'ConvNotFound
    unless (dscEpoch dsc == epoch) $ throwS @'MLSStaleMessage
    Eff.removeAllMLSClients gid

    newGid <- Eff.makeFreshGroupId

    Eff.deleteGroupIdForSubConversation gid
    Eff.setGroupIdForSubConversation newGid (tUntagged lcnvId) scnvId

    -- the following overwrites any prior information about the subconversation
    Eff.createSubConversation cnvId scnvId cs (Epoch 0) newGid Nothing

    pure (scMLSData sconv, gid)

  -- notify all backends that the subconversation has a new ID
  let remotes = bucketRemote (map rmId (convRemoteMembers cnv))
  Eff.runFederatedConcurrently_ remotes $ \_ -> do
    void $
      fedClient @'Galley @"on-new-remote-subconversation"
        NewRemoteSubConversation
          { nrscConvId = cnvId,
            nrscSubConvId = scnvId,
            nrscMlsData = mlsData
          }
    fedClient @'Galley @"on-delete-mls-conversation"
      (OnDeleteMLSConversationRequest [oldGid])

deleteRemoteSubConversation ::
  ( Members
      '[ ErrorS 'ConvAccessDenied,
         ErrorS 'ConvNotFound,
         ErrorS 'MLSNotEnabled,
         ErrorS 'MLSStaleMessage,
         Error FederationError,
         FederatorAccess,
         Input Env
       ]
      r
  ) =>
  Local UserId ->
  Remote ConvId ->
  SubConvId ->
  DeleteSubConversationRequest ->
  Sem r ()
deleteRemoteSubConversation lusr rcnvId scnvId dsc = do
  assertMLSEnabled
  let deleteRequest =
        DeleteSubConversationFedRequest
          { dscreqUser = tUnqualified lusr,
            dscreqConv = tUnqualified rcnvId,
            dscreqSubConv = scnvId,
            dscreqGroupId = dscGroupId dsc,
            dscreqEpoch = dscEpoch dsc
          }
  response <-
    runFederated
      rcnvId
      (fedClient @'Galley @"delete-sub-conversation" deleteRequest)
  case response of
    DeleteSubConversationResponseError e -> rethrowErrors @MLSDeleteSubConvStaticErrors e
    DeleteSubConversationResponseSuccess -> pure ()

type HasLeaveSubConversationEffects r =
  ( Members
      '[ ConversationStore,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input Env,
         Input UTCTime,
         MemberStore,
         ProposalStore,
         SubConversationStore,
         TinyLog
       ]
      r
  )

type LeaveSubConversationStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'ConvAccessDenied,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSNotEnabled
   ]

leaveSubConversation ::
  ( HasLeaveSubConversationEffects r,
    Members
      '[ Error MLSProtocolError,
         Error FederationError,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSNotEnabled,
         Resource,
         SubConversationSupply
       ]
      r,
    Members LeaveSubConversationStaticErrors r
  ) =>
  Local UserId ->
  ClientId ->
  Qualified ConvId ->
  SubConvId ->
  Sem r ()
leaveSubConversation lusr cli qcnv sub =
  foldQualified
    lusr
    (leaveLocalSubConversation cid)
    (leaveRemoteSubConversation cid)
    qcnv
    sub
  where
    cid = mkClientIdentity (tUntagged lusr) cli

leaveLocalSubConversation ::
  ( HasLeaveSubConversationEffects r,
    Members
      '[ Error MLSProtocolError,
         ErrorS 'MLSStaleMessage,
         ErrorS 'MLSNotEnabled,
         Resource,
         SubConversationSupply,
         MemberStore
       ]
      r,
    Members LeaveSubConversationStaticErrors r
  ) =>
  ClientIdentity ->
  Local ConvId ->
  SubConvId ->
  Sem r ()
leaveLocalSubConversation cid lcnv sub = do
  assertMLSEnabled
  cnv <- getConversationAndCheckMembership (cidQualifiedUser cid) lcnv
  mlsConv <- noteS @'ConvNotFound =<< mkMLSConversation cnv
  subConv <-
    noteS @'ConvNotFound
      =<< Eff.getSubConversation (tUnqualified lcnv) sub
  idx <-
    note (mlsProtocolError "Client is not a member of the subconversation") $
      cmLookupIndex cid (scMembers subConv)
  -- remove the leaver from the member list
  let (gid, epoch) = (cnvmlsGroupId &&& cnvmlsEpoch) (scMLSData subConv)
  Eff.removeMLSClients gid (cidQualifiedUser cid) . Set.singleton . ciClient $ cid
  let cm = cmRemoveClient cid (scMembers subConv)
  if Map.null cm
    then do
      deleteLocalSubConversation
        (cidQualifiedUser cid)
        lcnv
        sub
        $ DeleteSubConversationRequest gid epoch
    else
      createAndSendRemoveProposals
        (qualifyAs lcnv (SubConv mlsConv subConv))
        (Identity idx)
        (cidQualifiedUser cid)
        cm

leaveRemoteSubConversation ::
  ( Members
      '[ ErrorS 'ConvNotFound,
         ErrorS 'ConvAccessDenied,
         Error FederationError,
         Error MLSProtocolError,
         FederatorAccess
       ]
      r
  ) =>
  ClientIdentity ->
  Remote ConvId ->
  SubConvId ->
  Sem r ()
leaveRemoteSubConversation cid rcnv sub = do
  res <-
    runFederated rcnv $
      fedClient @'Galley @"leave-sub-conversation" $
        LeaveSubConversationRequest
          { lscrUser = ciUser cid,
            lscrClient = ciClient cid,
            lscrConv = tUnqualified rcnv,
            lscrSubConv = sub
          }
  case res of
    LeaveSubConversationResponseError e ->
      rethrowErrors @'[ErrorS 'ConvNotFound, ErrorS 'ConvAccessDenied] e
    LeaveSubConversationResponseProtocolError e ->
      throw (mlsProtocolError e)
    LeaveSubConversationResponseOk -> pure ()
