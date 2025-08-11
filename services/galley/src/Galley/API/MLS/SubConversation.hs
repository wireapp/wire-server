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
    getSubConversationGroupInfo,
    getSubConversationGroupInfoFromLocalConv,
    leaveSubConversation,
    HasLeaveSubConversationEffects,
    LeaveSubConversationStaticErrors,
    leaveLocalSubConversation,
    MLSGetSubConvStaticErrors,
    MLSDeleteSubConvStaticErrors,
    resetLocalSubConversation,
  )
where

import Control.Arrow
import Control.Monad.Codensity hiding (reset)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Galley.API.MLS
import Galley.API.MLS.Conversation
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.Removal
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.App (Env)
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore qualified as Eff
import Galley.Effects.SubConversationStore qualified as Eff
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.Group.Serialisation qualified as Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.StoredConversation
import Wire.StoredConversation qualified as Data

type MLSGetSubConvStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'ConvAccessDenied,
     ErrorS 'MLSSubConvUnsupportedConvType
   ]

getSubConversation ::
  ( Member SubConversationStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'MLSSubConvUnsupportedConvType) r,
    Member (Error FederationError) r,
    Member FederatorAccess r,
    Member TeamStore r
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
  ( Member SubConversationStore r,
    Member ConversationStore r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'MLSSubConvUnsupportedConvType) r,
    Member TeamStore r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getLocalSubConversation qusr lconv sconv = do
  c <- getConversationAsMember qusr lconv

  unless (Data.convType c == RegularConv || Data.convType c == One2OneConv) $
    throwS @'MLSSubConvUnsupportedConvType

  msub <- Eff.getSubConversation (tUnqualified lconv) sconv
  sub <- case msub of
    Nothing -> do
      (_mlsMeta, mlsProtocol) <- noteS @'ConvNotFound (mlsMetadata c)

      case mlsProtocol of
        MLSMigrationMixed -> throwS @'MLSSubConvUnsupportedConvType
        MLSMigrationMLS -> pure ()

      -- deriving this deterministically to prevent race conditions with
      -- multiple threads creating the subconversation
      pure (newSubConversationFromParent lconv sconv)
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
  Sem r GroupInfoData
getSubConversationGroupInfo lusr qcnvId subconv = do
  assertMLSEnabled
  foldQualified
    lusr
    (getSubConversationGroupInfoFromLocalConv (tUntagged lusr) subconv)
    (getGroupInfoFromRemoteConv lusr . fmap (flip SubConv subconv))
    qcnvId

getSubConversationGroupInfoFromLocalConv ::
  ( Members
      '[ ConversationStore,
         SubConversationStore,
         MemberStore
       ]
      r
  ) =>
  (Members MLSGroupInfoStaticErrors r) =>
  Qualified UserId ->
  SubConvId ->
  Local ConvId ->
  Sem r GroupInfoData
getSubConversationGroupInfoFromLocalConv qusr subConvId lcnvId = do
  void $ getLocalConvForUser qusr lcnvId
  Eff.getSubConversationGroupInfo (tUnqualified lcnvId) subConvId
    >>= noteS @'MLSMissingGroupInfo

type MLSDeleteSubConvStaticErrors =
  '[ ErrorS 'ConvAccessDenied,
     ErrorS 'ConvNotFound,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSStaleMessage
   ]

deleteSubConversation ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (Error FederationError) r,
    Member FederatorAccess r,
    Member (Input Env) r,
    Member MemberStore r,
    Member Resource r,
    Member SubConversationStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  MLSReset ->
  Sem r ()
deleteSubConversation lusr qconv sconv reset = do
  assertMLSEnabled
  foldQualified
    lusr
    (\lcnv -> resetLocalSubConversation (tUntagged lusr) lcnv sconv reset)
    (\rcnv -> resetRemoteSubConversation lusr rcnv sconv reset)
    qconv

resetRemoteSubConversation ::
  ( Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (Error FederationError) r,
    Member FederatorAccess r
  ) =>
  Local UserId ->
  Remote ConvId ->
  SubConvId ->
  MLSReset ->
  Sem r ()
resetRemoteSubConversation lusr rcnvId scnvId reset = do
  let deleteRequest =
        DeleteSubConversationFedRequest
          { dscreqUser = tUnqualified lusr,
            dscreqConv = tUnqualified rcnvId,
            dscreqSubConv = scnvId,
            dscreqGroupId = reset.groupId,
            dscreqEpoch = reset.epoch
          }
  response <-
    runFederated
      rcnvId
      (fedClient @'Galley @"delete-sub-conversation" deleteRequest)
  case response of
    DeleteSubConversationResponseError e -> rethrowErrors @MLSDeleteSubConvStaticErrors e
    DeleteSubConversationResponseSuccess -> pure ()

type HasLeaveSubConversationEffects r =
  ( Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member (Input Env) r,
    Member Now r,
    Member MemberStore r,
    Member ProposalStore r,
    Member Random r,
    Member SubConversationStore r,
    Member TinyLog r
  )

type LeaveSubConversationStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'ConvAccessDenied,
     ErrorS 'MLSStaleMessage,
     ErrorS 'MLSNotEnabled
   ]

leaveSubConversation ::
  ( HasLeaveSubConversationEffects r,
    Member (Error MLSProtocolError) r,
    Member (Error FederationError) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member Resource r,
    Members LeaveSubConversationStaticErrors r,
    Member TeamStore r
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
    Member (Error MLSProtocolError) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member (ErrorS 'MLSNotEnabled) r,
    Member (Error FederationError) r,
    Member Resource r,
    Members LeaveSubConversationStaticErrors r,
    Member TeamStore r
  ) =>
  ClientIdentity ->
  Local ConvId ->
  SubConvId ->
  Sem r ()
leaveLocalSubConversation cid lcnv sub = do
  assertMLSEnabled
  cnv <- getConversationAsMember (cidQualifiedUser cid) lcnv
  mlsConv <- noteS @'ConvNotFound =<< mkMLSConversation cnv
  subConv <-
    noteS @'ConvNotFound
      =<< Eff.getSubConversation (tUnqualified lcnv) sub
  idx <-
    note (mlsProtocolError "Client is not a member of the subconversation") $
      cmLookupIndex cid (scMembers subConv)
  let (gid, epoch) = (cnvmlsGroupId &&& cnvmlsEpoch) (scMLSData subConv)
  -- plan to remove the leaver from the member list
  Eff.planClientRemoval gid (Identity cid)
  let cm = cmRemoveClient cid (scMembers subConv)
  if Map.null cm
    then do
      resetLocalSubConversation
        (cidQualifiedUser cid)
        lcnv
        sub
        $ MLSReset gid epoch
    else
      lowerCodensity $
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

resetLocalSubConversation ::
  ( Member ConversationStore r,
    Member (ErrorS 'ConvAccessDenied) r,
    Member (ErrorS 'ConvNotFound) r,
    Member (ErrorS 'MLSStaleMessage) r,
    Member MemberStore r,
    Member Resource r,
    Member SubConversationStore r,
    Member TeamStore r
  ) =>
  Qualified UserId ->
  Local ConvId ->
  SubConvId ->
  MLSReset ->
  Sem r ()
resetLocalSubConversation qusr lcnvId scnvId reset = do
  let cnvId = tUnqualified lcnvId
      lConvOrSubId = qualifyAs lcnvId (SubConv cnvId scnvId)
  cnv <- getConversationAsMember qusr lcnvId

  lowerCodensity $ do
    withCommitLock lConvOrSubId reset.groupId reset.epoch
    lift $ do
      sconv <- Eff.getSubConversation cnvId scnvId >>= noteS @'ConvNotFound
      let (gid, epoch) = (cnvmlsGroupId &&& cnvmlsEpoch) (scMLSData sconv)
      unless (reset.groupId == gid) $ throwS @'ConvNotFound
      unless (reset.epoch == epoch) $ throwS @'MLSStaleMessage
      Eff.removeAllMLSClients gid

      -- swallowing the error and starting with GroupIdGen 0 if nextGenGroupId fails
      let newGid =
            fromRight
              ( Group.newGroupId
                  cnv.metadata.cnvmType
                  (flip SubConv scnvId <$> tUntagged lcnvId)
              )
              $ nextGenGroupId gid

      -- the following overwrites any prior information about the subconversation
      void $ Eff.createSubConversation cnvId scnvId newGid
