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
{-# OPTIONS_GHC -Wwarn #-}

-- | Identify users for law enforcement.  (Wire has legal requirements to cooperate with the
-- authorities.  The wire backend operations team uses this to answer identification requests
-- manually.)
module Brig.User.EJPD (ejpdRequest) where

import Bilge.Request
import Bilge.Response
import Brig.API.Handler
import Brig.API.User (lookupHandle, lookupProfiles)
import Brig.App
import Brig.Data.Connection qualified as Conn
import Brig.Data.User (lookupUser)
import Brig.Effects.ConnectionStore
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider qualified as GalleyProvider
import Control.Arrow
import Control.Error hiding (bool)
import Control.Lens (view, (^.))
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Imports hiding (head)
import Network.HTTP.Types.Method
import Polysemy
import Polysemy.TinyLog
import Servant.OpenApi.Internal.Orphans ()
import System.Logger.Message qualified as Log
import Wire.API.Connection (Relation, RelationWithHistory (..), UserConnection (..), relationDropHistory)
import Wire.API.Federation.Error
import Wire.API.Push.Token qualified as PushTok
import Wire.API.Routes.Internal.Brig.EJPD
import Wire.API.Team.Member qualified as Team
import Wire.API.User
import Wire.NotificationSubsystem
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

ejpdRequest ::
  forall r.
  ( Member GalleyProvider r,
    Member NotificationSubsystem r,
    Member Rpc r,
    Member (ConnectionStore InternalPaging) r,
    Member (Concurrency 'Unsafe) r,
    Member (Final IO) r, -- 'lookupProfiles' has ExceptT and ReaderT in its return type.
    Member TinyLog r
  ) =>
  Maybe Bool ->
  EJPDRequestBody ->
  (Handler r) EJPDResponseBody
ejpdRequest (fromMaybe False -> includeContacts) (EJPDRequestBody handles) = do
  ExceptT $ Right . EJPDResponseBody . catMaybes <$> forM handles responseItemForHandle
  where
    -- find uid given handle
    responseItemForHandle :: Handle -> (AppT r) (Maybe EJPDResponseItem)
    responseItemForHandle hdl = do
      mbUid <- wrapClient $ lookupHandle hdl
      mbUsr <- maybe (pure Nothing) (wrapClient . lookupUser NoPendingInvitations) mbUid
      maybe (pure Nothing) (fmap Just . responseItemForExistingUser includeContacts) mbUsr

    -- construct response item given uid
    responseItemForExistingUser :: Bool -> User -> (AppT r) EJPDResponseItem
    responseItemForExistingUser reallyIncludeContacts target = do
      let uid = userId target
      luid <- qualifyLocal uid

      ptoks <-
        PushTok.tokenText . view PushTok.token <$$> liftSem (getPushTokens uid)

      mbContacts <-
        if reallyIncludeContacts
          then do
            env <- ask
            contacts :: [(UserId, RelationWithHistory)] <-
              wrapClient $ -- FUTUREWORK: use polysemy effect, not wrapClient
                Conn.lookupContactListWithRelation uid

            localContacts :: [EJPDContact] <-
              catMaybes <$> do
                forM contacts $ \(uid', relationDropHistory -> rel) -> do
                  mbUsr <- wrapClient $ lookupUser NoPendingInvitations uid' -- FUTUREWORK: use polysemy effect, not wrapClient
                  maybe (pure Nothing) (fmap (Just . EJPDContactFound rel) . responseItemForExistingUser False) mbUsr

            remoteContacts :: [EJPDContact] <- liftSem do
              let getPage = flip (remoteConnectedUsersPaginated luid) maxBound
              firstPage <- getPage Nothing

              let userProfiles :: (Page InternalPaging (Remote UserConnection)) -> Sem r [EJPDContact]
                  userProfiles pg = do
                    let qualifiedRemoteContactIds :: [(Qualified UserId, Relation)]
                        qualifiedRemoteContactIds = (ucTo &&& ucStatus) . qUnqualified . tUntagged <$> pageItems pg
                        userRelationMap = Map.fromList qualifiedRemoteContactIds

                        mkItem :: UserProfile -> Maybe EJPDContact
                        mkItem up = do
                          rel <- Map.lookup up.profileQualifiedId userRelationMap
                          pure $
                            EJPDContactFound rel $
                              EJPDResponseItem
                                { ejpdResponseUserId = up.profileQualifiedId,
                                  ejpdResponseTeamId = up.profileTeam,
                                  ejpdResponseName = up.profileName,
                                  ejpdResponseHandle = up.profileHandle,
                                  ejpdResponseEmail = up.profileEmail,
                                  ejpdResponsePhone = Nothing,
                                  ejpdResponsePushTokens = mempty,
                                  -- this is already a contact of the requested subject, so we don't need to fetch contacts recursively
                                  ejpdResponseContacts = Nothing,
                                  -- same as above
                                  ejpdResponseTeamContacts = Nothing,
                                  -- same as above
                                  ejpdResponseConversations = Nothing,
                                  ejpdResponseAssets = Nothing
                                }

                        logErr :: FederationError -> Sem r ()
                        logErr e = warn $ Log.msg $ "error fetching remote contact profile:\n" <> displayException e

                    profs :: Either FederationError [UserProfile] <-
                      runReaderT (unAppT (runExceptT (lookupProfiles luid (map fst qualifiedRemoteContactIds)))) env

                    either (\e -> logErr e $> (map (EJPDContactRemoteError . fst) qualifiedRemoteContactIds)) (pure . mapMaybe mkItem) profs

                  go :: InternalPage (QualifiedWithTag 'QRemote UserConnection) -> Sem r [EJPDContact]
                  go page =
                    if pageHasMore page
                      then do
                        newPage <- getPage $ Just $ pageState page
                        pis <- userProfiles page
                        (pis <>) <$> go newPage
                      else userProfiles page

              go firstPage

            pure . Just . Set.fromList $ localContacts <> remoteContacts
          else do
            pure Nothing

      mbTeamContacts <-
        case (reallyIncludeContacts, userTeam target) of
          (True, Just tid) -> do
            memberList <- liftSem $ GalleyProvider.getTeamMembers tid
            let members = (view Team.userId <$> (memberList ^. Team.teamMembers)) \\ [uid]

            contactsFull :: [Maybe EJPDResponseItem] <-
              forM members $ \uid' -> do
                mbUsr <- wrapClient $ lookupUser NoPendingInvitations uid'
                maybe (pure Nothing) (fmap Just . responseItemForExistingUser False) mbUsr

            pure . Just . (,Team.toNewListType (memberList ^. Team.teamMemberListType)) . Set.fromList . catMaybes $ contactsFull
          _ -> do
            pure Nothing

      mbConversations <-
        if reallyIncludeContacts
          then liftSem $ Just . Set.fromList <$> GalleyProvider.getEJPDConvInfo uid
          else pure Nothing

      mbAssets <- do
        urls <- forM (userAssets target) $ \(asset :: Asset) -> do
          cgh <- asks (view cargoholdEndpoint)
          let key = toByteString' $ assetKey asset
          resp <- liftSem $ rpcWithRetries "cargohold" cgh (method GET . paths ["/i/assets", key])
          pure $
            case (statusCode resp, responseJsonEither resp) of
              (200, Right (A.String loc)) -> loc
              _ ->
                T.pack $
                  "could not fetch asset: "
                    <> show key
                    <> ", error: "
                    <> show (statusCode resp, responseBody resp)
        pure $ case urls of
          [] -> Nothing
          something -> Just (Set.fromList something)

      pure $
        EJPDResponseItem
          (tUntagged luid)
          (userTeam target)
          (userDisplayName target)
          (userHandle target)
          (userEmail target)
          (userPhone target)
          (Set.fromList ptoks)
          mbContacts
          mbTeamContacts
          mbConversations
          mbAssets
