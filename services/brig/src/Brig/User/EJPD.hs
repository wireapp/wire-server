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

-- | Identify users for law enforcement.  (Wire has legal requirements to cooperate with the
-- authorities.  The wire backend operations team uses this to answer identification requests
-- manually.)
module Brig.User.EJPD (ejpdRequest) where

import Brig.API.Handler
import Brig.API.User (lookupHandle)
import Brig.App
import qualified Brig.Data.Connection as Conn
import Brig.Data.User (lookupUser)
import qualified Brig.IO.Intra as Intra
import Brig.Options (setDefaultUserLocale)
import Brig.Sem.UserQuery (UserQuery)
import Brig.Types.User (HavePendingInvitations (NoPendingInvitations), Locale)
import Control.Error hiding (bool)
import Control.Lens (view, (^.))
import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Qualified
import qualified Data.Set as Set
import Imports hiding (head)
import Polysemy
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Connection (Relation, RelationWithHistory (..), relationDropHistory)
import qualified Wire.API.Push.Token as PushTok
import Wire.API.Routes.Internal.Brig.EJPD (EJPDRequestBody (EJPDRequestBody), EJPDResponseBody (EJPDResponseBody), EJPDResponseItem (EJPDResponseItem))
import qualified Wire.API.Team.Member as Team
import Wire.API.User (User, userDisplayName, userEmail, userHandle, userId, userPhone, userTeam)

ejpdRequest ::
  forall r.
  Member UserQuery r =>
  Maybe Bool ->
  EJPDRequestBody ->
  (Handler r) EJPDResponseBody
ejpdRequest includeContacts (EJPDRequestBody handles) = do
  ExceptT $ Right . EJPDResponseBody . catMaybes <$> forM handles (go1 (fromMaybe False includeContacts))
  where
    -- find uid given handle
    go1 :: Bool -> Handle -> (AppT r) (Maybe EJPDResponseItem)
    go1 includeContacts' handle = do
      loc <- fmap (qTagUnsafe @'QLocal) $ Qualified () <$> viewFederationDomain
      locale <- setDefaultUserLocale <$> view settings
      mbUid <- wrapClient $ lookupHandle handle
      mbUsr <- maybe (pure Nothing) (liftSem . lookupUser loc locale NoPendingInvitations) mbUid
      maybe (pure Nothing) (fmap Just . go2 loc locale includeContacts') mbUsr

    -- construct response item given uid
    go2 :: Local x -> Locale -> Bool -> User -> (AppT r) EJPDResponseItem
    go2 loc locale includeContacts' target = do
      let uid = userId target

      ptoks <-
        PushTok.tokenText . view PushTok.token <$$> wrapHttp (Intra.lookupPushToken uid)

      mbContacts <-
        if includeContacts'
          then do
            contacts :: [(UserId, RelationWithHistory)] <-
              wrapClient $ Conn.lookupContactListWithRelation uid

            contactsFull :: [Maybe (Relation, EJPDResponseItem)] <-
              forM contacts $ \(uid', relationDropHistory -> rel) -> do
                mbUsr <- liftSem $ lookupUser loc locale NoPendingInvitations uid'
                maybe (pure Nothing) (fmap (Just . (rel,)) . go2 loc locale False) mbUsr

            pure . Just . Set.fromList . catMaybes $ contactsFull
          else do
            pure Nothing

      mbTeamContacts <-
        case (includeContacts', userTeam target) of
          (True, Just tid) -> do
            memberList <- wrapHttp $ Intra.getTeamMembers tid
            let members = (view Team.userId <$> (memberList ^. Team.teamMembers)) \\ [uid]

            contactsFull :: [Maybe EJPDResponseItem] <-
              forM members $ \uid' -> do
                mbUsr <- liftSem $ lookupUser loc locale NoPendingInvitations uid'
                maybe (pure Nothing) (fmap Just . go2 loc locale False) mbUsr

            pure . Just . (,Team.toNewListType (memberList ^. Team.teamMemberListType)) . Set.fromList . catMaybes $ contactsFull
          _ -> do
            pure Nothing

      pure $
        EJPDResponseItem
          uid
          (userTeam target)
          (userDisplayName target)
          (userHandle target)
          (userEmail target)
          (userPhone target)
          (Set.fromList ptoks)
          mbContacts
          mbTeamContacts
