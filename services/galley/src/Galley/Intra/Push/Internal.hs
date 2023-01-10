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

module Galley.Intra.Push.Internal where

import Bilge hiding (options)
import Control.Lens (makeLenses, set, view, (.~))
import Data.Aeson (Object)
import Data.Id (ConnId, UserId)
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List1
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Galley.Env
import Galley.Intra.Util
import Galley.Monad
import Galley.Options
import Galley.Types.Conversations.Members
import Gundeck.Types.Push.V2 (RecipientClients (..))
import qualified Gundeck.Types.Push.V2 as Gundeck
import Imports hiding (forkIO)
import UnliftIO.Async (mapConcurrently_)
import Wire.API.Event.Conversation (Event (evtFrom))
import qualified Wire.API.Event.FeatureConfig as FeatureConfig
import qualified Wire.API.Event.Team as Teams
import Wire.API.Team.Member

data PushEvent
  = ConvEvent Event
  | TeamEvent Teams.Event
  | FeatureConfigEvent FeatureConfig.Event

pushEventJson :: PushEvent -> Object
pushEventJson (ConvEvent e) = toJSONObject e
pushEventJson (TeamEvent e) = toJSONObject e
pushEventJson (FeatureConfigEvent e) = toJSONObject e

data RecipientBy user = Recipient
  { _recipientUserId :: user,
    _recipientClients :: RecipientClients
  }
  deriving stock (Functor, Foldable, Traversable, Show)

makeLenses ''RecipientBy

type Recipient = RecipientBy UserId

data PushTo user = Push
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Gundeck.Route,
    _pushNativePriority :: Maybe Gundeck.Priority,
    _pushAsync :: Bool,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: List1 (RecipientBy user),
    pushJson :: Object,
    pushRecipientListType :: ListType
  }
  deriving stock (Functor, Foldable, Traversable, Show)

makeLenses ''PushTo

type Push = PushTo UserId

push :: Foldable f => f Push -> App ()
push ps = do
  let pushes = foldMap (toList . mkPushTo) ps
  traverse_ pushLocal (nonEmpty pushes)
  where
    mkPushTo :: PushTo a -> Maybe (PushTo a)
    mkPushTo p =
      nonEmpty (toList (_pushRecipients p)) <&> \nonEmptyRecipients ->
        p {_pushRecipients = List1 nonEmptyRecipients}

-- | Asynchronously send multiple pushes, aggregating them into as
-- few requests as possible, such that no single request targets
-- more than 128 recipients.
pushLocal :: NonEmpty (PushTo UserId) -> App ()
pushLocal ps = do
  opts <- view options
  let limit = currentFanoutLimit opts
  -- Do not fan out for very large teams
  let (asyncs, syncs) = partition _pushAsync (removeIfLargeFanout limit $ toList ps)
  traverse_ (asyncCall Gundeck <=< jsonChunkedIO) (pushes asyncs)
  mapConcurrently_ (call Gundeck <=< jsonChunkedIO) (pushes syncs)
  where
    pushes :: [PushTo UserId] -> [[Gundeck.Push]]
    pushes = map (map (\p -> toPush p (recipientList p))) . chunk 0 []

    chunk :: Int -> [PushTo a] -> [PushTo a] -> [[PushTo a]]
    chunk _ acc [] = [acc]
    chunk n acc (y : ys)
      | n >= maxRecipients = acc : chunk 0 [] (y : ys)
      | otherwise =
          let totalLength = (n + length (_pushRecipients y))
           in if totalLength > maxRecipients
                then
                  let (y1, y2) = splitPush (maxRecipients - n) y
                   in chunk maxRecipients (y1 : acc) (y2 : ys)
                else chunk totalLength (y : acc) ys

    -- n must be strictly > 0 and < length (_pushRecipients p)
    splitPush :: Int -> PushTo a -> (PushTo a, PushTo a)
    splitPush n p =
      let (r1, r2) = splitAt n (toList (_pushRecipients p))
       in (p {_pushRecipients = fromJust $ maybeList1 r1}, p {_pushRecipients = fromJust $ maybeList1 r2})

    maxRecipients :: Int
    maxRecipients = 128

    recipientList :: PushTo UserId -> [Gundeck.Recipient]
    recipientList p = map (toRecipient p) . toList $ _pushRecipients p

    toPush :: PushTo user -> [Gundeck.Recipient] -> Gundeck.Push
    toPush p r =
      let pload = Gundeck.singletonPayload (pushJson p)
       in Gundeck.newPush (pushOrigin p) (unsafeRange (Set.fromList r)) pload
            & Gundeck.pushOriginConnection .~ _pushConn p
            & Gundeck.pushTransient .~ _pushTransient p
            & maybe id (set Gundeck.pushNativePriority) (_pushNativePriority p)

    toRecipient :: PushTo user -> RecipientBy UserId -> Gundeck.Recipient
    toRecipient p r =
      Gundeck.recipient (_recipientUserId r) (_pushRoute p)
        & Gundeck.recipientClients .~ _recipientClients r

    -- Ensure that under no circumstances we exceed the threshold
    removeIfLargeFanout :: Integral a => Range n m a -> [PushTo user] -> [PushTo user]
    removeIfLargeFanout limit =
      filter
        ( \p ->
            (pushRecipientListType p == ListComplete)
              && (length (_pushRecipients p) <= fromIntegral (fromRange limit))
        )

recipient :: LocalMember -> Recipient
recipient = userRecipient . lmId

userRecipient :: user -> RecipientBy user
userRecipient u = Recipient u RecipientClientsAll

newPush1 :: ListType -> Maybe UserId -> PushEvent -> List1 Recipient -> Push
newPush1 recipientListType from e rr =
  Push
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = Gundeck.RouteAny,
      _pushNativePriority = Nothing,
      _pushAsync = False,
      pushRecipientListType = recipientListType,
      pushJson = pushEventJson e,
      pushOrigin = from,
      _pushRecipients = rr
    }

newPushLocal1 :: ListType -> UserId -> PushEvent -> List1 Recipient -> Push
newPushLocal1 lt uid = newPush1 lt (Just uid)

newPush :: ListType -> Maybe UserId -> PushEvent -> [Recipient] -> Maybe Push
newPush _ _ _ [] = Nothing
newPush t u e (r : rr) = Just $ newPush1 t u e (list1 r rr)

newPushLocal :: ListType -> UserId -> PushEvent -> [Recipient] -> Maybe Push
newPushLocal lt uid = newPush lt (Just uid)

newConversationEventPush :: Event -> Local [UserId] -> Maybe Push
newConversationEventPush e users =
  let musr = guard (tDomain users == qDomain (evtFrom e)) $> qUnqualified (evtFrom e)
   in newPush ListComplete musr (ConvEvent e) (map userRecipient (tUnqualified users))

pushSlowly :: Foldable f => f Push -> App ()
pushSlowly ps = do
  mmillis <- view (options . optSettings . setDeleteConvThrottleMillis)
  let delay = 1000 * fromMaybe defDeleteConvThrottleMillis mmillis
  forM_ ps $ \p -> do
    push [p]
    threadDelay delay
