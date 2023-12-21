{-# LANGUAGE TemplateHaskell #-}

module Wire.NotificationSubsystem where

import Control.Lens (makeLenses, set, (.~))
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Range (Range, fromRange, unsafeRange)
import Data.Set qualified as Set
import Gundeck.Types hiding (Push (..), Recipient, newPush)
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Numeric.Natural (Natural)
import Polysemy
import Polysemy.Async (Async, sequenceConcurrently)
import Polysemy.Input
import Wire.API.Team.Member
import Wire.Arbitrary
import Wire.GundeckAPIAccess
import Wire.Sem.Delay (Delay, delay)

data RecipientBy user = Recipient
  { _recipientUserId :: user,
    _recipientClients :: RecipientClients
  }
  deriving stock (Functor, Foldable, Traversable, Show, Ord, Eq, Generic)
  deriving (Arbitrary) via GenericUniform (RecipientBy user)

makeLenses ''RecipientBy

type Recipient = RecipientBy UserId

data PushTo user = PushTo
  { _pushConn :: Maybe ConnId,
    _pushTransient :: Bool,
    _pushRoute :: Route,
    _pushNativePriority :: Maybe Priority,
    -- we never push asynchronounsly
    -- _pushAsync :: Bool,
    pushOrigin :: Maybe UserId,
    _pushRecipients :: NonEmpty (RecipientBy user),
    pushJson :: Object
    -- we probably don't rely on the list type
    -- pushRecipientListType :: ListType
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable, Show)
  deriving (Arbitrary) via GenericUniform (PushTo user)

makeLenses ''PushTo

type PushToUser = PushTo UserId

data NotificationSubsystem m a where
  Push :: [PushToUser] -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck ::
  ( Member (GundeckAPIAccess) r,
    Member Async r,
    Member (Input NotificationSubsystemConfig) r
  ) =>
  Sem (NotificationSubsystem : r) a ->
  Sem r a
runNotificationSubsystemGundeck = interpret $ \case
  Push ps -> pushImpl ps

data NotificationSubsystemConfig = NotificationSubsystemConfig
  { fanoutLimit :: Range 1 HardTruncationLimit Int32,
    chunkSize :: Natural
  }

-- TODO: write a test for listtype
pushImpl ::
  forall r.
  ( Member (GundeckAPIAccess) r,
    Member (Input NotificationSubsystemConfig) r,
    Member (Async) r
  ) =>
  [PushToUser] ->
  Sem r ()
pushImpl ps = do
  currentFanoutLimit <- inputs fanoutLimit
  pushChunkSize <- inputs chunkSize

  let pushes :: [[V2.Push]] =
        mkPushes pushChunkSize $
          removeIfLargeFanout currentFanoutLimit ps
  void $
    sequenceConcurrently $
      pushV2 <$> pushes

removeIfLargeFanout :: Range n m Int32 -> [PushTo user] -> [PushTo user]
removeIfLargeFanout limit =
  filter \PushTo {_pushRecipients} ->
    length _pushRecipients <= fromIntegral (fromRange limit)

mkPushes :: Natural -> [PushToUser] -> [[V2.Push]]
mkPushes chunkSize = map (map toV2Push) . chunkPushes chunkSize

{-# INLINE [1] toV2Push #-}
toV2Push :: PushToUser -> V2.Push
toV2Push p =
  (V2.newPush p.pushOrigin (unsafeRange (Set.fromList recipients)) pload)
    & V2.pushOriginConnection .~ _pushConn p
    & V2.pushTransient .~ _pushTransient p
    & maybe id (set V2.pushNativePriority) p._pushNativePriority
  where
    pload :: List1 Object
    pload = List1.singleton (pushJson p)
    recipients :: [V2.Recipient]
    recipients = map toRecipient $ toList p._pushRecipients
    toRecipient :: RecipientBy UserId -> V2.Recipient
    toRecipient r =
      (recipient r._recipientUserId p._pushRoute)
        { V2._recipientClients = r._recipientClients
        }

{-# INLINE [1] chunkPushes #-}
chunkPushes :: Natural -> [PushTo a] -> [[PushTo a]]
chunkPushes maxRecipients
  | maxRecipients > 0 = go 0 []
  | otherwise = const []
  where
    go _ [] [] = []
    go _ acc [] = [acc]
    go n acc (y : ys)
      | n >= maxRecipients = acc : go 0 [] (y : ys)
      | otherwise =
          let totalLength = (n + fromIntegral (length y._pushRecipients))
           in if totalLength > maxRecipients
                then
                  let (y1, y2) = splitPush (maxRecipients - n) y
                   in go maxRecipients (y1 : acc) (y2 : ys)
                else go totalLength (y : acc) ys

    -- n must be strictly > 0 and < length (_pushRecipients p)
    splitPush :: Natural -> PushTo a -> (PushTo a, PushTo a)
    splitPush n p =
      let (r1, r2) = splitAt (fromIntegral n) (toList p._pushRecipients)
       in (p {_pushRecipients = fromJust $ nonEmpty r1}, p {_pushRecipients = fromJust $ nonEmpty r2})

-- TODO: Test
pushSlowly ::
  ( Member NotificationSubsystem r,
    Member Delay r
  ) =>
  [PushToUser] ->
  Sem r ()
pushSlowly ps = do
  -- TODO this comes from the app configuration
  let mmillies = 10000
      d = 1000 * mmillies
  for_ ps \p -> do
    delay d
    push [p]

newPush1 :: Maybe UserId -> Object -> NonEmpty Recipient -> PushToUser
newPush1 from e rr =
  PushTo
    { _pushConn = Nothing,
      _pushTransient = False,
      _pushRoute = RouteAny,
      _pushNativePriority = Nothing,
      -- _pushAsync = False,
      -- pushRecipientListType = recipientListType,
      pushJson = e,
      pushOrigin = from,
      _pushRecipients = rr
    }

newPush :: Maybe UserId -> Object -> [Recipient] -> Maybe PushToUser
newPush _ _ [] = Nothing
newPush u e (r : rr) = Just $ newPush1 u e (r :| rr)

newPushLocal :: UserId -> Object -> [Recipient] -> Maybe PushToUser
newPushLocal uid = newPush (Just uid)

newPushLocal1 :: UserId -> Object -> NonEmpty Recipient -> PushToUser
newPushLocal1 uid = newPush1 (Just uid)
