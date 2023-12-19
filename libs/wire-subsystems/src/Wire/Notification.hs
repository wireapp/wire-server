{-# LANGUAGE TemplateHaskell #-}

module Wire.Notification where

import Bilge as B
import Control.Lens
import Data.Aeson
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List1 (List1)
import Data.List1 qualified as List1
import Data.Range (Range, fromRange, unsafeRange)
import Data.Set qualified as Set
import Data.Text.Encoding (encodeUtf8)
import Gundeck.Types hiding (Push (..))
import Gundeck.Types.Push.V2 qualified as V2
import Imports
import Network.HTTP.Client qualified as HTTP
import Numeric.Natural (Natural)
import Polysemy
import Util.Options
import Wire.API.Team.Member
import Wire.Arbitrary
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Safe), pooledMapConcurrentlyN_)

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
  deriving stock (Eq, Generic, Functor, Foldable, Traversable, Show)
  deriving (Arbitrary) via GenericUniform (PushTo user)

makeLenses ''PushTo

type PushToUser = PushTo UserId

data NotificationSubsystem m a where
  Push :: [PushToUser] -> NotificationSubsystem m ()
  PushSlowly :: [PushToUser] -> NotificationSubsystem m ()

makeSem ''NotificationSubsystem

data GundeckAccessDetails = GundeckAccessDetails
  { endpoint :: Endpoint,
    httpManager :: HTTP.Manager
  }

data GundeckAPIAccess m a where
  PushV2 :: [V2.Push] -> GundeckAPIAccess m ()

makeSem ''GundeckAPIAccess

-- | We interpret this using 'GundeckAPIAccess' so we can mock it out for testing.
runNotificationSubsystemGundeck ::
  ( Member (GundeckAPIAccess) r,
    Member (Concurrency 'Safe) r,
    Member (Final IO) r
  ) =>
  Sem (NotificationSubsystem : r) a ->
  Sem r a
runNotificationSubsystemGundeck = interpret $ \case
  Push ps -> pushImpl ps
  PushSlowly ps -> pushSlowlyImpl ps

-- TODO: write a test which says all listed notification are sent
-- TODO: write a test which tests the chunking of notifications
-- TODO: write a test for listtype and maximum fanout limit thing
pushImpl ::
  forall r.
  ( Member (GundeckAPIAccess) r,
    Member (Concurrency Safe) r,
    Member (Final IO) r
  ) =>
  [PushToUser] ->
  Sem r ()
pushImpl ps = do
  -- TODO: where from do we get the configuration
  let currentFanoutLimit :: Range 1 HardTruncationLimit Int32 = undefined
      -- should probably be a type reflecting the number of capabilities
      -- of the RTS
      maxThreads :: Int = undefined
      pushChunkSize :: Natural = 128

      pushes :: [[V2.Push]] =
        mkPushes pushChunkSize $
          removeIfLargeFanout currentFanoutLimit ps

  pooledMapConcurrentlyN_ maxThreads (undefined . pushV2 @r) pushes

removeIfLargeFanout :: Range n m Int32 -> [PushTo user] -> [PushTo user]
removeIfLargeFanout limit = filter \PushTo {_pushRecipients} -> length _pushRecipients <= fromIntegral (fromRange limit)

mkPushes :: Natural -> [PushToUser] -> [[V2.Push]]
mkPushes chunkSize = map (map fromV2Push) . chunkPushes chunkSize

{-# INLINE fromV2Push #-}
fromV2Push :: PushToUser -> V2.Push
fromV2Push p =
  (newPush p.pushOrigin (unsafeRange (Set.fromList recipients)) pload)
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

{-# INLINE chunkPushes #-}
chunkPushes :: Natural -> [PushTo a] -> [[PushTo a]]
chunkPushes maxRecipients = go 0 []
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

pushSlowlyImpl ::
  ( Member GundeckAPIAccess r,
    Member (Concurrency 'Safe) r,
    Member (Final IO) r
  ) =>
  [PushToUser] ->
  Sem r ()
pushSlowlyImpl _ = pushImpl []

-- TODO: Test manually if this even works.
runGundeckAPIAccess :: Member (Embed IO) r => GundeckAccessDetails -> Sem (GundeckAPIAccess : r) a -> Sem r a
runGundeckAPIAccess accessDetails = interpret $ \case
  PushV2 pushes -> do
    chunkedReq <- jsonChunkedIO pushes
    let req =
          B.host (encodeUtf8 accessDetails.endpoint._host)
            . B.port accessDetails.endpoint._port
            . path "/i/push/v2"
            . expect2xx
            . chunkedReq
    B.runHttpT accessDetails.httpManager $
      -- Because of 'expect2xx' we don't actually need to check the response
      void $
        B.post req
