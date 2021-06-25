module Galley.API.Message where

import Control.Lens
import Data.Domain (Domain)
import Data.Id (ClientId, UserId)
import Data.Json.Util (UTCTimeMillis)
import qualified Data.Map as Map
import Data.Qualified (Qualified (Qualified))
import qualified Data.Set as Set
import Data.Set.Lens
import Imports
import Wire.API.Message
import Wire.API.User.Client

data QualifiedMismatch = QualifiedMismatch
  { qmMissing :: QualifiedUserClients,
    qmRedundant :: QualifiedUserClients,
    qmDeleted :: QualifiedUserClients
  }

mkQualifiedMismatch ::
  QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedMismatch
mkQualifiedMismatch missing redundant deleted =
  QualifiedMismatch
    (QualifiedUserClients missing)
    (QualifiedUserClients redundant)
    (QualifiedUserClients deleted)

mkMessageSendingStatus :: UTCTimeMillis -> QualifiedMismatch -> QualifiedUserClients -> MessageSendingStatus
mkMessageSendingStatus time mismatch failures =
  MessageSendingStatus
    { mssTime = time,
      mssMissingClients = qmMissing mismatch,
      mssRedundantClients = qmRedundant mismatch,
      mssDeletedClients = qmDeleted mismatch,
      mssFailedToSend = failures
    }

type QualifiedRecipient = (Domain, Recipient)

qualifiedRecipientToUser :: QualifiedRecipient -> Qualified UserId
qualifiedRecipientToUser (dom, (uid, _)) = Qualified uid dom

type Recipient = (UserId, ClientId)

type RecipientMap a = Map UserId (Map ClientId a)

type RecipientSet = Map UserId (Set ClientId)

type QualifiedRecipientMap a = Map Domain (RecipientMap a)

type QualifiedRecipientSet = Map Domain RecipientSet

isUserPresent :: QualifiedRecipientSet -> Domain -> UserId -> Bool
isUserPresent qrs d u =
  let memberUsers = Map.keysSet $ Map.findWithDefault mempty d qrs
   in Set.member u memberUsers

qualifiedRecipientMapToSet :: QualifiedRecipientMap a -> QualifiedRecipientSet
qualifiedRecipientMapToSet = Map.map (Map.map Map.keysSet)

qualifiedRecipientSetSingleton :: Domain -> UserId -> ClientId -> QualifiedRecipientSet
qualifiedRecipientSetSingleton dom uid cid =
  Map.singleton dom
    . Map.singleton uid
    . Set.singleton
    $ cid

-- A Venn diagram of words in this function:
--
--                +-----------------------------------+
--                |                                   |
--    +---------->|                 +-----------------+--------------------+
--    |           |                 |                 |                    | <------+
--    |           |                 |                 |   Deleted Clients  |        |
--    |           |                 |                 |                    |        |
-- Expected       |                 |                 |                    |   Recipients
-- Clients        |  Missing        | Valid           |       Extra        |
--                |  Clients        | Clients    +------------Clients------+
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 |            | Redundant Clients       |
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 |            |    |                    |
--                |                 +------------+-------------------------+
--                |                                   |
--                +-----------------------------------+
checkMessageClients ::
  -- | Sender domain
  Domain ->
  -- | Sender User
  UserId ->
  QualifiedUserClients ->
  QualifiedNewOtrMessage ->
  (Maybe (QualifiedRecipientMap ByteString), QualifiedMismatch)
checkMessageClients senderDomain senderUser (QualifiedUserClients expected) msg =
  let senderClient = qualifiedNewOtrSender msg
      -- Recipients provided in the message.
      recipientMap =
        qualifiedUserClientMap
          . qualifiedOtrRecipientsMap
          . qualifiedNewOtrRecipients
          $ msg
      recipients = qualifiedRecipientMapToSet recipientMap
      -- Whoever is expected but not in recipients is missing.
      missing = qualifiedDiff expected recipients
      -- Whoever is in recipient but not expected is extra.
      extra = qualifiedDiff recipients expected
      -- The clients which belong to users who are expected are considered
      -- deleted.
      --
      -- FUTUREWORK: Optimize this by partitioning extra, this way redundants
      -- wouldn't need a qualifiedDiff.
      deleted = nestedKeyFilter (isUserPresent expected) extra
      -- If sender includes a message for themself, it is considered redundant
      redundantSender
        | isUserPresent expected senderDomain senderUser =
          qualifiedRecipientSetSingleton senderDomain senderUser senderClient
        | otherwise = mempty
      -- The clients which are extra but not deleted, must belong to users which
      -- are not in the convesation and hence considered redundant.
      redundant = qualifiedDiff extra deleted <> redundantSender
      -- The clients which are recipients but not extra or the sender client are
      -- considered valid.
      valid = qualifiedDiff recipients (extra <> redundantSender)
      validMap = selectClients valid recipientMap
      -- Resolve whether the message is valid using client mismatch strategy
      usersWithMissingClients = extractUsers missing
      isValidMessage = case qualifiedNewOtrClientMismatchStrategy msg of
        MismatchIgnoreAll -> True
        MismatchReportAll -> Map.null missing
        MismatchIgnoreOnly ignoredUsers ->
          let nonIgnoredUsersMissingClients = usersWithMissingClients `Set.difference` ignoredUsers
           in Set.null nonIgnoredUsersMissingClients
        MismatchReportOnly strictUsers ->
          let strictUsersMisingClients = strictUsers `Set.difference` usersWithMissingClients
           in Set.null strictUsersMisingClients
   in ( guard isValidMessage $> validMap,
        mkQualifiedMismatch missing redundant deleted
      )
  where
    nestedKeyFilter :: (a -> b -> Bool) -> Map a (Map b c) -> Map a (Map b c)
    nestedKeyFilter predicate =
      Map.filterWithKey
        ( \a ->
            not . Map.null
              . Map.filterWithKey (\b _ -> predicate a b)
        )

    extractUsers :: QualifiedRecipientSet -> Set (Qualified UserId)
    extractUsers =
      setOf $
        reindexed (uncurry (flip Qualified)) (itraversed <.> itraversed)
          . asIndex

    -- TODO: I am pretty sure this was different
    selectClients :: QualifiedRecipientSet -> QualifiedRecipientMap a -> QualifiedRecipientMap a
    selectClients = Map.intersectionWith (Map.intersectionWith (flip Map.restrictKeys))

qualifiedDiff :: QualifiedRecipientSet -> QualifiedRecipientSet -> QualifiedRecipientSet
qualifiedDiff =
  Map.differenceWith
    . guardedOp (not . Map.null)
    . Map.differenceWith
    . guardedOp (not . Set.null)
    $ Set.difference
  where
    guarded :: (a -> Bool) -> a -> Maybe a
    guarded p x = guard (p x) $> x

    guardedOp :: (c -> Bool) -> (a -> b -> c) -> (a -> b -> Maybe c)
    guardedOp p f a b = guarded p (f a b)
