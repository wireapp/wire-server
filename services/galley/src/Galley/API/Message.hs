module Galley.API.Message where

import Control.Lens (asIndex, itraversed, to, (<.>), (^..))
import Data.Domain (Domain)
import Data.Id (ClientId, UserId)
import qualified Data.Map as Map
import Data.Qualified (Qualified (Qualified))
import qualified Data.Set as Set
import Imports
import Wire.API.Message
import Wire.API.User.Client

data QualifiedMismatch = QualifiedMismatch
  { qmMissing :: QualifiedUserClients,
    qmRedundant :: QualifiedUserClients,
    qmDeleted :: QualifiedUserClients
  }

checkMessageClients :: Domain -> UserId -> ClientId -> QualifiedUserClients -> QualifiedNewOtrMessage -> (Bool, QualifiedMismatch)
checkMessageClients senderDomain senderUser senderClient expectedClients msg =
  let recipients = toQualifiedClientMap . qualifiedOtrRecipientsMap . qualifiedNewOtrRecipients $ msg
      -- Whoever is expected but not in recipients is missing.
      missing = qualifiedDiff expectedClients recipients
      -- Whoever is in recipient but not expected is extra.
      extras = qualifiedDiff recipients expectedClients
      -- The clients which belong to users who are expected are considered
      -- deleted.
      --
      -- FUTUREWORK: Optimize this by partitioning extras, this way redandants
      -- wouldn't need a qualifiedDiff.
      deleted = QualifiedUserClients $ qualifiedUserClients extras & nestedKeyFilter (isUserPresent expectedClients)
      -- If sender includes a message for themself, it is considered redandant
      redandantSender =
        if isUserPresent expectedClients senderDomain senderUser
          then QualifiedUserClients (Map.singleton senderDomain (Map.singleton senderUser (Set.singleton senderClient)))
          else mempty
      -- The clients which are extra but not deleted, must belong to users which
      -- are not in the convesation and hence considered redandant.
      redandant = qualifiedDiff extras deleted <> redandantSender
      usersWithMissingClients = extractUsers missing
      isValidMessage = case qualifiedNewOtrClientMismatchStrategy msg of
        MismatchIgnoreAll -> True
        MismatchReportAll ->
          Map.null $ qualifiedUserClients missing
        MismatchIgnoreOnly ignoredUsers ->
          let nonIgnoredUsersMissingClients = usersWithMissingClients `Set.difference` ignoredUsers
           in Set.null nonIgnoredUsersMissingClients
        MismatchReportOnly strictUsers ->
          let strictUsersMisingClients = strictUsers `Set.difference` usersWithMissingClients
           in Set.null strictUsersMisingClients
   in (isValidMessage, QualifiedMismatch missing redandant deleted)
  where
    isUserPresent :: QualifiedUserClients -> Domain -> UserId -> Bool
    isUserPresent (QualifiedUserClients cs) d u =
      let memberUsers = Map.keysSet $ Map.findWithDefault mempty d cs
       in Set.member u memberUsers

    nestedKeyFilter :: (a -> b -> Bool) -> Map a (Map b c) -> Map a (Map b c)
    nestedKeyFilter predicate =
      Map.filterWithKey
        ( \a ->
            not . Map.null
              . Map.filterWithKey (\b _ -> predicate a b)
        )

    extractUsers :: QualifiedUserClients -> Set (Qualified UserId)
    extractUsers (QualifiedUserClients qmap) =
      Set.fromList $
        qmap
          ^.. (itraversed <.> itraversed)
            . asIndex
            . to (uncurry (flip Qualified))

qualifiedDiff :: QualifiedUserClients -> QualifiedUserClients -> QualifiedUserClients
qualifiedDiff (QualifiedUserClients qmapA) (QualifiedUserClients qmapB) =
  QualifiedUserClients $ Map.differenceWith unqualifiedDiff qmapA qmapB
  where
    unqualifiedDiff :: Map UserId (Set ClientId) -> Map UserId (Set ClientId) -> Maybe (Map UserId (Set ClientId))
    unqualifiedDiff mapA mapB = mapDiff setDiff mapA mapB

    mapDiff :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Maybe (Map k a)
    mapDiff f mapA mapB =
      let diffMap = Map.differenceWith f mapA mapB
       in if Map.null diffMap
            then Nothing
            else Just diffMap

    setDiff :: Ord a => Set a -> Set a -> Maybe (Set a)
    setDiff setA setB =
      let diffSet = Set.difference setA setB
       in if Set.null diffSet
            then Nothing
            else Just diffSet

toQualifiedClientMap :: QualifiedUserClientMap a -> QualifiedUserClients
toQualifiedClientMap (QualifiedUserClientMap mapQ) =
  QualifiedUserClients $ Map.map toClientMap mapQ
  where
    toClientMap :: Map UserId (Map ClientId a) -> Map UserId (Set ClientId)
    toClientMap mapU = Map.map Map.keysSet mapU
