module Galley.API.Message where

import Data.Domain (Domain)
import Data.Id
import qualified Data.Map as Map
import qualified Data.Set as Set
import Imports
import Wire.API.Message
import Wire.API.User.Client as Client

data QualifiedMismatch = QualifiedMismatch
  { qmMissing :: Client.QualifiedUserClients,
    qmRedundant :: Client.QualifiedUserClients,
    qmDeleted :: Client.QualifiedUserClients
  }

checkMessageClients :: UserId -> ClientId -> Client.QualifiedUserClients -> QualifiedNewOtrMessage -> (Bool, QualifiedMismatch)
checkMessageClients sender senderClient expectedClients msg =
  let mismatchStrat = qualifiedNewOtrClientMismatchStrategy msg
      (QualifiedOtrRecipients recipients) = qualifiedNewOtrRecipients msg
      missing = qualifiedDiff expectedClients (toQualifiedClientMap recipients)
      extras = qualifiedDiff (toQualifiedClientMap recipients) expectedClients
   in undefined

qualifiedDiff :: Client.QualifiedUserClients -> Client.QualifiedUserClients -> Client.QualifiedUserClients
qualifiedDiff (Client.QualifiedUserClients qmapA) (Client.QualifiedUserClients qmapB) =
  Client.QualifiedUserClients $ Map.differenceWith unqualifiedDiff qmapA qmapB
  where
    unqualifiedDiff :: UserClients -> UserClients -> Maybe UserClients
    unqualifiedDiff (UserClients mapA) (UserClients mapB) = UserClients <$> mapDiff setDiff mapA mapB

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
    toClientMap :: UserClientMap a -> UserClients
    toClientMap (UserClientMap mapU) = UserClients $ Map.map Map.keysSet mapU

partitionRedandantDeleted ::
  -- | is reduandant
  (Domain -> UserId -> ClientId -> Bool) -> QualifiedUserClients -> (QualifiedUserClients , QualifiedUserClients )
partitionRedandantDeleted f (QualifiedUserClientMap qMap) = undefined
