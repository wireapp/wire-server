module Galley.API.Internal
  ( rmUserH,
    deleteLoop,
    refreshMetrics,
  )
where

import Cassandra
import Control.Exception.Safe (catchAny)
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadCatch, throwM)
import Data.Id
import Data.IdMapping (MappedOrLocalId (Local), partitionMappedOrLocalIds)
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import Data.Metrics.Middleware as Metrics
import Data.Range
import Data.String.Conversions (cs)
import Galley.API.Error (federationNotImplemented)
import Galley.API.Teams (uncheckedRemoveTeamMember)
import qualified Galley.API.Teams as Teams
import Galley.API.Util (isMember, resolveOpaqueConvId)
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Intra.Push as Intra
import qualified Galley.Queue as Q
import Galley.Types (ConvType (..), evtFrom)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (err, result)
import Network.Wai.Utilities
import System.Logger.Class

rmUserH :: UserId ::: Maybe ConnId -> Galley Response
rmUserH (user ::: conn) = do
  empty <$ rmUser user conn

rmUser :: UserId -> Maybe ConnId -> Galley ()
rmUser user conn = do
  let n = unsafeRange 100 :: Range 1 100 Int32
  Data.ResultSet tids <- Data.teamIdsFrom user Nothing (rcast n)
  leaveTeams tids
  Data.ResultSet cids <- Data.conversationIdsFrom user Nothing (rcast n)
  let u = list1 user []
  leaveConversations u cids
  Data.eraseClients user
  where
    leaveTeams tids = for_ (result tids) $ \tid -> do
      Data.teamMembersUnsafeForLargeTeams tid >>= uncheckedRemoveTeamMember user conn tid user
      when (hasMore tids) $
        leaveTeams =<< liftClient (nextPage tids)
    leaveConversations :: List1 UserId -> Page OpaqueConvId -> Galley ()
    leaveConversations u ids = do
      (localConvIds, remoteConvIds) <- partitionMappedOrLocalIds <$> traverse resolveOpaqueConvId (result ids)
      -- FUTUREWORK(federation, #1275): leave remote conversations.
      -- If we could just get all conversation IDs at once and then leave conversations
      -- in batches, it would make everything much easier.
      for_ (nonEmpty remoteConvIds) $
        throwM . federationNotImplemented
      cc <- Data.conversations localConvIds
      pp <- for cc $ \c -> case Data.convType c of
        SelfConv -> return Nothing
        One2OneConv -> Data.removeMember user (Data.convId c) >> return Nothing
        ConnectConv -> Data.removeMember user (Data.convId c) >> return Nothing
        RegularConv
          | isMember (makeIdOpaque user) (Data.convMembers c) -> do
            e <- Data.removeMembers c user (Local <$> u)
            return $
              (Intra.newPush (evtFrom e) (Intra.ConvEvent e) (Intra.recipient <$> Data.convMembers c))
                <&> set Intra.pushConn conn
                . set Intra.pushRoute Intra.RouteDirect
          | otherwise -> return Nothing
      for_
        (List1 <$> nonEmpty (catMaybes pp))
        Intra.push
      when (hasMore ids) $
        leaveConversations u =<< liftClient (nextPage ids)

deleteLoop :: Galley ()
deleteLoop = do
  q <- view deleteQueue
  safeForever "deleteLoop" $ do
    i@(TeamItem tid usr con) <- Q.pop q
    Teams.uncheckedDeleteTeam usr con tid `catchAny` someError q i
  where
    someError q i x = do
      err $ "error" .= show x ~~ msg (val "failed to delete")
      ok <- Q.tryPush q i
      unless ok $
        err (msg (val "delete queue is full, dropping item") ~~ "item" .= show i)
      liftIO $ threadDelay 1000000

refreshMetrics :: Galley ()
refreshMetrics = do
  m <- view monitor
  q <- view deleteQueue
  safeForever "refreshMetrics" $ do
    n <- Q.len q
    gaugeSet (fromIntegral n) (Metrics.path "galley.deletequeue.len") m
    threadDelay 1000000

safeForever :: (MonadIO m, MonadLogger m, MonadCatch m) => String -> m () -> m ()
safeForever funName action = forever $
  action `catchAny` \exc -> do
    err $ "error" .= show exc ~~ msg (val $ cs funName <> " failed")
    threadDelay 60000000 -- pause to keep worst-case noise in logs manageable
