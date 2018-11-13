{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.API.Internal
    ( rmUser
    , deleteLoop
    ) where

import Imports
import Cassandra
import Control.Exception.Safe (catchAny)
import Control.Lens hiding ((.=))
import Data.Id
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import Data.Range
import Galley.API.Util (isMember)
import Galley.API.Teams (uncheckedRemoveTeamMember)
import Galley.App
import Galley.Types (ConvType (..), evtFrom)
import Network.Wai
import Network.Wai.Predicate hiding (err, result)
import Network.Wai.Utilities
import System.Logger.Class

import qualified Galley.API.Teams   as Teams
import qualified Galley.Data        as Data
import qualified Galley.Queue       as Q
import qualified Galley.Intra.Push  as Intra

rmUser :: UserId ::: Maybe ConnId -> Galley Response
rmUser (user ::: conn) = do
    let n = unsafeRange 100 :: Range 1 100 Int32
    Data.ResultSet tids <- Data.teamIdsFrom user Nothing (rcast n)
    leaveTeams tids
    Data.ResultSet cids <- Data.conversationIdsFrom user Nothing (rcast n)
    let u = list1 user []
    leaveConversations u cids
    Data.eraseClients user
    return empty
  where
    leaveTeams tids = for_ (result tids) $ \tid -> do
        Data.teamMembers tid >>= uncheckedRemoveTeamMember user conn tid user
        when (hasMore tids) $
            leaveTeams =<< liftClient (nextPage tids)

    leaveConversations u ids = do
        cc <- Data.conversations (result ids)
        pp <- for cc $ \c -> case Data.convType c of
            SelfConv    -> return Nothing
            One2OneConv -> Data.removeMember user (Data.convId c) >> return Nothing
            ConnectConv -> Data.removeMember user (Data.convId c) >> return Nothing
            RegularConv
                | isMember user (Data.convMembers c) -> do
                      e <- Data.removeMembers c user u
                      return $ (Intra.newPush (evtFrom e) (Intra.ConvEvent e) (Intra.recipient <$> Data.convMembers c)) <&>
                                     set Intra.pushConn conn
                                   . set Intra.pushRoute Intra.RouteDirect
                | otherwise -> return Nothing
        for_ (List1 <$> nonEmpty (catMaybes pp))
            Intra.push
        when (hasMore ids) $
            leaveConversations u =<< liftClient (nextPage ids)

deleteLoop :: Galley ()
deleteLoop = do
    q <- view deleteQueue
    forever $ do
        i@(TeamItem tid usr con) <- Q.pop q
        Teams.uncheckedDeleteTeam usr con tid `catchAny` someError q i
  where
    someError q i x = do
        err $ "error" .= show x ~~ msg (val "failed to delete")
        ok <- Q.tryPush q i
        unless ok $
            err (msg (val "delete queue is full, dropping item") ~~ "item" .= show i)
        liftIO $ threadDelay 1000000
