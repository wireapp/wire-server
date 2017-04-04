{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Galley.API.Internal (rmUser) where

import Cassandra
import Control.Lens
import Control.Monad
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Id
import Data.Int
import Data.List.NonEmpty (nonEmpty)
import Data.List1
import Data.Maybe (catMaybes)
import Data.Range
import Galley.API.Util (isMember)
import Galley.App
import Galley.Types (ConvType (..))
import Network.Wai
import Network.Wai.Predicate hiding (result)
import Network.Wai.Utilities

import qualified Galley.Data       as Data
import qualified Galley.Intra.Push as Intra

rmUser :: UserId ::: Maybe ConnId -> Galley Response
rmUser (user ::: conn) = do
    let n = unsafeRange 100 :: Range 1 100 Int32
    Data.ResultSet ids <- Data.conversationIdsFrom user Nothing (rcast n)
    let u = list1 user []
    leaveConversations u ids
    Data.eraseClients user
    return empty
  where
    leaveConversations u ids = do
        cc <- Data.conversations (result ids)
        pp <- for cc $ \c -> case Data.convType c of
            SelfConv    -> return Nothing
            One2OneConv -> Data.deleteMember user (Data.convId c) >> return Nothing
            ConnectConv -> Data.deleteMember user (Data.convId c) >> return Nothing
            RegularConv
                | isMember user (Data.convMembers c) -> do
                      e <- Data.rmMembers c user u
                      return $ (Intra.newPush e (Intra.recipient <$> Data.convMembers c)) <&>
                                     set Intra.pushConn conn
                                   . set Intra.pushRoute Intra.RouteDirect
                | otherwise -> return Nothing
        for_ (List1 <$> nonEmpty (catMaybes pp))
            Intra.push
        when (hasMore ids) $
            leaveConversations u =<< liftClient (nextPage ids)
