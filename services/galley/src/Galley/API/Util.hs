{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Galley.API.Util where

import Brig.Types (Relation (..))
import Brig.Types.Intra (ConnectionStatus (..))
import Control.Lens ((&), (.~))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Id
import Data.Foldable (find, for_, toList)
import Data.Maybe (isJust)
import Data.Range
import Data.Semigroup ((<>))
import Data.Time
import Galley.App
import Galley.API.Error
import Galley.Data.Services (BotMember, newBotMember)
import Galley.Intra.Push
import Galley.Intra.User
import Galley.Types
import Network.HTTP.Types.Status
import Network.Wai.Predicate
import Network.Wai.Utilities.Error

import qualified Data.Text.Lazy as LT
import qualified Galley.Data    as Data

type JSON = Media "application" "json"

ensureConnected :: UserId -> Range 1 n [UserId] -> Galley ()
ensureConnected u (fromRange -> uids) = do
    conns <- getConnections u uids (Just Accepted)
    unless (all (isConnected u conns) uids) $
        throwM notConnected
  where
    isConnected u1 conns u2 =
        let c1 = find (connection u1 u2) conns
            c2 = find (connection u2 u1) conns
        in isJust c1 && isJust c2

    connection u1 u2 cs =
           csFrom   cs == u1
        && csTo     cs == u2
        && csStatus cs == Accepted

-- | Try to accept a 1-1 conversation, promoting connect conversations as appropriate.
acceptOne2One :: UserId -> Data.Conversation -> Maybe ConnId -> Galley Data.Conversation
acceptOne2One usr conv conn = case Data.convType conv of
    One2OneConv ->
        if usr `isMember` mems then
            return conv
        else do
            now <- liftIO getCurrentTime
            mm  <- snd <$> Data.addMembers now cid usr (rcast $ rsingleton usr)
            return $ conv { Data.convMembers = mems <> toList mm }
    ConnectConv -> case mems of
        [_,_] | usr `isMember` mems -> promote
        [_,_] -> throwM convNotFound
        _     -> do
            when (length mems > 2) $
                throwM badConvState
            now <- liftIO getCurrentTime
            (e, mm) <- Data.addMembers now cid usr (rcast $ rsingleton usr)
            conv'   <- if isJust (find ((usr /=) . memId) mems) then promote else pure conv
            let mems' = mems <> toList mm
            for_ (newPush e (recipient <$> mems')) $ \p ->
                push1 $ p & pushConn  .~ conn & pushRoute .~ RouteDirect
            return $ conv' { Data.convMembers = mems' }
    _ -> throwM $ invalidOp "accept: invalid conversation type"
  where
    cid   = Data.convId conv
    mems  = Data.convMembers conv

    promote = do
        Data.acceptConnect cid
        return $ conv { Data.convType = One2OneConv }

    badConvState = Error status500 "bad-state"
                 $ "Connect conversation with more than 2 members: "
                <> LT.pack (show cid)

isMember :: Foldable m => UserId -> m Member -> Bool
isMember u = isJust . find ((u ==) . memId)

isBot :: Member -> Bool
isBot = isJust . memService

findMember :: Data.Conversation -> UserId -> Maybe Member
findMember c u = find ((u ==) . memId) (Data.convMembers c)

botsAndUsers :: Foldable t => t Member -> ([BotMember], [Member])
botsAndUsers = foldr fn ([], [])
  where
    fn m ~(bb, mm) = case newBotMember m of
        Nothing -> (bb, m:mm)
        Just  b -> (b:bb, mm)

