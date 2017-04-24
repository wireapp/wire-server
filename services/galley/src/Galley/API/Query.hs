{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.API.Query where

import Cassandra (result, hasMore)
import Control.Monad
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Foldable (toList)
import Data.Id
import Data.Int
import Data.Maybe (mapMaybe)
import Data.Range
import Galley.App
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.Data as Data
import Galley.Types
import Galley.Types.Bot (botConvView)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result)
import Network.Wai.Utilities

getBotConversation :: BotId ::: ConvId ::: JSON -> Galley Response
getBotConversation (zbot ::: zcnv :::  _) = do
    c <- Data.conversation zcnv >>= ifNothing convNotFound
    unless (botUserId zbot `isMember` Data.convMembers c) $
        throwM convNotFound
    let cmems = mapMaybe mkMember (toList (Data.convMembers c))
    let cview = botConvView zcnv (Data.convName c) cmems
    return $ json cview
  where
    mkMember m
        | memId m /= botUserId zbot = Just (OtherMember (memId m) (memService m))
        | otherwise                 = Nothing

getConversation :: UserId ::: ConvId ::: JSON -> Galley Response
getConversation (zusr ::: cnv ::: _) = do
    c <- Data.conversation cnv >>= ifNothing convNotFound
    unless (zusr `isMember` Data.convMembers c) $
        throwM convNotFound
    a <- conversationView zusr c
    return $ json a

getConversationIds :: UserId ::: Maybe ConvId ::: Range 1 1000 Int32 ::: JSON -> Galley Response
getConversationIds (zusr ::: start ::: size ::: _) = do
    ResultSet ids <- Data.conversationIdsFrom zusr start size
    return . json $ ConversationList (result ids) (hasMore ids)

getConversations :: UserId ::: Maybe (Either (Range 1 32 (List ConvId)) ConvId) ::: Range 1 500 Int32 ::: JSON -> Galley Response
getConversations (zusr ::: range ::: size ::: _) =
    withConvIds zusr range size $ \more ids -> do
        cs <- filter (isMember zusr . Data.convMembers) <$> Data.conversations ids
        json . flip ConversationList more <$> mapM (conversationView zusr) cs

getMember :: UserId ::: ConvId -> Galley Response
getMember (zusr ::: cnv) = json <$> Data.member cnv zusr

internalGetMember :: ConvId ::: UserId -> Galley Response
internalGetMember (cnv ::: usr) = json <$> Data.member cnv usr

getConversationMeta :: ConvId -> Galley Response
getConversationMeta cnv = maybe (setStatus status404 empty) json <$> Data.conversationMeta cnv

-----------------------------------------------------------------------------
-- Internal

-- | Invoke the given continuation 'k' with a list of conversation IDs
-- which are looked up based on:
--
-- * just limited by size
-- * an (exclusive) starting point (conversation ID) and size
-- * a list of conversation IDs
--
-- The last case returns those conversation IDs which have an associated
-- user. Additionally 'k' is passed in a 'hasMore' indication (which is
-- always false if the third lookup-case is used).
withConvIds :: UserId
            -> Maybe (Either (Range 1 32 (List ConvId)) ConvId)
            -> Range 1 500 Int32
            -> (Bool -> [ConvId] -> Galley Response)
            -> Galley Response
withConvIds usr range size k = case range of
    Nothing        -> do
        ResultSet r <- Data.conversationIdsFrom usr Nothing (rcast size)
        k (hasMore r) (result r)

    Just (Right c) -> do
        ResultSet r <- Data.conversationIdsFrom usr (Just c) (rcast size)
        k (hasMore r) (result r)

    Just (Left cc) -> do
        ids <- Data.conversationIdsOf usr cc
        k False ids
{-# INLINE withConvIds #-}
