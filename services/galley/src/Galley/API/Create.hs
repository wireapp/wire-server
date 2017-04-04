{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.API.Create
    ( createGroupConversation
    , createSelfConversation
    , createOne2OneConversation
    , createConnectConversation
    ) where

import Control.Lens hiding ((??))
import Control.Monad (when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.Foldable (for_, toList)
import Data.Id
import Data.List1
import Data.Monoid ((<>))
import Data.Range
import Data.Time
import Data.Traversable (mapM)
import Galley.App
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.Intra.Push
import Galley.Types
import Galley.Validation (rangeChecked, rangeCheckedMaybe)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities
import Prelude hiding (head, mapM)

import qualified Data.List         as List
import qualified Data.Set          as Set
import qualified Data.UUID.Tagged  as U
import qualified Galley.Data       as Data

createGroupConversation :: UserId ::: ConnId ::: Request ::: JSON -> Galley Response
createGroupConversation (zusr::: zcon ::: req ::: _) = do
    j <- fromBody req invalidPayload
    case newConvUsers j of
        [] -> fun j (rcast rnil)
        xs -> do
            uids <- rangeChecked xs :: Galley (Range 1 64 [UserId])
            ensureConnected zusr uids
            fun j (rcast uids)
  where
    fun j uids = do
        n <- rangeCheckedMaybe (newConvName j)
        c <- Data.createConversation zusr n (access j) uids
        notifyCreatedConversation zusr (Just zcon) c
        conversationResponse status201 zusr c

    access a = case Set.toList (newConvAccess a) of
        []     -> singleton InviteAccess
        (x:xs) -> list1 x xs

createSelfConversation :: UserId -> Galley Response
createSelfConversation zusr = do
    c <- Data.conversation (Id . toUUID $ zusr)
    maybe create (conversationResponse status200 zusr) c
  where
    create = do
        c <- Data.createSelfConversation zusr Nothing
        conversationResponse status201 zusr c

createOne2OneConversation :: UserId ::: ConnId ::: Request ::: JSON -> Galley Response
createOne2OneConversation (zusr ::: zcon ::: req ::: _) = do
    j <- fromBody req invalidPayload
    u <- rangeChecked (newConvUsers j) :: Galley (Range 1 1 [UserId])
    (x, y) <- toUUIDs zusr (List.head $ fromRange u)
    when (x == y) $
        throwM $ invalidOp "Cannot create a 1-1 with yourself"
    ensureConnected zusr u
    n <- rangeCheckedMaybe (newConvName j)
    c <- Data.conversation (Data.one2OneConvId x y)
    maybe (create x y n) (conversationResponse status200 zusr) c
  where
    create x y n = do
        c <- Data.createOne2OneConversation x y n
        notifyCreatedConversation zusr (Just zcon) c
        conversationResponse status201 zusr c

createConnectConversation :: UserId ::: Maybe ConnId ::: Request ::: JSON -> Galley Response
createConnectConversation (usr ::: conn ::: req ::: _) = do
    j      <- fromBody req invalidPayload
    (x, y) <- toUUIDs usr (cRecipient j)
    n      <- rangeCheckedMaybe (cName j)
    conv   <- Data.conversation (Data.one2OneConvId x y)
    maybe (create x y n j) (update n j) conv
  where
    create x y n j = do
        (c, e) <- Data.createConnectConversation x y n j
        notifyCreatedConversation usr conn c
        for_ (newPush e (recipient <$> Data.convMembers c)) $ \p ->
            push1 $ p
                  & pushRoute .~ RouteDirect
                  & pushConn  .~ conn
        conversationResponse status201 usr c

    update n j conv = let mems = Data.convMembers conv in
        conversationResponse status200 usr =<< if
            | usr `isMember` mems -> connect n j conv
            | otherwise           -> do
                now <- liftIO getCurrentTime
                mm  <- snd <$> Data.addMembers now (Data.convId conv) usr (rcast $ rsingleton usr)
                let conv' = conv {
                    Data.convMembers = Data.convMembers conv <> toList mm
                }
                if List.null mems then
                    connect n j conv'
                else do
                    conv'' <- acceptOne2One usr conv' conn
                    if Data.convType conv'' == ConnectConv
                        then connect n j conv''
                        else return conv''

    connect n j conv
        | Data.convType conv == ConnectConv = do
            n' <- case n of
                Just  x -> do
                    Data.updateConversation (Data.convId conv) x
                    return . Just $ fromRange x
                Nothing -> return $ Data.convName conv
            t <- liftIO getCurrentTime
            let e = Event ConvConnect (Data.convId conv) usr t (Just $ EdConnect j)
            for_ (newPush e (recipient <$> Data.convMembers conv)) $ \p ->
                push1 $ p
                      & pushRoute .~ RouteDirect
                      & pushConn  .~ conn
            return $ conv { Data.convName = n' }
        | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

conversationResponse :: Status -> UserId -> Data.Conversation -> Galley Response
conversationResponse s u c = do
    a <- conversationView u c
    return $ json a & setStatus s . location (cnvId a)

notifyCreatedConversation :: UserId -> Maybe ConnId -> Data.Conversation -> Galley ()
notifyCreatedConversation usr conn c = do
    now  <- liftIO getCurrentTime
    pushSome =<< mapM (toPush now) (Data.convMembers c)
  where
    route | Data.convType c == RegularConv = RouteAny
          | otherwise                      = RouteDirect

    toPush t m = do
        c' <- conversationView (memId m) c
        let e = Event ConvCreate (Data.convId c) usr t (Just $ EdConversation c')
        return $ newPush1 e (list1 (recipient m) [])
               & pushConn  .~ conn
               & pushRoute .~ route

location :: ConvId -> Response -> Response
location = addHeader hLocation . toByteString'

toUUIDs :: UserId -> UserId -> Galley (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
    a' <- U.fromUUID (toUUID a) & ifNothing invalidUUID4
    b' <- U.fromUUID (toUUID b) & ifNothing invalidUUID4
    return (a', b')
