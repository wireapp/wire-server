{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Brig.Data.Invitation
    ( module T
    , insertInvitation
    , deleteInvitation
    , lookupInvitation
    , lookupInvitationCode
    , lookupInvitations
    , lookupInvitationByCode
    , lookupInvitationInfo

    , mkInvitationCode
    , mkInvitationId

    , InvitationInfo (..)
    ) where

import Brig.App (AppIO, currentTime)
import Brig.Data.Instances ()
import Brig.Data.Types as T
import Brig.Types
import Cassandra
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.Id
import Data.Int
import Data.Maybe (catMaybes)
import Data.Range
import Data.UUID.V4
import Data.Time.Clock
import OpenSSL.Random (randBytes)
import System.Logger.Message

import qualified Data.Text.Ascii     as Ascii
import qualified System.Logger.Class as Log

mkInvitationCode :: IO InvitationCode
mkInvitationCode = InvitationCode . Ascii.encodeBase64Url <$> randBytes 24

mkInvitationId :: IO InvitationId
mkInvitationId = Id <$> nextRandom

data InvitationInfo = InvitationInfo
    { iiCode    :: InvitationCode
    , iiInviter :: UserId
    , iiInvId   :: InvitationId
    } deriving (Eq, Show)

insertInvitation :: UserId -> Email -> Name -> AppIO (Invitation, InvitationCode)
insertInvitation u email name = do
    iid  <- liftIO mkInvitationId
    code <- liftIO mkInvitationCode
    now  <- liftIO =<< view currentTime
    let inv = Invitation u iid (Left email) now name
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery invitationInsert (u, iid, code, Just email, Nothing, now, name)
        addPrepQuery invitationInfoInsert (code, u, iid)
    return (inv, code)

lookupInvitation :: UserId -> InvitationId -> AppIO (Maybe Invitation)
lookupInvitation u r = maybe (return Nothing) toInvitation =<<
    retry x1 (query1 invitationSelect (params Quorum (u, r)))

lookupInvitationByCode :: InvitationCode -> AppIO (Maybe Invitation)
lookupInvitationByCode i = lookupInvitationInfo i >>= \case
    Just InvitationInfo{..} -> lookupInvitation iiInviter iiInvId
    _                       -> return Nothing

lookupInvitationCode :: UserId -> InvitationId -> AppIO (Maybe InvitationCode)
lookupInvitationCode u r = fmap runIdentity <$>
    retry x1 (query1 invitationCodeSelect (params Quorum (u, r)))

lookupInvitations :: UserId -> Maybe InvitationId -> Range 1 500 Int32 -> AppIO (ResultPage Invitation)
lookupInvitations inviter start (fromRange -> size) = do
    page <- case start of
        Just ref -> retry x1 $ paginate invitationsSelectFrom (paramsP Quorum (inviter, ref) (size + 1))
        Nothing  -> retry x1 $ paginate invitationsSelect (paramsP Quorum (Identity inviter) (size + 1))
    toResult (hasMore page) <$> mapM toInvitation (trim page)
  where
    trim p = take (fromIntegral size) (result p)
    toResult more invs = cassandraResultPage $ emptyPage { result  = catMaybes invs
                                                         , hasMore = more
                                                         }

deleteInvitation :: UserId -> InvitationId -> AppIO ()
deleteInvitation u i = do
    code <- lookupInvitationCode u i
    case code of
        Just invCode -> retry x5 $ batch $ do
            setType BatchLogged
            setConsistency Quorum
            addPrepQuery invitationDelete (u, i)
            addPrepQuery invitationInfoDelete (Identity invCode)
        Nothing -> do
            Log.warn $ msg (val "No invitation_code found")
                    ~~ field "user" (toByteString' u)
                    ~~ field "invitation_id" (toByteString i)
            retry x5 $ write invitationDelete (params Quorum (u, i))

lookupInvitationInfo :: InvitationCode -> AppIO (Maybe InvitationInfo)
lookupInvitationInfo ic@(InvitationCode c)
    | c == mempty = return Nothing
    | otherwise   = fmap (toInvitationInfo ic)
                 <$> retry x1 (query1 invitationInfoSelect (params Quorum (Identity ic)))

invitationInfoInsert :: PrepQuery W (InvitationCode, UserId, InvitationId) ()
invitationInfoInsert = "INSERT INTO invitation_info (code, inviter, id) VALUES (?, ?, ?)"

invitationInfoDelete :: PrepQuery W (Identity InvitationCode) ()
invitationInfoDelete = "DELETE FROM invitation_info WHERE code = ?"

invitationInfoSelect :: PrepQuery R (Identity InvitationCode) (UserId, InvitationId)
invitationInfoSelect = "SELECT inviter, id FROM invitation_info WHERE code = ?"

invitationInsert :: PrepQuery W (UserId, InvitationId, InvitationCode, Maybe Email, Maybe Phone, UTCTime, Name) ()
invitationInsert = "INSERT INTO invitation (inviter, id, code, email, phone, created_at, name) VALUES (?, ?, ?, ?, ?, ?, ?)"

invitationDelete :: PrepQuery W (UserId, InvitationId) ()
invitationDelete = "DELETE FROM invitation where inviter = ? AND id = ?"

invitationSelect :: PrepQuery R (UserId, InvitationId) (UserId, InvitationId, Maybe Email, Maybe Phone, UTCTime, Name)
invitationSelect = "SELECT inviter, id, email, phone, created_at, name FROM invitation WHERE inviter = ? AND id = ?"

invitationCodeSelect :: PrepQuery R (UserId, InvitationId) (Identity InvitationCode)
invitationCodeSelect = "SELECT code FROM invitation WHERE inviter = ? AND id = ?"

invitationsSelect :: PrepQuery R (Identity UserId) (UserId, InvitationId, Maybe Email, Maybe Phone, UTCTime, Name)
invitationsSelect = "SELECT inviter, id, email, phone, created_at, name FROM invitation WHERE inviter = ?"

invitationsSelectFrom :: PrepQuery R (UserId, InvitationId) (UserId, InvitationId, Maybe Email, Maybe Phone, UTCTime, Name)
invitationsSelectFrom = "SELECT inviter, id, email, phone, created_at, name FROM invitation WHERE inviter = ? AND id > ? ORDER BY id ASC"

-- Conversions
toInvitationInfo :: InvitationCode -> (UserId, InvitationId) -> InvitationInfo
toInvitationInfo i (u, r) = InvitationInfo i u r

-- Helper
toInvitation :: (UserId, InvitationId, Maybe Email, Maybe Phone, UTCTime, Name) -> AppIO (Maybe Invitation)
toInvitation (u, i, Just e , Nothing, t, n) = return . Just $ Invitation u i (Left e) t n
toInvitation (u, i, Nothing, Just p , t, n) = return . Just $ Invitation u i (Right p) t n
toInvitation (u, i, _      , _      , _, _) = do
    Log.err $ msg (val "Failed to construct invitation") ~~ field "user" (toByteString' u) ~~ field "invite" (toByteString' i)
    return Nothing
