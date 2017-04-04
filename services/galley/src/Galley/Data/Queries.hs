{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Data.Queries where

import Cassandra
import Data.Functor.Identity
import Data.Id
import Data.Int
import Data.Misc
import Data.Monoid
import Data.Text (Text)
import Galley.Types hiding (Conversation)
import Galley.Types.Bot

import qualified Data.Text.Lazy as LT

-- Conversations ------------------------------------------------------------

selectConv :: PrepQuery R (Identity ConvId) (ConvType, UserId, Maybe (Set Access), Maybe Text)
selectConv = "select type, creator, access, name from conversation where conv = ?"

selectConvs :: PrepQuery R (Identity [ConvId]) (ConvId, ConvType, UserId, Maybe (Set Access), Maybe Text)
selectConvs = "select conv, type, creator, access, name from conversation where conv in ?"

insertConv :: PrepQuery W (ConvId, ConvType, UserId, Set Access, Maybe Text) ()
insertConv = "insert into conversation (conv, type, creator, access, name) values (?, ?, ?, ?, ?)"

updateConvName :: PrepQuery W (Text, ConvId) ()
updateConvName = "update conversation set name = ? where conv = ?"

updateConvType :: PrepQuery W (ConvType, ConvId) ()
updateConvType = "update conversation set type = ? where conv = ?"

-- User Conversations -------------------------------------------------------

selectUserConvs :: PrepQuery R (Identity UserId) (Identity ConvId)
selectUserConvs = "select conv from user where user = ? order by conv"

selectUserConvsIn :: PrepQuery R (UserId, [ConvId]) (Identity ConvId)
selectUserConvsIn = "select conv from user where user = ? and conv in ? order by conv"

selectUserConvsFrom :: PrepQuery R (UserId, ConvId) (Identity ConvId)
selectUserConvsFrom = "select conv from user where user = ? and conv > ? order by conv"

insertUserConv :: PrepQuery W (UserId, ConvId) ()
insertUserConv = "insert into user (user, conv) values (?, ?)"

deleteUserConv :: PrepQuery W (UserId, ConvId) ()
deleteUserConv = "delete from user where user = ? and conv =  ?"

-- Members ------------------------------------------------------------------

type MemberStatus = Int32

selectMember :: PrepQuery R (ConvId, UserId) (UserId, Maybe ServiceId, Maybe ProviderId, MemberStatus, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectMember = "select user, service, provider, status, otr_muted, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from member where conv = ? and user = ?"

selectMembers :: PrepQuery R (Identity [ConvId]) (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, MemberStatus, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)
selectMembers = "select conv, user, service, provider, status, otr_muted, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref from member where conv in ?"

insertMember :: PrepQuery W (ConvId, UserId, Maybe ServiceId, Maybe ProviderId) ()
insertMember = "insert into member (conv, user, service, provider, status) values (?, ?, ?, ?, 0)"

removeMember :: PrepQuery W (ConvId, UserId) ()
removeMember = "delete from member where conv = ? and user = ?"

updateOtrMemberMuted :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberMuted = "update member set otr_muted = ?, otr_muted_ref = ? where conv = ? and user = ?"

updateOtrMemberArchived :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateOtrMemberArchived = "update member set otr_archived = ?, otr_archived_ref = ? where conv = ? and user = ?"

updateMemberHidden :: PrepQuery W (Bool, Maybe Text, ConvId, UserId) ()
updateMemberHidden = "update member set hidden = ?, hidden_ref = ? where conv = ? and user = ?"

-- Clients ------------------------------------------------------------------

selectClients :: PrepQuery R (Identity [UserId]) (UserId, Set ClientId)
selectClients = "select user, clients from clients where user in ?"

rmClients :: PrepQuery W (Identity UserId) ()
rmClients = "delete from clients where user = ?"

addMemberClient :: ClientId -> QueryString W (Identity UserId) ()
addMemberClient c =
    let t = LT.fromStrict (client c) in
    QueryString $ "update clients set clients = clients + {'" <> t <> "'} where user = ?"

rmMemberClient :: ClientId -> QueryString W (Identity UserId) ()
rmMemberClient c =
    let t = LT.fromStrict (client c) in
    QueryString $ "update clients set clients = clients - {'" <> t <> "'} where user = ?"

-- Services -----------------------------------------------------------------

rmSrv :: PrepQuery W (ProviderId, ServiceId) ()
rmSrv = "delete from service where provider = ? AND id = ?"

insertSrv :: PrepQuery W (ProviderId, ServiceId, HttpsUrl, ServiceToken, Set (Fingerprint Rsa), Bool) ()
insertSrv = "insert into service (provider, id, base_url, auth_token, fingerprints, enabled) values (?, ?, ?, ?, ?, ?)"

selectSrv :: PrepQuery R (ProviderId, ServiceId) (HttpsUrl, ServiceToken, Set (Fingerprint Rsa), Bool)
selectSrv = "select base_url, auth_token, fingerprints, enabled from service where provider = ? AND id = ?"

-- Bots ---------------------------------------------------------------------

insertBot :: PrepQuery W (ConvId, BotId, ServiceId, ProviderId) ()
insertBot = "insert into member (conv, user, service, provider, status) values (?, ?, ?, ?, 0)"

