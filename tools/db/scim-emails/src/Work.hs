{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Work where

import Brig.Data.UserKey
import Cassandra
import Cassandra.Util
import Conduit
import Control.Lens
import qualified Data.CaseInsensitive as CI
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.String.Conversions
import Data.Time
import Imports
import SAML2.Util (parseURI', renderURI)
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Types.Email as SAMLEmail
import System.Logger (Logger)
import qualified System.Logger as Log
import URI.ByteString
import Wire.API.User.Identity

runCommand :: Logger -> ClientState -> ClientState -> ClientState -> IO ()
runCommand l brig galley spar = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient spar) getSCIMTeams)
      -- find all SCIM enabled teams, (guess: SCIM doing something wrong?)
      -- user with given email inside user table also has an entry in user_keys
      -- logs mismatches
      .| C.mapM
        ( \(i, tids) -> do
            Log.info l (Log.field "teams" (show ((i - 1) * pageSize + fromIntegral (length tids))))
            pure $ map runIdentity tids
        )
      .| C.mapM_ (mapM_ (findMismatchesForTeam l brig galley spar))

findMismatchesForTeam :: Logger -> ClientState -> ClientState -> ClientState -> TeamId -> IO ()
findMismatchesForTeam l brig galley spar tid = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) (getTeamMembers tid))
      .| C.mapM
        ( \(i, p) -> do
            Log.info l $
              Log.field "team members" (show ((i - 1) * pageSize + fromIntegral (length p)))
                . Log.field "team" (idToText tid)
            pure p
        )
      .| C.mapM_ (mapM_ (checkUser l brig spar tid))

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: TeamId -> ConduitM () [(UserId, Maybe UserId, Maybe UTCTime)] Client ()
getTeamMembers tid = paginateC cql (paramsP LocalQuorum (Identity tid) pageSize) x5
  where
    cql :: PrepQuery R (Identity TeamId) (UserId, Maybe UserId, Maybe UTCTime)
    cql = "SELECT user, invited_by, invited_at FROM team_member where team = ?"

getEmailAndExternalId :: UserId -> Client (Maybe (Maybe Email, Maybe (Writetime Email), Maybe Email, Maybe UserSSOId))
getEmailAndExternalId uid =
  retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Maybe Email, Maybe (Writetime Email), Maybe Email, Maybe UserSSOId)
    cql = "SELECT email, writetime(email), email_unvalidated, sso_id from user where id = ?"

getSCIMTeams :: ConduitM () [Identity TeamId] Client ()
getSCIMTeams = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Identity TeamId)
    cql = "SELECT distinct team from scim_external"

getSAMLUser :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
getSAMLUser uref = do
  mbUid <- getSAMLUserNew uref
  case mbUid of
    Nothing -> getSAMLUserLegacy uref
    Just uid -> pure $ Just uid
  where
    getSAMLUserNew :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserNew (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, normalizeQualifiedNameId subject))
      where
        sel :: PrepQuery R (SAML.Issuer, NormalizedUNameID) (Identity UserId)
        sel = "SELECT uid FROM user_v2 WHERE issuer = ? AND normalized_uname_id = ?"

    getSAMLUserLegacy :: (HasCallStack, MonadClient m) => SAML.UserRef -> m (Maybe UserId)
    getSAMLUserLegacy (SAML.UserRef tenant subject) =
      runIdentity
        <$$> (retry x1 . query1 sel $ params LocalQuorum (tenant, subject))
      where
        sel :: PrepQuery R (SAML.Issuer, SAML.NameID) (Identity UserId)
        sel = "SELECT uid FROM user WHERE issuer = ? AND sso_id = ?"

instance Cql (URIRef Absolute) where
  ctype = Tagged TextColumn
  toCql = CqlText . SAML.renderURI

  fromCql (CqlText t) = parseURI' t
  fromCql _ = Left "URI: expected CqlText"

deriving instance Cql SAML.Issuer

instance Cql SAML.NameID where
  ctype = Tagged TextColumn
  toCql = CqlText . cs . SAML.encodeElem

  fromCql (CqlText t) = SAML.decodeElem (cs t)
  fromCql _ = Left "NameID: expected CqlText"

checkUser :: Logger -> ClientState -> ClientState -> TeamId -> (UserId, Maybe UserId, Maybe UTCTime) -> IO ()
checkUser l brig spar tid (uid, mInvitedBy, mInvitedAt) = do
  maybeEmails <- runClient brig $ getEmailAndExternalId uid
  case maybeEmails of
    Nothing ->
      Log.warn l (Log.msg (Log.val "No information found") . Log.field "user" (idToText uid))
    Just (mEmail, mEmailWritetime, unvalidatedEmail, ssoId) -> do
      case mEmail of
        Nothing ->
          Log.warn l $
            Log.msg (Log.val "No email found")
              . Log.field "user" (idToText uid)
              . Log.field "team" (idToText tid)
        Just email -> do
          let emailWritetime = writeTimeToUTC <$> mEmailWritetime
          case unvalidatedEmail of
            Nothing -> pure ()
            Just ue ->
              Log.warn l $
                Log.msg (Log.val "Unvalidated email found")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "unvalidatedEmail" (show ue)
                  . Log.field "team" (idToText tid)
          case ssoId of
            Nothing ->
              Log.warn l $
                Log.msg (Log.val "No SSO Id Found")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "invited_by" (showMaybe mInvitedBy)
                  . Log.field "invited_at" (showMaybe mInvitedAt)
                  . Log.field "team" (idToText tid)
            Just (UserSSOId ref) -> do
              mUserIdFromSpar <- runClient spar $ getSAMLUser ref
              case mUserIdFromSpar of
                Nothing ->
                  Log.warn l $
                    Log.msg (Log.val "No UserId found for SSO Ref")
                      . Log.field "user" (idToText uid)
                      . Log.field "email" (show email)
                      . Log.field "userSSOIdRef" (show ref)
                      . Log.field "team" (idToText tid)
                Just userIdFromSpar ->
                  when (userIdFromSpar /= uid) $
                    Log.warn l $
                      Log.msg (Log.val "Wrong UserId found for SSO Ref")
                        . Log.field "user" (idToText uid)
                        . Log.field "email" (show email)
                        . Log.field "userSSOIdRef" (show ref)
                        . Log.field "ssoUserId" (show userIdFromSpar)
                        . Log.field "team" (idToText tid)
            Just (UserScimExternalId extId) -> do
              when (extId /= fromEmail email) $
                Log.warn l $
                  Log.msg (Log.val "External Id is not in sync with email")
                    . Log.field "user" (idToText uid)
                    . Log.field "email" (fromEmail email)
                    . Log.field "external id" extId
                    . Log.field "team" (idToText tid)
              mUidFromEmail <- runClient brig $ lookupKey (userEmailKey email)
              case mUidFromEmail of
                Nothing -> do
                  mTimes <- runClient spar $ readScimUserTimes uid
                  Log.warn l $
                    Log.msg (Log.val "Email missing from user_keys")
                      . Log.field "user" (idToText uid)
                      . Log.field "email" (fromEmail email)
                      . Log.field "email_write_time" (showMaybe emailWritetime)
                      . Log.field "scim_created_at" (showMaybe $ fst <$> mTimes)
                      . Log.field "scim_updated_at" (showMaybe $ snd <$> mTimes)
                      . Log.field "invited_by" (showMaybe mInvitedBy)
                      . Log.field "invited_at" (showMaybe mInvitedAt)
                      . Log.field "team" (idToText tid)
                Just uidFromEmail ->
                  when (uidFromEmail /= uid) $
                    Log.warn l $
                      Log.msg (Log.val "Wrong user found when looking up by email")
                        . Log.field "expected_user" (idToText uid)
                        . Log.field "found_user" (idToText uidFromEmail)
                        . Log.field "email" (fromEmail email)
                        . Log.field "team" (idToText tid)

-- | Read creation and last-update time from database for a given user id.
readScimUserTimes :: (HasCallStack, MonadClient m) => UserId -> m (Maybe (UTCTime, UTCTime))
readScimUserTimes uid = do
  retry x1 . query1 sel $ params LocalQuorum (Identity uid)
  where
    sel :: PrepQuery R (Identity UserId) (UTCTime, UTCTime)
    sel = "SELECT created_at, last_updated_at FROM scim_user_times WHERE uid = ?"

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""

-- | Used as a lookup key for 'UnqualifiedNameID' that only depends on the
-- lowercase version of the identifier. Use 'normalizeUnqualifiedNameId' or
-- 'normalizeQualifiedNameId' to create values.
newtype NormalizedUNameID = NormalizedUNameID {unNormalizedUNameID :: Text}
  deriving stock (Eq, Ord, Generic)

instance Cql NormalizedUNameID where
  ctype = Tagged TextColumn
  toCql = CqlText . unNormalizedUNameID
  fromCql (CqlText t) = pure $ NormalizedUNameID t
  fromCql _ = Left "NormalizedNameID: expected CqlText"

normalizeUnqualifiedNameId :: SAML.UnqualifiedNameID -> NormalizedUNameID
normalizeUnqualifiedNameId = NormalizedUNameID . CI.foldCase . nameIdTxt
  where
    nameIdTxt :: SAML.UnqualifiedNameID -> ST
    nameIdTxt (SAML.UNameIDUnspecified txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDEmail email) = SAMLEmail.render $ CI.original email
    nameIdTxt (SAML.UNameIDX509 txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDWindows txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDKerberos txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDEntity uri) = renderURI uri
    nameIdTxt (SAML.UNameIDPersistent txt) = SAML.unsafeFromXmlText txt
    nameIdTxt (SAML.UNameIDTransient txt) = SAML.unsafeFromXmlText txt

-- | Qualifiers are ignored.
normalizeQualifiedNameId :: SAML.NameID -> NormalizedUNameID
normalizeQualifiedNameId = normalizeUnqualifiedNameId . view SAML.nameID
