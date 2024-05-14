{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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

module Util where

import Bilge
import Bilge.Assert
import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Retry (constantDelay, exponentialBackoff, retrying)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Tuple.Extra
import Data.UUID qualified as UUID
import Data.UUID.V4
import Imports
import System.Random
import Test.Tasty.HUnit
import TestSetup
import UnliftIO.Retry (limitRetries, recoverAll)
import Web.Cookie
import Wire.API.Team
import Wire.API.Team.Invitation
import Wire.API.Team.Member as Team
import Wire.API.Team.Role
import Wire.API.User as User

eventually :: (MonadIO m, MonadMask m, MonadUnliftIO m) => m a -> m a
eventually = recoverAll (limitRetries 7 <> exponentialBackoff 50000) . const

createTeamWithNMembers :: (HasCallStack) => Int -> TestM (UserId, TeamId, [UserId])
createTeamWithNMembers n = do
  (owner, tid) <- createBindingTeam
  mems <- replicateM n $ do
    mem <- addUserToTeam owner tid
    pure (mem ^. Team.userId)
  pure (owner, tid, mems)

createBindingTeam :: (HasCallStack) => TestM (UserId, TeamId)
createBindingTeam = do
  first User.userId <$> createBindingTeam'

createBindingTeam' :: (HasCallStack) => TestM (User, TeamId)
createBindingTeam' = do
  owner <- randomTeamCreator'
  refreshIndex
  pure (owner, fromMaybe (error "createBindingTeam: no team id") (owner.userTeam))

randomTeamCreator' :: (HasCallStack) => TestM User
randomTeamCreator' = randomUser'' True True True

randomUser :: (HasCallStack) => TestM UserId
randomUser = qUnqualified <$> randomUser' False True True

randomUser' :: (HasCallStack) => Bool -> Bool -> Bool -> TestM (Qualified UserId)
randomUser' isCreator hasPassword hasEmail = userQualifiedId <$> randomUser'' isCreator hasPassword hasEmail

randomUser'' :: (HasCallStack) => Bool -> Bool -> Bool -> TestM User
randomUser'' isCreator hasPassword hasEmail = selfUser <$> randomUserProfile' isCreator hasPassword hasEmail

randomUserProfile' :: (HasCallStack) => Bool -> Bool -> Bool -> TestM SelfProfile
randomUserProfile' isCreator hasPassword hasEmail = randomUserProfile'' isCreator hasPassword hasEmail <&> fst

randomUserProfile'' :: (HasCallStack) => Bool -> Bool -> Bool -> TestM (SelfProfile, (Email, Phone))
randomUserProfile'' isCreator hasPassword hasEmail = do
  b <- view tsBrig
  e <- liftIO randomEmail
  p <- liftIO randomPhone
  let pl =
        object $
          ["name" .= fromEmail e]
            <> ["password" .= defPassword | hasPassword]
            <> ["email" .= fromEmail e | hasEmail]
            -- <> ["phone" .= fromPhone p]
            <> ["team" .= BindingNewTeam (newNewTeam (unsafeRange "teamName") DefaultIcon) | isCreator]
  (,(e, p)) . responseJsonUnsafe <$> (post (b . path "/i/users" . Bilge.json pl) <!! const 201 === statusCode)

randomPhone :: (MonadIO m) => m Phone
randomPhone = liftIO $ do
  nrs <- map show <$> replicateM 14 (randomRIO (0, 9) :: IO Int)
  let phone = parsePhone . Text.pack $ "+0" ++ concat nrs
  pure $ fromMaybe (error "Invalid random phone#") phone

randomEmailUser :: (HasCallStack) => TestM (UserId, Email)
randomEmailUser = randomUserProfile'' False False True <&> bimap (User.userId . selfUser) fst

randomPhoneUser :: (HasCallStack) => TestM (UserId, Phone)
randomPhoneUser = randomUserProfile'' False False True <&> bimap (User.userId . selfUser) snd

randomEmailPhoneUser :: (HasCallStack) => TestM (UserId, (Email, Phone))
randomEmailPhoneUser = randomUserProfile'' False False True <&> first (User.userId . selfUser)

defPassword :: PlainTextPassword8
defPassword = plainTextPassword8Unsafe "topsecretdefaultpassword"

randomEmail :: (MonadIO m) => m Email
randomEmail = do
  uid <- liftIO nextRandom
  pure $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

setHandle :: UserId -> Text -> TestM ()
setHandle uid h = do
  b <- view tsBrig
  put
    ( b
        . paths ["/i/users", toByteString' uid, "handle"]
        . Bilge.json (HandleUpdate h)
    )
    !!! do
      const 200 === statusCode

randomHandle :: (MonadIO m) => m Text
randomHandle = liftIO $ do
  nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
  pure (Text.pack (map chr nrs))

refreshIndex :: TestM ()
refreshIndex = do
  brig <- view tsBrig
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

addUserToTeam :: (HasCallStack) => UserId -> TeamId -> TestM TeamMember
addUserToTeam = addUserToTeamWithRole Nothing

addUserToTeamWithRole :: (HasCallStack) => Maybe Role -> UserId -> TeamId -> TestM TeamMember
addUserToTeamWithRole role inviter tid = do
  (inv, rsp2) <- addUserToTeamWithRole' role inviter tid
  let invitee :: User = responseJsonUnsafe rsp2
      inviteeId = User.userId invitee
  let invmeta = Just (inviter, inCreatedAt inv)
  mem <- getTeamMember inviter tid inviteeId
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
  liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
  pure mem

addUserToTeamWithRole' :: (HasCallStack) => Maybe Role -> UserId -> TeamId -> TestM (Invitation, ResponseLBS)
addUserToTeamWithRole' role inviter tid = do
  brig <- view tsBrig
  inviteeEmail <- randomEmail
  let invite = InvitationRequest Nothing role Nothing inviteeEmail Nothing
  invResponse <- postInvitation tid inviter invite
  inv <- responseJsonError invResponse
  inviteeCode <- getInvitationCode tid (inInvitation inv)
  r <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (acceptInviteBody inviteeEmail inviteeCode)
      )
  pure (inv, r)

acceptInviteBody :: Email -> InvitationCode -> RequestBody
acceptInviteBody email code =
  RequestBodyLBS . encode $
    object
      [ "name" .= Name "bob",
        "email" .= fromEmail email,
        "password" .= defPassword,
        "team_code" .= code
      ]

getInvitationCode :: (HasCallStack) => TeamId -> InvitationId -> TestM InvitationCode
getInvitationCode t ref = do
  brig <- view tsBrig
  let getm :: TestM (Maybe InvitationCode)
      getm = do
        r <-
          get
            ( brig
                . path "/i/teams/invitation-code"
                . queryItem "team" (toByteString' t)
                . queryItem "invitation_id" (toByteString' ref)
            )
        let lbs = fromMaybe "" $ responseBody r
        pure $ fromByteString . Text.encodeUtf8 =<< lbs ^? key "code" . _String

  fromMaybe (error "No code?")
    <$> retrying
      (constantDelay 800000 <> limitRetries 3)
      (\_ -> pure . isNothing)
      (const getm)

postInvitation :: TeamId -> UserId -> InvitationRequest -> TestM ResponseLBS
postInvitation t u i = do
  brig <- view tsBrig
  post $
    brig
      . paths ["teams", toByteString' t, "invitations"]
      . contentJson
      . body (RequestBodyLBS $ encode i)
      . zAuthAccess u "conn"

zAuthAccess :: UserId -> ByteString -> (Request -> Request)
zAuthAccess u conn =
  zUser u
    . zConn conn
    . zType "access"

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

zType :: ByteString -> Request -> Request
zType = header "Z-Type"

getTeamMember :: (HasCallStack) => UserId -> TeamId -> UserId -> TestM TeamMember
getTeamMember getter tid gettee = do
  g <- view tsGalley
  getTeamMember' g getter tid gettee

getTeamMember' :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => Galley -> UserId -> TeamId -> UserId -> m TeamMember
getTeamMember' g getter tid gettee = do
  r <-
    get (g . paths ["teams", toByteString' tid, "members", toByteString' gettee] . zUser getter)
      <!! const 200 === statusCode
  responseJsonError r
