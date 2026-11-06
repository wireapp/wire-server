-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.EmailSubsystem where

import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Meeting qualified as API
import Wire.API.User
import Wire.EmailSubsystem

data SentMail = SentMail
  { locale :: Maybe Locale,
    content :: SentMailContent
  }
  deriving (Show, Eq)

data SentMailContent
  = PasswordResetMail PasswordResetPair
  | -- | hostName, hostEmail, meeting
    MeetingInvitationMail Text EmailAddress API.Meeting
  | -- | hostName, hostEmail, meeting, changeSummary
    MeetingUpdateMail Text EmailAddress API.Meeting Text
  | -- | hostName, hostEmail, meeting
    MeetingCancelMail Text EmailAddress API.Meeting
  deriving (Show, Eq)

inMemoryEmailSubsystemInterpreter :: (Member (State (Map EmailAddress [SentMail])) r) => InterpreterFor EmailSubsystem r
inMemoryEmailSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale ->
    -- Historical convention: newest first. Meeting mails below append
    -- (chronological), which the meeting-email spec assertions rely on.
    modify $ Map.insertWith (<>) email [SentMail mLocale $ PasswordResetMail keyCodePair]
  SendMeetingInvitationEmail email hostName hostEmail meeting mLocale ->
    modify $ Map.insertWith (flip (<>)) email [SentMail mLocale $ MeetingInvitationMail hostName hostEmail meeting]
  SendMeetingUpdateEmail email hostName hostEmail meeting changeSummary mLocale ->
    modify $ Map.insertWith (flip (<>)) email [SentMail mLocale $ MeetingUpdateMail hostName hostEmail meeting changeSummary]
  SendMeetingCancelEmail email hostName hostEmail meeting mLocale ->
    modify $ Map.insertWith (flip (<>)) email [SentMail mLocale $ MeetingCancelMail hostName hostEmail meeting]
  _ -> error "inMemoryEmailSubsystemInterpreter: implement on demand"

getEmailsSentTo :: (Member (State (Map EmailAddress [SentMail])) r) => EmailAddress -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email
