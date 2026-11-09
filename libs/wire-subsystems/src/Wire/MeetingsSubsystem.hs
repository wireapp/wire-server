{-# LANGUAGE TemplateHaskell #-}

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

module Wire.MeetingsSubsystem where

import Data.Code (Key, Value)
import Data.Domain (Domain)
import Data.Id
import Data.Misc (IpAddr, PlainTextPassword8)
import Data.Qualified
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Wire.API.Meeting
import Wire.API.User.EmailAddress (EmailAddress)

data MeetingsSubsystem m a where
  CreateMeeting ::
    Local UserId ->
    ConnId ->
    NewMeeting ->
    MeetingsSubsystem m MeetingWithConversation
  UpdateMeeting ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeeting ->
    MeetingsSubsystem m (Maybe MeetingWithConversation)
  DeleteMeeting ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    MeetingsSubsystem m Bool
  RefreshMeetingLink ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    RefreshMeetingLinkRequest ->
    MeetingsSubsystem m (Maybe MeetingWithConversation)
  -- | Unauthenticated check of a meeting join link
  -- (@GET /meeting/{domain}/{key}/{code}/code-check@). The link's code key
  -- addresses the code row (stable across refreshes); its code value is the
  -- rotating capability embedded in the link URL and must match the live
  -- row, so a refreshed link invalidates stale URLs.
  -- 'CodeCheckNotFound' (surfaced as 404) when the key has no live local
  -- meeting behind it: unknown keys, stale code values, expired meetings,
  -- keys addressing a conversation code, or code-store modes without
  -- meeting-code support. No creator/membership requirement and no
  -- meetings-feature gate. A password-protected code requires the matching
  -- 'password' query param ('CodeCheckInvalidPassword', surfaced as 403);
  -- a passwordless code checks with or without a password.
  CodeCheckMeetingLink ::
    -- | Client address (X-Forwarded-For); rate-limit key for the unauthenticated check.
    IpAddr ->
    -- | The domain from the link path; must be the local domain.
    Domain ->
    Key ->
    Value ->
    Maybe PlainTextPassword8 ->
    MeetingsSubsystem m CodeCheckMeetingLinkResult
  -- | Join a meeting through its join link (WPB-28989), like
  -- @POST /conversations/join@: resolve the link's code and join the
  -- meeting's conversation via 'CodeAccess'. Re-joining by an existing
  -- member is an idempotent no-op ('NoChanges' -> 'Unchanged'). Returns
  -- 'JoinMeetingNotFound' when the link does not resolve,
  -- 'JoinMeetingInvalidPassword' when the code carries a password the
  -- request does not match. Errors are returned, not thrown, so
  -- interpreters stay observable in tests (handler-space throws are not);
  -- the route handler maps them to 404/403.
  JoinMeeting ::
    Local UserId ->
    ConnId ->
    -- | The domain from the link path; must be the local domain.
    Domain ->
    Key ->
    Value ->
    Maybe PlainTextPassword8 ->
    MeetingsSubsystem m JoinMeetingResult
  GetMeeting ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe Meeting)
  ListMeetings ::
    Local UserId ->
    MeetingsSubsystem m [Meeting]
  CreateMeetingV16 ::
    Local UserId ->
    ConnId ->
    NewMeetingV16 ->
    MeetingsSubsystem m MeetingWithConversationV16
  UpdateMeetingV16 ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeeting ->
    MeetingsSubsystem m (Maybe MeetingWithConversationV16)
  GetMeetingV16 ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingV16)
  ListMeetingsV16 ::
    Local UserId ->
    MeetingsSubsystem m [MeetingV16]
  CreateMeetingV18 ::
    Local UserId ->
    ConnId ->
    NewMeetingV18 ->
    MeetingsSubsystem m MeetingWithConversationV18
  UpdateMeetingV18 ::
    Local UserId ->
    ConnId ->
    Qualified MeetingId ->
    UpdateMeeting ->
    MeetingsSubsystem m (Maybe MeetingWithConversationV18)
  GetMeetingV18 ::
    Local UserId ->
    Qualified MeetingId ->
    MeetingsSubsystem m (Maybe MeetingV18)
  ListMeetingsV18 ::
    Local UserId ->
    MeetingsSubsystem m [MeetingV18]
  AddInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  RemoveInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  ReplaceInvitedEmails ::
    Local UserId ->
    Qualified MeetingId ->
    [EmailAddress] ->
    MeetingsSubsystem m Bool
  CleanupOldMeetings ::
    UTCTime ->
    Int ->
    MeetingsSubsystem m Int64

-- | Outcome of checking a meeting join link ('CodeCheckMeetingLink').
data CodeCheckMeetingLinkResult
  = -- | No live local meeting behind the key+code.
    CodeCheckNotFound
  | -- | The code is password-protected and the query param does not match.
    CodeCheckInvalidPassword
  | -- | The link resolved; carries the meeting metadata.
    CodeCheckOk MeetingCodeCheck
  deriving stock (Eq, Show)

-- | Outcome of joining a meeting through its join link ('JoinMeeting').
data JoinMeetingResult
  = -- | No live local meeting, or no live join code row for it.
    JoinMeetingNotFound
  | -- | The join code is password-protected and the supplied password does
    -- not match (or is missing).
    JoinMeetingInvalidPassword
  | -- | The link resolved and the caller was joined (or re-joined
    -- idempotently); carries the meeting with its conversation view.
    JoinMeetingOk MeetingWithConversation
  deriving stock (Eq, Show)

makeSem ''MeetingsSubsystem
