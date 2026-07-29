-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.MeetingEvent where

import Data.Domain (Domain (..))
import Data.Id
import Data.Qualified (Qualified (..))
import Data.Time
import Data.UUID qualified as UUID
import Imports
import Wire.API.Event.Meeting

testObject_Event_meeting_create_manual_1 :: Event
testObject_Event_meeting_create_manual_1 =
  Event
    { evtType = Create,
      evtMeeting = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), qDomain = Domain {_domainText = "example.com"}},
      evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "example.com"}},
      evtFrom = EventFromUser $ Qualified {qUnqualified = Id (fromJust (UUID.fromString "a471447c-aa30-4592-81b0-dec6c1c02bca")), qDomain = Domain {_domainText = "example.com"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtTeam = Nothing
    }

testObject_Event_meeting_update_manual_1 :: Event
testObject_Event_meeting_update_manual_1 =
  Event
    { evtType = Update,
      evtMeeting = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), qDomain = Domain {_domainText = "example.com"}},
      evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "example.com"}},
      evtFrom = EventFromUser $ Qualified {qUnqualified = Id (fromJust (UUID.fromString "a471447c-aa30-4592-81b0-dec6c1c02bca")), qDomain = Domain {_domainText = "example.com"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtTeam = Nothing
    }

testObject_Event_meeting_delete_manual_1 :: Event
testObject_Event_meeting_delete_manual_1 =
  Event
    { evtType = Delete,
      evtMeeting = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), qDomain = Domain {_domainText = "example.com"}},
      evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "example.com"}},
      evtFrom = EventFromUser $ Qualified {qUnqualified = Id (fromJust (UUID.fromString "a471447c-aa30-4592-81b0-dec6c1c02bca")), qDomain = Domain {_domainText = "example.com"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtTeam = Nothing
    }

testObject_Event_meeting_member_add_manual_1 :: Event
testObject_Event_meeting_member_add_manual_1 =
  Event
    { evtType = MemberAdd,
      evtMeeting = Qualified {qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")), qDomain = Domain {_domainText = "example.com"}},
      evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "example.com"}},
      evtFrom = EventFromUser $ Qualified {qUnqualified = Id (fromJust (UUID.fromString "a471447c-aa30-4592-81b0-dec6c1c02bca")), qDomain = Domain {_domainText = "example.com"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtTeam = Nothing
    }
