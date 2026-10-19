{-# LANGUAGE StrictData #-}

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

-- | RFC 5545 iCalendar generation for meeting emails. The produced VCALENDAR
-- is attached to invitation/update/cancellation emails as a @text/calendar@
-- part; the email templates reference it via a @cid:@ link (see
-- 'icsCidUrl').
--
-- Date-times are emitted in UTC (@Z@ form); no VTIMEZONE component is
-- generated — calendar clients render the times in each recipient's local
-- zone. Recurrence is not encoded.
module Wire.EmailSubsystem.Ics
  ( IcsMethod (..),
    icsAttachment,
    icsCidUrl,
  )
where

import Data.CaseInsensitive (mk)
import Data.Default (def)
import Data.Domain (domainText)
import Data.Id (toUUID)
import Data.Map qualified as Map
import Data.Misc (httpsUrlToText)
import Data.Qualified (qDomain, qUnqualified)
import Data.Range (fromRange)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID (toText)
import Data.Version (Version (..))
import Imports
import Network.Mail.Mime (Disposition (..), Encoding (Base64), Part (..), PartContent (..))
import Network.URI
import Text.ICalendar.Printer
import Text.ICalendar.Types
import Wire.API.Meeting (Meeting (..))
import Wire.API.User (EmailAddress, fromEmail)

-- | iTIP method carried in the ICS @METHOD@ property.
data IcsMethod = IcsRequest | IcsCancel
  deriving (Eq, Show)

methodText :: IcsMethod -> Text
methodText IcsRequest = "REQUEST"
methodText IcsCancel = "CANCEL"

-- | Content-ID reference used for the @${ics_url}@ template hole.
icsCidUrl :: Text
icsCidUrl = "cid:" <> icsFilename

-- | Strip ASCII control characters (C0 and DEL) — notably CR, which
-- iCalendar's printer does not escape but its own parser treats as a
-- content-line terminator, so a raw CR would let a meeting title or host
-- name inject ICS properties.
sanitizeText :: Text -> Text
sanitizeText = T.filter (\c -> c >= ' ' && c /= '\x7f')

-- | Additionally drop double quotes so a host name cannot break out of a
-- quoted ICS parameter value (e.g. @ORGANIZER;CN="..."@).
sanitizeParamValue :: Text -> Text
sanitizeParamValue = T.replace "\"" "" . sanitizeText

-- | RFC 6068 percent-encoding of the address in a @mailto:@ URI; only
-- unreserved characters plus @@@ and @+@ are left raw.
percentEncode :: Text -> Text
percentEncode =
  T.pack
    . escapeURIString (\c -> isUnreserved c || c `elem` ("@+" :: String))
    . T.unpack

-- | Filename for both the @Content-ID@ (@\<meeting.ics\>@) and the attachment
-- disposition.
icsFilename :: Text
icsFilename = "meeting.ics"

-- | The same UID across invitation/update/cancel so calendar clients
-- correlate the events.
meetingUid :: Meeting -> Text
meetingUid m =
  toText (toUUID (qUnqualified m.id))
    <> "@"
    <> domainText (qDomain m.id)

meetingUidL :: Meeting -> LT.Text
meetingUidL = LT.fromStrict . meetingUid

-- | RFC 5545 @SEQUENCE@: derived from the meeting's @updated_at@ (epoch
-- seconds, second granularity) so meeting updates produce a larger sequence
-- than the invitation they revise; calendar clients ignore updates that
-- reuse the UID with an unchanged sequence. Cancellation mails increment by
-- one (RFC 5546 §3.2.6) even without an intervening update.
meetingSequence :: Meeting -> Integer
meetingSequence = floor . utcTimeToPOSIXSeconds . (.updatedAt)

sequenceForMethod :: IcsMethod -> Meeting -> Integer
sequenceForMethod method meeting =
  meetingSequence meeting + case method of
    IcsRequest -> 0
    IcsCancel -> 1

-- | Serialize the meeting as an RFC 5545 VCALENDAR.
meetingIcs :: IcsMethod -> Meeting -> Text -> EmailAddress -> EmailAddress -> LByteString
meetingIcs method meeting hostName organizer attendee =
  printICalendar def calendar
  where
    calendar =
      VCalendar
        { vcProdId = ProdId "-//Wire//Meeting//EN" def,
          vcVersion = MaxICalVersion (Version [2, 0] []) def,
          vcScale = def,
          vcMethod = Just (Method (mk (LT.fromStrict (methodText method))) def),
          vcOther = mempty,
          vcTimeZones = mempty,
          vcEvents = Map.singleton (meetingUidL meeting, Nothing) event,
          vcTodos = mempty,
          vcJournals = mempty,
          vcFreeBusys = mempty,
          vcOtherComps = mempty
        }

    event =
      VEvent
        { veDTStamp = DTStamp meeting.updatedAt def,
          veUID = UID (meetingUidL meeting) def,
          veClass = def,
          veDTStart = Just (DTStartDateTime (UTCDateTime meeting.startTime) def),
          veCreated = Nothing,
          veDescription = Nothing,
          veGeo = Nothing,
          veLastMod = Nothing,
          veLocation = Nothing,
          veOrganizer =
            Just
              Organizer
                { organizerValue = mailtoUri organizer,
                  organizerCN = Just (LT.fromStrict (sanitizeParamValue hostName)),
                  organizerDir = Nothing,
                  organizerSentBy = Nothing,
                  organizerLanguage = Nothing,
                  organizerOther = def
                },
          vePriority = def,
          veSeq = Sequence (sequenceForMethod method meeting) def,
          veStatus =
            Just $ case method of
              IcsRequest -> ConfirmedEvent def
              IcsCancel -> CancelledEvent def,
          veSummary = Just (Summary (LT.fromStrict (sanitizeText (fromRange meeting.title))) Nothing Nothing def),
          veTransp = def,
          veUrl = joinUri =<< meeting.link,
          veRecurId = Nothing,
          veRRule = mempty,
          veDTEndDuration = Just (Left (DTEndDateTime (UTCDateTime meeting.endTime) def)),
          veAttach = mempty,
          veAttendee =
            Set.singleton
              Attendee
                { attendeeValue = mailtoUri attendee,
                  attendeeCUType = def,
                  attendeeMember = mempty,
                  attendeeRole = def,
                  attendeePartStat = def,
                  attendeeRSVP = False,
                  attendeeDelTo = mempty,
                  attendeeDelFrom = mempty,
                  attendeeSentBy = Nothing,
                  attendeeCN = Nothing,
                  attendeeDir = Nothing,
                  attendeeLanguage = Nothing,
                  attendeeOther = def
                },
          veCategories = mempty,
          veComment = mempty,
          veContact = mempty,
          veExDate = mempty,
          veRStatus = mempty,
          veRelated = mempty,
          veResources = mempty,
          veRDate = mempty,
          veAlarms = mempty,
          veOther = mempty
        }

    mailtoUri addr =
      nullURI
        { uriScheme = "mailto:",
          uriPath = T.unpack (percentEncode (fromEmail addr))
        }

    -- iCalendar's URL property holds a Network.URI; HttpsUrl wraps a
    -- URI.ByteString URIRef, so re-serialize and re-parse. On the (not
    -- provably impossible) parse disagreement, omit the URL property rather
    -- than crash the mail render.
    joinUri l =
      case parseURI (T.unpack (httpsUrlToText l)) of
        Just uri -> Just (URL uri def)
        Nothing -> Nothing

-- | A mime-mail part carrying the ICS attachment.
icsAttachment :: IcsMethod -> Meeting -> Text -> EmailAddress -> EmailAddress -> Part
icsAttachment method meeting hostName organizer attendee =
  Part
    { partType = "text/calendar; charset=utf-8; method=" <> methodText method,
      partEncoding = Base64,
      partDisposition = AttachmentDisposition icsFilename,
      partHeaders = [("Content-ID", "<" <> icsFilename <> ">")],
      partContent = PartContent (meetingIcs method meeting hostName organizer attendee)
    }
