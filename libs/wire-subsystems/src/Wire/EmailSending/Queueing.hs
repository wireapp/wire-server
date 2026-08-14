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

-- | Queueing interpreter for the 'EmailSending' effect.
--
-- Instead of sending mail directly (SMTP\/SES), this interpreter enqueues a
-- 'BackgroundJobSendEmail' job on the shared @background-jobs@ queue. The
-- actual send is performed by the background-worker (see
-- "Wire.BackgroundJobsRunner.Interpreter").
--
-- This is the single seam for *all* email sending in brig: every mail flows
-- through 'EmailSending', so interpreting it to a queue covers verification,
-- activation, password-reset, invitation, new-client, deletion, SAML IdP-change,
-- provider and enterprise-audit emails with one constructor.
module Wire.EmailSending.Queueing
  ( emailViaQueueInterpreter,
    toSerializableMail,
    fromSerializableMail,
  )
where

import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.Id (RequestId, randomId)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Imports
import Network.AMQP qualified as Q
import Network.Mail.Mime
  ( Address (..),
    Disposition (..),
    Encoding (..),
    Mail (..),
    Part (..),
    PartContent (..),
  )
import Polysemy
import Wire.API.BackgroundJobs
import Wire.API.BackgroundJobs.Email
import Wire.BackgroundJobsPublisher.RabbitMQ qualified as Publisher
import Wire.EmailSending (EmailSending (SendMail))

-- | Interpret 'EmailSending' by enqueuing a 'BackgroundJobSendEmail' job.
--
-- The job id is minted with 'randomId' and the message is published directly to
-- the @background-jobs@ queue via the channel-level 'Publisher.publishJob'. This
-- keeps the interpreter's only effect requirement 'Embed' 'IO', so it drops into
-- the producer's effect stack exactly where the old direct-send interpreter sat
-- (it needs neither 'Random' nor 'BackgroundJobPublisher' to be present at that
-- point in the stack).
emailViaQueueInterpreter ::
  (Member (Embed IO) r) =>
  RequestId ->
  MVar Q.Channel ->
  InterpreterFor EmailSending r
emailViaQueueInterpreter requestId channelMVar =
  interpret \case
    SendMail mail -> do
      channel <- embed (readMVar channelMVar)
      jobId <- embed (randomId @IO)
      Publisher.publishJob requestId channel jobId (BackgroundJobSendEmail (toSerializableMail mail))

--------------------------------------------------------------------------------
-- Mail <-> record conversion
--------------------------------------------------------------------------------

toSerializableMail :: Mail -> SendEmailJob
toSerializableMail mail = SendEmailJob {sejMail = fromMail mail}

-- | Reconstruct a 'Mail' from a deserialized job.
--
-- The job comes off an internal queue, but this is defense in depth: rather
-- than trusting it, the conversion rejects jobs whose part nesting exceeds
-- 'maxPartNesting', whose flat content is not strictly valid base64, or whose
-- header-rendered fields contain CR\/LF\/NUL (header injection). Anything
-- produced by 'toSerializableMail' always decodes.
fromSerializableMail :: SendEmailJob -> Either Text Mail
fromSerializableMail job = toMail job.sejMail

fromMail :: Mail -> SerializableMail
fromMail m =
  SerializableMail
    { smFrom = fromAddress m.mailFrom,
      smTo = fromAddress <$> m.mailTo,
      smCc = fromAddress <$> m.mailCc,
      smBcc = fromAddress <$> m.mailBcc,
      smHeaders = fromHeader <$> m.mailHeaders,
      smParts = (fromPart <$>) <$> m.mailParts
    }

toMail :: SerializableMail -> Either Text Mail
toMail m = do
  mailFrom <- toAddress m.smFrom
  mailTo <- traverse toAddress m.smTo
  mailCc <- traverse toAddress m.smCc
  mailBcc <- traverse toAddress m.smBcc
  mailHeaders <- traverse toHeader m.smHeaders
  mailParts <- traverse (traverse (toPart 0)) m.smParts
  pure
    Mail
      { mailFrom = mailFrom,
        mailTo = mailTo,
        mailCc = mailCc,
        mailBcc = mailBcc,
        mailHeaders = mailHeaders,
        mailParts = mailParts
      }

fromAddress :: Address -> SerializableMailAddress
fromAddress a =
  SerializableMailAddress {smaName = a.addressName, smaEmail = a.addressEmail}

toAddress :: SerializableMailAddress -> Either Text Address
toAddress a = do
  addressName <- traverse (validateHeaderField "address name") a.smaName
  addressEmail <- validateHeaderField "address email" a.smaEmail
  pure Address {addressName = addressName, addressEmail = addressEmail}

-- | mime-mail headers are @[(ByteString, Text)]@: the name is a (ASCII)
-- ByteString, the value is already 'Text'.
fromHeader :: (ByteString, Text) -> SerializableMailHeader
fromHeader (name, value) =
  SerializableMailHeader {smhName = Text.decodeUtf8 name, smhValue = value}

toHeader :: SerializableMailHeader -> Either Text (ByteString, Text)
toHeader h = do
  name <- validateHeaderField "header name" h.smhName
  value <- validateHeaderField "header value" h.smhValue
  pure (Text.encodeUtf8 name, value)

fromPart :: Part -> SerializableMailPart
fromPart p =
  SerializableMailPart
    { smpType = p.partType,
      smpEncoding = fromEncoding p.partEncoding,
      smpDisposition = fromDisposition p.partDisposition,
      smpHeaders = fromHeader <$> p.partHeaders,
      smpContent = encodeContent p.partContent
    }

toPart :: Int -> SerializableMailPart -> Either Text Part
toPart depth p
  | depth > maxPartNesting = Left "part nesting deeper than the maximum"
  | otherwise = do
      partType <- validateHeaderField "part type" p.smpType
      partDisposition <- toDisposition p.smpDisposition
      partHeaders <- traverse toHeader p.smpHeaders
      partContent <- decodeContent depth p.smpContent
      pure
        Part
          { partType = partType,
            partEncoding = toEncoding p.smpEncoding,
            partDisposition = partDisposition,
            partHeaders = partHeaders,
            partContent = partContent
          }

fromEncoding :: Encoding -> SerializableEncoding
fromEncoding = \case
  None -> SerializableEncodingNone
  Base64 -> SerializableEncodingBase64
  QuotedPrintableBinary -> SerializableEncodingQuotedPrintableBinary
  QuotedPrintableText -> SerializableEncodingQuotedPrintableText

toEncoding :: SerializableEncoding -> Encoding
toEncoding = \case
  SerializableEncodingNone -> None
  SerializableEncodingBase64 -> Base64
  SerializableEncodingQuotedPrintableBinary -> QuotedPrintableBinary
  SerializableEncodingQuotedPrintableText -> QuotedPrintableText

fromDisposition :: Disposition -> SerializableDisposition
fromDisposition = \case
  DefaultDisposition ->
    SerializableDisposition {smdType = SerializableDispositionDefault, smdFilename = ""}
  InlineDisposition filename ->
    SerializableDisposition {smdType = SerializableDispositionInline, smdFilename = filename}
  AttachmentDisposition filename ->
    SerializableDisposition {smdType = SerializableDispositionAttachment, smdFilename = filename}

toDisposition :: SerializableDisposition -> Either Text Disposition
toDisposition d = case d.smdType of
  -- The filename is dropped by DefaultDisposition (never rendered), but it is
  -- validated anyway to keep every Text field on a SerializableMail* free of
  -- CR/LF/NUL, uniformly.
  SerializableDispositionDefault ->
    DefaultDisposition <$ validateHeaderField "disposition filename" d.smdFilename
  SerializableDispositionInline ->
    InlineDisposition <$> validateHeaderField "disposition filename" d.smdFilename
  SerializableDispositionAttachment ->
    AttachmentDisposition <$> validateHeaderField "disposition filename" d.smdFilename

-- | Encode a part's content for serialization. Flat byte content becomes
-- base64 'Text'; nested alternative groups recurse via 'fromPart'. The
-- conversion is total, so any 'Mail' round-trips through the jobs queue.
encodeContent :: PartContent -> SerializablePartContent
encodeContent = \case
  PartContent bs -> SerializablePartContentText (Text.decodeUtf8 . BL.toStrict $ B64.encode bs)
  NestedParts ps -> SerializablePartContentNestedParts (fromPart <$> ps)

-- | Inverse of 'encodeContent'. Nested alternative groups recurse one level
-- deeper (bounded by 'maxPartNesting' via 'toPart'); flat content must decode
-- as strict base64, which anything produced by 'encodeContent' is.
decodeContent :: Int -> SerializablePartContent -> Either Text PartContent
decodeContent depth = \case
  SerializablePartContentText t -> case B64.decode (BL.fromStrict (Text.encodeUtf8 t)) of
    Left err -> Left ("invalid base64 in part content: " <> T.pack err)
    Right bs -> Right (PartContent bs)
  SerializablePartContentNestedParts ps -> NestedParts <$> traverse (toPart (depth + 1)) ps

-- | Maximum nesting depth of parts (0 = top-level) accepted by 'toPart'.
-- Mails built with the mime-mail smart constructors nest at most two or three
-- levels; anything deeper on the queue is malformed or adversarial.
maxPartNesting :: Int
maxPartNesting = 10

-- | Validate a field that mime-mail renders into an RFC 5322 header position
-- (address names\/emails, header names\/values, part type, disposition
-- filename). CR, LF or NUL would allow a malformed job to inject additional
-- headers or body parts. Producers never emit these; checking is defense in
-- depth for jobs read off the queue.
validateHeaderField :: Text -> Text -> Either Text Text
validateHeaderField fieldName value
  | T.any (\c -> c == '\r' || c == '\n' || c == '\0') value =
      Left (fieldName <> " contains CR/LF/NUL")
  | otherwise = Right value
