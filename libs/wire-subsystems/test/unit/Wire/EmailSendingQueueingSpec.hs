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

-- | Unit tests for the email queueing conversion ('Wire.EmailSending.Queueing')
-- and the 'SendEmailJob' payload serialization.
--
-- These cover the data path shared by the producer (brig enqueues a
-- 'BackgroundJobSendEmail') and the consumer (the background-worker reconstructs
-- the 'Mail' and sends it): the @Mail@ <-> 'SendEmailJob' conversion must be a
-- round-trip, and the job must survive JSON encoding/decoding through the
-- @background-jobs@ queue.
module Wire.EmailSendingQueueingSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Text.Lazy qualified as LT
import Imports
import Network.Mail.Mime
  ( Address (..),
    Mail (..),
    Part (..),
    PartContent (..),
    emptyMail,
    htmlPart,
    plainPart,
  )
import Test.Hspec
import Test.QuickCheck
import Wire.API.BackgroundJobs.Email
import Wire.EmailSending.Queueing

spec :: Spec
spec = do
  describe "toSerializableMail / fromSerializableMail" $ do
    it "round-trips Mail -> SerializableMail -> Mail -> SerializableMail" $ do
      let job = toSerializableMail sampleMail
      toSerializableMail <$> fromSerializableMail job `shouldBe` Right job

    it "preserves all address lists, headers and parts" $ do
      let sm = (toSerializableMail sampleMail).sejMail
      sm.smFrom `shouldBe` smaFrom
      sm.smTo `shouldBe` [smaTo]
      sm.smCc `shouldBe` [SerializableMailAddress Nothing "cc@example.com"]
      sm.smBcc `shouldBe` []
      sm.smHeaders
        `shouldBe` [ SerializableMailHeader "Subject" "Verify your email",
                     SerializableMailHeader "X-Foo" "bar"
                   ]
      length sm.smParts `shouldBe` 1
      length (concat sm.smParts) `shouldBe` 2

    -- Exercises the non-default disposition branches (Inline/Attachment carry a
    -- filename) via the public conversion API, since brig's render path only
    -- produces DefaultDisposition.
    it "round-trips Inline/Attachment dispositions and all encodings" $ do
      toSerializableMail <$> fromSerializableMail variantJob `shouldBe` Right variantJob

    it "round-trips nested parts (NestedParts) through Mail and JSON" $ do
      let job = toSerializableMail nestedMail
      toSerializableMail <$> fromSerializableMail job `shouldBe` Right job
      Aeson.decode (Aeson.encode job) `shouldBe` Just job

    it "decodes a job nested to the maximum allowed depth" $ do
      fromSerializableMail (nestedJobAtDepth 10) `shouldSatisfy` isRight

    -- Defense in depth at the worker boundary: anything malformed read off
    -- the queue is rejected instead of rendered and sent.
    it "rejects a job nested deeper than the maximum" $ do
      fromSerializableMail (nestedJobAtDepth 11) `shouldSatisfy` isLeft

    it "rejects flat content that is not valid base64" $ do
      let job = SendEmailJob {sejMail = sampleJob.sejMail {smParts = [[partWithContent (SerializablePartContentText "not base64!!!")]]}}
      fromSerializableMail job `shouldSatisfy` isLeft

    -- One case per call site of 'validateHeaderField', so a future refactor
    -- that drops one (e.g. a missing 'traverse' over 'smaName') fails here.
    it "rejects NUL in every header-rendered field" $ do
      let base = sampleJob.sejMail
          okPart = partWithContent (SerializablePartContentText "aGk=")
          withMail mail = SendEmailJob {sejMail = mail}
          jobs :: [SendEmailJob]
          jobs =
            [ withMail base {smFrom = base.smFrom {smaName = Just "Wire\0"}},
              withMail base {smTo = [base.smFrom {smaEmail = "evil\0@example.com"}]},
              withMail base {smParts = [[okPart {smpType = "text/plain\0"}]]},
              withMail
                base
                  { smParts =
                      [ [ okPart
                            { smpDisposition =
                                SerializableDisposition {smdType = SerializableDispositionInline, smdFilename = "evil\0.txt"}
                            }
                        ]
                      ]
                  },
              withMail
                base
                  { smParts =
                      [ [ okPart
                            { smpDisposition =
                                SerializableDisposition {smdType = SerializableDispositionDefault, smdFilename = "evil\0.txt"}
                            }
                        ]
                      ]
                  }
            ]
      mapM_ (\job -> fromSerializableMail job `shouldSatisfy` isLeft) jobs

    it "rejects CR/LF in header-rendered fields" $ do
      let job =
            SendEmailJob
              { sejMail =
                  sampleJob.sejMail
                    { smHeaders = [SerializableMailHeader "Subject" "hi\r\nBcc: evil@example.com"]
                    }
              }
      fromSerializableMail job `shouldSatisfy` isLeft

  describe "SendEmailJob JSON serialization" $ do
    it "round-trips the sample job through Aeson" $ do
      let job = toSerializableMail sampleMail
      Aeson.decode (Aeson.encode job) `shouldBe` Just job

    it "decoding a serialized job yields a job that round-trips back to itself" $ do
      let job = toSerializableMail sampleMail
          decoded = Aeson.decode (Aeson.encode job) :: Maybe SendEmailJob
      (fmap toSerializableMail . fromSerializableMail <$> decoded) `shouldBe` Just (Right job)

    -- Exercises the wire-api schema machinery (record fields, nested lists,
    -- and the encoding/disposition enums) for arbitrary payloads.
    it "encode . decode = id for arbitrary SendEmailJob" $
      property $ \(job :: SendEmailJob) ->
        Aeson.decode @SendEmailJob (Aeson.encode job) === Just job

-- | A mail shaped exactly like the ones brig builds (see
-- 'Wire.EmailSubsystem.Interpreter'): one alternative with a plain and an html
-- part, @to@/@cc@ addresses, and a couple of headers.
sampleMail :: Mail
sampleMail =
  (emptyMail smaFromMail)
    { mailTo = [smaToMail],
      mailCc = [Address Nothing "cc@example.com"],
      mailBcc = [],
      mailHeaders =
        [ ("Subject", "Verify your email"),
          ("X-Foo", "bar")
        ],
      mailParts =
        [ [ plainPart (LT.pack "Please verify your email."),
            htmlPart (LT.fromStrict "<p>Please verify your email.</p>")
          ]
        ]
    }
  where
    smaFromMail = Address (Just "Wire") "noreply@example.com"
    smaToMail = Address (Just "Alice") "alice@example.com"

-- | A mail whose single part carries nested sub-parts — the structure
-- 'encodeContent' must now handle instead of erroring.
nestedMail :: Mail
nestedMail = sampleMail {mailParts = [[nestedPart]]}
  where
    nestedPart =
      (plainPart (LT.pack "outer"))
        { partContent = NestedParts [plainPart (LT.pack "inner plain"), htmlPart (LT.pack "<p>inner html</p>")]
        }

sampleJob :: SendEmailJob
sampleJob = toSerializableMail sampleMail

-- | A job whose single part's content is nested @n@ levels deep (n
-- 'SerializablePartContentNestedParts' wrappers around base64 text).
nestedJobAtDepth :: Int -> SendEmailJob
nestedJobAtDepth n =
  SendEmailJob {sejMail = sampleJob.sejMail {smParts = [[partAtDepth n]]}}
  where
    partAtDepth :: Int -> SerializableMailPart
    partAtDepth 0 = partWithContent (SerializablePartContentText "aGk=")
    partAtDepth k = partWithContent (SerializablePartContentNestedParts [partAtDepth (k - 1)])

partWithContent :: SerializablePartContent -> SerializableMailPart
partWithContent content =
  SerializableMailPart
    { smpType = "text/plain",
      smpEncoding = SerializableEncodingNone,
      smpDisposition = SerializableDisposition {smdType = SerializableDispositionDefault, smdFilename = ""},
      smpHeaders = [],
      smpContent = content
    }

-- | A job with non-default dispositions (Inline/Attachment, which carry a
-- filename) and encodings other than the default, to exercise those branches.
variantJob :: SendEmailJob
variantJob =
  SendEmailJob
    { sejMail =
        SerializableMail
          { smFrom = smaFrom,
            smTo = [smaTo],
            smCc = [],
            smBcc = [],
            smHeaders = [SerializableMailHeader "Subject" "Attachments"],
            smParts =
              [ [ SerializableMailPart
                    { smpType = "image/png",
                      smpEncoding = SerializableEncodingBase64,
                      smpDisposition =
                        SerializableDisposition
                          { smdType = SerializableDispositionInline,
                            smdFilename = "logo.png"
                          },
                      smpHeaders = [],
                      smpContent = SerializablePartContentText "iVBORw0KGgo="
                    },
                  SerializableMailPart
                    { smpType = "application/pdf",
                      smpEncoding = SerializableEncodingQuotedPrintableText,
                      smpDisposition =
                        SerializableDisposition
                          { smdType = SerializableDispositionAttachment,
                            smdFilename = "doc.pdf"
                          },
                      smpHeaders = [SerializableMailHeader "Content-ID" "<doc>"],
                      smpContent = SerializablePartContentText "JVBERi0="
                    }
                ]
              ]
          }
    }

smaFrom :: SerializableMailAddress
smaFrom = SerializableMailAddress {smaName = Just "Wire", smaEmail = "noreply@example.com"}

smaTo :: SerializableMailAddress
smaTo = SerializableMailAddress {smaName = Just "Alice", smaEmail = "alice@example.com"}
