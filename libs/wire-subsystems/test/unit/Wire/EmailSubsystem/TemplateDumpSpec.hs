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

-- | Not a unit test: renders every sample email from
-- "Wire.EmailSubsystem.TemplateFixtures" to disk (subject, plain text and html
-- for each), plus an @index.html@ that shows all of them one under the other,
-- for eyeballing template changes.
--
-- Only runs when @WIRE_EMAIL_DUMP_DIR@ is set, otherwise it is pending:
--
-- > WIRE_EMAIL_DUMP_DIR=/tmp/emails make c package=wire-subsystems test=1
-- > WIRE_EMAIL_DUMP_DIR=/tmp/emails WIRE_EMAIL_DUMP_LOCALES=de,ar,ja ... # default: en
-- > WIRE_EMAIL_DUMP_DIR=/tmp/emails WIRE_EMAIL_DUMP_LOCALES=all ...      # all 20 locales
module Wire.EmailSubsystem.TemplateDumpSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Imports
import Network.Mail.Mime
import System.FilePath ((</>))
import Test.Hspec
import Wire.API.Locale
import Wire.EmailSubsystem.TemplateFixtures

spec :: Spec
spec = focus $ describe "email template dump" $ do
  mDir <- runIO $ lookupEnv "WIRE_EMAIL_DUMP_DIR"
  case mDir of
    Nothing ->
      it "writes all rendered emails to disk" $
        pendingWith "set WIRE_EMAIL_DUMP_DIR=<dir> to render all email templates for visual inspection"
    Just dir -> do
      wanted <- runIO localeFilter
      teamTemplates <- runIO loadTestTeamTemplates
      userTemplates <- runIO loadTestUserTemplates
      it "writes all rendered emails to disk" $ do
        let teamByLocale = byLocale teamTemplates
            userByLocale = byLocale userTemplates
            -- only 'de' and 'en' ship team templates, all locales ship user
            -- templates, so neither list alone covers everything.
            entries =
              concatMap localeEntries . filter wanted . sort . nub $
                map fst teamByLocale <> map fst userByLocale
            localeEntries loc =
              [DumpEntry loc "team" s | ts <- maybeToList (lookup loc teamByLocale), s <- teamSamples ts]
                <> [DumpEntry loc "user" s | ts <- maybeToList (lookup loc userByLocale), s <- userSamples loc ts]
        when (null entries) $
          expectationFailure "no locales selected; check WIRE_EMAIL_DUMP_LOCALES"
        traverse_ (writeEntry dir) entries
        writeText (dir </> "index.html") (renderIndex entries)
        putStrLn $ "\nwrote " <> show (length entries) <> " emails to " <> (dir </> "index.html")

-- | Locales to render, from @WIRE_EMAIL_DUMP_LOCALES@: a comma-separated list
-- of locale codes (as in the template directory names), or @all@. Defaults to
-- the default locale only, since all 20 locales make for an unwieldy index.
localeFilter :: IO (Locale -> Bool)
localeFilter =
  lookupEnv "WIRE_EMAIL_DUMP_LOCALES" <&> \case
    Nothing -> (== defLocale)
    Just "all" -> const True
    Just raw ->
      let selected = map T.strip . T.splitOn "," . T.pack $ raw
       in \loc -> locToText loc `elem` selected

data DumpEntry = DumpEntry
  { entryLocale :: Locale,
    -- | @team@ or @user@, the template set the email comes from.
    entryGroup :: Text,
    entrySample :: EmailSample
  }

-- | Path of this entry's files relative to the dump directory, without
-- extension, e.g. @en/team-member-welcome@.
entryStem :: DumpEntry -> FilePath
entryStem e =
  T.unpack (locToText e.entryLocale)
    </> T.unpack (e.entryGroup <> "-" <> T.map dash (T.pack e.entrySample.sampleName))
  where
    dash c = if c == ' ' then '-' else c

writeEntry :: FilePath -> DumpEntry -> IO ()
writeEntry dir e = do
  let stem = dir </> entryStem e
      mail = e.entrySample.sampleMail
  createDirectoryIfMissing True (dir </> T.unpack (locToText e.entryLocale))
  writeText (stem <> ".subject.txt") (mailSubject mail)
  LBS.writeFile (stem <> ".txt") (mailBody "text/plain" mail)
  LBS.writeFile (stem <> ".html") (mailBody "text/html" mail)

writeText :: FilePath -> Text -> IO ()
writeText path = LBS.writeFile path . LTE.encodeUtf8 . LT.fromStrict

mailSubject :: Mail -> Text
mailSubject mail = fromMaybe "(no subject)" $ lookup "Subject" mail.mailHeaders

-- | The first body part whose content type starts with the given prefix. The
-- renderers all produce exactly one @text/plain@ and one @text/html@ part.
mailBody :: Text -> Mail -> LByteString
mailBody contentType mail =
  fromMaybe "" . listToMaybe $
    [ content
    | alternatives <- mail.mailParts,
      part <- alternatives,
      contentType `T.isPrefixOf` part.partType,
      PartContent content <- [part.partContent]
    ]

-------------------------------------------------------------------------------
-- Index page

renderIndex :: [DumpEntry] -> Text
renderIndex entries =
  T.unlines $
    [ "<!DOCTYPE html>",
      "<html lang=\"en\"><head><meta charset=\"utf-8\">",
      "<title>Wire email templates</title>",
      "<style>",
      "body { font-family: sans-serif; margin: 2rem; background: #f4f4f6; color: #222; }",
      "h1 { margin-bottom: 0; }",
      "h2.locale { margin-top: 3rem; border-bottom: 2px solid #333; }",
      "nav a { margin-right: .75rem; }",
      "section.email { background: #fff; border: 1px solid #ddd; border-radius: 4px;",
      "                margin: 1.5rem 0; padding: 1rem 1.25rem; }",
      "section.email h3 { margin: 0; font-family: monospace; font-size: 1rem; color: #555; }",
      ".subject { font-size: 1.15rem; font-weight: bold; margin: .5rem 0 1rem; }",
      ".errors { color: #b00020; font-weight: bold; }",
      ".panes { display: flex; gap: 1rem; align-items: stretch; }",
      "pre { flex: 1 1 30%; margin: 0; background: #fafafa; border: 1px solid #eee;",
      "      padding: .75rem; white-space: pre-wrap; overflow: auto; font-size: .8rem; }",
      -- iframes isolate each email's own CSS from the index and from each other;
      -- drag the bottom edge to resize when a body does not fit.
      "iframe { flex: 1 1 70%; height: 40rem; border: 1px solid #eee; background: #fff;",
      "         resize: vertical; }",
      "</style></head><body>",
      "<h1>Wire email templates</h1>",
      "<p>" <> tshow (length entries) <> " emails</p>",
      "<nav>" <> foldMap localeLink locales <> "</nav>"
    ]
      <> concatMap localeSection locales
      <> ["</body></html>"]
  where
    locales = nub (map (locToText . entryLocale) entries)
    localeLink loc = "<a href=\"#" <> loc <> "\">" <> loc <> "</a>"
    localeSection loc =
      ("<h2 class=\"locale\" id=\"" <> loc <> "\">" <> loc <> "</h2>")
        : map emailSection (filter ((== loc) . locToText . entryLocale) entries)

emailSection :: DumpEntry -> Text
emailSection e =
  T.unlines
    [ "<section class=\"email\">",
      "<h3>" <> escape (T.pack (entryStem e)) <> "</h3>",
      "<div class=\"subject\">" <> escape (mailSubject e.entrySample.sampleMail) <> "</div>",
      errors,
      "<div class=\"panes\">",
      "<pre>" <> escape (LT.toStrict (LTE.decodeUtf8 (mailBody "text/plain" e.entrySample.sampleMail))) <> "</pre>",
      "<iframe src=\"" <> T.pack (entryStem e) <> ".html\" title=\"html body\"></iframe>",
      "</div>",
      "</section>"
    ]
  where
    errors = case nub e.entrySample.sampleErrors of
      [] -> ""
      errs -> "<p class=\"errors\">unreplaced variables: " <> escape (tshow errs) <> "</p>"

escape :: Text -> Text
escape =
  T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

tshow :: (Show a) => a -> Text
tshow = T.pack . show
