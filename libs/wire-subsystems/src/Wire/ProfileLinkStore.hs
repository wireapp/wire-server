{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Wire.ProfileLinkStore where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.Handle
import Data.Id
import Data.Misc
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Vector (Vector)
import Debug.Trace
import Hasql.Statement (Statement)
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
import Imports
import Polysemy
import Text.HTML.Scalpel
import URI.ByteString (serializeURIRef)
import Wire.API.PostgresMarshall
import Wire.API.User
import Wire.Postgres
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | FUTUREWORK: Merge this into UserStore when Cassadnra is out of picture
data ProfileLinkStore m a where
  UpsertProfileLinks :: UserId -> [UnverifiedLink] -> ProfileLinkStore m ()
  UpdateVerified :: UserId -> ProfileLink x -> Bool -> ProfileLinkStore m ()
  GetProfileLinks :: UserId -> ProfileLinkStore m [ProfileLink (Maybe UTCTime)]

makeSem ''ProfileLinkStore

-- TODO: Move this to its own module
data ProfileLinkSubsystem m a where
  VerifyLink :: UserId -> Handle -> ProfileLink (Maybe UTCTime) -> ProfileLinkSubsystem m VerifiedLink

makeSem ''ProfileLinkSubsystem

interpretProfileLinkStorePostgres :: (PGConstraints r, Member Now r) => InterpreterFor ProfileLinkStore r
interpretProfileLinkStorePostgres = interpret $ \case
  UpsertProfileLinks uid links -> upsertProfileLinksImpl uid links
  UpdateVerified uid link verified -> updateVerifiedImpl uid link verified
  GetProfileLinks uid -> getProfileLinksImpl uid

interpretProfileLinkSubsystem :: (Member (Embed IO) r, Member ProfileLinkStore r, Member Now r) => InterpreterFor ProfileLinkSubsystem r
interpretProfileLinkSubsystem = interpret $ \case
  VerifyLink uid handle link -> verifyLinkImpl uid handle link

-- | FUTUREWORK: Use global manager
verifyLinkImpl :: (Member (Embed IO) r, Member ProfileLinkStore r, Member Now r) => UserId -> Handle -> ProfileLink (Maybe UTCTime) -> Sem r (ProfileLink Bool)
verifyLinkImpl uid handle link = do
  now <- Now.get
  -- A link is considered verified if it was verified less than 24h ago
  let isAlreadyVerified = maybe False (< 3600 * 24) $ diffUTCTime now <$> link.verified
  traceM $ "isAlreadyVerified: " <> show isAlreadyVerified
  if isAlreadyVerified
    then pure link {verified = True}
    else do
      verificationResult <- liftIO $ verify `Exception.catch` (\(_ :: SomeException) -> pure False)
      traceM $ "verificationResult: " <> show verificationResult
      updateVerified uid link verificationResult
      pure link {verified = verificationResult}
  where
    verify :: IO Bool
    verify = do
      let linkStr = Text.unpack . Text.decodeUtf8 . BS.toStrict . BS.toLazyByteString $ serializeURIRef link.url.httpsUrl
      traceM $ "link: " <> linkStr
      fromMaybe False <$> scrapeURL linkStr scraper

    scraper :: Scraper Text Bool
    scraper = do
      let backlink = ("https://account.wire.com" <> "/@" <> Text.unpack (fromHandle handle))
          linkSelector = "link" @: ["href" @= backlink]
          anchorSelector = "a" @: ["href" @= backlink]
      rels <- (<>) <$> attrs "rel" linkSelector <*> attrs "rel" anchorSelector
      traceM $ "rels: " <> show rels
      pure $ any (\rel -> any (== "me") $ Text.words rel) rels

getProfileLinksImpl :: (PGConstraints r) => UserId -> Sem r [ProfileLink (Maybe UTCTime)]
getProfileLinksImpl uid =
  map (\(name, url, verified) -> ProfileLink {..}) <$> runStatement uid select
  where
    select :: Statement UserId [(LinkName, HttpsUrl, Maybe UTCTime)]
    select =
      dimapPG
        [vectorStatement|
         SELECT link_name :: text, url :: text, verified_at :: timestamptz?
         FROM profile_links
         WHERE uesr_id = $1:: uuid
       |]

upsertProfileLinksImpl :: (PGConstraints r) => UserId -> [UnverifiedLink] -> Sem r ()
upsertProfileLinksImpl uid links =
  runTransaction Serializable Write $ do
    existingLinks <- Set.fromList <$> Transaction.statement uid selectProfileLinks
    let flattenedLinks = Set.fromList $ map (\l -> (l.name, l.url)) links
        deletedLinks = Set.map fst $ Set.difference existingLinks flattenedLinks
    Transaction.statement (uid, deletedLinks) deleteLinks
    Transaction.statement (mkRow flattenedLinks) upsertLinks
  where
    selectProfileLinks :: Statement UserId [(LinkName, HttpsUrl)]
    selectProfileLinks =
      dimapPG
        [vectorStatement|
           SELECT link_name :: text, url :: text
           FROM profile_links
           WHERE user_id = $1 :: uuid
        |]

    deleteLinks :: Statement (UserId, Set LinkName) ()
    deleteLinks =
      lmapPG @(_, Vector _)
        [resultlessStatement|
          DELETE FROM profile_links
          WHERE user_id = $1 :: uuid
          AND link_name = ANY($2 :: text[])
        |]

    upsertLinks :: Statement ([UserId], [LinkName], [HttpsUrl]) ()
    upsertLinks =
      lmapPG @(Vector _, Vector _, Vector _)
        [resultlessStatement|
          INSERT INTO profile_links (user_id, link_name, url)
          SELECT * FROM UNNEST($1 :: uuid[], $2 :: text[], $3 :: text[])
          ON CONFLICT (user_id, link_name) DO UPDATE
            SET url = EXCLUDED.url,
                verified_at = NULL
        |]

    mkRow :: Set (LinkName, HttpsUrl) -> ([UserId], [LinkName], [HttpsUrl])
    mkRow flattenedLinks =
      let flattenedLinksList = Set.toList flattenedLinks
       in ( (replicate (Set.size flattenedLinks) uid),
            (map fst flattenedLinksList),
            (map snd flattenedLinksList)
          )

updateVerifiedImpl :: (Member Now r, PGConstraints r) => UserId -> ProfileLink x -> Bool -> Sem r ()
updateVerifiedImpl uid link isVerfied = do
  verifiedTime <- if isVerfied then Just <$> Now.get else pure Nothing
  runStatement (uid, link.name, link.url, verifiedTime) markVerfied
  where
    markVerfied :: Statement (UserId, LinkName, HttpsUrl, Maybe UTCTime) ()
    markVerfied =
      lmapPG
        [resultlessStatement|
          UPDATE profile_links
          SET verfied_at = $4 :: timestamptz?
          WHERE user_id = $1 :: uuid
          AND link_name  = $2 :: text
          AND url = $3 :: text
        |]
