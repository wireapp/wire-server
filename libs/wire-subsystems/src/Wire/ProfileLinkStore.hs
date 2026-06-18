{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Wire.ProfileLinkStore where

import Data.Id
import Data.Misc
import Data.Set qualified as Set
import Data.Time
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User
import Wire.Postgres
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | FUTUREWORK: Merge this into UserStore when Cassadnra is out of picture
data ProfileLinkStore m a where
  UpsertProfileLinks :: UserId -> [UnverifiedLink] -> ProfileLinkStore m ()
  UpdateVerified :: UserId -> UnverifiedLink -> Bool -> ProfileLinkStore m ()
  GetProfileLinks :: UserId -> ProfileLinkStore m [ProfileLink (Maybe UTCTime)]

makeSem ''ProfileLinkStore

interpretProfileLinkStorePostgres :: (PGConstraints r, Member Now r) => InterpreterFor ProfileLinkStore r
interpretProfileLinkStorePostgres = interpret $ \case
  UpsertProfileLinks uid links -> upsertProfileLinksImpl uid links
  UpdateVerified uid link verified -> updateVerifiedImpl uid link verified
  GetProfileLinks uid -> getProfileLinksImpl uid

getProfileLinksImpl :: (PGConstraints r) => UserId -> Sem r [ProfileLink (Maybe UTCTime)]
getProfileLinksImpl uid =
  map (\(name, url, verified) -> ProfileLink {..}) <$> runStatement uid select
  where
    select :: Statement UserId [(LinkName, HttpsUrl, Maybe UTCTime)]
    select =
      dimapPG
        [vectorStatement|
         SELECT name :: text, url :: text, verified_at :: timestamptz?
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
          AND name = ANY($2 :: text[])
        |]

    upsertLinks :: Statement ([UserId], [LinkName], [HttpsUrl]) ()
    upsertLinks =
      lmapPG @(Vector _, Vector _, Vector _)
        [resultlessStatement|
          INSERT INTO profile_links (user_id, name, url)
          SELECT * FROM UNNEST($1 :: uuid[], $2 :: text[], $3 :: text[])
          ON CONFLICT (user_id, name) DO UPDATE
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

updateVerifiedImpl :: (Member Now r, PGConstraints r) => UserId -> UnverifiedLink -> Bool -> Sem r ()
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
          AND name  = $2 :: text
          AND url = $3 :: text
        |]
