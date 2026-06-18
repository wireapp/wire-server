{-# LANGUAGE RecordWildCards #-}

module Wire.MockInterpreters.ProfileLinkStore where

import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Set qualified as Set
import Data.Time
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User
import Wire.ProfileLinkStore
import Wire.Sem.Now as Now

type ProfileLinks = Map (UserId, LinkName) (HttpsUrl, Maybe UTCTime)

runInMemoryProfileLinkStore :: (Member Now r) => InterpreterFor ProfileLinkStore r
runInMemoryProfileLinkStore =
  evalState mempty
    . inMemoryProfileLinkStoreInterpreter
    . raiseUnder

noopProfileLinkSubsystem :: InterpreterFor ProfileLinkSubsystem r
noopProfileLinkSubsystem = interpret $ \case
  VerifyLink _ _ link -> pure $ link {verified = False}

inMemoryProfileLinkStoreInterpreter :: (Member Now r, Member (State ProfileLinks) r) => InterpreterFor ProfileLinkStore r
inMemoryProfileLinkStoreInterpreter = interpret $ \case
  UpsertProfileLinks uid links ->
    modify $ \old ->
      let existingLinks = Set.fromList $ Map.foldMapWithKey (\(_, n) (u, _) -> [(n, u)]) $ Map.filterWithKey (\(u, _) _ -> u == uid) old
          flattenedLinks = Set.fromList $ map (\l -> (l.name, l.url)) links
          deletedLinks = Set.map ((uid,) . fst) $ Set.difference existingLinks flattenedLinks
       in Set.foldr
            ( \(n, u) ->
                Map.insertWith
                  ( \(uNew, _) (uOld, ts) ->
                      if uNew == uOld
                        then (uNew, ts)
                        else (uNew, Nothing)
                  )
                  (uid, n)
                  (u, Nothing)
            )
            (Map.withoutKeys old deletedLinks)
            flattenedLinks
  UpdateVerified uid link verified -> do
    timestamp <- Now.get
    modify $ Map.insert (uid, link.name) (link.url, if verified then Just timestamp else Nothing)
  GetProfileLinks uid -> do
    relevant <- gets (Map.filterWithKey (\(u, _) _ -> u == uid))
    pure $ Map.foldMapWithKey (\(_, name) (url, verified) -> [ProfileLink {..}]) relevant
