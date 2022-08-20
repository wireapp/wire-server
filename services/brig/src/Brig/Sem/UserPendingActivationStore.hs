{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.UserPendingActivationStore where

import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

data UserPendingActivationStore m a where
  Add :: UserPendingActivation -> UserPendingActivationStore m ()
  List :: UserPendingActivationStore m (Page UserPendingActivation)
  RemoveMultiple :: [UserId] -> UserPendingActivationStore m ()

makeSem ''UserPendingActivationStore

remove :: Member UserPendingActivationStore r => UserId -> Sem r ()
remove uid = removeMultiple [uid]
