{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.UserPendingActivationStore where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Wire.Sem.Paging

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

data UserPendingActivationStore p m a where
  Add :: UserPendingActivation -> UserPendingActivationStore p m ()
  List ::
    Maybe (PagingState p UserPendingActivation) ->
    UserPendingActivationStore p m (Page p UserPendingActivation)
  RemoveMultiple :: [UserId] -> UserPendingActivationStore p m ()

makeSem ''UserPendingActivationStore

remove :: forall p r. (Member (UserPendingActivationStore p) r) => UserId -> Sem r ()
remove uid = removeMultiple [uid]
