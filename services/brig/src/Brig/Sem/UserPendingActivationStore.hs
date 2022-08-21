{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.UserPendingActivationStore where

import Data.Id
import Data.Time.Clock
import GHC.Exts (Any)
import Imports hiding (Any)
import Polysemy

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

-- | A backend-independent paging mechanism, with immediate results and
-- possibly a 'NextPageToken' which can be used by 'getNext' to load the next
-- page.
data Page m a = Page
  { results :: [a],
    nextPageToken :: Maybe (NextPageToken m a)
  }

type role Page nominal nominal

-- | An interpretation-specific means of performign paging. In an ideal world,
-- paging could just return a @ConduitT () o (Sem r) ()@, but this thing is
-- impossible to implement given the available polysemy tools --- at least,
-- without forcing the entire stream immediately.
--
-- Instead, we're left needing to pump the paging mechanism ourselves, see the
-- 'getNext' action. This would be fine, except that different interpretations
-- are going to require different types of state to implement their paging.
-- A type family would be ideal here, but unfortunately we can't attach type
-- instances to *interpretations*.
--
-- So instead we do a dirty, awful thing where we use 'Any' and just
-- 'Unsafe.Coerce.unsafeCoece' to and from it. I know. I'm sorry.
-- Interpretations are responsible for consistently coercing 'getNextPage'.
--
-- Despite all of this, it's not as horrible as it sounds. Assuming the
-- interpreter is consistent in its usage, we are protected by three things:
--
-- 1. The 'NextPageToken' itself is well-typed with respect to what it can
--    produce. It has nominal roles to ensure users can't break the invariants.
--
-- 2. The @m@ type variable is used for the ST-trick, where we tie it to the
--    same monadic context in which  polysemy is running its effects. Thus we
--    are guaranteed that the only 'NextPageToken' that will typecheck when
--    passed to 'getNext' actually came from a corresponding call to 'list'
--    _within the same interpretation._
--
-- 3. There is a WARNING pragma attached to the data constructor, meaning any
--    user who attempts to build one will get yelled at. Unfortunately, we need
--    to expose a means of constructing this type so interpreter authors can
--    use it, but a WARNING is the next best thing.
--
--    Interpretation authors may use @-fno-warn-deprecations@ to disable this
--    WARNING.
data NextPageToken m a = ExtremelyUnsafeNextPageToken
  { getNextPage :: Any
  }

type role NextPageToken nominal nominal

{-# WARNING
  ExtremelyUnsafeNextPageToken
  "Under NO CIRCUMSTANCES should you construct a NextPageToken in application code. \
    \If you are implementing an interpreter, read the haddocks on NextPageToken for \
    \information on what to do and how to turn off this warning."
  #-}

data UserPendingActivationStore m a where
  Add :: UserPendingActivation -> UserPendingActivationStore m ()
  List :: UserPendingActivationStore m (Page m UserPendingActivation)
  GetNext :: NextPageToken m UserPendingActivation -> UserPendingActivationStore m (Page m UserPendingActivation)
  RemoveMultiple :: [UserId] -> UserPendingActivationStore m ()

makeSem ''UserPendingActivationStore

remove :: Member UserPendingActivationStore r => UserId -> Sem r ()
remove uid = removeMultiple [uid]
