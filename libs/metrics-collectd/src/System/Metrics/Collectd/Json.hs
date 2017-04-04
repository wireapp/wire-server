module System.Metrics.Collectd.Json where

import Data.Aeson
import Data.Scientific
import Data.Sequence hiding (reverse)
import Data.Text (Text)
import System.Metrics.Collectd.Json.Path

import qualified Data.HashMap.Strict as Map

data Selection = Selection
    { values :: Seq ([Text], Scientific)
    , errors :: Seq [Text]
    }

instance Monoid Selection where
    mempty        = Selection empty empty
    a `mappend` b = Selection (values a >< values b) (errors a >< errors b)

select :: Path -> Value -> Selection
select p v = go (path p) [] v
  where
    go [] r x = case fromNumber x of
        Nothing -> mempty { errors = singleton $ reverse r }
        Just  n -> mempty { values = singleton $ (reverse r, n) }

    go (Name n m : ss) r (Object o) =
        maybe (mempty { errors = singleton . reverse $ n:r })
              (go ss (ap n r m))
              (Map.lookup n o)

    go (Star m : ss) r (Object o) =
        foldr (mappend . f) mempty (Map.toList o)
      where
        f (k, j) = go ss (ap k r m) j

    go (s:_) r _ = mempty { errors = singleton . reverse $ toText s : r }

    ap k r modifier = case modifier of
        Just Drop       -> r
        Just (Rename t) -> t : r
        Nothing         -> k : r

fromNumber :: Value -> Maybe Scientific
fromNumber (Number n) = Just n
fromNumber _          = Nothing
