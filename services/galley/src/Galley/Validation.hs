{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Galley.Validation
    ( rangeChecked
    , rangeCheckedMaybe
    ) where

import Control.Monad.Catch
import Data.Range
import Data.String (fromString)
import Galley.App
import Network.HTTP.Types
import Network.Wai.Utilities

rangeChecked :: Within a n m => a -> Galley (Range n m a)
rangeChecked = either (throwM . Error status400 "client-error" . fromString) return . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe :: Within a n m => Maybe a -> Galley (Maybe (Range n m a))
rangeCheckedMaybe Nothing  = return Nothing
rangeCheckedMaybe (Just a) = return (checked a)
{-# INLINE rangeCheckedMaybe #-}
