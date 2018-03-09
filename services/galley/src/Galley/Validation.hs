{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Galley.Validation
   ( rangeChecked
   , rangeCheckedMaybe

   , fromConvTeamSize
   , fromMemberSize

   , ConvAndTeamSizeChecked
   , ConvMemberAddSizeChecked

   , checkedConvAndTeamSize
   , checkedMemberAddSize
   ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Data.List1 (list1, List1)
import Data.Range
import Data.String (fromString)
import Data.Word
import Galley.API.Error
import Galley.App
import Galley.Options

rangeChecked :: (MonadThrow monad, Within a n m) => a -> monad (Range n m a)
rangeChecked = either throwErr return . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe :: (MonadThrow monad, Within a n m) => Maybe a -> monad (Maybe (Range n m a))
rangeCheckedMaybe Nothing  = return Nothing
rangeCheckedMaybe (Just a) = Just <$> rangeChecked a
{-# INLINE rangeCheckedMaybe #-}

-- Between 0 and (setMaxConvAndTeamSize - 1)
newtype ConvAndTeamSizeChecked a = ConvAndTeamSizeChecked { fromConvTeamSize :: a }
-- Between 1 and setMaxConvAndTeamSize
newtype ConvMemberAddSizeChecked a = ConvMemberAddSizeChecked { fromMemberSize :: a }

checkedConvAndTeamSize :: (MonadReader Env m, MonadThrow m)
                       => Bounds a => a -> m (ConvAndTeamSizeChecked a)
checkedConvAndTeamSize x = do
    o <- view options
    let minV  = 0
        limit = o^.optSettings.setMaxConvAndTeamSize - 1
    if within x minV (fromIntegral limit)
        then return (ConvAndTeamSizeChecked x)
        else throwErr (errorMsg minV limit "")

checkedMemberAddSize :: [a] -> Galley (ConvMemberAddSizeChecked (List1 a))
checkedMemberAddSize []       = throwErr "List must be of at least size 1"
checkedMemberAddSize l@(x:xs) = do
    o <- view options
    let minV  = 1
        limit = (o^.optSettings.setMaxConvAndTeamSize)
    if within l minV (fromIntegral limit)
        then return (ConvMemberAddSizeChecked $ list1 x xs)
        else throwErr (errorMsg minV limit "")

throwErr :: MonadThrow m => String -> m a
throwErr = throwM . invalidRange . fromString
