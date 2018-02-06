{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Galley.Validation 
       ( rangeChecked
       , rangeCheckedMaybe

       , fromMaxConvTeamSize
       , fromMaxMember

       , MaxConvAndTeamSizeChecked
       , MaxMemberAddSizeChecked

       , toMaxMemberAddSizeChecked

       , checkedConvAndTeamSize
       , checkedMaxMemberAddSize
       , singletonCheckedMember
       ) where

import Control.Lens
import Control.Monad.Catch
import Data.List1 (list1, singleton, List1)
import Data.Range
import Data.String (fromString)
import Galley.App
import Galley.Options
import Network.HTTP.Types
import Network.Wai.Utilities

rangeChecked :: Within a n m => a -> Galley (Range n m a)
rangeChecked = either (throwM . Error status400 "client-error" . fromString) return . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe :: Within a n m => Maybe a -> Galley (Maybe (Range n m a))
rangeCheckedMaybe Nothing  = return Nothing
rangeCheckedMaybe (Just a) = return (checked a)
{-# INLINE rangeCheckedMaybe #-}

-- Between 0 and (setMaxConvAndTeamSize - 1), previously Range 0 127
newtype MaxConvAndTeamSizeChecked a = MaxConvAndTeamSizeChecked { fromMaxConvTeamSize :: a }
-- Between 1 and setMaxConvAndTeamSize, previously Range 1 128
newtype MaxMemberAddSizeChecked a = MaxMemberAddSizeChecked { fromMaxMember :: a }

toMaxMemberAddSizeChecked :: a
                          -> MaxConvAndTeamSizeChecked [a]
                          -> MaxMemberAddSizeChecked (List1 a)
toMaxMemberAddSizeChecked x (MaxConvAndTeamSizeChecked xs) = MaxMemberAddSizeChecked $ list1 x xs

checkedConvAndTeamSize :: Bounds a => a -> Galley (MaxConvAndTeamSizeChecked a)
checkedConvAndTeamSize x = do
    o <- view options
    if within x 0 (fromIntegral (o^.optSettings.setMaxConvAndTeamSize) - 1)
        then return (MaxConvAndTeamSizeChecked x)
        else throwM $ Error status400 "client-error" "wrong size"

checkedMaxMemberAddSize :: [a] -> Galley (MaxMemberAddSizeChecked (List1 a))
checkedMaxMemberAddSize []       = throwM $ Error status400 "client-error" "wrong size"
checkedMaxMemberAddSize l@(x:xs) = do
    o <- view options
    if within l 1 (fromIntegral (o^.optSettings.setMaxConvAndTeamSize))
        then return (MaxMemberAddSizeChecked $ list1 x xs)
        else throwM $ Error status400 "client-error" "wrong size"

singletonCheckedMember :: a -> MaxMemberAddSizeChecked (List1 a)
singletonCheckedMember x = MaxMemberAddSizeChecked $ singleton x
