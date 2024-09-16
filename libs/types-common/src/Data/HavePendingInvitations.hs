module Data.HavePendingInvitations where

import Imports
import Wire.Arbitrary

data HavePendingInvitations
  = WithPendingInvitations
  | NoPendingInvitations
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via GenericUniform HavePendingInvitations

fromBool :: Bool -> HavePendingInvitations
fromBool True = WithPendingInvitations
fromBool False = NoPendingInvitations
