module Data.HavePendingInvitations where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary

data HavePendingInvitations
  = WithPendingInvitations
  | NoPendingInvitations
  deriving (Eq, Show, Ord, Generic)
  deriving (Arbitrary) via GenericUniform HavePendingInvitations
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema HavePendingInvitations

instance ToSchema HavePendingInvitations where
  schema = enum @Bool "HavePendingInvitations" $ mconcat [element True WithPendingInvitations, element False NoPendingInvitations]

fromBool :: Bool -> HavePendingInvitations
fromBool True = WithPendingInvitations
fromBool False = NoPendingInvitations
