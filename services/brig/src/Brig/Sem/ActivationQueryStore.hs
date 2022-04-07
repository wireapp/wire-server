-- |
module Brig.Sem.ActivationQueryStore where

import Cassandra.CQL (Ascii)
import Data.Id
import Imports
import Polysemy
import Wire.API.User.Activation

data ActivationQueryStore m a where
  KeyInsert ::
    ActivationKey ->
    Text ->
    Text ->
    ActivationCode ->
    Maybe UserId ->
    Int32 ->
    Int32 ->
    ActivationQueryStore m ()
  -- FUTUREWORK: make the return type of KeySelect into a record
  KeySelect ::
    ActivationKey ->
    ActivationQueryStore m (Maybe (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32))
  CodeSelect :: ActivationKey -> ActivationQueryStore m (Maybe (Maybe UserId, ActivationCode))
  KeyDelete :: ActivationKey -> ActivationQueryStore m ()

makeSem ''ActivationQueryStore
