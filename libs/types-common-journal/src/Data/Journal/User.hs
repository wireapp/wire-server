{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Journal.User
    ( -- * Types
      Action   (..)
    , UserEvent

      -- * Constructors
    , new

      -- * Lenses
    , action
    , userId
    , colourId
    , name
    , email
    , phoneNr
    , locale
    , timestamp
    , correlationId
    , handle
    )
where

import Data.Id
import Data.Int
import Data.Journal.Internal
import Data.Journal.Types
import Data.Monoid
import Data.Orphans          ()
import Data.ProtocolBuffers
import Data.Text             (Text)
import Data.Time.Clock
import Data.Word
import GHC.Generics

import Prelude


data Action
    = Create
    | Update
    | Suspend
    | Resume
    | Remove
    | Activate
    | Delete
    deriving (Ord, Eq, Show, Generic)

instance Encode Action
instance Decode Action

instance Enum Action where
    toEnum 1 = Create
    toEnum 2 = Update
    toEnum 3 = Suspend
    toEnum 4 = Resume
    toEnum 5 = Remove
    toEnum 6 = Activate
    toEnum 7 = Delete
    toEnum x = error $ "Data.Journal.User.Action.toEnum: outside bounds: " ++ show x

    fromEnum Create   = 1
    fromEnum Update   = 2
    fromEnum Suspend  = 3
    fromEnum Resume   = 4
    fromEnum Remove   = 5
    fromEnum Activate = 6
    fromEnum Delete   = 7

instance Bounded Action where
    minBound = Create
    maxBound = Delete


data UserEvent = UserEvent
    { _action        :: Required 1 (Enumeration Action)
    , _userId        :: Required 2 (Value UserId)
    , _colourId      :: Optional 3 (Value Int32)
    , _name          :: Optional 4 (Value Text)
    , _email         :: Optional 5 (Value Text)
    , _phoneNr       :: Optional 6 (Value Text)
    , _locale        :: Optional 7 (Value Text)
    , _timestamp     :: Optional 8 (Value Word64)
    , _correlationId :: Optional 9 (Value (CorrelationId RequestId))
    , _handle        :: Optional 10 (Value Text)
    } deriving (Eq, Show, Generic)

instance Encode UserEvent
instance Decode UserEvent

new :: Action -> UserId -> UserEvent
new a u = UserEvent
    { _action        = putField a
    , _userId        = putField u
    , _colourId      = mempty
    , _name          = mempty
    , _email         = mempty
    , _phoneNr       = mempty
    , _locale        = mempty
    , _timestamp     = mempty
    , _correlationId = mempty
    , _handle        = mempty
    }

action :: Functor f => (Action -> f Action) -> UserEvent -> f UserEvent
action = flens _action (\s a -> s { _action = a })

userId :: Functor f => (UserId -> f UserId) -> UserEvent -> f UserEvent
userId = flens _userId (\s a -> s { _userId = a })

colourId :: Functor f => (Maybe Int32 -> f (Maybe Int32)) -> UserEvent -> f UserEvent
colourId = flens _colourId (\s a -> s { _colourId = a })

name :: Functor f => (Maybe Text -> f (Maybe Text)) -> UserEvent -> f UserEvent
name = flens _name (\s a -> s { _name = a })

email :: Functor f => (Maybe Text -> f (Maybe Text)) -> UserEvent -> f UserEvent
email = flens _email (\s a -> s { _email = a })

phoneNr :: Functor f => (Maybe Text -> f (Maybe Text)) -> UserEvent -> f UserEvent
phoneNr = flens _phoneNr (\s a -> s { _phoneNr = a })

locale :: Functor f => (Maybe Text -> f (Maybe Text)) -> UserEvent -> f UserEvent
locale = flens _locale (\s a -> s { _locale = a })

timestamp :: Functor f => (Maybe UTCTime -> f (Maybe UTCTime)) -> UserEvent -> f UserEvent
timestamp f =
    flens _timestamp
          (\s a -> s { _timestamp = a })
          (fmap (fmap toTimestamp) . f . fmap fromTimestamp)

correlationId :: Functor f => (Maybe (CorrelationId RequestId)-> f (Maybe (CorrelationId RequestId))) -> UserEvent -> f UserEvent
correlationId = flens _correlationId (\s a -> s { _correlationId = a })

handle :: Functor f => (Maybe Text -> f (Maybe Text)) -> UserEvent -> f UserEvent
handle = flens _handle (\s a -> s { _handle = a })
