{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.User.Event where

import Imports
import Brig.Types
import Brig.Types.Intra
import Data.Id

data Event
    = UserEvent       !UserEvent
    | ConnectionEvent !ConnectionEvent
    | PropertyEvent   !PropertyEvent
    | ClientEvent     !ClientEvent

data UserEvent
    = UserCreated !UserAccount
    | UserActivated !UserAccount
        -- ^ A user is activated when the first user identity
        -- (email address or phone number) is verified.
    | UserSuspended !UserId
        -- ^ Account & API access of a user has been suspended.
    | UserResumed !UserId
        -- ^ Account & API access of a previously suspended user
        -- has been restored.
    | UserDeleted !UserId
        -- ^ The user account has been deleted.
    | UserUpdated
        { eupId         :: !UserId
        , eupName       :: !(Maybe Name)
        , eupPict       :: !(Maybe Pict) -- ^ DEPRECATED
        , eupAccentId   :: !(Maybe ColourId)
        , eupAssets     :: !(Maybe [Asset])
        , eupHandle     :: !(Maybe Handle)
        , eupSearchable :: !(Maybe SearchableStatus)
        }
    | UserLocaleUpdated
        { elcId     :: !UserId
        , elcLocale :: !Locale
        }
    | UserIdentityUpdated
        { eiuId    :: !UserId
        , eiuEmail :: !(Maybe Email)
        , eiuPhone :: !(Maybe Phone)
        }
    | UserIdentityRemoved
        { eirId    :: !UserId
        , eirEmail :: !(Maybe Email)
        , eirPhone :: !(Maybe Phone)
        }

data ConnectionEvent
    = ConnectionUpdated
        { ucConn :: !UserConnection
        , ucPrev :: !(Maybe Relation)
        , ucName :: !(Maybe Name)
        }

data PropertyEvent
    = PropertySet !UserId !PropertyKey !PropertyValue
    | PropertyDeleted !UserId !PropertyKey
    | PropertiesCleared !UserId

data ClientEvent
    = ClientAdded !UserId !Client
    | ClientRemoved !UserId !Client

emailRemoved :: UserId -> Email -> UserEvent
emailRemoved u e = UserIdentityRemoved u (Just e) Nothing

phoneRemoved :: UserId -> Phone -> UserEvent
phoneRemoved u p = UserIdentityRemoved u Nothing (Just p)

emailUpdated :: UserId -> Email -> UserEvent
emailUpdated u e = UserIdentityUpdated u (Just e) Nothing

phoneUpdated :: UserId -> Phone -> UserEvent
phoneUpdated u p = UserIdentityUpdated u Nothing (Just p)

handleUpdated :: UserId -> Handle -> UserEvent
handleUpdated u h = (emptyUpdate u) { eupHandle = Just h }

searchableStatusUpdated :: UserId -> SearchableStatus -> UserEvent
searchableStatusUpdated u s = (emptyUpdate u) { eupSearchable = Just s }

profileUpdated :: UserId -> UserUpdate -> UserEvent
profileUpdated u UserUpdate{..} = UserUpdated u uupName uupPict uupAccentId uupAssets Nothing Nothing

emptyUpdate :: UserId -> UserEvent
emptyUpdate u = UserUpdated u Nothing Nothing Nothing Nothing Nothing Nothing

localeUpdate :: UserId -> Locale -> UserEvent
localeUpdate = UserLocaleUpdated

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated{..} = ucFrom ucConn

userEventUserId :: UserEvent -> UserId
userEventUserId (UserCreated u)         = userId (accountUser u)
userEventUserId (UserActivated u)       = userId (accountUser u)
userEventUserId (UserSuspended u)       = u
userEventUserId (UserResumed u)         = u
userEventUserId (UserDeleted u)         = u
userEventUserId UserUpdated{..}         = eupId
userEventUserId UserLocaleUpdated{..}   = elcId
userEventUserId UserIdentityUpdated{..} = eiuId
userEventUserId UserIdentityRemoved{..} = eirId

propEventUserId :: PropertyEvent -> UserId
propEventUserId (PropertySet       u _ _) = u
propEventUserId (PropertyDeleted   u _  ) = u
propEventUserId (PropertiesCleared u    ) = u
