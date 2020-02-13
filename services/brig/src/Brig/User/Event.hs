{-# LANGUAGE RecordWildCards #-}

module Brig.User.Event where

import Brig.Types
import Brig.Types.Intra
import Data.Id
import Imports

data Event
  = UserEvent !UserEvent
  | ConnectionEvent !ConnectionEvent
  | PropertyEvent !PropertyEvent
  | ClientEvent !ClientEvent

data UserEvent
  = UserCreated !UserAccount
  | -- | A user is activated when the first user identity (email address or phone number)
    -- is verified. {#RefActivationEvent}
    UserActivated !UserAccount
  | -- | Account & API access of a user has been suspended.
    UserSuspended !UserId
  | -- | Account & API access of a previously suspended user
    -- has been restored.
    UserResumed !UserId
  | -- | The user account has been deleted.
    UserDeleted !UserId
  | UserUpdated
      { eupId :: !UserId,
        eupName :: !(Maybe Name),
        -- | DEPRECATED
        eupPict :: !(Maybe Pict),
        eupAccentId :: !(Maybe ColourId),
        eupAssets :: !(Maybe [Asset]),
        eupHandle :: !(Maybe Handle),
        eupLocale :: !(Maybe Locale),
        eupManagedBy :: !(Maybe ManagedBy),
        eupSearchable :: !(Maybe SearchableStatus)
      }
  | UserIdentityUpdated
      { eiuId :: !UserId,
        eiuEmail :: !(Maybe Email),
        eiuPhone :: !(Maybe Phone)
      }
  | UserIdentityRemoved
      { eirId :: !UserId,
        eirEmail :: !(Maybe Email),
        eirPhone :: !(Maybe Phone)
      }
  | UserLegalHoldDisabled !UserId
  | UserLegalHoldEnabled !UserId
  | LegalHoldClientRequested LegalHoldClientRequestedData

data ConnectionEvent
  = ConnectionUpdated
      { ucConn :: !UserConnection,
        ucPrev :: !(Maybe Relation),
        ucName :: !(Maybe Name)
      }

data PropertyEvent
  = PropertySet !UserId !PropertyKey !PropertyValue
  | PropertyDeleted !UserId !PropertyKey
  | PropertiesCleared !UserId

data ClientEvent
  = ClientAdded !UserId !Client
  | ClientRemoved !UserId !Client

data LegalHoldClientRequestedData
  = LegalHoldClientRequestedData
      { lhcTargetUser :: !UserId,
        lhcLastPrekey :: !LastPrekey,
        lhcClientId :: !ClientId
      }
  deriving stock (Show)

emailRemoved :: UserId -> Email -> UserEvent
emailRemoved u e = UserIdentityRemoved u (Just e) Nothing

phoneRemoved :: UserId -> Phone -> UserEvent
phoneRemoved u p = UserIdentityRemoved u Nothing (Just p)

emailUpdated :: UserId -> Email -> UserEvent
emailUpdated u e = UserIdentityUpdated u (Just e) Nothing

phoneUpdated :: UserId -> Phone -> UserEvent
phoneUpdated u p = UserIdentityUpdated u Nothing (Just p)

handleUpdated :: UserId -> Handle -> UserEvent
handleUpdated u h = (emptyUpdate u) {eupHandle = Just h}

searchableStatusUpdated :: UserId -> SearchableStatus -> UserEvent
searchableStatusUpdated u s = (emptyUpdate u) {eupSearchable = Just s}

localeUpdate :: UserId -> Locale -> UserEvent
localeUpdate u loc = (emptyUpdate u) {eupLocale = Just loc}

managedByUpdate :: UserId -> ManagedBy -> UserEvent
managedByUpdate u mb = (emptyUpdate u) {eupManagedBy = Just mb}

profileUpdated :: UserId -> UserUpdate -> UserEvent
profileUpdated u UserUpdate {..} = UserUpdated u uupName uupPict uupAccentId uupAssets Nothing Nothing Nothing Nothing

emptyUpdate :: UserId -> UserEvent
emptyUpdate u = UserUpdated u Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated {..} = ucFrom ucConn

userEventUserId :: UserEvent -> UserId
userEventUserId (UserCreated u) = userId (accountUser u)
userEventUserId (UserActivated u) = userId (accountUser u)
userEventUserId (UserSuspended u) = u
userEventUserId (UserResumed u) = u
userEventUserId (UserDeleted u) = u
userEventUserId UserUpdated {..} = eupId
userEventUserId UserIdentityUpdated {..} = eiuId
userEventUserId UserIdentityRemoved {..} = eirId
userEventUserId (UserLegalHoldDisabled uid) = uid
userEventUserId (UserLegalHoldEnabled uid) = uid
userEventUserId (LegalHoldClientRequested dat) = lhcTargetUser dat

propEventUserId :: PropertyEvent -> UserId
propEventUserId (PropertySet u _ _) = u
propEventUserId (PropertyDeleted u _) = u
propEventUserId (PropertiesCleared u) = u
