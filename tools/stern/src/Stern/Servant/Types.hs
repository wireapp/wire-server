{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RoleAnnotations   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-unused-binds #-}

module Stern.Servant.Types where

import Imports

import Brig.Types.Common
import Brig.Types.Intra
import Brig.Types.Search
import Brig.Types.User
import Brig.Types.Servant
import Data.Aeson
import Data.Id
import Data.Range
import Servant.API hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import Servant.API.Generic
import Servant.Swagger.UI
import Stern.Types
import URI.ByteString as URI


data API route = API
  { _apiSwaggerDoc
    :: route :- RootPrefix :> NoSwagger :>
       SwaggerSchemaUI "api-docs" "swagger.json"


  , _apiInternalGetStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Get NoContent
    -- FUTUREWORK: status204 would be more correct

  , _apiInternalHeadStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Head
    -- FUTUREWORK: would status204 be more correct here, too?  not sure how 'HEAD works...

  , _apiInternalMonitoring
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "monitoring" :> Get Value
    -- This is deprecated in favour of /i/metrics via prometheus middleware.


  , _apiSuspendUser
    :: route :- RootPrefix :>
       Summary "Suspend user with this ID" :>
       "users" :> Capture "uid" UserId :> "suspend" :> Post NoContent

  , _apiUnsuspendUser
    :: route :- RootPrefix :>
       Summary "Unsuspend user with this ID" :>
       "users" :> Capture "uid" UserId :> "unsuspend" :> Post NoContent

  , _apiUsersByEmail
    :: route :- RootPrefix :>
       Summary "Display user's info given an email address" :>
       "users" :> "by-email" :> QueryParamStrict "email" Email :> Get [UserAccount]

  , _apiUsersByPhone
    :: route :- RootPrefix :>
       Summary "Display user's info given a phone number" :>
       "users" :> "by-phone" :> QueryParamStrict "phone" Phone :> Get [UserAccount]

  , _apiUsersByIds
    :: route :- RootPrefix :>
       Summary "Display active users' info given a list of ids" :>
       "users" :> "by-ids" :> QueryParamStrict "ids" UserIdsQuery :> Get [UserAccount]

  , _apiUsersByHandles
    :: route :- RootPrefix :>
       Summary "Display active users' info given a list of handles" :>
       "users" :> "by-handles" :> QueryParamStrict "handles" HandlesQuery :> Get [UserAccount]

  , _apiUserConnections
    :: route :- RootPrefix :>
       Summary "Display user's connections" :>
       "users" :> Capture "uid" UserId :> Get UserConnectionsByStatus

  , _apiUsersConnections
    :: route :- RootPrefix :>
       Summary "Display a list of user connection statusses given a list of ids" :>
       "users" :> QueryParamStrict  "ids" UserIdsQuery :> Get [ConnectionStatus]

  , _apiUserSearchOnBehalf
    :: route :- RootPrefix :>
       Summary "Search for users on behalf of" :>
       "users" :> Capture  "uid" UserId :> "search" :>
       SwaggerDesc "Search query" (QueryParamStrict "q" Text) :>
       SwaggerDesc "Number of results to return" (QueryParamOptional "size" (Range 1 100 Int32)) :>
       Get (SearchResult Contact)

  , _apiRevokeIdentity
    :: route :- RootPrefix :>
       Summary "Revoke a verified user identity." :>
       Description "Forcefully revokes a verified user identity. \
             \WARNING: If the given identity is the only verified \
             \user identity of an account, the account will be \
             \deactivated (\"wireless\") and might thus become inaccessible. \
             \If the given identity is not taken / verified, this is a no-op." :>
       "users" :> "revoke-identity" :>
       SwaggerDesc "A verified email address" (QueryParamOptional "email" Email) :>
       SwaggerDesc "A verified phone number (E.164 format)." (QueryParamOptional "phone" Phone) :>
       Post NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Identity revoked or not verified / taken." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiChangeEmail
    :: route :- RootPrefix :>
       Summary "Change a user's email address." :>
       Description "The new e-mail address must be verified before the change takes effect." :>
       "users" :> Capture "uid" UserId :> "email" :>
       ReqBody EmailUpdate :>
       Put NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Change of email address initiated." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiChangePhone
    :: route :- RootPrefix :>
       Summary "Change a user's phone number." :>
       Description "The new phone number must be verified before the change takes effect." :>
       "users" :> Capture "uid" UserId :> "phone" :>
       ReqBody PhoneUpdate :>
       Put NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Change of phone number initiated." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiDeleteUser
    :: route :- RootPrefix :>
       Summary "Delete a user (irrevocable!)" :>
       Description "Email or Phone must match UserId's (to prevent copy/paste mistakes)" :>
       "users" :> Capture "uid" UserId :>
       SwaggerDesc "Matching verified email address"
         (QueryParamOptional "email" Email) :>
       SwaggerDesc "Matching verified phone number (E.164 format)."
         (QueryParamOptional "phone" Phone) :>
       Delete NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Account deleted" Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @
       -- FUTUREWORK: represent in swagger that the two query params are mutually exclusive.
       -- (is that even possible in swagger2?)

  , _apiCheckBlacklistStatus
    :: route :- RootPrefix :>
       Summary "Fetch blacklist information on a email/phone" :>
       "users" :> "blacklist" :>
       SwaggerDesc "An email address to check" (QueryParamOptional "email" Email) :>
       SwaggerDesc "A phone to check" (QueryParamOptional "phone" Phone) :>
       Verb 'HEAD 200 '[JSON] BlackListStatus
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "The email/phone IS blacklisted" Doc.end
       -- Doc.response 404 "The email/phone is NOT blacklisted" Doc.end
       -- @
       -- FUTUREWORK: represent in swagger that the two query params are mutually exclusive.
       -- (is that even possible in swagger2?)

  , _apiBlacklistUser
    :: route :- RootPrefix :>
       Summary "Add the email/phone to our blacklist" :>
       "users" :> "blacklist" :>
       SwaggerDesc "An email address to add" (QueryParamOptional "email" Email) :>
       SwaggerDesc "A phone to add" (QueryParamOptional "phone" Phone) :>
       Post NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Operation succeeded" Doc.end
       -- @
       -- FUTUREWORK: represent in swagger that the two query params are mutually exclusive.
       -- (is that even possible in swagger2?)

  , _apiWhitelistUser
    :: route :- RootPrefix :>
       Summary "Remove the email/phone from our blacklist" :>
       "users" :> "blacklist" :>
       SwaggerDesc "An email address to remove" (QueryParamOptional "email" Email) :>
       SwaggerDesc "A phone to remove" (QueryParamOptional "phone" Phone) :>
       Delete NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Operation succeeded" Doc.end
       -- @
       -- FUTUREWORK: represent in swagger that the two query params are mutually exclusive.
       -- (is that even possible in swagger2?)

  , _apiTeamInfoByEmail
    :: route :- RootPrefix :>
       Summary "Fetch a team information given a member's email" :>
       "teams" :>
       SwaggerDesc "A verified email address" (QueryParamStrict "email" Email) :>
       Get TeamInfo

  , _apiTeamInfo
    :: route :- RootPrefix :>
       Summary "Get information about a team" :>
       "teams" :> Capture "tid" TeamId :>
       Get TeamInfo


    -- feature flags

  , _apiGetFeatureStatusLegalHold
    :: route :- RootPrefix :>
       Summary "Show whether legalhold feature is enabled for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "legalhold" :>
       Get SetLegalHoldStatus

  , _apiPutFeatureStatusLegalHold
    :: route :- RootPrefix :>
       Summary "Disable / enable legalhold feature for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "legalhold" :>
       ReqBody SetLegalHoldStatus :>
       Put NoContent

  , _apiGetFeatureStatusSSO
    :: route :- RootPrefix :>
       Summary "Show whether SSO feature is enabled for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "sso" :>
       Get SetSSOStatus

  , _apiPutFeatureStatusSSO
    :: route :- RootPrefix :>
       Summary "Disable / enable SSO feature for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "sso" :>
       ReqBody SetSSOStatus :>
       Put NoContent


    -- Billing & GDPR (may only be relevant internally at Wire)

  , _apiGetTeamInvoice
    :: route :- RootPrefix :>
       Summary "Get a specific invoice by Number" :>
       Description "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "invoices" :> Capture "inr" InvoiceId :>
       Get URI.URI

  , _apiGetTeamBilling
    :: route :- RootPrefix :>
       Summary "Get billing information about a team" :>
       Description "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       Get TeamBillingInfo
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Team Billing Information" Doc.end
       -- Doc.response 404 "No team or no billing info for given team" Doc.end
       -- @

  , _apiPutTeamBilling
    :: route :- RootPrefix :>
       Summary "Update billing information about a team. Non \
               \specified fields will NOT be updated" :>
       Description "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       ReqBody TeamBillingInfoUpdate :>
       Put TeamBillingInfo

  , _apiPostTeamBilling
    :: route :- RootPrefix :>
       Summary "Set billing information about a team. Can \
               \only be used on teams that do NOT have any \
               \billing information set. To update team billing \
               \info, use the update endpoint" :>
       Description "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       ReqBody TeamBillingInfo :>
       Post TeamBillingInfo

  , _apiGetConsentLog
    :: route :- RootPrefix :>
       Summary "Fetch the consent log given an email address of a non-user" :>
       Description "Relevant only internally at Wire" :>
       "i" :> "consent" :>
       SwaggerDesc "An email address" (QueryParamStrict "email" Email) :>
       Put ConsentLog

  , _apiGetMetaInfo
    :: route :- RootPrefix :>
       Summary "Fetch a user's meta info given a user id: TEMPORARY!" :>
       Description "Relevant only internally at Wire" :>
       "i" :> "user" :> "meta-info" :>
       QueryParamStrict "id" UserId :>
       Put UserMetaInfo

  }
  deriving (Generic)


-- | FUTUREWORK: This type will go away as soon as we move to `/` on stern and replace the old
-- implementation.
type RootPrefix = "servant"

newtype UserIdsQuery = UserIdsQuery [UserId]
  deriving (Eq, Ord, Show, Generic)

newtype HandlesQuery = HandlesQuery [Handle]
  deriving (Eq, Show, Generic)

data BlackListStatus = BlackListed | NotBlackListed
  deriving (Eq, Ord, Bounded, Enum, Generic)

instance ToJSON BlackListStatus where
  toJSON BlackListed = object [ "status" .= String "The given user key IS blacklisted" ]
  toJSON NotBlackListed = object [ "status" .= String "The given user key is NOT blacklisted" ]
