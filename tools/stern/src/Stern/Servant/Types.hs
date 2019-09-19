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
import Brig.Types.User
import Data.Aeson
import Data.Id
import Data.Range
import GHC.TypeLits (Symbol)
import Servant.API
import Servant.API.Generic
import Servant.Swagger.UI
import Stern.Types


data API route = API
  { _apiSwaggerDoc
    :: route :- RootPrefix :> NoSwagger :>
       SwaggerSchemaUI "api-docs" "swagger.json"


  , _apiInternalGetStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Verb 'GET 200 '[JSON] NoContent
    -- FUTUREWORK: status204 would be more correct

  , _apiInternalHeadStatus
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "status" :> Verb 'HEAD 200 '[JSON] NoContent
    -- FUTUREWORK: would status204 be more correct here, too?  not sure how 'HEAD works...

  , _apiInternalMonitoring
    :: route :- RootPrefix :> NoSwagger :>
       "i" :> "monitoring" :> Get '[JSON] Value
    -- This is deprecated in favour of /i/metrics via prometheus middleware.


  , _apiSuspendUser
    :: route :- RootPrefix :>
       Summary "Suspends user with this ID" :>
       "users" :> Capture "uid" UserId :> "suspend" :> Post '[JSON] NoContent

  , _apiUnsuspendUser
    :: route :- RootPrefix :>
       Summary "Unsuspends user with this ID" :>
       "users" :> Capture "uid" UserId :> "unsuspend" :> Post '[JSON] NoContent

  , _apiUsersByEmail
    :: route :- RootPrefix :>
       Summary "Displays user's info given an email address" :>
       "users" :> QueryParam "email" Email :> Get '[JSON] [UserAccount]

  , _apiUsersByPhone
    :: route :- RootPrefix :>
       Summary "Displays user's info given a phone number" :>
       "users" :> QueryParam "email" Phone :> Get '[JSON] [UserAccount]

  , _apiUsersByIds
    :: route :- RootPrefix :>
       Summary "Displays active users info given a list of ids" :>
       "users" :> QueryParam "ids" UserIdsQuery :> Get '[JSON] [UserAccount]

  , _apiUsersByHandles
    :: route :- RootPrefix :>
       Summary "Displays active users info given a list of handles" :>
       "users" :> QueryParam "handles" HandlesQuery :> Get '[JSON] [UserAccount]

  , _apiUserConnections
    :: route :- RootPrefix :>
       Summary "Displays user's connections" :>
       "users" :> Capture "uid" UserId :> Get '[JSON] UserConnectionsByStatus

  , _apiUsersConnections
    :: route :- RootPrefix :>
       Summary "Displays a list of user connection statusses given a list of ids" :>
       "users" :> QueryParam  "ids" UserIdsQuery :> Get '[JSON] [ConnectionStatus]

  , _apiUserSearchOnBehalf
    :: route :- RootPrefix :>
       Summary "Search for users on behalf of" :>
       "users" :> Capture  "uid" UserId :> "search" :>
       SwaggerDesc "Search query" (QueryParam "q" Text) :>
       SwaggerDesc "Number of results to return" (QueryParam "size" (Range 1 100 Int32)) :>
       Get '[JSON] [ConnectionStatus]

  , _apiRevokeIdentity
    :: route :- RootPrefix :>
       Summary "Revoke a verified user identity." :>
       Notes "Forcefully revokes a verified user identity. \
             \WARNING: If the given identity is the only verified \
             \user identity of an account, the account will be \
             \deactivated (\"wireless\") and might thus become inaccessible. \
             \If the given identity is not taken / verified, this is a no-op." :>
       "users" :> "revoke-identity" :>
       SwaggerDesc "A verified email address" (QueryParam "email" Email) :>
       SwaggerDesc "A verified phone number (E.164 format)." (QueryParam "phone" Phone) :>
       Post '[JSON] NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Identity revoked or not verified / taken." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiChangeEmail
    :: route :- RootPrefix :>
       Summary "Change a user's email address." :>
       Notes "The new e-mail address must be verified before the change takes effect." :>
       "users" :> Capture "uid" UserId :> "email" :>
       ReqBody '[JSON] EmailUpdate :>
       Put '[JSON] NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Change of email address initiated." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiChangePhone
    :: route :- RootPrefix :>
       Summary "Change a user's phone number." :>
       Notes "The new phone number must be verified before the change takes effect." :>
       "users" :> Capture "uid" UserId :> "phone" :>
       ReqBody '[JSON] PhoneUpdate :>
       Put '[JSON] NoContent
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Change of phone number initiated." Doc.end
       -- Doc.response 400 "Bad request" (Doc.model Doc.errorModel)
       -- @

  , _apiDeleteUser
    :: route :- RootPrefix :>
       Summary "Delete a user (irrevocable!)" :>
       Notes "Email or Phone must match UserId's (to prevent copy/paste mistakes)" :>
       "users" :> Capture "uid" UserId :>
       SwaggerDesc "Matching verified email address"
         (QueryParam "email" Email) :>
       SwaggerDesc "Matching verified phone number (E.164 format)."
         (QueryParam "phone" Phone) :>
       Delete '[JSON] NoContent
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
       SwaggerDesc "An email address to check" (QueryParam "email" Email) :>
       SwaggerDesc "A phone to check" (QueryParam "phone" Phone) :>
       Verb 'HEAD 200 '[JSON] NoContent
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
       SwaggerDesc "An email address to add" (QueryParam "email" Email) :>
       SwaggerDesc "A phone to add" (QueryParam "phone" Phone) :>
       Post '[JSON] NoContent
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
       SwaggerDesc "An email address to remove" (QueryParam "email" Email) :>
       SwaggerDesc "A phone to remove" (QueryParam "phone" Phone) :>
       Delete '[JSON] NoContent
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
       SwaggerDesc "A verified email address" (QueryParam "email" Email) :>
       Get '[JSON] TeamInfo

  , _apiTeamInfo
    :: route :- RootPrefix :>
       Summary "Gets information about a team" :>
       "teams" :> Capture "tid" TeamId :>
       Get '[JSON] TeamInfo


    -- feature flags

  , _apiGetFeatureStatusLegalHold
    :: route :- RootPrefix :>
       Summary "Shows whether legalhold feature is enabled for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "legalhold" :>
       Get '[JSON] SetLegalHoldStatus

  , _apiPutFeatureStatusLegalHold
    :: route :- RootPrefix :>
       Summary "Disable / enable legalhold feature for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "legalhold" :>
       ReqBody '[JSON] SetLegalHoldStatus :>
       Put '[JSON] NoContent

  , _apiGetFeatureStatusSSO
    :: route :- RootPrefix :>
       Summary "Shows whether SSO feature is enabled for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "sso" :>
       Get '[JSON] SetSSOStatus

  , _apiPutFeatureStatusSSO
    :: route :- RootPrefix :>
       Summary "Disable / enable SSO feature for team" :>
       "teams" :> Capture "tid" TeamId :> "features" :> "sso" :>
       ReqBody '[JSON] SetSSOStatus :>
       Put '[JSON] NoContent


    -- Billing & GDPR (may only be relevant internally at Wire)

  , _apiGetTeamInvoice
    :: route :- RootPrefix :>
       Summary "Get a specific invoice by Number" :>
       Notes "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "invoices" :> Capture "inr" InvoiceId :>
       Verb 'GET 307 '[JSON] NoContent
       -- FUTUREWORK: add "Redirect to PDF download" as description to swagger.

  , _apiGetTeamBilling
    :: route :- RootPrefix :>
       Summary "Gets billing information about a team" :>
       Notes "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       Get '[JSON] TeamBillingInfo
       -- FUTUREWORK: describe response:
       -- @
       -- Doc.response 200 "Team Billing Information" Doc.end
       -- Doc.response 404 "No team or no billing info for given team" Doc.end
       -- @

  , _apiPutTeamBilling
    :: route :- RootPrefix :>
       Summary "Updates billing information about a team. Non \
               \specified fields will NOT be updated" :>
       Notes "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       ReqBody '[JSON] TeamBillingInfoUpdate :>
       Put '[JSON] TeamBillingInfo

  , _apiPostTeamBilling
    :: route :- RootPrefix :>
       Summary "Sets billing information about a team. Can \
               \only be used on teams that do NOT have any \
               \billing information set. To update team billing \
               \info, use the update endpoint" :>
       Notes "Relevant only internally at Wire" :>
       "teams" :> Capture "tid" TeamId :> "billing" :>
       ReqBody '[JSON] TeamBillingInfo :>
       Post '[JSON] TeamBillingInfo

  , _apiGetConsentLog
    :: route :- RootPrefix :>
       Summary "Fetch the consent log given an email address of a non-user" :>
       Notes "Relevant only internally at Wire" :>
       "i" :> "consent" :>
       SwaggerDesc "An email address" (QueryParam "email" Email) :>
       Put '[JSON] ConsentLog

  , _apiGetMetaInfo
    :: route :- RootPrefix :>
       Summary "Fetch a user's meta info given a user id: TEMPORARY!" :>
       Notes "Relevant only internally at Wire" :>
       "i" :> "user" :> "meta-info" :>
       QueryParam' '[Strict] "id" UserId :>
       Put '[JSON] UserMetaInfo

  }
  deriving (Generic)


-- | FUTUREWORK: This type will go away as soon as we move to `/` on stern and replace the old
-- implementation.
type RootPrefix = "servant"

data NoSwagger

newtype UserIdsQuery = UserIdsQuery [UserId]
  deriving (Eq, Ord, Show, Generic)

newtype HandlesQuery = HandlesQuery [Handle]
  deriving (Eq, Show, Generic)

-- TODO: move to a module for stuff to be pushed to swagger2.
data SwaggerDesc (notes :: Symbol) (val :: k)
type role SwaggerDesc phantom phantom

-- TODO: move to a module for stuff to be pushed to swagger2.
data Notes (notes :: Symbol)
type role Notes phantom

-- TODO: import Servant verb aliases qualified and make ones here that fit our purposes
-- better.  (@'[JSON]@ implicit, @Head@ with 'NoContent' implicit, something about default
-- status codes?  ...)
