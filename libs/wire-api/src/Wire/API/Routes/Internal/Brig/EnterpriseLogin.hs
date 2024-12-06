-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Routes.Internal.Brig.EnterpriseLogin where

import Data.Domain
import Servant
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Named

--------------------------------------------------------------------------------
-- API Internal

type EnterpriseLoginApi =
  Named
    "domain-registration-lock"
    ( Summary "Adds a domain to the Deny-list"
        :> Description
             "This creates an entry in the email domain registration table with domain-redirect=locked \
             \and team-invites=allowed. Any previous entry for that domain is overwritten."
        :> "domain-registration"
        :> Capture "domain" Domain
        :> "lock"
        :> Post '[JSON] NoContent
    )
    :<|> Named
           "domain-registration-unlock"
           ( Summary "Unlocks a domain"
               :> Description
                    "If the domain-redirect value for that domain is locked, it will be set to none. \
                    \Otherwise this results in and error. \
                    \Does not modify the team-invites value and does not create an entry if it's missing."
               :> "domain-registration"
               :> Capture "domain" Domain
               :> "unlock"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "domain-registration-pre-authorize"
           ( Summary "Pre-authorizes a domain"
               :> Description
                    "If the domain-redirect value for that domain is none, or if there is no entry for that domain, \
                    \this will set the status of the domain-redirect to pre-authorized. \
                    \`team-invitation` is not altered (if the entry is missing, it will be set to allowed). \
                    \This means that the customer claiming this domain has the necessary commercial contract with Wire \
                    \and can continue to register the domain on their own."
               :> "domain-registration"
               :> Capture "domain" Domain
               :> "preauthorize"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "domain-registration-unauthorize"
           ( Summary "Un-authorizes a domain"
               :> Description
                    "If the domain-redirect value for that domain is `pre-authorized`, `backend:{url}` or `no-registration`, \
                    \this will set it to none. Returns an error otherwise. \
                    \Does not modify the `team-invites` value nor creates an entry if it's missing."
               :> "domain-registration"
               :> Capture "domain" Domain
               :> "unauthorize"
               :> Post '[JSON] NoContent
           )
    :<|> Named
           "domain-registration-update"
           ( Summary "Updates a domain"
               :> Description
                    "This creates or updates the entry in the email domain registration table \
                    \for that domain with the given configuration. \
                    \This is the most flexible endpoint, that can set any arbitrary value, to deal with edge cases in the process."
               :> "domain-registration"
               :> Capture "domain" Domain
               :> ReqBody '[JSON] DomainRegistrationUpdate
               :> Put '[JSON] NoContent
           )
    :<|> Named
           "domain-registration-delete"
           ( Summary "Deletes a domain"
               :> Description
                    "This deletes the entry in the domain table, making the domain available to be registered from scratch again. \
                    \This also means that the domain is removed from the deny-list and is not pre-authorized."
               :> "domain-registration"
               :> Capture "domain" Domain
               :> Delete '[JSON] NoContent
           )
    :<|> Named
           "domain-registration-get"
           ( Summary "Returns the current entry in the domain table for that domain"
               :> Description "Returns the current entry in the domain table for that domain, as a JSON document"
               :> "domain-registration"
               :> Capture "domain" Domain
               :> Get '[JSON] DomainRegistration
           )
