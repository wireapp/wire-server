-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley.Feature where

import Data.Id
import GHC.TypeLits
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.OAuth
import Wire.API.Routes.Features
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.Team.Feature
import Wire.API.Team.SearchVisibility (TeamSearchVisibilityView)

type FeatureAPIGetPut cfg =
  FeatureAPIGet cfg :<|> FeatureAPIPut cfg

type FeatureAPI =
  FeatureAPIGet SSOConfig
    :<|> FeatureAPIGetPut LegalholdConfig
    :<|> FeatureAPIGetPut SearchVisibilityAvailableConfig
    :<|> SearchVisibilityGet
    :<|> SearchVisibilitySet
    :<|> FeatureAPIGet ValidateSAMLEmailsConfig
    :<|> FeatureAPIGet DigitalSignaturesConfig
    :<|> FeatureAPIGetPut AppLockConfig
    :<|> FeatureAPIGetPut FileSharingConfig
    :<|> FeatureAPIGet ClassifiedDomainsConfig
    :<|> FeatureAPIGetPut ConferenceCallingConfig
    :<|> FeatureAPIGetPut SelfDeletingMessagesConfig
    :<|> FeatureAPIGetPut GuestLinksConfig
    :<|> FeatureAPIGetPut SndFactorPasswordChallengeConfig
    :<|> From 'V5 ::> FeatureAPIGetPut MLSConfig
    :<|> FeatureAPIGetPut ExposeInvitationURLsToTeamAdminConfig
    :<|> FeatureAPIGetPut SearchVisibilityInboundConfig
    :<|> FeatureAPIGetPut OutlookCalIntegrationConfig
    :<|> From 'V5 ::> FeatureAPIGet MlsE2EIdConfig
    :<|> From 'V5 ::> Until 'V6 ::> Named "put-MlsE2EIdConfig@v5" (ZUser :> FeatureStatusBasePutPublic MlsE2EIdConfig)
    :<|> From 'V6 ::> FeatureAPIPut MlsE2EIdConfig
    :<|> From 'V5 ::> FeatureAPIGetPut MlsMigrationConfig
    :<|> From 'V5 ::> FeatureAPIGetPut EnforceFileDownloadLocationConfig
    :<|> From 'V5 ::> FeatureAPIGet LimitedEventFanoutConfig
    :<|> AllTeamFeaturesUserGet
    :<|> AllTeamFeaturesTeamGet
    :<|> DeprecatedFeatureAPI
    :<|> AllDeprecatedFeatureConfigAPI DeprecatedFeatureConfigs
    :<|> FeatureAPIGet DomainRegistrationConfig
    :<|> FeatureAPIGetPut ChannelsConfig

type DeprecationNotice1 = "This endpoint is potentially used by the old Android client. It is not used by iOS, team management, or webapp as of June 2022"

type DeprecationNotice2 = "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022"

type DeprecatedFeatureConfigs =
  [ LegalholdConfig,
    SSOConfig,
    SearchVisibilityAvailableConfig,
    ValidateSAMLEmailsConfig,
    DigitalSignaturesConfig,
    AppLockConfig,
    FileSharingConfig,
    ClassifiedDomainsConfig,
    ConferenceCallingConfig,
    SelfDeletingMessagesConfig,
    GuestLinksConfig,
    SndFactorPasswordChallengeConfig,
    MLSConfig
  ]

type family AllDeprecatedFeatureConfigAPI cfgs where
  AllDeprecatedFeatureConfigAPI '[cfg] = FeatureConfigDeprecatedGet DeprecationNotice2 cfg
  AllDeprecatedFeatureConfigAPI (cfg : cfgs) =
    FeatureConfigDeprecatedGet DeprecationNotice2 cfg
      :<|> AllDeprecatedFeatureConfigAPI cfgs

type DeprecatedFeatureAPI =
  FeatureStatusDeprecatedGet DeprecationNotice1 SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedPut DeprecationNotice1 SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedGet DeprecationNotice1 ValidateSAMLEmailsConfig
    :<|> FeatureStatusDeprecatedGet DeprecationNotice2 DigitalSignaturesConfig

type FeatureAPIGet cfg =
  Named
    '("get", cfg)
    ( Description (FeatureAPIDesc cfg)
        :> (ZUser :> FeatureStatusBaseGet cfg)
    )

type FeatureAPIPut cfg =
  Named
    '("put", cfg)
    ( Description (FeatureAPIDesc cfg)
        :> ZUser
        :> FeatureStatusBasePutPublic cfg
    )

type FeatureStatusDeprecatedGet d f =
  Named
    '("get-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedGet d f)

type FeatureStatusDeprecatedPut d f =
  Named
    '("put-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedPut d f)

type FeatureStatusBaseGet featureConfig =
  Summary (AppendSymbol "Get config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> Get '[Servant.JSON] (LockableFeature featureConfig)

type FeatureStatusBasePutPublic featureConfig =
  Summary (AppendSymbol "Put config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> CanThrowMany (FeatureErrors featureConfig)
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> ReqBody '[Servant.JSON] (Feature featureConfig)
    :> Put '[Servant.JSON] (LockableFeature featureConfig)

-- | A type for a GET endpoint for a feature with a deprecated path
type FeatureStatusBaseDeprecatedGet desc featureConfig =
  ( Summary
      (AppendSymbol "[deprecated] Get config for " (FeatureSymbol featureConfig))
      :> Until 'V2
      :> Description
           ( "Deprecated. Please use `GET /teams/:tid/features/"
               `AppendSymbol` FeatureSymbol featureConfig
               `AppendSymbol` "` instead.\n"
               `AppendSymbol` desc
           )
      :> CanThrow 'NotATeamMember
      :> CanThrow OperationDenied
      :> CanThrow 'TeamNotFound
      :> "teams"
      :> Capture "tid" TeamId
      :> "features"
      :> DeprecatedFeatureName featureConfig
      :> Get '[Servant.JSON] (LockableFeature featureConfig)
  )

-- | A type for a PUT endpoint for a feature with a deprecated path
type FeatureStatusBaseDeprecatedPut desc featureConfig =
  Summary
    (AppendSymbol "[deprecated] Get config for " (FeatureSymbol featureConfig))
    :> Until 'V2
    :> Description
         ( "Deprecated. Please use `PUT /teams/:tid/features/"
             `AppendSymbol` FeatureSymbol featureConfig
             `AppendSymbol` "` instead.\n"
             `AppendSymbol` desc
         )
    :> CanThrow 'NotATeamMember
    :> CanThrow OperationDenied
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> DeprecatedFeatureName featureConfig
    :> ReqBody '[Servant.JSON] (Feature featureConfig)
    :> Put '[Servant.JSON] (LockableFeature featureConfig)

type FeatureConfigDeprecatedGet desc featureConfig =
  Named
    '("get-config", featureConfig)
    ( Summary (AppendSymbol "[deprecated] Get feature config for feature " (FeatureSymbol featureConfig))
        :> Until 'V2
        :> Description ("Deprecated. Please use `GET /feature-configs` instead.\n" `AppendSymbol` desc)
        :> ZUser
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> "feature-configs"
        :> FeatureSymbol featureConfig
        :> Get '[Servant.JSON] (LockableFeature featureConfig)
    )

type AllTeamFeaturesUserGet =
  Named
    "get-all-feature-configs-for-user"
    ( Summary
        "Gets feature configs for a user"
        :> Description
             "Gets feature configs for a user. If the user is a member of a team and has the required permissions, this will return the team's feature configs.\
             \If the user is not a member of a team, this will return the personal feature configs (the server defaults)."
        :> DescriptionOAuthScope 'ReadFeatureConfigs
        :> ZUser
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> "feature-configs"
        :> Get '[Servant.JSON] AllTeamFeatures
    )

type AllTeamFeaturesTeamGet =
  Named
    "get-all-feature-configs-for-team"
    ( Summary "Gets feature configs for a team"
        :> Description "Gets feature configs for a team. User must be a member of the team and have permission to view team features."
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamNotFound
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "features"
        :> Get '[JSON] AllTeamFeatures
    )

type SearchVisibilityGet =
  Named
    "get-search-visibility"
    ( Summary "Shows the value for search visibility"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "search-visibility"
        :> Get '[JSON] TeamSearchVisibilityView
    )

type SearchVisibilitySet =
  Named
    "set-search-visibility"
    ( Summary "Sets the search visibility for the whole team"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'TeamSearchVisibilityNotEnabled
        :> CanThrow 'TeamNotFound
        :> CanThrow TeamFeatureError
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "search-visibility"
        :> ReqBody '[JSON] TeamSearchVisibilityView
        :> MultiVerb 'PUT '[JSON] '[RespondEmpty 204 "Search visibility set"] ()
    )
