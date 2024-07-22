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
import Imports
import Servant hiding (WithStatus)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.ApplyMods
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MakesFederatedCall
import Wire.API.OAuth
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.Team.Feature
import Wire.API.Team.SearchVisibility (TeamSearchVisibilityView)

type FeatureAPI =
  FeatureStatusGet SSOConfig
    :<|> FeatureStatusGet LegalholdConfig
    :<|> FeatureStatusPut
           '[ MakesFederatedCall 'Galley "on-conversation-updated",
              MakesFederatedCall 'Galley "on-mls-message-sent"
            ]
           '( 'ActionDenied 'RemoveConversationMember,
              '( AuthenticationError,
                 '( 'CannotEnableLegalHoldServiceLargeTeam,
                    '( 'LegalHoldNotEnabled,
                       '( 'LegalHoldDisableUnimplemented,
                          '( 'LegalHoldServiceNotRegistered,
                             '( 'UserLegalHoldIllegalOperation,
                                '( 'LegalHoldCouldNotBlockConnections, '())
                              )
                           )
                        )
                     )
                  )
               )
            )
           LegalholdConfig
    :<|> FeatureStatusGet SearchVisibilityAvailableConfig
    :<|> FeatureStatusPut '[] '() SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedGet "This endpoint is potentially used by the old Android client. It is not used by iOS, team management, or webapp as of June 2022" SearchVisibilityAvailableConfig
    :<|> FeatureStatusDeprecatedPut "This endpoint is potentially used by the old Android client. It is not used by iOS, team management, or webapp as of June 2022" SearchVisibilityAvailableConfig
    :<|> SearchVisibilityGet
    :<|> SearchVisibilitySet
    :<|> FeatureStatusGet ValidateSAMLEmailsConfig
    :<|> FeatureStatusDeprecatedGet "This endpoint is potentially used by the old Android client. It is not used by iOS, team management, or webapp as of June 2022" ValidateSAMLEmailsConfig
    :<|> FeatureStatusGet DigitalSignaturesConfig
    :<|> FeatureStatusDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is potentially used by the old Android client. It is not used by team management, or webapp as of June 2022" DigitalSignaturesConfig
    :<|> FeatureStatusGet AppLockConfig
    :<|> FeatureStatusPut '[] '() AppLockConfig
    :<|> FeatureStatusGet FileSharingConfig
    :<|> FeatureStatusPut '[] '() FileSharingConfig
    :<|> FeatureStatusGet ClassifiedDomainsConfig
    :<|> FeatureStatusGet ConferenceCallingConfig
    :<|> FeatureStatusGet SelfDeletingMessagesConfig
    :<|> FeatureStatusPut '[] '() SelfDeletingMessagesConfig
    :<|> FeatureStatusGet GuestLinksConfig
    :<|> FeatureStatusPut '[] '() GuestLinksConfig
    :<|> FeatureStatusGet SndFactorPasswordChallengeConfig
    :<|> FeatureStatusPut '[] '() SndFactorPasswordChallengeConfig
    :<|> From 'V5 ::> Until 'V6 ::> FeatureStatusGetVersioned 'V5 MLSConfig
    :<|> From 'V5 ::> FeatureStatusPut '[] '() MLSConfig
    :<|> FeatureStatusGet ExposeInvitationURLsToTeamAdminConfig
    :<|> FeatureStatusPut '[] '() ExposeInvitationURLsToTeamAdminConfig
    :<|> FeatureStatusGet SearchVisibilityInboundConfig
    :<|> FeatureStatusPut '[] '() SearchVisibilityInboundConfig
    :<|> FeatureStatusGet OutlookCalIntegrationConfig
    :<|> FeatureStatusPut '[] '() OutlookCalIntegrationConfig
    :<|> From 'V5 ::> FeatureStatusGet MlsE2EIdConfig
    :<|> From 'V5 ::> Until 'V6 ::> Named "put-MlsE2EIdConfig@v5" (ZUser :> FeatureStatusBasePutPublic '() MlsE2EIdConfig)
    :<|> From 'V6 ::> FeatureStatusPut '[] '() MlsE2EIdConfig
    :<|> From 'V5 ::> FeatureStatusGet MlsMigrationConfig
    :<|> From 'V5 ::> FeatureStatusPut '[] '() MlsMigrationConfig
    :<|> From 'V5
      ::> FeatureStatusGetWithDesc
            EnforceFileDownloadLocationConfig
            "<p><b>Custom feature: only supported for some decidated on-prem systems.</b></p>"
    :<|> From 'V5
      ::> FeatureStatusPutWithDesc
            '[]
            '()
            EnforceFileDownloadLocationConfig
            "<p><b>Custom feature: only supported for some decidated on-prem systems.</b></p>"
    :<|> From 'V5 ::> FeatureStatusGet LimitedEventFanoutConfig
    :<|> AllFeatureConfigsUserGet
    :<|> AllFeatureConfigsTeamGet
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" LegalholdConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" SSOConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" SearchVisibilityAvailableConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ValidateSAMLEmailsConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" DigitalSignaturesConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" AppLockConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" FileSharingConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ClassifiedDomainsConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" ConferenceCallingConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is not used by team management, or webapp, and is potentially used by the old Android client as of June 2022" SelfDeletingMessagesConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" GuestLinksConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" SndFactorPasswordChallengeConfig
    :<|> FeatureConfigDeprecatedGet "The usage of this endpoint was removed in iOS in version 3.101. It is used by team management, webapp, and potentially the old Android client as of June 2022" MLSConfig

type FeatureStatusGet f = FeatureStatusGetWithDesc f ""

type FeatureStatusGetVersioned v f = FeatureStatusGetWithDescVersioned v f ""

type FeatureStatusGetWithDesc f desc =
  Named
    '("get", f)
    ( Description desc
        :> (ZUser :> FeatureStatusBaseGet Nothing f)
    )

type FeatureStatusGetWithDescVersioned v f desc =
  Named
    '(AppendSymbol "get@" (VersionSymbol v), f)
    ( Description desc
        :> (ZUser :> FeatureStatusBaseGet (Just v) f)
    )

type FeatureStatusPut segs errs f = FeatureStatusPutWithDesc segs errs f ""

type FeatureStatusPutWithDesc segs errs f desc =
  Named
    '("put", f)
    ( Description desc
        :> (ApplyMods segs (ZUser :> FeatureStatusBasePutPublic errs f))
    )

type FeatureStatusDeprecatedGet d f =
  Named
    '("get-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedGet d f)

type FeatureStatusDeprecatedPut d f =
  Named
    '("put-deprecated", f)
    (ZUser :> FeatureStatusBaseDeprecatedPut d f)

type family FeatureVerb (verb :: StdMethod) featureConfig (mVersion :: Maybe Version)

type instance FeatureVerb verb featureConfig 'Nothing = Verb verb 200 '[Servant.JSON] (WithStatus featureConfig)

type instance FeatureVerb verb featureConfig ('Just v) = Verb verb 200 '[Servant.JSON] (Versioned v (WithStatus featureConfig))

type FeatureStatusBaseGet mVersion featureConfig =
  Summary (AppendSymbol "Get config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> FeatureVerb 'GET featureConfig mVersion

type FeatureStatusBasePutPublic errs featureConfig =
  Summary (AppendSymbol "Put config for " (FeatureSymbol featureConfig))
    :> CanThrow OperationDenied
    :> CanThrow 'NotATeamMember
    :> CanThrow 'TeamNotFound
    :> CanThrow TeamFeatureError
    :> CanThrowMany errs
    :> "teams"
    :> Capture "tid" TeamId
    :> "features"
    :> FeatureSymbol featureConfig
    :> ReqBody '[Servant.JSON] (WithStatusNoLock featureConfig)
    :> Put '[Servant.JSON] (WithStatus featureConfig)

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
      :> Get '[Servant.JSON] (WithStatus featureConfig)
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
    :> ReqBody '[Servant.JSON] (WithStatusNoLock featureConfig)
    :> Put '[Servant.JSON] (WithStatus featureConfig)

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
        :> Get '[Servant.JSON] (WithStatus featureConfig)
    )

type AllFeatureConfigsUserGet =
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
        :> Get '[Servant.JSON] AllFeatureConfigs
    )

type AllFeatureConfigsTeamGet =
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
        :> Get '[JSON] AllFeatureConfigs
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
