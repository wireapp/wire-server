module Wire.API.Routes.Public.Brig.DomainVerification where

import Data.Aeson.Types qualified as A
import Data.Domain
import Data.Misc
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Imports
import Servant
import Wire.API.EnterpriseLogin
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.User

type DomainVerificationAPI =
  Named
    "verify-dns-record"
    ( Summary "Verify DNS record and save configuration"
        :> "domain-verification"
        :> Capture "domain" Domain
        :> "backend"
        :> ReqBody '[JSON] DomainRegistrationConfig
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 200 "Domain verified")
    )
    :<|> Named
           "get-domain-registration"
           ( Summary "Get domain registration configuration by email"
               :> "get-domain-registration"
               :> ReqBody '[JSON] GetDomainRegistrationRequest
               :> Post '[JSON] GetDomainRegistrationResponse
           )

data DomainRegistrationConfig
  = DomainRegistrationConfigRemove
  | DomainRegistrationConfigBackend HttpsUrl
  | DomainRegistrationConfigNoRegistration
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema DomainRegistrationConfig)

instance ToSchema DomainRegistrationConfig where
  schema :: ValueSchema NamedSwaggerDoc DomainRegistrationConfig
  schema =
    object "DomainRegistrationConfig" $
      field
        "configuration"
        (domainRegistrationConfigToText .= withParser schema domainRegistrationConfigFromText)

domainRegistrationConfigToText :: DomainRegistrationConfig -> Text
domainRegistrationConfigToText = \case
  DomainRegistrationConfigRemove -> "remove"
  (DomainRegistrationConfigBackend backendUrl) -> "backend:" <> httpsUrlToText backendUrl
  DomainRegistrationConfigNoRegistration -> "no-registration"

domainRegistrationConfigFromText :: Text -> A.Parser DomainRegistrationConfig
domainRegistrationConfigFromText "remove" = pure DomainRegistrationConfigRemove
domainRegistrationConfigFromText "no-registration" = pure DomainRegistrationConfigNoRegistration
domainRegistrationConfigFromText s = case Text.breakOn ":" s of
  ("backend", Text.tail -> backend) -> DomainRegistrationConfigBackend <$> either fail pure (httpsUrlFromText backend)
  _ -> fail $ "Invalid domain registration configuration: " <> Text.unpack s

newtype GetDomainRegistrationRequest = GetDomainRegistrationRequest {domainRegistrationRequestEmail :: EmailAddress}
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema GetDomainRegistrationRequest)

instance ToSchema GetDomainRegistrationRequest where
  schema =
    object "GetDomainRegistrationRequest" $
      GetDomainRegistrationRequest
        <$> domainRegistrationRequestEmail
          .= field "email" schema

newtype GetDomainRegistrationResponse = GetDomainRegistrationResponse {domainRegistrationResponseRedirect :: DomainRedirect}
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema GetDomainRegistrationResponse)

instance ToSchema GetDomainRegistrationResponse where
  schema =
    object "GetDomainRegistrationResponse" $
      GetDomainRegistrationResponse
        <$> domainRegistrationResponseRedirect
          .= field "domain-redirect" domainRedirectTextSchema
