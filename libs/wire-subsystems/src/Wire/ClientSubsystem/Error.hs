module Wire.ClientSubsystem.Error where

import Data.Id
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.Error
import Wire.AuthenticationSubsystem.Error
import Wire.Error

data ClientDataError
  = TooManyClients
  | ClientReAuthError !ReAuthError
  | ClientMissingAuth
  | MalformedPrekeys
  | MLSPublicKeyDuplicate
  | MLSNotEnabled
  | KeyPackageDecodingError
  | InvalidKeyPackageRef
  deriving (Show, Eq)

data ClientError
  = ClientNotFound
  | ClientDataError !ClientDataError
  | ClientUserNotFound !UserId
  | ClientLegalHoldCannotBeRemoved
  | ClientLegalHoldCannotBeAdded
  | -- | this error is thrown if legalhold if incompatible with different features
    --   for now, this is the case for MLS and federation
    ClientLegalHoldIncompatible
  | ClientFederationError FederationError
  | ClientCapabilitiesCannotBeRemoved
  | ClientMissingLegalholdConsentOldClients
  | ClientMissingLegalholdConsent
  | ClientCodeAuthenticationFailed
  | ClientCodeAuthenticationRequired
  deriving (Show)

instance Exception ClientError

clientErrorToHttpError :: ClientError -> HttpError
clientErrorToHttpError =
  \case
    ClientNotFound -> StdError $ errorToWai @'E.ClientNotFound
    (ClientDataError e) -> clientDataErrorToHttpError e
    (ClientUserNotFound _) -> StdError $ errorToWai @'E.InvalidUser
    ClientLegalHoldCannotBeRemoved -> StdError $ can'tDeleteLegalHoldClient
    ClientLegalHoldCannotBeAdded -> StdError $ can'tAddLegalHoldClient
    ClientLegalHoldIncompatible -> StdError $ Wai.mkError status409 "mls-legal-hold-not-allowed" "A user who is under legal-hold may not participate in MLS conversations"
    (ClientFederationError e) -> StdError $ federationErrorToWai e
    ClientCapabilitiesCannotBeRemoved -> StdError $ clientCapabilitiesCannotBeRemoved
    ClientMissingLegalholdConsentOldClients -> StdError $ errorToWai @'E.MissingLegalholdConsentOldClients
    ClientMissingLegalholdConsent -> StdError $ errorToWai @'E.MissingLegalholdConsent
    ClientCodeAuthenticationFailed -> StdError $ verificationCodeAuthFailed
    ClientCodeAuthenticationRequired -> StdError $ verificationCodeRequired

clientDataErrorToHttpError :: ClientDataError -> HttpError
clientDataErrorToHttpError = \case
  TooManyClients -> StdError (errorToWai @'E.TooManyClients)
  (ClientReAuthError e) -> reauthError e
  ClientMissingAuth -> StdError (errorToWai @'E.MissingAuth)
  MalformedPrekeys -> StdError (errorToWai @'E.MalformedPrekeys)
  MLSPublicKeyDuplicate -> StdError (errorToWai @'E.MLSDuplicatePublicKey)
  KeyPackageDecodingError -> StdError (errorToWai @'E.KeyPackageDecodingError)
  InvalidKeyPackageRef -> StdError (errorToWai @'E.InvalidKeyPackageRef)
  MLSNotEnabled -> StdError (errorToWai @'E.MLSNotEnabled)

can'tDeleteLegalHoldClient :: Wai.Error
can'tDeleteLegalHoldClient =
  Wai.mkError
    status400
    "client-error"
    "LegalHold clients cannot be deleted. LegalHold must be disabled on this user by an admin"

can'tAddLegalHoldClient :: Wai.Error
can'tAddLegalHoldClient =
  Wai.mkError
    status400
    "client-error"
    "LegalHold clients cannot be added manually. LegalHold must be enabled on this user by an admin"

clientCapabilitiesCannotBeRemoved :: Wai.Error
clientCapabilitiesCannotBeRemoved = Wai.mkError status409 "client-capabilities-cannot-be-removed" "You can only add capabilities to a client, not remove them."
