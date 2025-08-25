module Wire.ConversationSubsystem.Config where

import Data.Range
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Galley qualified as E
import Wire.API.MLS.Keys
import Wire.Error

data ConversationSubsystemConfig = ConversationSubsystemConfig
  { maxConvSize :: Word16,
    mlsKeys :: Maybe (MLSKeysByPurpose MLSPrivateKeys)
  }

data ConversationSubsystemError
  = ConvExceedsMaxSize Word16
  | MLSNotEnabled
  | MLSConvCreatedWithNonEmptyMemberList

conversationSubsystemErrorToHttpError :: ConversationSubsystemError -> HttpError
conversationSubsystemErrorToHttpError =
  StdError . \case
    ConvExceedsMaxSize n -> Wai.mkError status400 "client-error" . fromString $ errorMsg (0 :: Word16) n ""
    MLSNotEnabled -> errorToWai @E.MLSNotEnabled
    MLSConvCreatedWithNonEmptyMemberList -> errorToWai @E.MLSNonEmptyMemberList
