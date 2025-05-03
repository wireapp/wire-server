module Wire.MockInterpreters (module MockInterpreters) where

-- Run this from project root to generate the imports:
-- ls libs/wire-subsystems/test/unit/Wire/MockInterpreters | sed 's|\(.*\)\.hs|import Wire.MockInterpreters.\1 as MockInterpreters|'

import Wire.MockInterpreters.ActivationCodeStore as MockInterpreters
import Wire.MockInterpreters.BlockListStore as MockInterpreters
import Wire.MockInterpreters.CryptoSign as MockInterpreters
import Wire.MockInterpreters.DomainRegistrationStore as MockInterpreters
import Wire.MockInterpreters.DomainVerificationChallengeStore as MockInterpreters
import Wire.MockInterpreters.EmailSending as MockInterpreters
import Wire.MockInterpreters.EmailSubsystem as MockInterpreters
import Wire.MockInterpreters.EnterpriseLoginSubsystem as MockInterpreters
import Wire.MockInterpreters.Error as MockInterpreters
import Wire.MockInterpreters.Events as MockInterpreters
import Wire.MockInterpreters.FederationConfigStore as MockInterpreters
import Wire.MockInterpreters.GalleyAPIAccess as MockInterpreters
import Wire.MockInterpreters.HashPassword as MockInterpreters
import Wire.MockInterpreters.IndexedUserStore as MockInterpreters
import Wire.MockInterpreters.InvitationStore as MockInterpreters
import Wire.MockInterpreters.Now as MockInterpreters
import Wire.MockInterpreters.PasswordResetCodeStore as MockInterpreters
import Wire.MockInterpreters.PasswordStore as MockInterpreters
import Wire.MockInterpreters.PropertyStore as MockInterpreters
import Wire.MockInterpreters.Random as MockInterpreters
import Wire.MockInterpreters.RateLimit as MockInterpreters
import Wire.MockInterpreters.SessionStore as MockInterpreters
import Wire.MockInterpreters.SparAPIAccess as MockInterpreters
import Wire.MockInterpreters.TinyLog as MockInterpreters
import Wire.MockInterpreters.UserGroupStore as MockInterpreters
import Wire.MockInterpreters.UserKeyStore as MockInterpreters
import Wire.MockInterpreters.UserStore as MockInterpreters
import Wire.MockInterpreters.UserSubsystem as MockInterpreters
import Wire.MockInterpreters.VerificationCodeStore as MockInterpreters
