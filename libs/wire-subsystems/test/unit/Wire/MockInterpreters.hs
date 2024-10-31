module Wire.MockInterpreters (module MockInterpreters) where

-- Run this from project root to generate the imports:
-- ls libs/wire-subsystems/test/unit/Wire/MockInterpreters | sed 's|\(.*\)\.hs|import Wire.MockInterpreters.\1 as MockInterpreters|'

import Wire.MockInterpreters.BlockListStore as MockInterpreters
import Wire.MockInterpreters.EmailSubsystem as MockInterpreters
import Wire.MockInterpreters.Error as MockInterpreters
import Wire.MockInterpreters.Events as MockInterpreters
import Wire.MockInterpreters.FederationConfigStore as MockInterpreters
import Wire.MockInterpreters.GalleyAPIAccess as MockInterpreters
import Wire.MockInterpreters.HashPassword as MockInterpreters
import Wire.MockInterpreters.IndexedUserStore as MockInterpreters
import Wire.MockInterpreters.Now as MockInterpreters
import Wire.MockInterpreters.PasswordResetCodeStore as MockInterpreters
import Wire.MockInterpreters.PasswordStore as MockInterpreters
import Wire.MockInterpreters.PropertyStore as MockInterpreters
import Wire.MockInterpreters.Random as MockInterpreters
import Wire.MockInterpreters.SessionStore as MockInterpreters
import Wire.MockInterpreters.UserKeyStore as MockInterpreters
import Wire.MockInterpreters.UserStore as MockInterpreters
import Wire.MockInterpreters.UserSubsystem as MockInterpreters
import Wire.MockInterpreters.VerificationCodeStore as MockInterpreters
