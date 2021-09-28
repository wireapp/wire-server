module Spar.Sem.BrigAccess where

import Brig.Types.Intra
import Brig.Types.User
import Data.Handle (Handle)
import Data.Id (TeamId, UserId)
import Data.Misc (PlainTextPassword)
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Web.Cookie
import Wire.API.User.RichInfo as RichInfo
import Wire.API.User.Scim (ValidExternalId (..))

data BrigAccess m a where
  CreateSAML :: SAML.UserRef -> UserId -> TeamId -> Name -> ManagedBy -> BrigAccess m UserId
  CreateNoSAML :: Email -> TeamId -> Name -> BrigAccess m UserId
  UpdateEmail :: UserId -> Email -> BrigAccess m ()
  GetAccount :: HavePendingInvitations -> UserId -> BrigAccess m (Maybe UserAccount)
  GetByHandle :: Handle -> BrigAccess m (Maybe UserAccount)
  GetByEmail :: Email -> BrigAccess m (Maybe UserAccount)
  SetName :: UserId -> Name -> BrigAccess m ()
  SetHandle :: UserId -> Handle {- not 'HandleUpdate'! -} -> BrigAccess m ()
  SetManagedBy :: UserId -> ManagedBy -> BrigAccess m ()
  SetVeid :: UserId -> ValidExternalId -> BrigAccess m ()
  SetRichInfo :: UserId -> RichInfo -> BrigAccess m ()
  GetRichInfo :: UserId -> BrigAccess m RichInfo
  CheckHandleAvailable :: Handle -> BrigAccess m Bool
  Delete :: UserId -> BrigAccess m ()
  EnsureReAuthorised :: Maybe UserId -> Maybe PlainTextPassword -> BrigAccess m ()
  SsoLogin :: UserId -> BrigAccess m SetCookie
  GetStatus :: UserId -> BrigAccess m AccountStatus
  GetStatusMaybe :: UserId -> BrigAccess m (Maybe AccountStatus)
  SetStatus :: UserId -> AccountStatus -> BrigAccess m ()

makeSem ''BrigAccess
