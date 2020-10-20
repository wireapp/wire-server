module Util.Invitation where

import Bilge
import Brig.Types.User as Brig
import Control.Lens
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Util
import Wire.API.Team.Invitation (Invitation (..))

getInvitation :: BrigReq -> Email -> Http Invitation
getInvitation brig email =
  responseJsonUnsafe
    <$> Bilge.get
      ( brig . path "/teams/invitations/by-email" . contentJson
          . queryItem "email" (toByteString' email)
          . expect2xx
      )

getInvitationCode ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  BrigReq ->
  TeamId ->
  InvitationId ->
  m (Maybe InvitationCode)
getInvitationCode brig t ref = do
  r <-
    get
      ( brig
          . path "/i/teams/invitation-code"
          . queryItem "team" (toByteString' t)
          . queryItem "invitation_id" (toByteString' ref)
      )
  let lbs = fromMaybe "" $ responseBody r
  return $ fromByteString . fromMaybe (error "No code?") $ encodeUtf8 <$> (lbs ^? key "code" . _String)
