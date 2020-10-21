module Util.Invitation
  ( headInvitation404,
    getInvitation,
    getInvitationCode,
    registerInvitation,
  )
where

import Bilge
import Bilge.Assert ((!!!), (<!!), (===))
import Brig.Types.User as Brig
import Control.Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Util
import Wire.API.Team.Invitation (Invitation (..))

headInvitation404 :: BrigReq -> Email -> Http ()
headInvitation404 brig email = do
  Bilge.head (brig . path "/teams/invitations/by-email" . contentJson . queryItem "email" (toByteString' email))
    !!! const 404 === statusCode

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

registerInvitation :: Email -> InvitationCode -> Bool -> TestSpar ()
registerInvitation email inviteeCode shouldSucceed = do
  env <- ask
  let brig = env ^. teBrig
  call $
    void $
      post
        ( brig . path "/register"
            . contentJson
            . json (acceptWithName (Name "Bob") email inviteeCode)
        )
        <!! const (if shouldSucceed then 201 else 400) === statusCode

acceptWithName :: Name -> Email -> InvitationCode -> Aeson.Value
acceptWithName name email code =
  Aeson.object
    [ "name" Aeson..= fromName name,
      "email" Aeson..= fromEmail email,
      "password" Aeson..= defPassword,
      "team_code" Aeson..= code
    ]
