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

module Util.Invitation
  ( headInvitation404,
    getInvitation,
    getInvitationCode,
    registerInvitation,
    acceptWithName,
  )
where

import Bilge
import Bilge.Assert ((!!!), (<!!), (===))
import Control.Lens
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Util
import Wire.API.Team.Invitation (Invitation (..))
import Wire.API.User

headInvitation404 :: (HasCallStack) => BrigReq -> Email -> Http ()
headInvitation404 brig email = do
  Bilge.head (brig . path "/teams/invitations/by-email" . contentJson . queryItem "email" (toByteString' email))
    !!! const 404 === statusCode

getInvitation :: (HasCallStack) => BrigReq -> Email -> Http Invitation
getInvitation brig email =
  responseJsonUnsafe
    <$> Bilge.get
      ( brig
          . path "/i/teams/invitations/by-email"
          . contentJson
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
  pure $ fromByteString (maybe (error "No code?") encodeUtf8 (lbs ^? key "code" . _String))

registerInvitation :: (HasCallStack) => Email -> Name -> InvitationCode -> Bool -> TestSpar ()
registerInvitation email name inviteeCode shouldSucceed = do
  env <- ask
  let brig = env ^. teBrig
  call $
    void $
      post
        ( brig
            . path "/register"
            . contentJson
            . json (acceptWithName name email inviteeCode)
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
