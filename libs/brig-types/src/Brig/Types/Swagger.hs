{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Types.Swagger
  ( module Brig.Types.Swagger,
    pendingLoginError,
  )
where

import Data.Misc (modelLocation)
import Data.Swagger.Build.Api (DataType, Model)
import Wire.API.Call.TURN (modelRtcConfiguration, modelRtcIceServer)
import Wire.API.Connection (modelConnection, modelConnectionList, modelConnectionRequest, modelConnectionUpdate)
import Wire.API.Properties (modelPropertyDictionary, modelPropertyValue)
import Wire.API.Swagger (models)
import Wire.API.Team.Invitation (modelTeamInvitation, modelTeamInvitationList, modelTeamInvitationRequest)
import Wire.API.Team.Role (typeRole)
import Wire.API.User (modelSelf, modelUser)
import Wire.API.User (modelEmailUpdate, modelNewUser, modelPhoneUpdate, modelUserUpdate)
import Wire.API.User (modelChangeHandle, modelChangeLocale, modelChangePassword)
import Wire.API.User (modelDelete, modelVerifyDelete)
import Wire.API.User.Activation (modelActivate, modelActivationResponse, modelSendActivationCode)
import Wire.API.User.Auth (modelAccessToken, modelCookie, modelCookieList, modelLogin, modelLoginCodeResponse, modelRemoveCookies, modelSendLoginCode)
import Wire.API.User.Client (modelClient, modelDeleteClient, modelNewClient, modelPubClient, modelSigkeys, modelUpdateClient)
import Wire.API.User.Client.Prekey (modelClientPrekey, modelPrekey, modelPrekeyBundle)
import Wire.API.User.Handle (modelCheckHandles, modelUserHandleInfo)
import Wire.API.User.Password (modelCompletePasswordReset, modelNewPasswordReset)
import Wire.API.User.Profile (modelAsset)
import Wire.API.User.Profile (modelUserDisplayName)
import Wire.API.User.Profile (typeManagedBy)
import Wire.API.User.RichInfo (modelRichField, modelRichInfo)
import Wire.API.User.Search (modelSearchContact, modelSearchResult)
import Wire.Swagger (pendingLoginError)

-- | Actually all models of the whole API,
-- but it doesn't hurt and makes it less likely to forget one.
brigModels :: [Model]
brigModels = Wire.API.Swagger.models

self :: Model
self = modelSelf

user :: Model
user = modelUser

managedBy :: DataType
managedBy = typeManagedBy

asset :: Model
asset = modelAsset

richField :: Model
richField = modelRichField

richInfo :: Model
richInfo = modelRichInfo

userDisplayName :: Model
userDisplayName = modelUserDisplayName

newUser :: Model
newUser = modelNewUser

userUpdate :: Model
userUpdate = modelUserUpdate

emailUpdate :: Model
emailUpdate = modelEmailUpdate

phoneUpdate :: Model
phoneUpdate = modelPhoneUpdate

newPasswordReset :: Model
newPasswordReset = modelNewPasswordReset

completePasswordReset :: Model
completePasswordReset = modelCompletePasswordReset

changePassword :: Model
changePassword = modelChangePassword

changeLocale :: Model
changeLocale = modelChangeLocale

changeHandle :: Model
changeHandle = modelChangeHandle

userHandleInfo :: Model
userHandleInfo = modelUserHandleInfo

checkHandles :: Model
checkHandles = modelCheckHandles

connection :: Model
connection = modelConnection

connectionUpdate :: Model
connectionUpdate = modelConnectionUpdate

connectionRequest :: Model
connectionRequest = modelConnectionRequest

connectionList :: Model
connectionList = modelConnectionList

role :: DataType
role = typeRole

teamInvitationRequest :: Model
teamInvitationRequest = modelTeamInvitationRequest

teamInvitation :: Model
teamInvitation = modelTeamInvitation

teamInvitationList :: Model
teamInvitationList = modelTeamInvitationList

activate :: Model
activate = modelActivate

sendActivationCode :: Model
sendActivationCode = modelSendActivationCode

activationResponse :: Model
activationResponse = modelActivationResponse

delete :: Model
delete = modelDelete

verifyDelete :: Model
verifyDelete = modelVerifyDelete

sendLoginCode :: Model
sendLoginCode = modelSendLoginCode

loginCodeResponse :: Model
loginCodeResponse = modelLoginCodeResponse

login :: Model
login = modelLogin

accessToken :: Model
accessToken = modelAccessToken

removeCookies :: Model
removeCookies = modelRemoveCookies

cookie :: Model
cookie = modelCookie

cookieList :: Model
cookieList = modelCookieList

newClient :: Model
newClient = modelNewClient

updateClient :: Model
updateClient = modelUpdateClient

deleteClient :: Model
deleteClient = modelDeleteClient

client :: Model
client = modelClient

pubClient :: Model
pubClient = modelPubClient

sigkeys :: Model
sigkeys = modelSigkeys

location :: Model
location = modelLocation

prekeyBundle :: Model
prekeyBundle = modelPrekeyBundle

clientPrekey :: Model
clientPrekey = modelClientPrekey

prekey :: Model
prekey = modelPrekey

propertyValue :: Model
propertyValue = modelPropertyValue

propertyDictionary :: Model
propertyDictionary = modelPropertyDictionary

searchResult :: Model
searchResult = modelSearchResult

searchContact :: Model
searchContact = modelSearchContact

rtcConfiguration :: Model
rtcConfiguration = modelRtcConfiguration

rtcIceServer :: Model
rtcIceServer = modelRtcIceServer
