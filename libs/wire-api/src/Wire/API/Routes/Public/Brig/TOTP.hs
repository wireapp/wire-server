-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Managing TOTP (RFC 6238) second factors for the calling user.
module Wire.API.Routes.Public.Brig.TOTP where

import Data.Id as Id
import GHC.TypeLits (AppendSymbol)
import Network.Wai.Utilities.Headers (CacheControl)
import Servant (JSON)
import Servant hiding (Handler, JSON, Tagged, addHeader, respond)
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named (Named)
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.API.User.Auth.TOTP

-- | Prepended to every route description for as long as the handlers are the
-- mock from @Brig.API.TOTP.Mock@.  This is the only text that disappears when
-- the real handlers land.
type MockWarning =
  "**This endpoint is currently a mock.**  The request body is parsed (so a \
  \wrong shape is reported immediately) and then ignored; the response is \
  \randomly generated and nothing is stored.  Concretely: `GET /self/totp` \
  \does not list what you just enrolled, activating the same credential twice \
  \succeeds twice, no password or code is checked, and no error from the list \
  \below is ever actually returned.  Only the `uri` field is consistent with \
  \its own response (see below), because that is the one thing clients build \
  \on.\n\n"

type TOTPCredentialCapture =
  Capture' '[Description "The credential id returned by `POST /self/totp`"] "id" TOTPCredentialId

type TOTPAPI =
  Named
    "totp-enrol"
    ( Summary "Begin enrolling an authenticator app (TOTP, RFC 6238)"
        :> Description
             ( MockWarning
                 `AppendSymbol` "Enrolment is **two steps**.  This is the first: the server \
                                \generates a shared secret, hands it out exactly once, and \
                                \stores the credential in a *pending* state.  A pending \
                                \credential cannot be used as a second factor.  The second \
                                \step is `POST /self/totp/{id}/activate`, where the client \
                                \proves the authenticator app really received the secret by \
                                \sending a code it produced.\n\n\
                                \The response contains both an `otpauth://` URI and the raw \
                                \base32 `secret`; they carry the same secret, so a client can \
                                \use whichever it prefers.  **The client renders the QR \
                                \code** -- the server returns a URI, never an image.  The URI \
                                \is\n\n\
                                \`otpauth://totp/{issuer}:{label}?secret={secret}&issuer={issuer}&algorithm={algorithm}&digits={digits}&period={period}`\n\n\
                                \with `label` percent-encoded, `secret` the unpadded base32 \
                                \secret, `algorithm` the upper-case spelling (`SHA1`, \
                                \`SHA256`, `SHA512`) of the `algorithm` field -- note the JSON \
                                \field is lower case and the URI parameter is upper case -- \
                                \and `digits`/`period` the matching fields.\n\n\
                                \The `secret` is returned **exactly once**.  `GET /self/totp` \
                                \returns metadata only and will never return it again; a \
                                \client that loses it has to start a new enrolment.\n\n\
                                \An enrolment that is never activated **expires** (see \
                                \`expires_at`); activating after that fails with \
                                \`totp-enrolment-expired` and the whole flow starts over.\n\n\
                                \`client_time` is the client's idea of \"now\".  The server \
                                \compares it to its own clock and rejects a device that is too \
                                \far off with `totp-client-clock-skew`, because a mis-set \
                                \clock otherwise shows up much later as a mysterious \
                                \\"invalid code\" at login.  Send it from day one.  The \
                                \response also echoes `server_time` so a client can diagnose \
                                \drift without waiting to be rejected.\n\n\
                                \The response carries `Cache-Control: no-store`: it contains a \
                                \secret and must not end up in any cache.\n\n\
                                \This endpoint requires re-authentication.  The request body \
                                \contains the password (and, once the user already has an \
                                \activated authenticator, a second factor) alongside the \
                                \enrolment fields -- they are all at the top level of the \
                                \same JSON object, not nested under a separate key."
             )
        :> From 'V19
        :> CanThrow 'BadCredentials
        :> CanThrow 'TOTPRequired
        :> CanThrow 'TOTPInvalidCode
        :> CanThrow 'TOTPCodeReused
        :> CanThrow 'TOTPClientClockSkew
        :> CanThrow 'TOTPTooManyCredentials
        :> CanThrow 'TOTPDisabled
        :> CanThrow 'RateLimitExceeded
        :> ZLocalUser
        :> "self"
        :> "totp"
        :> ReqBody '[JSON] NewTOTPCredential
        :> MultiVerb1
             'POST
             '[JSON]
             ( WithHeaders
                 '[Header "Cache-Control" CacheControl]
                 (TOTPEnrolment, CacheControl)
                 (Respond 201 "Enrolment started; the secret is in the body" TOTPEnrolment)
             )
    )
    :<|> Named
           "totp-activate"
           ( Summary "Finish enrolling an authenticator app"
               :> Description
                    ( MockWarning
                        `AppendSymbol` "The second step of enrolment.  `code` is a code read off the \
                                       \authenticator app that scanned the QR code from `POST \
                                       \/self/totp`; sending it proves the app really has the \
                                       \secret.  Only after this does the credential count as a \
                                       \second factor.\n\n\
                                       \The response contains the user's **recovery codes**.  They \
                                       \stand in for an authenticator the user no longer has, each \
                                       \works once, and they are shown **once**: there is no \
                                       \endpoint that returns them again.  Clients should make the \
                                       \user save or print them before leaving the screen.\n\n\
                                       \If the enrolment has expired in the meantime the answer is \
                                       \`totp-enrolment-expired` and the flow starts over at `POST \
                                       \/self/totp`.  `client_time` means the same thing as it does \
                                       \there."
                    )
               :> From 'V19
               :> CanThrow 'TOTPNotFound
               :> CanThrow 'TOTPAlreadyActivated
               :> CanThrow 'TOTPEnrolmentExpired
               :> CanThrow 'TOTPInvalidCode
               :> CanThrow 'TOTPClientClockSkew
               :> CanThrow 'TOTPDisabled
               :> CanThrow 'RateLimitExceeded
               :> ZLocalUser
               :> "self"
               :> "totp"
               :> TOTPCredentialCapture
               :> "activate"
               :> ReqBody '[JSON] ActivateTOTPCredential
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (Respond 200 "Credential activated; recovery codes in the body" TOTPRecoveryCodes)
           )
    :<|> Named
           "totp-list"
           ( Summary "List the calling user's authenticator apps"
               :> Description
                    ( MockWarning
                        `AppendSymbol` "Metadata only.  The shared secret and the `otpauth://` URI are \
                                       \returned by `POST /self/totp` and never again, so they do not \
                                       \appear here.\n\n\
                                       \A credential with `activated_at: null` is a pending enrolment: \
                                       \it is not a second factor yet and it will expire."
                    )
               :> From 'V19
               :> CanThrow 'TOTPDisabled
               :> ZLocalUser
               :> "self"
               :> "totp"
               :> MultiVerb1
                    'GET
                    '[JSON]
                    (Respond 200 "The calling user's TOTP credentials" [TOTPCredentialInfo])
           )
    :<|> Named
           "totp-rename"
           ( Summary "Rename an authenticator app"
               :> Description
                    ( MockWarning
                        `AppendSymbol` "Changes only the label shown in `GET /self/totp`.  It does not \
                                       \touch the secret, so codes keep working.  Works on pending \
                                       \credentials as well as activated ones."
                    )
               :> From 'V19
               :> CanThrow 'TOTPNotFound
               :> CanThrow 'TOTPDisabled
               :> ZLocalUser
               :> "self"
               :> "totp"
               :> TOTPCredentialCapture
               :> ReqBody '[JSON] TOTPCredentialName
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Credential renamed")
           )
    :<|> Named
           "totp-delete"
           ( Summary "Remove an authenticator app"
               :> Description
                    ( MockWarning
                        `AppendSymbol` "Removing a second factor is itself a sensitive operation, so it \
                                       \takes a re-authentication body: the password, plus a second \
                                       \factor if the user has one.  Either a code from a (possibly \
                                       \different) authenticator app or a recovery code is accepted \
                                       \in `totp_code` -- a user whose only authenticator is the one \
                                       \they are trying to remove would otherwise be stuck.\n\n\
                                       \Removing the last activated credential turns the second \
                                       \factor off for this account."
                    )
               :> From 'V19
               :> CanThrow 'BadCredentials
               :> CanThrow 'TOTPNotFound
               :> CanThrow 'TOTPRequired
               :> CanThrow 'TOTPInvalidCode
               :> CanThrow 'TOTPCodeReused
               :> CanThrow 'TOTPDisabled
               :> CanThrow 'RateLimitExceeded
               :> ZLocalUser
               :> "self"
               :> "totp"
               :> TOTPCredentialCapture
               :> ReqBody '[JSON] ReauthBody
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 200 "Credential removed")
           )
