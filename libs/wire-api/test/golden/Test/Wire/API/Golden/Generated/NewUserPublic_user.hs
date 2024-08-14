{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.NewUserPublic_user where

import Data.Id (Id (Id))
import Data.LanguageCodes qualified (ISO639_1 (SO))
import Data.Misc (plainTextPassword8Unsafe)
import Data.Set qualified as Set
import Data.Text.Ascii (AsciiChars (validate))
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, fromRight, undefined)
import Wire.API.Asset
import Wire.API.User
import Wire.API.User.Activation (ActivationCode (ActivationCode, fromActivationCode))
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))

testObject_NewUserPublic_user_1 :: NewUserPublic
testObject_NewUserPublic_user_1 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name {fromName = "\\sY4]u\1033976\DLE\1027259\FS\ETX \US\ETB\1066640dw;}\1073386@\184511\r8"},
          newUserUUID = Nothing,
          newUserIdentity = Just (EmailIdentity (unsafeEmailAddress "some" "example")),
          newUserPict = Nothing,
          newUserAssets =
            [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
              ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
              ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)
            ],
          newUserAccentId = Just (ColourId {fromColourId = 39125}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = fromRight undefined (validate "cfTQLlhl6H6sYloQXsghILggxWoGhM2WGbxjzm0=")
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  (NewTeamMember (InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")}))
              ),
          newUserLabel =
            Just
              ( CookieLabel
                  { cookieLabelText = ">>Mp\2407\148999\&9:\1027133\1097490\DLED1j\1043362\1057440;\1065351\998954f#]"
                  }
              ),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SO, lCountry = Nothing}),
          newUserPassword =
            Just
              ( plainTextPassword8Unsafe
                  "dX\1020562\36210\1011406ht\99919\1002511\a>\CAN\vO95\NAK\n(\169817\1030028\23965f]_\172078\NUL2FQbNS=6g\1048060P\142526\1063467\985597\1071417M\RS7\SYN~\ETBm\1037229\ACK\SOH\vkgmBp\ETBw\24748\169199f\1023790%Q\EOT\140598kP|G\154373\ETBB-\nJWH(8)4$\989238\DEL<7\186902\FSI\bA\DLE\r?\1019914\b\b5\ACK\1009640d \SYN6\1102454G\CAN\b\t=qG\1060976 D\STXvV\tYpg\1016558\21533q\n \ETBL\1056539-\1111371\DC3\1024221F7Q\1090844]\25539i?\r\DLE\ESC{=\1107323_?e\1079481%\SOR\987580\ESC+\SOf\ETBq:g\\Rk\39309\173918[l\NAK\1087232VK\njwp\EOT3TJ\3983Ej\STXR7d83ON\ETBq\29567\EM\190684N8\n\SI\1030588u:G\42235FZ\FS<\NAK\194749\&7\1086892tH\1047800;hbS{\43951\FSsMs\994770\&9B4\1052158\&35c(~CUc\1016298\\V_XD3<L=\v\SUB\58328\SUBuH&\n(\986678\SOH\DLEtn\SOHnU\SO_\NAKt\121457\DLE|\b\133066UK\CAN[w<\1002559\1106328b+r\r%g'v\111126\25924E\1060948\996180D\147569\1065131\12003T@Fv\b%?U\\O]"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire,
          newUserSupportedProtocols = Just (Set.singleton BaseProtocolProteusTag)
        }
    )
