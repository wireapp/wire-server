module API.GalleyInternal where

import Imports
import TestLib.Prelude

putTeamMember :: (HasCallStack, ProducesJSON user, ProducesJSON team) => user -> team -> Int -> App Response
putTeamMember user team perms = do
  uid <- asString user
  tid <- asString team
  req <-
    baseRequest
      Galley
      Unversioned
      ("/i/teams/" <> tid <> "/members")

  submit
    "PUT"
    $ addJSONObject
      [ "member"
          .= object
            [ "user" .= uid,
              "permissions"
                .= object
                  [ "self" .= perms,
                    "copy" .= perms
                  ]
            ]
      ]
      req
