module API.GalleyInternal where

import GHC.Stack
import Testlib.Prelude

putTeamMember :: (HasCallStack, MakesValue user, MakesValue team) => user -> team -> Int -> App Response
putTeamMember user team perms = do
  uid <- objId user
  tid <- asString team
  req <-
    baseRequest
      OwnDomain
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

getTeamFeature :: HasCallStack => String -> String -> App Response
getTeamFeature featureName tid = do
  req <- baseRequest OwnDomain Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
  submit "GET" $ req
