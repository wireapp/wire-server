module Wire.API.Routes.Internal.Cannon where

import Control.Lens ((.~))
import Data.Id
import Data.OpenApi (HasInfo (info), HasTitle (title), OpenApi)
import Imports
import Servant
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Wire.API.Error
import Wire.API.Error.Cannon
import Wire.API.Internal.BulkPush
import Wire.API.RawJson
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named

type API =
  "i"
    :> ( Named
           "get-status"
           ( "status"
               :> MultiVerb
                    'GET
                    '[PlainText]
                    '[RespondEmpty 200 "Service is alive."]
                    ()
           )
           :<|> Named
                  "push-notification"
                  ( "push"
                      :> Capture "user" UserId
                      :> Capture "conn" ConnId
                      :> ReqBody '[JSON] RawJson
                      :> MultiVerb
                           'POST
                           '[JSON]
                           '[ ErrorResponse 'ClientGone,
                              RespondEmpty 200 "Successfully pushed."
                            ]
                           (Maybe ())
                  )
           :<|> Named
                  "bulk-push-notifications"
                  ( "bulkpush"
                      :> ReqBody '[JSON] BulkPushRequest
                      :> Post '[JSON] BulkPushResponse
                  )
           :<|> Named
                  "check-presence"
                  ( "presences"
                      :> Capture "uid" UserId
                      :> Capture "conn" ConnId
                      :> MultiVerb
                           'HEAD
                           '[JSON]
                           '[ ErrorResponse 'PresenceNotRegistered,
                              RespondEmpty 200 "Presence checked successfully."
                            ]
                           (Maybe ())
                  )
       )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @API)
    & info . title .~ "Wire-Server internal cannon API"
