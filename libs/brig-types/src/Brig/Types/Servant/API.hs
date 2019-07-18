{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- FUTUREWORK: move the 'ToSchema' instances to their home modules (where the data types
-- live), and turn warning about orphans back on.

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | Import this qualified.
module Brig.Types.Servant.API where

import Imports

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Activation
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Provider
import Brig.Types.User
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson
import Data.ByteString.Conversion (List(..))
import Data.Currency (Alpha)
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Misc
import Data.Proxy
import Data.Range
import Data.Text.Ascii
import Data.Text as Text (unlines)
import Data.UUID (UUID, fromText)
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import qualified Data.Json.Util
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import Servant.Swagger
import Servant.API.Generic
import Servant.Server.Generic
import URI.ByteString.QQ (uri)

import Brig.Types.Servant.Orphans
import qualified Brig.Types.Servant.API.Internal
import qualified Brig.Types.Servant.API.Users



import Data.String.Conversions
import System.Process (system)
import Data.Aeson (encode)
import Test.Hspec (hspec)
-- import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode swagger
  void $ system "cat /tmp/x | json_pp && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"
  -- hspec $ validateEveryToJSON (Proxy @(ToServantApi API'))

  -- related, but rejected:
  -- https://github.com/swagger-api/validator-badge
  -- https://github.com/navidsh/maven.swagger.validator
  -- https://editor.swagger.io/  (this finds dangling refs.  good.)
  -- https://apidevtools.org/swagger-parser/online/  (also finds dangling refs, but it's *very slow*)



  -- TODO: run https://www.npmjs.com/package/swagger-cli in integration tests.



--
----------------------------------------------------------------------

swagger :: Swagger
swagger = toSwagger api

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

data API route = API
  { _apiI     :: route :- "i"     :> ToServantApi Brig.Types.Servant.API.Internal.API
  , _apiUsers :: route :- "users" :> ToServantApi Brig.Types.Servant.API.Users.API
  }
  deriving (Generic)

-- TODO: read ~/src/wire-server-swaggrify/libs/brig-types/src/Brig/Types/Swagger.hs and see what we've missed.



----------------------------------------------------------------------
-- noise: this is how we'll build servers later.

data API' route = API'
  { _r0002 :: route :- "path" :> Get NoContent
  }
  deriving (Generic)

server' :: API' AsServer
server' = API' { _r0002 = undefined :: Handler NoContent }

app' :: Application
app' = genericServe server'


-- ...  and: specifying exceptions!

data Err = Err Int String String
  deriving (Eq, Show, Generic)

data AsExcepts

instance GenericMode AsExcepts where
  type AsExcepts :- route = [Err]

exceptionSpecs :: API' AsExcepts
exceptionSpecs = API'
  { _r0002 = [Err 303 "forward" "bla...", Err 400 "bad" "blaad..."]
  }

class ToSwaggerExceptions api target where
  toSwaggerExceptions :: Proxy api{- can probably be inferred from the other types? -} -> api AsExcepts -> target

instance ToSwaggerExceptions (Verb method status cts body){- oder doch API'? -} (Response -> Response) where
  toSwaggerExceptions _ errs = description %~ (<> cs (show errs))

-- TODO: read related material:
-- https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html
-- https://hsyl20.fr/home/posts/2018-11-04-trouble-with-typed-errors.html
-- https://gitlab.haskell.org/ghc/ghc/wikis/plugins/type-checker/row-types/coxswain
-- https://github.com/monadfix/servant-inline/blob/master/src/Servant/Inline.hs

-- TODO: perhaps we can write 'throw' functions that check the swagger docs and make sure
-- they're allowed to throw?  we could turn that lookup off for production.

-- TODO: move this upstream to servant-swagger.



{-

data ErrorCase (status :: HttpStatus) verb body
  deriving (Generic)

data HttpStatus = Http200 | Http404
  deriving (Generic)

instance ( verb ~ Verb v s cts
         , HasSwagger (verb body)
         ) => HasSwagger (ErrorCase status verb body) where
  toSwagger _ = toSwagger (Proxy @(verb body))
    & responses %~ (<> Data.HashMap.Strict.InsOrd.fromList [("299", errResp "bla")])


-- swagger ^. paths ==[("/",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Just (Operation {_operationTags = fromList [], _operationSummary = Nothing, _operationDescription = Nothing, _operationExternalDocs = Nothing, _operationOperationId = Nothing, _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/json;charset=utf-8]}), _operationParameters = [], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(204,Inline (Response {_responseDescription = "", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPatch = Nothing, _pathItemParameters = []})]


    -- TODO: now i could restrict 'v' to legal verbs using a closed type family somehow, then
    -- dispatch over 'v' to pick the record field of the PathItem corresponding to 'v', and
    -- add the responses to the 'operation' there.

    -- TODO: it's still a good question whether that's worth the trouble, since the handler
    -- can throw exceptions not matching this spec without getting caught.  the ToServer
    -- instances can just ignore the 'ErrorCase' types here.

 -- OHOHOHOHIKNOW!!  a better idea may be to write a 'FancyVerb' data type to replace 'Verb',
 -- make that contain all the information from artyom's draft, and use that throughout.


errResp :: Text -> Response
errResp desc = mempty & description .~ desc


data Swagger
  = Swagger {_swaggerInfo :: Info,
             _swaggerHost :: Maybe Host,
             _swaggerBasePath :: Maybe FilePath,
             _swaggerSchemes :: Maybe [Scheme],
             _swaggerConsumes :: MimeList,
             _swaggerProduces :: MimeList,
             _swaggerPaths :: InsOrdHashMap FilePath PathItem,
             _swaggerDefinitions :: Definitions Schema,
             _swaggerParameters :: Definitions Swagger2.Param,
             _swaggerResponses :: Definitions Response,
             _swaggerSecurityDefinitions :: Definitions SecurityScheme,
             _swaggerSecurity :: [SecurityRequirement],
             _swaggerTags :: Set Tag,
             _swaggerExternalDocs :: Maybe ExternalDocs}
        -- Defined in ‘Data.Swagger.Internal’


        Doc.response 200 "User exists" Doc.end
        Doc.errorResponse userNotFound

-}
