{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Test where

import Data.Id
import SAML2.WebSSO as SAML
import Servant
import Spar.App
import Spar.Data as Data


type IntegrationTests
    = "request"   :> Capture "reqid" (SAML.ID SAML.AuthnRequest) :> Capture "now" SAML.Time :> PostCreated '[JSON] ()
 :<|> "request"   :> Capture "reqid" (SAML.ID SAML.AuthnRequest) :> Get '[JSON] Bool
 :<|> "assertion" :> Capture "assid" (SAML.ID SAML.Assertion) :> Capture "now" SAML.Time :> PostCreated '[JSON] Bool
 :<|> "user"      :> ReqBody '[JSON] SAML.UserRef :> Capture "user-id" UserId :> PostCreated '[JSON] ()
 :<|> "user"      :> ReqBody '[JSON] SAML.UserRef :> Get '[JSON] (Maybe UserId)
 :<|> "verdict"   :> ReqBody '[JSON] SAML.AccessVerdict :> Post '[JSON] ServantErr

integrationTests :: ServerT IntegrationTests Spar
integrationTests
    = (\reqid now -> wrapMonadClientWithEnv $ Data.storeRequest reqid now)
 :<|> wrapMonadClientWithEnv . Data.checkAgainstRequest
 :<|> (\assid endoflife -> wrapMonadClientWithEnv $ Data.storeAssertion assid endoflife)
 :<|> (\uref uid -> wrapMonadClient $ Data.insertUser uref uid)
 :<|> wrapMonadClient . Data.getUser
 :<|> verdictHandler
