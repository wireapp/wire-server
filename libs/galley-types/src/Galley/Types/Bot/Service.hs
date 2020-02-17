{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Galley.Types.Bot.Service
  ( ServiceToken (..),
    ServiceRef,
    newServiceRef,
    serviceRefId,
    serviceRefProvider,
    Service,
    newService,
    serviceRef,
    serviceUrl,
    serviceToken,
    serviceFingerprints,
    serviceEnabled,
  )
where

import Galley.Types.Bot.Service.Internal
