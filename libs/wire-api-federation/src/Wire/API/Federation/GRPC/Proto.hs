{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Wire.API.Federation.GRPC.Proto where

import Data.Text as T
import GHC.Generics
import Imports
import Mu.Quasi.GRpc
import Mu.Schema
import Wire.API.Federation.GRPC.Helper

recompileUponProtoChanges

-- FUTUREWORK: Move this to a library which can be used by other components
grpc "TheSchema" id protoFile

data HelloRequestMessage = HelloRequestMessage {name :: T.Text}
  deriving
    ( Eq,
      Show,
      Generic,
      ToSchema TheSchema "HelloRequest",
      FromSchema TheSchema "HelloRequest"
    )

data HelloReplyMessage = HelloReplyMessage {message :: T.Text}
  deriving
    ( Eq,
      Show,
      Generic,
      ToSchema TheSchema "HelloReply",
      FromSchema TheSchema "HelloReply"
    )

data QualifiedHandle = QualifiedHandle {handleDomain :: T.Text, handle :: T.Text}
  deriving
    ( Eq,
      Show,
      Generic,
      ToSchema TheSchema "QualifiedHandle",
      FromSchema TheSchema "QualifiedHandle"
    )

data QualifiedId = QualifiedId {idDomain :: T.Text, id :: T.Text}
  deriving
    ( Eq,
      Show,
      Generic,
      ToSchema TheSchema "QualifiedId",
      FromSchema TheSchema "QualifiedId"
    )

data UserProfile = UserProfile
  { qualifiedId :: Maybe QualifiedId,
    displayName :: T.Text
  }
  deriving
    ( Eq,
      Show,
      Generic,
      ToSchema TheSchema "UserProfile",
      FromSchema TheSchema "UserProfile"
    )
