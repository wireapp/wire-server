module Wire.PostgresMarshall (PostgresMarshall (..), lmapPG) where

import Data.Aeson
import Data.Id
import Data.Profunctor
import Data.UUID

class PostgresMarshall a b where
  postgresMarshall :: a -> b

instance (PostgresMarshall a1 b1, PostgresMarshall a2 b2) => PostgresMarshall (a1, a2) (b1, b2) where
  postgresMarshall (a1, a2) = (postgresMarshall a1, postgresMarshall a2)

instance
  (PostgresMarshall a1 b1, PostgresMarshall a2 b2, PostgresMarshall a3 b3) =>
  PostgresMarshall (a1, a2, a3) (b1, b2, b3)
  where
  postgresMarshall (a1, a2, a3) = (postgresMarshall a1, postgresMarshall a2, postgresMarshall a3)

instance PostgresMarshall (Id a) UUID where
  postgresMarshall = toUUID

instance PostgresMarshall Object Value where
  postgresMarshall = Object

lmapPG :: (PostgresMarshall a b, Profunctor p) => p b x -> p a x
lmapPG = lmap postgresMarshall
