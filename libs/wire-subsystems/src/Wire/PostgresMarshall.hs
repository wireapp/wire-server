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
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3
  ) =>
  PostgresMarshall (a1, a2, a3) (b1, b2, b3)
  where
  postgresMarshall (a1, a2, a3) = (postgresMarshall a1, postgresMarshall a2, postgresMarshall a3)

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4
  ) =>
  PostgresMarshall (a1, a2, a3, a4) (b1, b2, b3, b4)
  where
  postgresMarshall (a1, a2, a3, a4) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
  where
  postgresMarshall (a1, a2, a3, a4, a5) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11,
    PostgresMarshall a12 b12
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11,
      postgresMarshall a12
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11,
    PostgresMarshall a12 b12,
    PostgresMarshall a13 b13
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11,
      postgresMarshall a12,
      postgresMarshall a13
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11,
    PostgresMarshall a12 b12,
    PostgresMarshall a13 b13,
    PostgresMarshall a14 b14
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11,
      postgresMarshall a12,
      postgresMarshall a13,
      postgresMarshall a14
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11,
    PostgresMarshall a12 b12,
    PostgresMarshall a13 b13,
    PostgresMarshall a14 b14,
    PostgresMarshall a15 b15
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11,
      postgresMarshall a12,
      postgresMarshall a13,
      postgresMarshall a14,
      postgresMarshall a15
    )

instance
  ( PostgresMarshall a1 b1,
    PostgresMarshall a2 b2,
    PostgresMarshall a3 b3,
    PostgresMarshall a4 b4,
    PostgresMarshall a5 b5,
    PostgresMarshall a6 b6,
    PostgresMarshall a7 b7,
    PostgresMarshall a8 b8,
    PostgresMarshall a9 b9,
    PostgresMarshall a10 b10,
    PostgresMarshall a11 b11,
    PostgresMarshall a12 b12,
    PostgresMarshall a13 b13,
    PostgresMarshall a14 b14,
    PostgresMarshall a15 b15,
    PostgresMarshall a16 b16
  ) =>
  PostgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
  where
  postgresMarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    ( postgresMarshall a1,
      postgresMarshall a2,
      postgresMarshall a3,
      postgresMarshall a4,
      postgresMarshall a5,
      postgresMarshall a6,
      postgresMarshall a7,
      postgresMarshall a8,
      postgresMarshall a9,
      postgresMarshall a10,
      postgresMarshall a11,
      postgresMarshall a12,
      postgresMarshall a13,
      postgresMarshall a14,
      postgresMarshall a15,
      postgresMarshall a16
    )

instance PostgresMarshall (Id a) UUID where
  postgresMarshall = toUUID

instance PostgresMarshall Object Value where
  postgresMarshall = Object

lmapPG :: (PostgresMarshall a b, Profunctor p) => p b x -> p a x
lmapPG = lmap postgresMarshall
