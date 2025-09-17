module Wire.API.PostgresMarshall
  ( PostgresMarshall (..),
    PostgresUnmarshall (..),
    lmapPG,
    rmapPG,
    dimapPG,
  )
where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Id
import Data.Profunctor
import Data.Text.Encoding qualified as Text
import Data.UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Statement
import Imports

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

---

class PostgresUnmarshall a b where
  postgresUnmarshall :: a -> Either Text b

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2) => PostgresUnmarshall (a1, a2) (b1, b2) where
  postgresUnmarshall (a1, a2) =
    (,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3) => PostgresUnmarshall (a1, a2, a3) (b1, b2, b3) where
  postgresUnmarshall (a1, a2, a3) =
    (,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4) => PostgresUnmarshall (a1, a2, a3, a4) (b1, b2, b3, b4) where
  postgresUnmarshall (a1, a2, a3, a4) =
    (,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5) => PostgresUnmarshall (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) where
  postgresUnmarshall (a1, a2, a3, a4, a5) =
    (,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6) =
    (,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7) =
    (,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8) =
    (,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    (,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =
    (,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =
    (,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11, PostgresUnmarshall a12 b12) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =
    (,,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11
      <*> postgresUnmarshall a12

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11, PostgresUnmarshall a12 b12, PostgresUnmarshall a13 b13) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =
    (,,,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11
      <*> postgresUnmarshall a12
      <*> postgresUnmarshall a13

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11, PostgresUnmarshall a12 b12, PostgresUnmarshall a13 b13, PostgresUnmarshall a14 b14) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =
    (,,,,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11
      <*> postgresUnmarshall a12
      <*> postgresUnmarshall a13
      <*> postgresUnmarshall a14

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11, PostgresUnmarshall a12 b12, PostgresUnmarshall a13 b13, PostgresUnmarshall a14 b14, PostgresUnmarshall a15 b15) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) =
    (,,,,,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11
      <*> postgresUnmarshall a12
      <*> postgresUnmarshall a13
      <*> postgresUnmarshall a14
      <*> postgresUnmarshall a15

instance (PostgresUnmarshall a1 b1, PostgresUnmarshall a2 b2, PostgresUnmarshall a3 b3, PostgresUnmarshall a4 b4, PostgresUnmarshall a5 b5, PostgresUnmarshall a6 b6, PostgresUnmarshall a7 b7, PostgresUnmarshall a8 b8, PostgresUnmarshall a9 b9, PostgresUnmarshall a10 b10, PostgresUnmarshall a11 b11, PostgresUnmarshall a12 b12, PostgresUnmarshall a13 b13, PostgresUnmarshall a14 b14, PostgresUnmarshall a15 b15, PostgresUnmarshall a16 b16) => PostgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16) where
  postgresUnmarshall (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    (,,,,,,,,,,,,,,,)
      <$> postgresUnmarshall a1
      <*> postgresUnmarshall a2
      <*> postgresUnmarshall a3
      <*> postgresUnmarshall a4
      <*> postgresUnmarshall a5
      <*> postgresUnmarshall a6
      <*> postgresUnmarshall a7
      <*> postgresUnmarshall a8
      <*> postgresUnmarshall a9
      <*> postgresUnmarshall a10
      <*> postgresUnmarshall a11
      <*> postgresUnmarshall a12
      <*> postgresUnmarshall a13
      <*> postgresUnmarshall a14
      <*> postgresUnmarshall a15
      <*> postgresUnmarshall a16

instance PostgresUnmarshall UUID (Id a) where
  postgresUnmarshall = Right . Id

instance PostgresUnmarshall Value Object where
  postgresUnmarshall (Object obj) = Right obj
  postgresUnmarshall v = Left $ "expected Object, got: " <> Text.decodeUtf8 (BS.toStrict (encode v))

instance (PostgresUnmarshall a b) => PostgresUnmarshall (Maybe a) (Maybe b) where
  postgresUnmarshall = mapM postgresUnmarshall

instance (PostgresUnmarshall a b) => PostgresUnmarshall (Vector a) (Vector b) where
  postgresUnmarshall = V.mapM postgresUnmarshall

---

lmapPG :: (PostgresMarshall a b, Profunctor p) => p b x -> p a x
lmapPG = lmap postgresMarshall

rmapPG :: (PostgresUnmarshall x y) => Statement a x -> Statement a y
rmapPG = refineResult postgresUnmarshall

dimapPG ::
  (PostgresMarshall a b, PostgresUnmarshall x y) =>
  Statement b x ->
  Statement a y
dimapPG = refineResult postgresUnmarshall . lmapPG
