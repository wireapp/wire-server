module Wire.Postgres
  ( -- | This module provides a composable DSL for constructing postgres
    -- statements. Queries are assembled from smaller 'QueryFragment's that
    -- carry both their SQL text and parameter encoders.
    --
    -- Typical usage involves combining fragments with monoidal operators and
    -- building a final 'Statement' using 'buildStatement'.
    --
    -- Example:
    --
    -- > let q =
    -- >       literal "select * from users"
    -- >       <> where_ [like "name" "alice"]
    -- >       <> orderBy [("created_at", Desc)]
    -- >       <> limit (10 :: Int)
    -- > in buildStatement q userDecoder
    --
    -- Note that the encoders are specialised to the specific values passed when
    -- constructing the fragments, so they don't require further values. The
    -- resulting statement can be run with something like @runStatement ()@.

    -- * Runners
    runStatement,
    runTransaction,
    runPipeline,
    parseCount,
    PGConstraints,

    -- * Query builder
    QueryFragment,
    literal,
    paramLiteral,
    argPattern,
    where_,
    like,
    Clause,
    mkClause,
    clause,
    clause1,
    orderBy,
    limit,
    buildStatement,

    -- * Type classes
    PostgresValue (..),
  )
where

import Control.Monad.Trans.State
import Data.Functor.Contravariant
import Data.Id
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Pipeline (Pipeline)
import Hasql.Pool
import Hasql.Pool qualified as Hasql
import Hasql.Session
import Hasql.Statement
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions
import Hasql.Transaction.Sessions qualified as Transaction
import Imports
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input
import Wire.API.Pagination

type PGConstraints r =
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Error Hasql.UsageError) r
  )

runStatement ::
  (PGConstraints r) =>
  a ->
  Statement a b ->
  Sem r b
runStatement a stmt = do
  pool <- input
  liftIO (use pool (statement a stmt)) >>= either throw pure

runTransaction ::
  (PGConstraints r) =>
  IsolationLevel ->
  Mode ->
  Transaction a ->
  Sem r a
runTransaction isolationLevel mode t = do
  pool <- input
  liftIO (use pool $ Transaction.transaction isolationLevel mode t) >>= either throw pure

runPipeline ::
  (PGConstraints r) =>
  Pipeline a ->
  Sem r a
runPipeline p = do
  pool <- input
  liftIO (use pool $ pipeline p) >>= either throw pure

class PostgresValue a where
  postgresType :: Text
  postgresValue :: a -> Enc.Value ()

  valueEncoder :: a -> Enc.Params ()
  valueEncoder = Enc.param . Enc.nonNullable . postgresValue

instance PostgresValue (Id a) where
  postgresType = "uuid"
  postgresValue u = const (toUUID u) >$< Enc.uuid

instance PostgresValue Text where
  postgresType = "text"
  postgresValue x = const x >$< Enc.text

instance PostgresValue UTCTime where
  postgresType = "timestamptz"
  postgresValue t = const t >$< Enc.timestamptz

instance PostgresValue Int32 where
  postgresType = "int"
  postgresValue n = const n >$< Enc.int4

-- | Parse count result returned by Postgres.
parseCount :: Int64 -> Either Text Int
parseCount = \case
  n | n < 0 -> Left "Negative count from database"
  n | n > fromIntegral (maxBound :: Int) -> Left "Count from database too large"
  n -> Right $ fromIntegral n

--------------------------------------------------------------------------------
-- Query builder DSL

data QueryFragment = QueryFragment
  { query :: State Int Text,
    encoder :: Enc.Params ()
  }

joinFragments :: Text -> QueryFragment -> QueryFragment -> QueryFragment
joinFragments sep f1 f2 =
  QueryFragment
    { query = separate <$> f1.query <*> f2.query,
      encoder = f1.encoder <> f2.encoder
    }
  where
    separate "" q = q
    separate q "" = q
    separate q1 q2 = q1 <> sep <> q2

instance Semigroup QueryFragment where
  (<>) = joinFragments " "

instance Monoid QueryFragment where
  mempty =
    QueryFragment
      { query = pure "",
        encoder = mempty
      }

literal :: Text -> QueryFragment
literal q =
  QueryFragment
    { query = pure q,
      encoder = mempty
    }

-- | Helper to construct a fragment with a single parameter.
paramLiteral :: Enc.Params () -> (Int -> Text) -> QueryFragment
paramLiteral encoder q =
  QueryFragment
    { query = q <$> nextIndex,
      encoder
    }

argPattern0 :: Text -> Int -> Text
argPattern0 t i = "$" <> T.pack (show i) <> " :: " <> t

argPattern :: Text -> Int -> Text
argPattern t i = "(" <> argPattern0 t i <> ")"

-- | Construct a WHERE clause from a list of fragments.
where_ :: [QueryFragment] -> QueryFragment
where_ frags = literal "where" <> foldr (joinFragments " and ") mempty frags

like :: Text -> Text -> QueryFragment
like field pat = paramLiteral
  (const (fuzzy pat) >$< Enc.param (Enc.nonNullable Enc.text))
  $ \i ->
    field <> " ilike " <> argPattern "text" i

-- | A portion of a WHERE clause with multiple values. The monoidal operation
-- of this type can be used to combine values into one clause. For example:
--
-- > clause "=" (mkClause "foo" 3 <> mkClause "bar" 4)
--
-- generates a pattern that will end up being expanded as @"(foo, bar) = (3, 4)"@.
data Clause = Clause
  { fields :: [Text],
    types :: [Text],
    encoder :: Enc.Params ()
  }

instance Semigroup Clause where
  cl1 <> cl2 =
    Clause
      { fields = cl1.fields <> cl2.fields,
        types = cl1.types <> cl2.types,
        encoder = cl1.encoder <> cl2.encoder
      }

instance Monoid Clause where
  mempty =
    Clause
      { fields = mempty,
        types = mempty,
        encoder = mempty
      }

mkClause :: forall a. (PostgresValue a) => Text -> a -> Clause
mkClause field value =
  Clause
    { fields = [field],
      types = [postgresType @a],
      encoder = valueEncoder value
    }

-- | Convert a 'Clause' to a 'QueryFragment'.
clause :: Text -> Clause -> QueryFragment
clause op cl =
  QueryFragment
    { query = do
        types <-
          fmap wrap $
            for cl.types $
              \ty -> argPattern0 ty <$> nextIndex
        let fields = wrap cl.fields
        pure $ fields <> " " <> op <> " " <> types,
      encoder = cl.encoder
    }
  where
    wrap :: [Text] -> Text
    wrap xs = "(" <> T.intercalate ", " xs <> ")"

-- | Fragment for a clause with a single value.
clause1 :: forall a. (PostgresValue a) => Text -> Text -> a -> QueryFragment
clause1 field op value = clause op (mkClause field value)

orderBy :: [(Text, SortOrder)] -> QueryFragment
orderBy os =
  literal $
    "order by "
      <> T.intercalate ", " (map (\(field, o) -> field <> " " <> sortOrderClause o) os)

limit :: forall a. (PostgresValue a) => a -> QueryFragment
limit n = paramLiteral (valueEncoder n) $ \i ->
  "limit " <> argPattern (postgresType @a) i

buildStatement :: QueryFragment -> Dec.Result b -> Statement () b
buildStatement frag dec =
  Statement
    (T.encodeUtf8 (evalState frag.query 1))
    frag.encoder
    dec
    True

nextIndex :: State Int Int
nextIndex = get <* modify succ

fuzzy :: Text -> Text
fuzzy x = "%" <> x <> "%"
