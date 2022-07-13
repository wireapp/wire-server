# Multiverb

We offer the typeclass `AsUnion` to convert a handler return type to a union type
including all possible responses of a `MultiVerb` endpoint.

Any glue code necessary to convert application types to and from the
canonical `Union` type corresponding to a `MultiVerb` endpoint should be
packaged into an `AsUnion` instance.

When using flat sum types, you can use Generics to automatically derive this instance, 
and for nested types, the following example code should help clarify usage.

It assumes some understanding of [Data.SOP](https://hackage.haskell.org/package/sop-core-0.5.0.2/docs/Data-SOP.html)

```haskell
data Success = Success

data Failure
  = InvalidEntry
  | AccessDenied

-- We need a way to map errors to servant and swagger.
instance KnownError (MapError e) => IsSwaggerError (e :: Failure) where
  addToSwagger = addStaticErrorToSwagger @(MapError e)

type instance MapError 'InvalidEntry = 'StaticError 400 "invalid-entry" "Invalid data entered"

type instance MapError 'AccessDenied = 'StaticError 403 "access-denied" "Access denied"

data FailureSuccess = SFFailure Failure | SFSuccess Success

sfToEither :: FailureSuccess -> Either Failure Success
sfToEither (SFFailure b) = Left b
sfToEither (SFSuccess d) = Right d

sfFromEither :: Either Failure Success -> FailureSuccess
sfFromEither (Left b) = SFFailure b
sfFromEither (Right d) = SFSuccess d

-- type instance ResponseType Failure = Failure
type instance ResponseType Success = Success

-- ErrorResponse offers facilities to create errors
type MyErrorResponses =
  '[ ErrorResponse 'InvalidEntry,
     ErrorResponse 'AccessDenied
   ]

-- Responses is a list of errors and a list of success cases.
type MyResponses =
  MyErrorResponses .++ '[Success]

accessDenied :: DynError
accessDenied = dynError @(MapError 'AccessDenied)

invalidEntry :: DynError
invalidEntry = dynError @(MapError 'InvalidEntry)

failToError :: Failure -> NS I (DynError : DynError : xs)
failToError = \case
  -- Z . I wraps the first value in the error list.
  -- They come from Data.SOP
  InvalidEntry -> Z . I $ accessDenied
  -- we wrap the value using I (identity), Z (zero) and S (successor) to indicate second item in the response list
  AccessDenied -> S . Z . I $ invalidEntry

instance (res ~ MyResponses) => AsUnion res FailureSuccess where
  toUnion =
    -- Type application here tells the type engine that we use DynError from ErrorResponse
    -- as the return type, and no Failure.
    eitherToUnion @'[DynError, DynError] @'[Success]
      failToError -- Maps a Failure case to its DynError equivalent
      (Z . I) -- [Success] only has one item.
      . sfToEither

  fromUnion =
    sfFromEither
      . eitherFromUnion @'[DynError, DynError] @'[Success]
        ( \case
            Z _ -> InvalidEntry
            S _ -> AccessDenied
        )
        (unI . unZ)
```
