# Schemas for documented bidirectional JSON encoding

## Introduction

A value of type `SchemaP d v w a b`, which we will refer to as a
*schema*, contains both a JSON parser and a JSON serialiser,
together with documentation-like metadata, such as a JSON or
Swagger schema.

The type variables are as follows:

 - `d`: documentation type, usually a `Monoid`.
 - `v`: type of JSON values being parsed (e.g. `Value`).
 - `w`: type of JSON values being serialised (e.g. `Value`).
 - `a`: input type.
 - `b`: output type.

Input and output types deserve some more explanation. We can think
of a value `sch` of type `SchemaP d v w a b` as a kind of special
"function" from `a` to `b`, but where `a` and `b` might potentially
live in different "languages". The parser portion of `sch` takes a
JSON-encoded value of type `a` and produces a value of type `b`,
while the serialiser portion of `sch` takes a haskell value of type
`a` and produces a JSON-encoding of something of type `b`.

In terms of composability, this way of representing schemas (based
on input and output types) is superior to the perhaps more natural
approach of using "bidirectional functions" or isomorphisms (based
on a single type parameter).

Although schemas cannot be composed as functions (i.e. they do not
form a `Category`), they still admit a number of important and
useful instances, such as `Profunctor` (and specifically `Choice`),
which makes it possible to use prism quite effectively to build
schema values.

Using type variables to represent JSON types might seem like
excessive generality, but it is useful to represent "intermediate"
schemas arising when building complex ones. For example, a schema
which is able to work with fields of a JSON object (see `field`)
should not output full-blown objects, but only lists of pairs, so
that they can be combined correctly via the usual `Monoid`
structure of lists when using the `Applicative` interface of
`SchemaP d v w a b`.

## Tutorial

To learn how to use `SchemaP` in practice, let us walk through two
basic examples, one for a record, and one for a sum type.

### Records

Consider the following record:

```haskell
data Person = Person
  { name :: Text
  , age :: Int }
```

we can produce a schema for `Person` as follows:

```haskell
personSchema :: ValueSchema NamedSwaggerDoc Person
personSchema = object "Person" $ Person
  <$> name .= field "name" schema
  <*> age .= field "age" schema
```

This simply builds up the schema for `Person` in terms of the ones for
`Text` and `Int`, as an object containing the fields `"name"` and
`"age"`.

Let us break down the example to see what the types look like at each
stage of the construction.

If we focus on the second line of the definition (after `<$>`) and
move right to left, we start with a call to `schema`, which is the
only method of the `ToSchema` class. This returns a schema for `Text`,
i.e. a `SchemaP` value `Text ~> Text`, by which we mean that the input
and output types are both `Text`.

After that, we use the `field` combinator to turn it into a
single-field object schema. This does not change the type, but it
changes the behaviour of both the parser and the pretty-printer. Used
by itself, this schema would convert JSON objects of the form `{
"name": "Bob" }` with the haskell value `"Bob" :: Text`, in both
directions.

Now, we use the `(.=)` combinator (which is an infix synonym for the
`lmap` method of `Profunctor`) to turn this into a schema `Person ~>
Text`. We are not changing the runtime behaviour of the schema in any
significant way. We are only modifying the input type. Now a JSON
object containing a whole `Person`, say `{ "name": "Bob", age: 42}`
will be converted to the haskell value `"Bob" :: Text`.

Let us pause here to observe how at this stage we have significantly
departed from the idea that a schema should somehow represent an
isomorphism between JSON and haskell values. The schema we have built
so far cannot possibly be an isomorphism, because it is a mapping
between two different types. This generality is important, since it
makes it possible to construct schemas incrementally, as we are doing
in this example: even though the final schema one is interested in
might well be an isomorphism, it is almost never possible to obtain it
compositionally by only going through isomorphisms.

At this point, we are almost done. The next line of the example
constructs a similar object schema, this time `Person ~> Int`,
corresponding to the "age" field. To put them all together, we simply
use the `Applicative` instance of `SchemaP`:

```haskell
Person <$> nameSchema <*> ageSchema
```

where `nameSchema` and `ageSchema` stand for the two schemas we
described above. The `Applicative` interface of `SchemaP` changes the
output type. At the level of the pretty printer, it simply
"concatenates" the outputs using a `Monoid` instance for the
corresponding JSON values (in this case, list of key-value pairs). At
the level of the parser, it combines them in the usual applicative
sense (i.e. by sequencing).

Finally, we want to turn this special schema, which can only parse
objects and outputs lists of pairs, into a general-purpose "value"
schema, i.e. one that can parse and serialise arbitrary JSON
values. This can be done using the `object` combinator, which
incidentally also takes a name for the schema and uses it for both the
documentation and parsing errors.

### Sum types

Let us now look at a similar example, but based on sum types.

```haskell
data Detail
  = Name Text
  | Age Int
```

Here is how we can implement a schema for `Detail`:

```haskell
detailSchema :: ValueSchema NamedSwaggerDoc Detail
detailSchema = named "Detail" $
  tag _Name schema <>
  tag _Age schema
```

Again, we can examine this value by moving right to left starting from
the second line of the definition. Once again, the `schema` call
builds a schema for `Text`, using a builtin instance of `ToSchema`.

Next, we use `tag` to turn a schema `Text ~> Text` into a schema
`Detail ~> Detail`. The first argument of `tag` is a *prism* of type
`Prism' Detail Text`, which can be automatically generated using
`makePrisms` from the lens library.

The use of a prism here is necessary, because it gives the pretty
printer a way to examine whether a value of type `Detail` happens to
be "tagged" with `Name`. For those not familiar with optics, it helps
to think of the prism `_Name` as a pair consisting of the constructor
`Name :: Text -> Detail`, and a partial function `Detail -> Maybe
Text`, which checks if a detail is actually a name, and if so returns
the actual name.

After tagging, the resulting shema is able to translate between a JSON
value such as `"Bob"` and the corresponding haskell value `Name
"Bob"`.

To put this schema and the analogous one for `Age` together, we can
now simply use the `Monoid` instance. At the parser level, this works
just like an `Alternative` instance, i.e. it tries parsing the various
cases one by one until it succeeds. Similarly, at the serialiser
level, it tries every case until the underlying lens returns a `Just`.

Finally, we add a name to the schema using the `named`
combinator. This does nothing to the JSON encoding-deconding part of
the schema, and only affects the documentation.

It is important to note how this sum type example is realised as a
"tagged" union on the haskell side, but un "untagged" one on the JSON
side. That means that the JSON values that this schema parses and
produces distinguish the two cases simply using the type of the
underlying value. In particular, this approach would not work for sums
that cannot be distinguished in this way, like for example `Either Int
Int`.

In those cases, one can for example make use of the `field` and
`object` combinators to move values inside JSON objects, and use the
keys as tags on the JSON side. Ultimately, since JSON does not
directly have a notion of sum types, how these types are represented
is up to the application, and the `SchemaP` combinators should have
enough flexibility to accommodate most choices of encoding.

## Advanced usage

Sometimes, JSON encoding of haskell types is not as straightfoward as
in the previous examples. For example, for backward-compatibility
reasons, it might be necessary to serialise an object with some extra
redundant information, which is then ignored when parsing.

The `SchemaP` combinator language is powerful enough to express these
sort of use cases quite concisely.

For example, consider a record type like:

```haskell
data Person = Person
  { firstName :: Text
  , lastName :: Text
  -- ... potentially other fields here
  }
```

If an old version of the application was working with a `fullName ::
Text` field instead of first/last, we can retain some form of
backwards compatibility for consumers of our JSON output if we simply
add a redundant `"full_name"` field in the serialised object.

Here is how to achieve this using `SchemaP`:

```haskell
personSchema = object "Person" $ Person
  <$> firstName .= field "first_name" schema
  <*> lastName .= field "last_name" schema
  <* fullName .= optional (field "full_name" schema)
  where
    fullName p = firstName p <> " " <> lastName p
```

Most of this schema definition should be familiar if you have followed
the record example above, but there are a few new ideas.

First, note that the `where` clause is defining a function `fullName`,
which is a normal haskell function `Person -> Text`.

Next, a `"full_name"` schema is constructed in the usual way, as a
schema of type `Text ~> Text`. However, since we do not want to
require this field when parsing, we wrap it inside `optional`, which
is a standard combinator in `Control.Applicative`. This makes it
indeed optional at the parser level, and changes its type to `Text ~>
Maybe Text`. The schema will convert between JSON values of the form
`{ "full_name": "Bob Ross" }` and `Just "Bob Ross" :: Maybe Text`.

At this point, we lift it to the level of `Person` by using our custom
`fullName` function with the `(.=)` combinator. Now the schema has
type `Person -> Maybe Text`. Given a whole object for a `Person`
value, it will extract its optional `full_name` field. In the other
direction, given a haskell value of type `Person`, it will output a
JSON object with just the `full_name` field.

Now we can assemble this field schema and the simple ones for first
and last name to obtain a schema for the whole `Person` type. This
works just like before, using the `Applicative` interface. The only
caveat is that, since we do not need the full name value of type
`Maybe Text` in order to reconstruct a person, we simply ignore it by
using the `<*` combinator from `Control.Applicative`.
