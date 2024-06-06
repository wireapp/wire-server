Wire uses a forest of polysemy effects system to structure its application logic.

A subsystem is a root node in this forest that implements one broad
segment of the wire backend, like `User`, `Notification`,
`Conversation`, ...

Each subsystem has an interpreter called `Eff` that implements its
logic in terms of more fine-grained effects.

The leaf effects are base effects that cannot be implemented in
finer-grained sub-effects, so the come with interpreters like `Rpc`,
`Cassandra`, `Mem`.

Testing: establishing equivalence between these interpreters for one
base effect should be feasible, *if* the base request itself really is
simple, and doesn't contain any application logic.  Given this
equivalence, we canwrite unit tests using the subsystems' `Eff`
interpreters with the fast `Mem` interpreters underneath.

Module structure (as of 2024-06-06):

```
src/
└── Wire
    ├── APIAccess               <-- talk to one of the legacy wire services (brig, galley, ...)
    │   ├── Federation.hs       <-- (...  or to a remote backend)
    │   ├── Federation
    │   │   └── Interpreter
    │   │       └── Rpc.hs
    │   ├── Galley.hs
    │   ├── Galley
    │   │   └── Interpreter
    │   │       └── Rpc.hs
    │   └── Gundeck.hs
    ├── MiniBackend.hs
    ├── MiniBackend             <-- in-memory implementation of everything we have already covered in tests
    │   └── Interpreter
    │       ├── Mem.hs
    └── Subsystem
        ├── Notification.hs
        ├── Notification
        │   └── Interpreter
        │       └── Eff.hs
        ├── User.hs
        ├── User
        │   ├── Interpreter
        │   │   └── Eff.hs
        │   └── UserStore.hs
        │   ├── UserStore
        │   │   └── Interpreter
        │   │       ├── Cassandra
        │   │       │   └── Unique.hs
        │   │       └── Cassandra.hs
        │   ├── StoredUser.hs
        │   ├── DeleteQueue.hs
        │   ├── DeleteQueue
        │   │   └── Interpreter
        │   │       └── Mem.hs
        │   ├── HandleBlacklist.hs
        │   ├── InternalEvent.hs
        │   ├── UserEvents.hs
        └── Util
            ├── ParseException.hs
            └── Rpc.hs
```

Every effect has a file with the interface type and a sub-directory
with at least `Interpreters`.  Common interpreters are `Cassandra`,
`Rpc`, `Mem` (for unit testing), `Eff` (abstract, using more basic
polysemy effects under the hood).  Other modules needed for
implementing the Subsystem are siblings to the `Interpreter`
directory.
