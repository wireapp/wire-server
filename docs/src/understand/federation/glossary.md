(glossary)=

# Federation Glossary

(glossary_backend)=
## Backend
> A set of servers, databases and DNS configurations together forming one single
> Wire Server entity as seen from outside. This set of servers can be owned and
> administrated by different legal entities in different countries. Sometimes
> also called a Wire \"instance\" or \"server\" or \"Wire installation\". Every
> resource (e.g. users, conversations, assets and teams) exists and is owned by
> one specific backend, which we can refer to as that resource\'s backend

(glossary_backend_domain)=
## Backend Domain

> The domain of a backend, which is used to qualify the names and
> identifiers of resources (users, clients, groups, etc) that are local
> to a given backend. See also 
> {ref}`consequences-backend-domain`

(glossary_infra_domain)=

## Infrastructure Domain or Infra Domain

> The domain under which the
> `Federator <glossary_federator>`{.interpreted-text role="ref"} of a
> given backend is reachable (via that backend\'s
> `Ingress <glossary_federation_ingress>`{.interpreted-text role="ref"})
> for other, remote backends.

(glossary_federation_ingress)=

## Federation Ingress

> Federation Ingress is the first point of contact of a given `backend
> <glossary_backend>`{.interpreted-text role="ref"} for other, remote
> backends. It also deals with the `authentication`{.interpreted-text
> role="ref"} of incoming requests. See
> `here <federation_ingress>`{.interpreted-text role="ref"} for more
> information.

(glossary_federator)=

## Federator

> The [Federator]{.title-ref} is the local point of contact for
> `other backend
> components <other-wire-server>`{.interpreted-text role="ref"} that
> want to make calls to remote backends. It is also the component that
> deals with the `authorization`{.interpreted-text role="ref"} of
> incoming requests from other backends after they have passed the
> `Federation Ingress
> <glossary_federation_ingress>`{.interpreted-text role="ref"}. See
> `here <federator>`{.interpreted-text role="ref"} for more information.

(glossary_asset)=
## Asset

> Any file or image sent via Wire (uploaded to and downloaded from a
> backend).

(glossary_qualified-user-id)=
## Qualified User Identifier (QUID)

> A combination of a UUID (unique on the user\'s backend) and a domain.

(glossary_qualified-user-name)=
## Qualified User Name (QUN)

> A combination of a name that is unique on the user\'s backend and a
> domain. The name is a string consisting of 2-256 characters which are
> either lower case alphanumeric, dashes, underscores or dots. See
> [here](https://github.com/wireapp/wire-server/blob/f683299a03207acb505254ff3121213383d0b672/libs/types-common/src/Data/Handle.hs#L76-L93)
> for the code defining the rules for user names. Note that in the
> wire-server source code, user names are called \'Handle\' and
> qualified user names \'Qualified Handle\'.

(glossary_qualified-client-id)=
## Qualified Client Identifier (QDID)

> A combination of a client identifier (a hash of the public key
> generated for a user\'s client) concatenated with a dot and the QUID
> of the associated user.

(glossary_qualified-group-id)=
## Qualified Group Identifier (QGID)

> The string [backend-domain.com/groups/]{.title-ref} concatenated with
> a UUID that is unique on a given backend.

(glossary_qualified-conversation-id)=
## Qualified Conversation Identifier (QCID)

> The same as a `QGID <glossary_qualified-group-id>`{.interpreted-text
> role="ref"}.

(glossary_qualified-team-id)=
## Qualified Team Identifier (QTID)

> The string [backend-domain.com/teams/]{.title-ref} concatenated with a
> UUID that is unique on a given backend.

(glossary_display-name)=
## (User) Profile/Display Name

> The profile/display name of a user is a UTF-8 encoded string with
> 1-128 characters.
