# Test output

## user 1 on example.com

```json
{
    "domain": "example.com",
    "id": "e708b850-3846-4514-8daf-4b4b3e49f783"
}
```

## user 2 on b.example.com

```json
{
    "domain": "b.example.com",
    "id": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a"
}
```

## user 1 sends connection request to user 2

request: `POST /v4/connections/b.example.com/4e2e3e60-5cce-4906-9e48-c5ecfe541e6a`
response:

```json
{
    "conversation": "c03581da-66a4-5abb-ac41-0e455ef7bd27",
    "from": "e708b850-3846-4514-8daf-4b4b3e49f783",
    "last_update": "2023-08-16T14:15:45.946Z",
    "qualified_conversation": {
        "domain": "b.example.com",
        "id": "c03581da-66a4-5abb-ac41-0e455ef7bd27"
    },
    "qualified_to": {
        "domain": "b.example.com",
        "id": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a"
    },
    "status": "sent",
    "to": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a"
}
```

## user 1 lists conversations ids

request: `POST /v4/conversations/list-ids`
response:

```json
{
    "has_more": false,
    "paging_state": "AQ==",
    "qualified_conversations": [
        {
            "domain": "example.com",
            "id": "e708b850-3846-4514-8daf-4b4b3e49f783"
        },
        {
            "domain": "example.com",
            "id": "ed1d3100-fc99-5403-88a0-35862d1eed1f"
        },
        {
            "domain": "b.example.com",
            "id": "c03581da-66a4-5abb-ac41-0e455ef7bd27"
        }
    ]
}
```

## user 1 lists connections

request: `POST /v4/list-connections`
response:

```json
{
    "connections": [
        {
            "conversation": "c03581da-66a4-5abb-ac41-0e455ef7bd27",
            "from": "e708b850-3846-4514-8daf-4b4b3e49f783",
            "last_update": "2023-08-16T14:15:45.946Z",
            "qualified_conversation": {
                "domain": "b.example.com",
                "id": "c03581da-66a4-5abb-ac41-0e455ef7bd27"
            },
            "qualified_to": {
                "domain": "b.example.com",
                "id": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a"
            },
            "status": "sent",
            "to": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a"
        }
    ],
    "has_more": false,
    "paging_state": "AQ=="
}
```

## user 1 gets conversation

request: `GET /v4/conversations/b.example.com/c03581da-66a4-5abb-ac41-0e455ef7bd27`
response:

```json
{
    "access": [
        "private"
    ],
    "access_role": [],
    "creator": "4e2e3e60-5cce-4906-9e48-c5ecfe541e6a",
    "id": "c03581da-66a4-5abb-ac41-0e455ef7bd27",
    "last_event": "0.0",
    "last_event_time": "1970-01-01T00:00:00.000Z",
    "members": {
        "others": [],
        "self": {
            "conversation_role": "wire_admin",
            "hidden": false,
            "hidden_ref": null,
            "id": "e708b850-3846-4514-8daf-4b4b3e49f783",
            "otr_archived": false,
            "otr_archived_ref": null,
            "otr_muted_ref": null,
            "otr_muted_status": null,
            "qualified_id": {
                "domain": "example.com",
                "id": "e708b850-3846-4514-8daf-4b4b3e49f783"
            },
            "service": null,
            "status": 0,
            "status_ref": "0.0",
            "status_time": "1970-01-01T00:00:00.000Z"
        }
    },
    "message_timer": null,
    "name": null,
    "protocol": "proteus",
    "qualified_id": {
        "domain": "b.example.com",
        "id": "c03581da-66a4-5abb-ac41-0e455ef7bd27"
    },
    "receipt_mode": null,
    "team": null,
    "type": 2
}
```
