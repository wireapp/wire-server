# User Rich info

Reference: {#RefRichInfo}

_Author: Artyom Kazak_

---

This page describes a part of the user profile called "Rich info". The corresponding feature is called "Rich profiles".

## Summary {#RefRichInfoSummary}

For every team user we can store a list of key-value pairs that are displayed in the user profile. This is similar to "custom profile fields" in Slack and other enterprise messengers.

Different users can have different sets of fields; there is no team-wide schema for fields. All field values are strings. Fields are passed as an ordered list, and the order information is preserved when displaying fields in client apps.

Only team members and partners can see the user's rich info.

## API {#RefRichInfoApi}

### Querying rich info {#RefRichInfoGet}

`GET /users/:uid/rich-info`. Sample output:

```json
{
    "version": 0,
    "fields": [
        {
            "type": "Department",
            "value": "Sales & Marketing"
        },
        {
            "type": "Favorite color",
            "value": "Blue"
        }
    ]
}
```

If the requesting user is not allowed to see rich info, error code 403 is returned with the `"insufficient-permissions"` error label.

Otherwise, if the rich info is missing, an empty field list is returned:

```json
{
    "version": 0,
    "fields": []
}
```

### Setting rich info {#RefRichInfoPut}

**Not implemented yet.** Currently the only way to set rich info is via SCIM.

### Events {#RefRichInfoEvents}

**Not implemented yet.**

When user's rich info changes, the backend sends out an event to all team members:

```json
{
    "type": "user.rich-info-update",
    "user": {
        "id": "<user ID>"
    }
}
```

Connected users who are not members of user's team will not receive an event (nor can they query user's rich info by other means).

## SCIM support {#RefRichInfoScim}

Rich info can be pushed to Wire by setting JSON keys under the `"urn:ietf:params:scim:schemas:extension:wire:1.0:User"` extension. Both `PUT /scim/v2/Users/:id` , `PATCH /scim/v2/Users/:id` and `POST /scim/v2/Users/:id` can contain rich info. Here is an example for `PUT`:

```javascript
PUT /scim/v2/Users/:id

{
    ...,
    "urn:ietf:params:scim:schemas:extension:wire:1.0:User": {
        "Department": "Sales & Marketing",
        "FavoriteColor": "Blue"
    }
}
```

Here is an example for `PATCH`:

```
PATCH /scim/v2/Users/:id

{
  "schemas": [
    "urn:ietf:params:scim:api:messages:2.0:PatchOp"
  ],
  "operations": [
    {
      "op": "add",
      "path": "urn:ietf:params:scim:schemas:extension:wire:1.0:User:Department",
      "value": "Development "
    },
    {
      "op": "replace",
      "path": "urn:ietf:params:scim:schemas:extension:wire:1.0:User:Country",
      "value": "Germany"
    },
    {
      "op": "remove",
      "path": "urn:ietf:params:scim:schemas:extension:wire:1.0:User:City"
    }
  ]
}

```

Rich info set via SCIM can be queried by doing a `GET /scim/v2/Users` or `GET /scim/v2/Users/:id` query.

### Set up SCIM RichInfo mapping in Azure {#RefRichInfoScimAgents}

Go to your provisioning page

![image](https://user-images.githubusercontent.com/628387/119977043-393b3000-bfb8-11eb-9e5b-18a955ca3181.png)

Click "Edit attribute mappings"

Then click "Mappings" And then click **Synchronize Azure Active Directory Users to _appname_**
![image](https://user-images.githubusercontent.com/628387/119977488-c9797500-bfb8-11eb-81b8-46376f5fdadb.png)

Click "Show Advanced options" and then **Edit attribute list for _appname_**
![image](https://user-images.githubusercontent.com/628387/119977905-3f7ddc00-bfb9-11eb-90e2-28da82c6f13e.png)

Add a new attribute name. The type should be `String` and the name should be prefixed with `urn:ietf:params:scim:schemas:extension:wire:1.0:User:`
e.g. `urn:ietf:params:scim:schemas:extension:wire:1.0:User:Location`

![image](https://user-images.githubusercontent.com/628387/119978050-70f6a780-bfb9-11eb-8919-93e32bf76d79.png)

Hit **Save** and afterwards hit **Add New Mapping**

Select the Azure AD Source attribute you want to map, and map it to the custom **Target Attribute** that you just added.
![image](https://user-images.githubusercontent.com/628387/119978316-c5018c00-bfb9-11eb-9290-2076ac1a05df.png)



## Limitations {#RefRichInfoLimitations}

* The whole of user-submitted information (field names and values) cannot exceed 5000 characters in length. There are no limitations on the number of fields, or the maximum of individual field names or values.

* Field values can not be empty (`""`). If they are empty, the corresponding field will be removed.
