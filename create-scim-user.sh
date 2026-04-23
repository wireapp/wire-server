SCIM_TOKEN="+TXYgTtk6uVjP3VICm5sqK7TVbs65yWc1Q9YbBk9AB8="

WIRE_BACKEND="https://nginz-https.blueberry.mobtown.wire.link"

# SCIM_USER='{
#    "schemas"     : ["urn:ietf:params:scim:schemas:core:2.0:User"],
#    "externalId"  : "scimmer",
#    "userName"    : "scimmer",
#    "displayName" : "The scimmer"
# }'
SCIM_USER='{                                                                       
      "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],                
      "externalId": "scimmer",                                                   
      "userName": "scimmer",                                                     
      "name": {                                                                 
        "givenName": "scimmer",                                                  
        "familyName": "User"                                                    
      },                                                                        
      "emails": [{                                                              
        "value": "scimmer@example.com",                                          
        "primary": true                                                         
      }],                                                                       
      "active": true                                                            
    }' 
curl -X POST \
  --header "Authorization: Bearer $SCIM_TOKEN" \
  --header 'Content-Type: application/json;charset=utf-8' \
  -d "$SCIM_USER" \
  $WIRE_BACKEND/scim/v2/Users
