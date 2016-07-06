

## List Resource Example

```json
GET /users

{
  "data":[
    {
      "attributes":{
        "userFirstName":"Isaac",
        "userLastName":"Newton",
        "userId":1
      },
      "id":"1",
      "meta":null,
      "type":"User",
      "links":{
        "self":"/users/1"
      }
    },
    {
      "attributes":{
        "userFirstName":"Albert",
        "userLastName":"Einstein",
        "userId":2
      },
      "id":"2",
      "meta":null,
      "type":"User",
      "links":{
        "self":"/users/2"
      }
    }
  ],
  "meta":{
    "user-count":2
  },
  "links":{
    "self":"/users"
  }
}
```


## Singleton Resource Example

```json
GET /users/1

{
  "data":{
    "attributes":{
      "userFirstName":"Isaac",
      "userLastName":"Newton",
      "userId":1
    },
    "id":"1",
    "meta":null,
    "type":"User",
    "links":{
      "self":"/users/1"
    }
  },
  "meta":null,
  "links":{
    "self":"/users/1"
  }
}
```


## Error Example

```json
GET /users/3

{
  "error":{
    "status":"404",
    "code":null,
    "id":null,
    "meta":null,
    "title":"Resource Not Found",
    "links":null,
    "detail":"There is no User with id: 3"
  },
  "meta":null,
  "links":{
    "self":"/users/3"
  }
}
```
