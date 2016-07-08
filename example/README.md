
#### Running the example

There's a `stack.yaml` file in this directory, so you can run and build this
example project in a familiar manner.

```bash
# pull your GHC deps
stack setup

# build the project
stack build

# run the webserver
stack exec example-exe
```

At this point you should have a webserver running at `http://localhost:8080`
You'll find available endpoints at the following urls:
- [http://localhost:8080/users](http://localhost:8080/users) - responds with a list of User resources
- [http://localhost:8080/users/1](http://localhost:8080/users/1) - responds with a singleton resource
- [http://localhost:8080/users/2](http://localhost:8080/users/2) - responds with a singleton resource
- [http://localhost:8080/users/3](http://localhost:8080/users/3) - responds with a 404 and error payload




## List Resource Example

```JSON
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

```JSON
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

```JSON
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
