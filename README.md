[![Circle CI](https://circleci.com/gh/toddmohney/json-api.svg?style=svg)](https://circleci.com/gh/toddmohney/json-api)

## Haskell Implementation of the JSON-API specification



#### Motivation

From the specification itself:

> If you’ve ever argued with your team about the way your JSON responses should
> be formatted, JSON API can be your anti-bikeshedding tool.
> 
> By following shared conventions, you can increase productivity, take advantage
> of generalized tooling, and focus on what matters: your application.
> 
> Clients built around JSON API are able to take advantage of its features around
> efficiently caching responses, sometimes eliminating network requests entirely.

All in all, API discoverability and other [HATEOAS](https://spring.io/understanding/HATEOAS)
principles make JSON-API an attractive resource serialization option.



#### The specification

Find the specification [here](http://jsonapi.org/)



#### Example usage

Let's start with an example User record:

```Haskell
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
```

From this, we can use the `json-api` package to produce a payload conformant
to the [JSON-API specification](http://jsonapi.org/) like so:

```Haskell
-- Builds the Document which will be serialized as our
-- web server's response payload
mkDocument :: User -> Links -> Document User Text Int
mkDocument usr links =
  Document
    (Singleton $ toResourceObject usr)
    (Just links)
    Nothing

-- Helper function to convert a User into a resource object
-- This could be our canonical serialization function for a User in any
-- response payload
toResourceObject :: User -> ResourceObject User Text
toResourceObject user =
  ResourceObject resourceId resourceType user resourceLinks resourceMetaData
  where
    resourceId       = ResourceId . pack . show . userId $ user
    resourceType     = ResourceType "User"
    resourceLinks    = Just $ userLinks user
    resourceMetaData = Nothing

-- helper function to build links for a User resource
userLinks :: User -> Links
userLinks user = toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/users/" <> (show $ userId user)
```

When delivered as a response from a web server, for example, we get a payload
that looks like this:

```JSON
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

Neat! We can see that if we would like the full User data for the User with
ID=1, we can query `/users/1`. Discoverability!

We can also see from the top-level `links` data that this particular payload originated
from `/users/1`.

This is a very simple example to provide an introduction to the basic idea
behind JSON-API and how to use this library. Check out [these examples](http://jsonapi.org/examples/)
for more robust representations of resourceful payloads. Here, you'll start to
see the more comprehensive benefits of a discoverable API.



#### Example Project

There is an [example project](https://github.com/toddmohney/json-api/tree/master/example) illustrating how the library can be used in the context of a web server.



#### Hackage

Module documentation can be found on [Hackage](http://hackage.haskell.org/package/json-api)
