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
    (Singleton $ toResource usr)
    (Just links)
    Nothing

-- Helper function to convert a User into a resource object
-- This could be our canonical serialization function for a User in any
-- response payload
toResource :: User -> Resource User Text
toResource user =
  Resource resourceId resourceType user resourceLinks resourceMetaData
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

##### Pagination Example

Let's use the same example User record:

```Haskell
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
```

Suppose we now have a list of 2 users;

```Haskell
let usrs =
  [ User 1 "Isaac" "Newton"
  , User 2 "Albert" "Einstein"
  ]
```

From this, we can use the `json-api` package to produce a payload for a collection with pagination links conformant
to the [JSON-API pagination specification](https://jsonapi.org/format/#fetching-pagination) like so:

```Haskell
let paginate = Pagination (PageIndex 1) (PageSize 1) (ResourceCount $ toEnum (length usrs))
let resourceLink = (fromJust . importURL) "/users"
let paginationLinks = mkPaginationLinks PageStrategy resourceLink paginate
let doc = mkDocuments [head usrs] (Just paginationLinks) Nothing
```

When delivered as a response from a web server, for example, we get a payload
that looks like this:

```JSON
{
  "data": [
    {
      "attributes": {
        "userFirstName": "Isaac",
        "userLastName": "Newton",
        "userId": 1
      },
      "relationships": null,
      "id": "1",
      "meta": null,
      "type": "users",
      "links": null
    }
  ],
  "meta": null,
  "included": [
  ],
  "links": {
    "next": "/users?page%5bsize%5d=1&page%5bnumber%5d=2",
    "first": "/users?page%5bsize%5d=1&page%5bnumber%5d=1",
    "last": "/users?page%5bsize%5d=1&page%5bnumber%5d=2"
  }
}
```

The key function in the code example is `mkPaginationLinks` which has the following signature;

```Haskell
mkPaginationLinks :: Strategy -> URL -> Pagination -> Links
```

`Strategy` is a sum type that represents the different paging strategies as laid out in the [JSON-API pagination specification](https://jsonapi.org/format/#fetching-pagination). At the time of writing this README, the library only supports 2 paging strategies Offset and Page. Offset is a 0 index based approach unlike Page, i.e. `page[offset]` 0 is the same as `page[number]` 1.

The `URL` type is used to build the links that appear in the JSON payload. The `Pagination` type contains the requisite information for the `mkPaginationLinks` function to generate the paging links.

So let's break this example down. To get started we need to create a `Pagination` record. The first attribute of the record is `PageIndex`. This attribute informs the caller that the page we are looking at is the first in the entire collection (`PageIndex` is either a 0 based index or 1 based index depending on the `Strategy`). So in our example as we are using `PageStrategy`, `PageIndex 1` implies we are after the first page. The second attribute of the record is `PageSize`. This atrribute tells the caller how many items can appear in the list at most. So in our example seeing there are only 2 users, a `PageSize` of 1 would mean that in total we have 2 pages. The third attribute is `ResourceCount`. This attribute is required by the function `mkPaginationLinks` to figure out which links to generate.

The links object in the JSON payload can have 4 attributes `next`, `prev`, `first` and `last`. This library only generates valid links. For example if the request is for the first page of a list, then the `prev` link is not present.

#### Example Project

There is an [example project](https://github.com/toddmohney/json-api/tree/master/example) illustrating how the library can be used in the context of a web server.

#### Hackage

Module documentation can be found on [Hackage](http://hackage.haskell.org/package/json-api)
