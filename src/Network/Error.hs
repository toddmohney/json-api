{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-
Error objects provide additional information about problems encountered while
performing an operation. Error objects MUST be returned as an array keyed by
errors in the top level of a JSON API document.

An error object MAY have the following members:

id: a unique identifier for this particular occurrence of the problem.

links: a links object containing the following members:

     about: a link that leads to further details about this particular occurrence of the problem.

status: the HTTP status code applicable to this problem, expressed as a string value.

code: an application-specific error code, expressed as a string value.

title: a short, human-readable summary of the problem that SHOULD NOT change
       from occurrence to occurrence of the problem, except for purposes of localization.

detail: a human-readable explanation specific to this occurrence of the problem.
        Like title, this field’s value can be localized.

source: an object containing references to the source of the error, optionally
        including any of the following members:

    pointer: a JSON Pointer [RFC6901] to the associated entity in the request
             document [e.g. "/data" for a primary data object, or "/data/attributes/title" for a specific attribute].

    parameter: a string indicating which URI query parameter caused the error.

meta: a meta object containing non-standard meta-information about the error.

-}

module Network.Error
(
) where

import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import           Data.Text
import           GHC.Generics
import           Network.Link (Links)
import           Network.Meta
import           Network.URL (URL)

-- Currently not supported:
-- "source" attribute

data Error a =
  Error { id     :: Maybe Text
        , links  :: Maybe Links
        , status :: Maybe Text
        , code   :: Maybe Text
        , title  :: Maybe Text
        , detail :: Maybe Text
        , meta   :: Maybe (Meta a)
        }
  deriving (Show, Eq, Generic)

instance ToJSON a   => ToJSON (Error a)
instance FromJSON a => FromJSON (Error a)

