{- |
Entry-point module for this package.
-}
module Network.JSONApi
( D.Document
, D.ResourceData (..)
, D.ErrorDocument (..)
, D.Included
, E.Error (..)
, R.Relationship
, R.Resource (..)
, R.Relationships
, R.ResourcefulEntity (..)
, I.HasIdentifier (..)
, I.Identifier (..)
, L.Links
, M.Meta
, M.MetaObject (..)
, L.mkLinks
, R.mkRelationship
, R.mkRelationships
, D.mkDocument
, D.mkDocument'
, D.singleton
, D.list
, D.mkCompoundDocument
, D.mkCompoundDocument'
, D.mkIncludedResource
, M.mkMeta
) where

import qualified Network.JSONApi.Error as E
import qualified Network.JSONApi.Document as D
import qualified Network.JSONApi.Identifier as I
import qualified Network.JSONApi.Link as L
import qualified Network.JSONApi.Meta as M
import qualified Network.JSONApi.Resource as R

myFunction = (1 + 1 (1 + 1))
