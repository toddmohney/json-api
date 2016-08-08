{- |
Entry-point module for this package.

Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi
  ( D.Document (..)
  , D.ErrorDocument (..)
  , E.Error (..)
  , D.ResourceData (..)
  , R.Relationship
  , R.Resource (..)
  , R.Identifier (..)
  , L.Links
  , M.Meta (..)
  , L.toLinks
  , R.mkRelationship
  , D.mkDocument
  ) where

import qualified Network.JSONApi.Error as E
import qualified Network.JSONApi.Document as D
import qualified Network.JSONApi.Link as L
import qualified Network.JSONApi.Meta as M
import qualified Network.JSONApi.Resource as R
