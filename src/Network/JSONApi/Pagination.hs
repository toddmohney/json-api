{-# LANGUAGE OverloadedStrings #-}

module Network.JSONApi.Pagination (
    Pagination (..)
  , PageIndex (..)
  , PageSize (..)
  , ResourceCount (..)
  , Strategy (..)
  , mkPaginationLinks
) where

import Network.JSONApi.Link (Links, Rel, mkLinks)
import Network.URL (URL, add_param)

{- |
Wrapper type for the various components of pagination being page size, page index
and the number of resources in total.
-}
data Pagination = Pagination {
                      getPaginationPageIndex :: PageIndex
                    , getPaginationPageSize :: PageSize
                    , getPaginationResourceCount :: ResourceCount
                  }

{- |
We can specify limits on the number of rows we would like back from the database
-}
newtype PageSize = PageSize {
  getPageSize :: Word
} deriving Show

newtype PageIndex = PageIndex {
  getPageIndex :: Word
} deriving Show

newtype ResourceCount = ResourceCount {
  getResourceCount :: Word
} deriving Show

{- |
Pagination strategies are commonly implemented by the server of which Page and Offset
are commonly used.
-}
data Strategy = PageStrategy | OffsetStrategy

{- |
Helper function to beuild relative links for a collection of resources of type ResourceEntity.

This helper function assumes that the first page is always page 0.
-}
mkPaginationLinks :: Strategy -> URL -> Pagination -> Links
mkPaginationLinks strategy baseUrl page =
  mkLinks (baseLinks ++ nextLinks ++ prevLinks)
    where
      pgIndex    = getPageIndex $ getPaginationPageIndex page
      pgSize     = getPageSize $ getPaginationPageSize page
      baseLinks  = [mkPaginationLink strategy "first" baseUrl (firstPageIndex strategy) pgSize
                  , mkPaginationLink strategy "last" baseUrl (lastPageIndex strategy page) pgSize]
      nextLinks  = [mkPaginationLink strategy "next" baseUrl (pgIndex + 1) pgSize | shouldGenNextLink strategy page]
      prevLinks  = [mkPaginationLink strategy "prev" baseUrl (pgIndex - 1) pgSize | shouldGenPrevLink strategy page]

{- |
If we are at the last page then we do not generate a next link. This function tells us whether to
generate a next link based on the page strategy.
-}
shouldGenNextLink :: Strategy -> Pagination -> Bool
shouldGenNextLink PageStrategy pagination =
  (getPageIndex . getPaginationPageIndex) pagination < numberOfPagesInPageList pagination
shouldGenNextLink OffsetStrategy pagination =
  (getPageIndex . getPaginationPageIndex) pagination < numberOfPagesInPageList pagination - 1

{- |
If we on the first page then we do not generate a prev link. This function tells us whether we can generate
a prev link.
-}
shouldGenPrevLink :: Strategy -> Pagination -> Bool
shouldGenPrevLink strategy pagination =
  (getPageIndex . getPaginationPageIndex) pagination > firstPageIndex strategy

{- |
This function calculates the number of pages in the list.
-}
numberOfPagesInPageList :: Pagination -> Word
numberOfPagesInPageList (Pagination _ pageSize resourceCount) =
  if resCount `mod` pgSize == 0
  then resCount `quot` pgSize
  else (resCount `quot` pgSize) + 1
    where
      pgSize     = getPageSize pageSize
      resCount   = getResourceCount resourceCount

{- |
Helper function used to generate a single pagination link.
-}
mkPaginationLink :: Strategy -> Rel -> URL -> Word -> Word -> (Rel, URL)
mkPaginationLink strategy key baseUrl pageNo pageSize =
  (key, link)
    where
      pageNoUrl = add_param baseUrl (strategyToQueryStringNumberKey strategy, show pageNo)
      link      = add_param pageNoUrl (strategyToQueryStringSizeKey strategy, show pageSize)

{- |
In the page strategy page numbering starts at 1, where as in the case of offset the numbering
starts at 0.
-}
firstPageIndex :: Strategy -> Word
firstPageIndex PageStrategy = 1
firstPageIndex OffsetStrategy = 0

lastPageIndex :: Strategy -> Pagination -> Word
lastPageIndex PageStrategy page = numberOfPagesInPageList page
lastPageIndex OffsetStrategy page = numberOfPagesInPageList page - 1

{- |
Simple pattern matcher than translates a Strategy to a query string element name.
-}
strategyToQueryStringNumberKey :: Strategy -> String
strategyToQueryStringNumberKey PageStrategy = "page[number]"
strategyToQueryStringNumberKey OffsetStrategy = "page[offset]"

strategyToQueryStringSizeKey :: Strategy -> String
strategyToQueryStringSizeKey PageStrategy = "page[size]"
strategyToQueryStringSizeKey OffsetStrategy = "page[limit]"
