{-# LANGUAGE OverloadedStrings #-}

module Network.JSONApi.PaginationSpec where

import Data.Map (toList)
import Data.Maybe (fromJust)
import Network.JSONApi
import Network.URL (importURL)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Pagination" $ do
    it "should return mandatory keys" $ do
      let p = Pagination (PageIndex 2) (PageSize 10) (ResourceCount 30)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next", "prev"]

    it "should return proper hrefs for paging strategy" $ do
      let p = Pagination (PageIndex 2) (PageSize 10) (ResourceCount 30)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          links `shouldBe` [("first", "/users?page%5bsize%5d=10&page%5bnumber%5d=1"),
                            ("last", "/users?page%5bsize%5d=10&page%5bnumber%5d=3"),
                            ("next", "/users?page%5bsize%5d=10&page%5bnumber%5d=3"),
                            ("prev", "/users?page%5bsize%5d=10&page%5bnumber%5d=1")]

    it "should return proper hrefs for offset strategy" $ do
      let p = Pagination (PageIndex 1) (PageSize 10) (ResourceCount 30)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          links `shouldBe` [("first", "/users?page%5blimit%5d=10&page%5boffset%5d=0"),
                            ("last", "/users?page%5blimit%5d=10&page%5boffset%5d=2"),
                            ("next", "/users?page%5blimit%5d=10&page%5boffset%5d=2"),
                            ("prev", "/users?page%5blimit%5d=10&page%5boffset%5d=0")]

    it "should support the page strategy" $ do
      let p = Pagination (PageIndex 0) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          (snd . head) links `shouldBe` "/users?page%5bsize%5d=10&page%5bnumber%5d=1"

    it "should support the offset strategy" $ do
      let p = Pagination (PageIndex 0) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          (snd . head) links `shouldBe` "/users?page%5blimit%5d=10&page%5boffset%5d=0"

    it "should omit prev when we are on the first page of a PageStrategy" $ do
      let p = Pagination (PageIndex 1) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next"]

    it "should omit next when we are on the last page of a PageStrategy" $ do
      let p = Pagination (PageIndex 2) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "prev"]

    it "should omit prev when we are on the first page of a OffsetStrategy" $ do
      let p = Pagination (PageIndex 0) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next"]

    it "should omit next when we are on the last page of a OffsetStrategy" $ do
      let p = Pagination (PageIndex 1) (PageSize 10) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "/users") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "prev"]