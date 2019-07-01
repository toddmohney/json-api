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
      let p = Pagination (PageSize 10) (PageNum 2) (ResourceCount 30)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next", "prev"]

    it "should return proper hrefs for paging strategy" $ do
      let p = Pagination (PageSize 10) (PageNum 2) (ResourceCount 30)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          links `shouldBe` [("first", "http://localhost/?page%5bsize%5d=10&page%5bnumber%5d=1"),
                            ("last", "http://localhost/?page%5bsize%5d=10&page%5bnumber%5d=3"),
                            ("next", "http://localhost/?page%5bsize%5d=10&page%5bnumber%5d=3"),
                            ("prev", "http://localhost/?page%5bsize%5d=10&page%5bnumber%5d=1")]

    it "should return proper hrefs for offset strategy" $ do
      let p = Pagination (PageSize 10) (PageNum 1) (ResourceCount 30)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          links `shouldBe` [("first", "http://localhost/?page%5blimit%5d=10&page%5boffset%5d=0"),
                            ("last", "http://localhost/?page%5blimit%5d=10&page%5boffset%5d=2"),
                            ("next", "http://localhost/?page%5blimit%5d=10&page%5boffset%5d=2"),
                            ("prev", "http://localhost/?page%5blimit%5d=10&page%5boffset%5d=0")]

    it "should support the page strategy" $ do
      let p = Pagination (PageSize 10) (PageNum 0) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          (snd . head) links `shouldBe` "http://localhost/?page%5bsize%5d=10&page%5bnumber%5d=1"

    it "should support the offset strategy" $ do
      let p = Pagination (PageSize 10) (PageNum 0) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          (snd . head) links `shouldBe` "http://localhost/?page%5blimit%5d=10&page%5boffset%5d=0"

    it "should omit prev when we are on the first page of a PageStrategy" $ do
      let p = Pagination (PageSize 10) (PageNum 1) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next"]

    it "should omit next when we are on the last page of a PageStrategy" $ do
      let p = Pagination (PageSize 10) (PageNum 2) (ResourceCount 20)
      let results = mkPaginationLinks PageStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "prev"]

    it "should omit prev when we are on the first page of a OffsetStrategy" $ do
      let p = Pagination (PageSize 10) (PageNum 0) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "next"]

    it "should omit next when we are on the last page of a OffsetStrategy" $ do
      let p = Pagination (PageSize 10) (PageNum 1) (ResourceCount 20)
      let results = mkPaginationLinks OffsetStrategy (fromJust $ importURL "http://localhost") p
      case results of
        Links lm -> do
          let links = toList lm
          map fst links `shouldBe` ["first", "last", "prev"]