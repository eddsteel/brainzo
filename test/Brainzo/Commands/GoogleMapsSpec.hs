{-# LANGUAGE OverloadedStrings #-}
module Brainzo.Commands.GoogleMapsSpec where

import Test.Hspec
import Brainzo.Commands.GoogleMaps(searchURL)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "searchURL" $ do
    it "returns a simple query unchanged" $ do
      searchURL "coffee" `shouldBe` "https://www.google.com/maps/search/coffee"

    it "returns a longer query with spaces replaced with plus symbols" $ do
      searchURL "the best coffee in town" `shouldBe` "https://www.google.com/maps/search/the+best+coffee+in+town"
