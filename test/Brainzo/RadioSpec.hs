{-# LANGUAGE OverloadedStrings #-}
module Brainzo.RadioSpec where

import Test.Hspec
import Brainzo.Radio(icyFormat)
import Turtle
import qualified Data.Text as Text

main :: IO ()
main = hspec spec

testMatch :: Text -> Text
testMatch txt = case match icyFormat txt of
                  [] -> ""
                  as -> last as

testSed :: Text -> Text
testSed txt =
    let
      pattern' = fmap Text.concat
                 (many (icyFormat <|> fmap Text.singleton anyChar))
    in
      head $ match pattern' txt

-- ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';
spec :: Spec
spec = do
  describe "stream format" $ do
    it "matches when there's a title and a blank URL" $ do
      testMatch "ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';" `shouldBe` "Angus & Julia Stone - Old Friend"
    it "matches when there's a blank title and a blank URL" $ do
      testMatch "ICY Info: StreamTitle='';StreamUrl='';" `shouldBe` ""
    it "matches when there's a title and URL" $ do
      testMatch "ICY Info: StreamTitle='Zero 7 - The Road';StreamUrl='http://SomaFM.com/lush/';" `shouldBe` "Zero 7 - The Road"
    it "matches when there's a ' in the title" $ do
      testMatch "ICY Info: StreamTitle='DIIV - Mire (Grant's Song)';StreamUrl='';" `shouldBe` "DIIV - Mire (Grant's Song)"
    it "matches when there's a blank title and no URL" $ do
      testMatch "ICY Info: StreamTitle='';" `shouldBe` ""
  describe "sed with stream format" $ do
    it "matches when there's a title and a blank URL" $ do
      testSed "ICY Info: StreamTitle='Angus & Julia Stone - Old Friend';StreamUrl='';" `shouldBe` "Angus & Julia Stone - Old Friend"
    it "matches when there's a blank title and a blank URL" $ do
      testSed "ICY Info: StreamTitle='';StreamUrl='';" `shouldBe` ""
    it "matches when there's a title and URL" $ do
      testSed "ICY Info: StreamTitle='Zero 7 - The Road';StreamUrl='http://SomaFM.com/lush/';" `shouldBe` "Zero 7 - The Road"
    it "matches when there's a ' in the input" $ do
      testSed "ICY Info: StreamTitle='DIIV - Mire (Grant's Song)';StreamUrl='';" `shouldBe` "DIIV - Mire (Grant's Song)"
    it "matches when there's a blank title and no url" $ do
      testSed "ICY Info: StreamTitle='';" `shouldBe` ""
