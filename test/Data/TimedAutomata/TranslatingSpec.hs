{-# LANGUAGE OverloadedStrings #-}
module Data.TimedAutomata.TranslatingSpec (spec) where

import Test.Hspec
import Data.TimedAutomata.Translating
import qualified Data.GraphViz as Dot
import Data.GraphViz.Attributes.Complete

spec :: Spec
spec = do
  describe "translateState" $ do
    it "non-accepting state" $ do
      let input = 
              Dot.DotNode "q0" [UnknownAttribute "init" "1", UnknownAttribute "match" "0"] in 
        let expected =
              Dot.DotNode "q0" [Shape Circle] in
          translateState input `shouldBe` expected
    it "accepting state" $ do
      let input = 
              Dot.DotNode "q0" [UnknownAttribute "init" "0", UnknownAttribute "match" "1"] in 
        let expected =
              Dot.DotNode "q0" [Shape DoubleCircle] in
          translateState input `shouldBe` expected
  describe "translateTransition" $ do
    it "no guard transition" $ do
      let input = 
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a"), UnknownAttribute "reset" "{0}"] in 
        let expected =
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a / x0 := 0")] in
          translateTransition input `shouldBe` expected
    it "no reset transition" $ do
      let input = 
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a"), UnknownAttribute "guard" "{x0 < 10}"] in 
        let expected =
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a, x0 < 10")] in
          translateTransition input `shouldBe` expected
    it "guard reset transition" $ do
      let input = 
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a"), UnknownAttribute "guard" "{x0 >= 10}", UnknownAttribute "reset" "{0}"] in 
        let expected =
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a, x0 >= 10 / x0 := 0")] in
          translateTransition input `shouldBe` expected
    it "multiple guard reset transition" $ do
      let input = 
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a"), UnknownAttribute "guard" "{x0 >= 10, x1 < 5}", UnknownAttribute "reset" "{0,1}"] in 
        let expected =
              Dot.DotEdge "q0" "q1" [Label (StrLabel "a, x0 >= 10, x1 < 5 / x0 := 0, x1 := 0")] in
          translateTransition input `shouldBe` expected
