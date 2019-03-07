{-# LANGUAGE OverloadedStrings #-}
module Data.TimedAutomata.ParserSpec (spec) where

import Test.Hspec
import Data.TimedAutomata.Parser
import qualified Data.GraphViz as Dot
import Data.GraphViz.Attributes.Complete

spec :: Spec
spec = do
  describe "parse" $ do
    it "empty automaton" $ do
      let input = "digraph G {}" in 
        let expected =
              Dot.DotGraph False True (Just (Dot.Str "G")) (Dot.DotStmts [] [] [] []) in
          parse input `shouldBe` expected
    it "single state" $ do
      let input = "digraph G {q0 [init=1,match=0]}" in 
        let expected =
              Dot.DotGraph False True (Just (Dot.Str "G")) (Dot.DotStmts [] [] [
                                                               Dot.DotNode "q0" [UnknownAttribute "init" "1",
                                                                                 UnknownAttribute "match" "0"]
                                                                               ] []) in
          parse input `shouldBe` expected
    it "complex example" $ do
      let input = "digraph G {q0 [init=1,match=0];q1 [init=0,match=1]; q0 -> q1[label=\"a\",guard=\"{x0 >= 10}\",reset=\"{0}\"];}" in 
        let expected =
              Dot.DotGraph False True (Just (Dot.Str "G")) (Dot.DotStmts [] [] [
                                                               Dot.DotNode "q0" [UnknownAttribute "init" "1",
                                                                                 UnknownAttribute "match" "0"],
                                                                 Dot.DotNode "q1" [UnknownAttribute "init" "0",
                                                                                   UnknownAttribute "match" "1"]
                                                                               ] [
                                                               Dot.DotEdge "q0" "q1" [
                                                                   Label (StrLabel "a"),
                                                                   UnknownAttribute "guard" "{x0 >= 10}",
                                                                     UnknownAttribute "reset" "{0}"
                                                                   ]
                                                               ]) in
          parse input `shouldBe` expected
