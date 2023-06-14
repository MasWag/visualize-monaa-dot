{-# LANGUAGE OverloadedStrings #-}
module Data.TimedAutomata.Translating where
import Data.TimedAutomata.Types
import qualified Data.GraphViz as Dot
import Data.GraphViz.Attributes.Complete
import Data.List
import qualified Data.Text.Lazy as T
import Data.Maybe

translateState :: InputState -> OutputState
translateState (Dot.DotNode nodeID attributes) =
  Dot.DotNode nodeID ((Shape shape):event)
  where
    event = case find (sameAttribute (Label (StrLabel ""))) attributes of
      Nothing -> [Label (StrLabel $ T.pack $ nodeID)]
      Just (Label (StrLabel str)) -> [Label (StrLabel $ T.append (T.pack $ nodeID ++ ", ") str)]
      Just _ -> [Label (StrLabel $ T.pack $ nodeID)]
    shape = case find (sameAttribute (UnknownAttribute "match" "1")) attributes of
      Nothing -> Circle
      Just (UnknownAttribute "match" "0") -> Circle
      Just (UnknownAttribute "match" _) -> DoubleCircle
      Just _ -> Circle

translateTransition :: InputTransition -> OutputTransition
translateTransition (Dot.DotEdge fromNode toNode attributes) =
  Dot.DotEdge fromNode toNode [Label (StrLabel label)]
  where
    label = T.concat [event, guardSep, guard, resetSep, reset]
    event = case find (sameAttribute (Label (StrLabel ""))) attributes of
      Nothing -> ""
      Just (Label (StrLabel str)) -> str
      Just _ -> ""
    guardSep = if guard == "" || event == "" then "" else ", "
    resetSep = if reset == "" then "" else " / "
    guard = case find (sameAttribute (UnknownAttribute "guard" "")) attributes of
      Nothing -> ""
      Just (UnknownAttribute "guard" str) ->
        if (T.head str) == '{' && (T.last str) == '}' then
          T.init $ T.tail str
        else
          ""
      Just _ -> ""
    reset = case find (sameAttribute (UnknownAttribute "reset" "")) attributes of
      Nothing -> ""
      Just (UnknownAttribute "reset" str) -> 
        if (T.head str) == '{' && (T.last str) == '}' then
          foldl (\l r -> if l == "" then r else T.concat [l, ", " ,r]) "" $ map (\n -> T.concat ["x", n, " := 0"]) $ T.split (==',') $ T.init $ T.tail str
        else
          ""
      Just _ -> ""

translateTimedAutomaton :: InputTimedAutomaton -> Maybe OutputTimedAutomaton
translateTimedAutomaton (Dot.DotGraph False True graphID (Dot.DotStmts _ _ nodes edges)) =
  Just $ Dot.DotGraph False True graphID (Dot.DotStmts [] [] (translatedNodes ++ dummyInitialStates) (translatedEdges ++ dummyInitialEdges))
  where
    translatedNodes = map translateState nodes
    initialStates = filter (\ (Dot.DotNode _ attributes) -> 
                              isJust $ find (== UnknownAttribute "init" "1") attributes) nodes
    dummyInitialStates = map (\ (Dot.DotNode nodeID _) ->
                                 Dot.DotNode ("dummy" ++ nodeID)
                                 [Shape PlainText, Label (StrLabel "")]) initialStates
    translatedEdges = map translateTransition edges
    dummyInitialEdges = map (\ (Dot.DotNode nodeID _) ->
                                (Dot.DotEdge ("dummy" ++ nodeID) nodeID []) ) initialStates
translateTimedAutomaton _ = Nothing
