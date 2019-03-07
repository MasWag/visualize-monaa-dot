module Data.TimedAutomata.Parser where
import Data.TimedAutomata.Types
import Data.Text.Lazy
import Data.GraphViz.Types

parse :: Text -> InputTimedAutomaton
parse = parseDotGraph

