module Data.TimedAutomata.Printer where
import Data.TimedAutomata.Types
import Data.GraphViz.Printing
import Data.Text.Lazy

-- |
-- Print the timed automaton
-- Maybe instanciating Show is better
print :: OutputTimedAutomaton -> Text
print = printIt -- "Hello World!!"

