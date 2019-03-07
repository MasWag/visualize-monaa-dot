module Data.TimedAutomata.Types where
import Data.GraphViz

type LabelType = String

type InputState = DotNode LabelType
type InputTransition = DotEdge LabelType
type InputTimedAutomaton = DotGraph LabelType

type OutputState = DotNode LabelType
type OutputTransition = DotEdge LabelType
type OutputTimedAutomaton = DotGraph LabelType
