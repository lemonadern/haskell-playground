import Data.Map (Map)

data State = S0 | S1 | S2
  deriving (Show)

data Input = A | B
  deriving (Show)

data Transition = Map (State, Input) State

doTransition :: (State, Input) -> State
doTransition _ = S0

main = do
  print S0
