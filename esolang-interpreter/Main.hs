module EsolangInterpreter where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( elemIndex )

data InterState = InterState {
  currentInstruction :: Int,
  memory :: [Int],
  currentCell :: Int,
  loopStack :: [Loop]
}

data Loop = Loop {
  index :: Int,
  executing :: Bool
}

data Instruction =
    MoveRight
  | MoveLeft
  | Flip
  | JumpForward
  | JumpBack
  | Ignore

moveRight :: InterState -> InterState
moveRight interState = interState
  { currentInstruction = currentInstruction interState + 1
  , currentCell        = currentCell interState + 1
  }

moveLeft :: InterState -> InterState
moveLeft interState = interState
  { currentInstruction = currentInstruction interState + 1
  , currentCell        = currentCell interState - 1
  }

flipIt :: InterState -> InterState
flipIt interState = interState
  { currentInstruction = currentInstruction interState + 1
  , memory             = flipBitAt (currentCell interState) (memory interState)
  }

jumpForward :: InterState -> InterState
jumpForward interState@InterState { memory = memory, currentCell = currentCell, currentInstruction = currentInstruction, loopStack = loopStack }
  = if memory !! currentCell == 0
    then interState
      { currentInstruction = currentInstruction + 1
      , loopStack = Loop { index = currentInstruction, executing = False }
                      : loopStack
      }
    else interState
      { currentInstruction = currentInstruction + 1
      , loopStack = Loop { index = currentInstruction, executing = True }
                      : loopStack
      }

jumpBack :: InterState -> InterState
jumpBack interState@InterState { memory = memory, currentCell = currentCell, currentInstruction = currentInstruction, loopStack = loopStack }
  = if memory !! currentCell == 1
    then interState { currentInstruction = index . head $ loopStack
                    , loopStack          = tail loopStack
                    }
    else interState { currentInstruction = currentInstruction + 1
                    , loopStack          = tail loopStack
                    }

ignoreIt :: InterState -> InterState
ignoreIt interState =
  interState { currentInstruction = currentInstruction interState + 1 }

interpreter :: String -> String -> String
interpreter code tape = execute InterState { currentInstruction = 0
                                           , memory = map digitToInt tape
                                           , currentCell        = 0
                                           , loopStack          = []
                                           }
 where
  execute interState
    | currentInstruction interState >= length code = result
    | currentCell interState < 0 || currentCell interState >= length tape = result
    | otherwise = execute $ performInstruction code interState
    where result = map intToDigit (memory interState)

flipBitAt :: Int -> [Int] -> [Int]
flipBitAt idx bits = concat [take idx bits, [flippedBit], drop (idx + 1) bits]
 where
  flippedBit = case lookup idx (zip [0 ..] bits) of
    Just 1  -> 0
    Just 0  -> 1
    Nothing -> error "Something wrong with your memory"

instructionOfSymbol :: Char -> Instruction
instructionOfSymbol '>' = MoveRight
instructionOfSymbol '<' = MoveLeft
instructionOfSymbol '*' = Flip
instructionOfSymbol '[' = JumpForward
instructionOfSymbol ']' = JumpBack
instructionOfSymbol _   = Ignore

performInstruction :: String -> InterState -> InterState
performInstruction code interState@InterState { currentInstruction = currentInstruction, loopStack = loopStack@(Loop { executing = False } : _) }
  = case instructionOfSymbol $ code !! currentInstruction of
    JumpForward -> interState
      { loopStack = Loop { executing = False, index = currentInstruction }
                      : loopStack
      , currentInstruction = currentInstruction + 1
      }
    JumpBack -> interState { loopStack          = tail loopStack
                           , currentInstruction = currentInstruction + 1
                           }
    _ -> ignoreIt interState
performInstruction code interState@InterState { currentInstruction = currentInstruction }
  = case instructionOfSymbol $ code !! currentInstruction of
    MoveRight   -> moveRight interState
    MoveLeft    -> moveLeft interState
    Flip        -> flipIt interState
    JumpForward -> jumpForward interState
    JumpBack    -> jumpBack interState
    Ignore      -> ignoreIt interState
