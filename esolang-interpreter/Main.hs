module EsolangInterpreter where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                )
import           Data.List                      ( elemIndex )

interpreter :: String -> String -> String
interpreter code tape = loop LoopState { currentInstruction = 0
                                       , memory = map digitToInt tape
                                       , currentCell = 0
                                       }
 where
  loop loopState@LoopState { currentInstruction = currentInstruction, memory = memory, currentCell = currentCell }
    | currentInstruction >= length code
    = result
    | currentCell < 0 || currentCell >= length tape
    = result
    | otherwise
    = loop $ performInstruction code loopState
    where result = map intToDigit memory

flipBitAt :: Int -> [Int] -> [Int]
flipBitAt idx bits = concat [take idx bits, [flippedBit], drop (idx + 1) bits]
 where
  flippedBit = case lookup idx (zip [0 ..] bits) of
    Just 1  -> 0
    Just 0  -> 1
    Nothing -> error "Something wrong with your memory"

findMatchingBraceFor :: Char -> Int -> String -> Int
findMatchingBraceFor '[' from code =
  let (_, _, result) = foldl
        (\(idx, tally, result) x -> case result of
          Just idxOfMatch -> (-1, -1, Just idxOfMatch)
          Nothing         -> if x == '[' && idx > from
            then (idx + 1, tally + 1, Nothing)
            else if x == ']' && tally /= 0
              then (idx + 1, tally - 1, Nothing)
              else if x == ']' && tally == 0
                then (-1, -1, Just idx)
                else (idx + 1, tally, Nothing)
        )
        (0, 0, Nothing)
        code
  in  case result of
        Just matchingIdx -> matchingIdx + 1
        Nothing          -> error "Something wrong with your code"
findMatchingBraceFor ']' from code =
  let adjustedFrom   = length code - from - 1
      (_, _, result) = foldr
        (\x (idx, tally, result) -> case result of
          Just idxOfMatch -> (-1, -1, Just idxOfMatch)
          Nothing         -> if x == ']' && idx > adjustedFrom
            then (idx + 1, tally + 1, Nothing)
            else if x == '[' && tally /= 0
              then (idx + 1, tally - 1, Nothing)
              else if x == '[' && tally == 0
                then (-1, -1, Just idx)
                else (idx + 1, tally, Nothing)
        )
        (0, 0, Nothing)
        code
  in  case result of
        Just matchingIdx -> length code - matchingIdx - 1
        Nothing          -> error "Something wrong with your code"

instructionOfSymbol :: Char -> Instruction
instructionOfSymbol '>' = MoveRight
instructionOfSymbol '<' = MoveLeft
instructionOfSymbol '*' = Flip
instructionOfSymbol '[' = JumpForward
instructionOfSymbol ']' = JumpBack
instructionOfSymbol _   = Ignore

performInstruction :: String -> LoopState -> LoopState
performInstruction code LoopState { currentInstruction = currentInstruction, memory = memory, currentCell = currentCell }
  = case instructionOfSymbol $ code !! currentInstruction of
    MoveRight -> LoopState { currentInstruction = currentInstruction + 1
                           , memory             = memory
                           , currentCell        = currentCell + 1
                           }
    MoveLeft -> LoopState { currentInstruction = currentInstruction + 1
                          , memory             = memory
                          , currentCell        = currentCell - 1
                          }
    Flip -> LoopState { currentInstruction = currentInstruction + 1
                      , memory             = flipBitAt currentCell memory
                      , currentCell        = currentCell
                      }
    JumpForward -> if memory !! currentCell == 0
      then LoopState
        { currentInstruction = findMatchingBraceFor '[' currentInstruction code
        , memory             = memory
        , currentCell        = currentCell
        }
      else ignored
    JumpBack -> if memory !! currentCell == 1
      then LoopState
        { currentInstruction = findMatchingBraceFor ']' currentInstruction code
        , memory             = memory
        , currentCell        = currentCell
        }
      else ignored
    Ignore -> ignored
 where
  ignored = LoopState { currentInstruction = currentInstruction + 1
                      , memory             = memory
                      , currentCell        = currentCell
                      }

data LoopState = LoopState {
  currentInstruction :: Int,
  memory :: [Int],
  currentCell :: Int
}

data Instruction =
    MoveRight
  | MoveLeft
  | Flip
  | JumpForward
  | JumpBack
  | Ignore
