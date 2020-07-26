-- 
-- Credits to Tom Manderson for finally helping me understand what Logic does

module Sudoku where

import Data

import Data.List
import Control.Monad
import Data.Foldable


-- Generates unique pairs for each Hole in the block, and generates constraints for each
blockToConstraints :: [Hole] -> [Constraint]
blockToConstraints l = fmap notEqual (pairs l) where
  notEqual (x, y) = NotEqual x y
  pairs l         = [(x,y) | (x:ys) <- tails l, y <- ys]


-- Generates constraints for the entire board based on blocks of rows, columns and squares
generateConstraints :: Board Hole -> [Constraint]
generateConstraints (Board (Four
  (Four a b c d)
  (Four e f g h)
  (Four i j k l)
  (Four m n o p))) = do
    let blocks = [[a, b, c, d], [e, f, g, h], [i, k, j, l], [m, n, o, p],
                  [a, b, e, f], [c, d, g, h], [i, j, m, n], [k, l, o, p],
                  [a, c, i, k], [b, d, j, l], [e, g, m, o], [f, h, n, p]]
    foldMap blockToConstraints blocks


-- Turns known types to Concrete types, creates variables for unknown cells
cellToHole :: Cell -> State Int Hole
-- i is the best name for an iterator
cellToHole Unknown   = State (\i -> (Variable i, i + 1))
cellToHole (Known n) = State (\i -> (Concrete n, i))

-- Helper function to use in substitute and instantiate
sub :: Int -> Digit -> Hole -> Hole
sub i d h = case h of
    -- If the i matches the number of the Variable, then replace it with a Concrete Digit
    Variable v -> if v == i then Concrete d else Variable v
    -- Do nothing to Concretes
    Concrete n -> Concrete n

-- Now since Board conveniently has Functor, we can just fmap it
substitute :: Int -> Digit -> Board Hole -> Board Hole
substitute i d bh = sub i d <$> bh

-- We can't just fmap NotEqual but we still use sub here
instantiate :: Int -> Digit -> Constraint -> Constraint
instantiate i d (NotEqual v w) = NotEqual (sub i d v) (sub i d w)


-- Converts an entire board of Concretes into Digits
holesToDigits :: Board Hole -> Board Digit
holesToDigits = fmap holeToDigit where
  holeToDigit (Concrete d) = d
  holeToDigit (Variable v) = error "Variable still in board"


-- Asserts whether all constraints are correct or not
evaluateConstraints :: [Constraint] -> Bool
evaluateConstraints       = all notEqual where
  notEqual (NotEqual v w) = v /= w

-- Attempts to find the next Variable. Skips Concretes, and returns Nothing if there are no variables left
findNext :: [Hole] -> Maybe Int
findNext []     = Nothing -- empty list for base case
findNext (x:xs) = case x of
  Variable i -> Just i
  _          -> findNext xs -- skip 


solver :: [Constraint] -> Board Hole -> Logic (Board Digit)
-- If the constraints are already violated, there's no point trying to substitute stuff in
solver constraints board = if evaluateConstraints constraints
  then case findNext (toList board) of
    Just i -> do
      choice <- Logic $ \_c n -> foldr _c n [D1, D2, D3, D4] -- Logic "List" of possible choices
      solver (fmap (instantiate i choice) constraints) (substitute i choice board)

    -- If there are no variables left, return the completed board
    Nothing -> return (holesToDigits board)
  else Logic $ \_c n -> n


-- Gets the 'value' of the state
evalState :: State s a -> s -> a
evalState m s = fst (runState m s)


-- Convert Cells to Holes, generate the list of Constraints, then run the solver
sudoku :: Board Cell -> Logic (Board Digit)
sudoku b = do
  let board       = evalState (traverse cellToHole b) 0
  let constraints = generateConstraints board
  solver constraints board
