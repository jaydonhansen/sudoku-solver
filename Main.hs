-- Currently main just runs the tests on the boards in Test.hs. 
-- Feel free to paste in your own test Board Cell and give it a spin to make that Logic printer go brrr.

module Main where

import Sudoku
import Data
import Test

main :: IO ()
main = runTests

-- Note: tests are visual unit tests only. If I had more time I would implement randomized tests.
runTests :: IO ()
runTests = do
  putStrLn "Testing with board 1..."
  testSudoku (sudoku testBoard1) testBoard1
  putStrLn "Testing with board 2..."
  testSudoku (sudoku testBoard2) testBoard2
  putStrLn "Testing with board 3..."
  testSudoku (sudoku testBoard3) testBoard3
  putStrLn "Testing with empty board..."
  testSudoku (sudoku testEmptyBoard) testEmptyBoard