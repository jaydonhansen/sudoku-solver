module Test where

import Sudoku
import Data

import Data.Maybe

-- Print functions for Cell/Digit
printCell :: Cell -> Char
printCell c = case c of
  Unknown -> ' '
  Known D1 -> '1'
  Known D2 -> '2'
  Known D3 -> '3'
  Known D4 -> '4'


printDigit :: Digit -> Char
printDigit d = case d of
  D1 -> '1'
  D2 -> '2'
  D3 -> '3'
  D4 -> '4'


-- Pretty printing from the spec
prettyFour :: (a -> Char) -> Four a -> Four a -> String
prettyFour s (Four a b c d) (Four e f g h) =
  unlines
    [ "/--\\/--\\"
    , "|" ++ [s a, s b] ++ "||"
    ++ [s e, s f] ++ "|"
    , "|" ++ [s c, s d] ++ "||"
    ++ [s g, s h] ++ "|"
    , "\\--/\\--/"
    ]

prettyBoard :: (a -> Char) -> Board a -> String
prettyBoard s (Board (Four a b c d)) =
  prettyFour s a b ++ prettyFour s c d


-- Get a single result out of a Logic
get :: Logic a -> Maybe a
get a = unLogic a (const . Just) Nothing

-- Get all results out of a Logic
getAll :: Logic a -> [a]
getAll a = unLogic a (:) []

-- Prints test board before solving and after
testSudoku :: Logic (Board Digit) -> Board Cell -> IO ()
testSudoku s b = do
  putStrLn "Board before solving:"
  putStrLn $ prettyBoard printCell b
  putStrLn $ show (length (getAll s)) ++ " solution/s found."
  case get s of
    Just s -> do
      putStrLn "Here's the first solution:"
      putStrLn $ prettyBoard printDigit (fromJust (Just s))
    Nothing -> putStrLn "There aren't any valid solutions so I can't give you one. You're a terrible person."
  -- If you really insist on printing all of them
  -- putStrLn $ concatMap (prettyBoard printDigit) (getAll s)

-- Some test boards
testBoard1 = Board
    (Four
    (Four (Known D4) Unknown (Known D2) Unknown)
    (Four (Known D2) (Known D1) (Known D4) (Known D3))
    (Four (Known D3) (Known D4) (Known D1) (Known D2))
    (Four (Known D1) (Known D2) (Known D3) (Known D4)))

testBoard2 = Board
    (Four
    (Four Unknown Unknown (Known D2) Unknown)
    (Four Unknown (Known D1) (Known D4) (Known D3))
    (Four Unknown Unknown (Known D1) (Known D2))
    (Four Unknown Unknown Unknown (Known D4)))

testBoard3 = Board
    (Four
    (Four Unknown Unknown Unknown Unknown)
    (Four Unknown Unknown Unknown Unknown)
    (Four Unknown Unknown (Known D4) Unknown)
    (Four Unknown Unknown Unknown (Known D2)))

testEmptyBoard = Board
    (Four
    (Four Unknown Unknown Unknown Unknown)
    (Four Unknown Unknown Unknown Unknown)
    (Four Unknown Unknown Unknown Unknown)
    (Four Unknown Unknown Unknown Unknown))