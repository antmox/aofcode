#!/usr/bin/env runhaskell

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split -- libghc-split-dev
import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Debug.Trace
import Text.Printf
import Numeric
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)


-------------------------------------------------------------------
-------------------------------------------------------------------

-- main =
--   getArgs >>= getInput >>= return . solve2001_1 >>= print
--   where
--     getInput (l:_) = readFile l
--     getInput []    = getContents

-- :load adv22.hs
-- readFile "inputs/2201.in" >>= return . solve2201_1

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 3 0 3 7 3      # . . . . .
-- 2 5 5 1 2      # . . .   .
-- 6 5 3 3 2  ->  # . .   . .  ->  21
-- 3 3 5 4 9      # .   .   .
-- 3 5 3 9 0      # . . . . .

isvisibletree grid cmax (x, y) =
  -- visible from W
  (all (\nx -> smaller nx y) [  0  ..(x-1 )]) ||
  -- visible from E
  (all (\nx -> smaller nx y) [(x+1)..(cmax)]) ||
  -- visible from N
  (all (\ny -> smaller x ny) [  0  ..(y-1 )]) ||
  -- visible from S
  (all (\ny -> smaller x ny) [(y+1)..(cmax)])
  where smaller nx ny = (((M.!) grid (x, y)) > ((M.!) grid (nx, ny)))

-- 1870
solve2208_1 input =
  length . filter (isvisibletree grid cmax) . M.keys $ grid
  where
    grid = M.fromList . concat
      . zipWith (\y -> map (\(x, v) -> ((x, y), v))) [0..]
      . map (zip [0..]) . lines $ input
    cmax = fst . maximum . M.keys $ grid

-- 3 0 3 7 3      # . . . . .
-- 2 5 5 1 2      # . 1 4 1 .
-- 6 5 3 3 2  ->  # . 6 1 2 .  ->  8
-- 3 3 5 4 9      # . 1 8 3 .
-- 3 5 3 9 0      # . . . . .

-- 517440
scenicscore grid cmax (x, y)
  | (x == 0 || x == cmax || y == 0 || y == cmax) = 0
  | otherwise =
      foldl1 (*) . map (succ . length) $ [
        -- looking W
        takeWhile (\nx -> smaller nx y) . reverse $ [  1  ..(x-1   )],
        -- looking E
        takeWhile (\nx -> smaller nx y)           $ [(x+1)..(cmax-1)],
        -- looking N
        takeWhile (\ny -> smaller x ny) . reverse $ [  1  ..(y-1   )],
        -- looking S
        takeWhile (\ny -> smaller x ny)           $ [(y+1)..(cmax-1)] ]
  where smaller nx ny = (((M.!) grid (x, y)) > ((M.!) grid (nx, ny)))

solve2208_2 input =
  maximum . map (scenicscore grid cmax) . M.keys $ grid
  where
    grid = M.fromList . concat
      . zipWith (\y -> map (\(x, v) -> ((x, y), v))) [0..]
      . map (zip [0..]) . lines $ input
    cmax = fst . maximum . M.keys $ grid


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

walk2207 (cwd, dsz) line @ ("$" : "cd" : d : [])
  | d == "/"  = ([]      , dsz)
  | d == ".." = (tail cwd, dsz)
  | otherwise = (d : cwd , dsz)
walk2207 (cwd, dsz) line @ (fwd @ (l : _) : _)
  | not . isDigit $ l = (cwd, dsz)
  | otherwise         = (cwd, foldl (\dsz dirs ->
       (M.insert dirs ((toInt fwd) + (M.findWithDefault 0 dirs dsz)) dsz)
                         ) dsz . tails $ cwd)

-- 1749646
solve2207_1 =
  sum . filter (< 100000) .
  M.elems . snd . foldl walk2207 ([], M.empty) . map words . lines

-- 1498966
solve2207_2 input =
  minimum . filter (> needed) . M.elems $ dsz
  where
    dsz = snd . foldl walk2207 ([], M.empty) . map words . lines $ input
    needed = 30000000 - (70000000 - ((M.!) dsz []))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

findmark size =
  (+ size) . fromJust
  . findIndex ( (==) size . length . nub )
  . divvy size 1 . head . lines

-- 1647
solve2206_1 = findmark 4

-- 2447
solve2206_2 = findmark 14


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

input2205 input =
  (stacks, instrs)
  where
    stacks_ : instrs_ : _ =
      splitOn "\n\n" $ input
    stacks =
      Seq.fromList
      . map (filter isLetter)
      . transpose
      . map (map (head . drop 1) . chunksOf 4)
      . lines $ stacks_
    instrs =
      map (map toInt . wordsBy (not . isDigit))
      . lines $ instrs_

-- (head . drop 1) == (flip (!!) 1)

-- QNHWJVJZW
solve2205_1 =
  map head . toList . uncurry (foldl move) . input2205
  where
    move stacks (nb : from : to : _) = let
      values  =
        reverse $ take nb ((Seq.index) stacks (from - 1))
      stacks1 =
        Seq.update (from - 1) (drop nb ((Seq.index) stacks (from - 1))) stacks
      stacks2 =
        Seq.update (to   - 1) (values ++ ((Seq.index) stacks1 (to - 1))) stacks1
      in stacks2

-- BPCZJLFJW
solve2205_2 =
  map head . toList . uncurry (foldl move) . input2205
  where
    move stacks (nb : from : to : _) = let
      values  =
        take nb ((Seq.index) stacks (from - 1))
      stacks1 =
        Seq.update (from - 1) (drop nb ((Seq.index) stacks (from - 1))) stacks
      stacks2 =
        Seq.update (to   - 1) (values ++ ((Seq.index) stacks1 (to - 1))) stacks1
      in stacks2


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 475
solve2204_1 =
  length . filter (overlap . map toInt . splitOne "-,") . lines
  where
    overlap (p11 : p12 : p21 : p22 : []) =
      (p11 >= p21 && p12 <= p22) ||
      (p21 >= p11 && p22 <= p12)

-- 825
solve2204_2 =
  length . filter (overlap . map toInt . splitOne "-,") . lines
  where
    overlap (p11 : p12 : p21 : p22 : []) =
      (p11 >= p21 && p11 <= p22) ||
      (p12 >= p21 && p12 <= p22) ||
      (p21 >= p11 && p21 <= p12) ||
      (p22 >= p11 && p22 <= p12)

--

splitOne = split . dropDelims . condense . oneOf


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 8394
solve2203_1 =
  sum . map (priort . head . uncurry intersect . split2) . lines
  where
    split2 x = splitAt ((`div` 2) . length $ x) x
    priort c
      | inRange ('a', 'z') c = (ord c) - (ord 'a') + 1
      | inRange ('A', 'Z') c = (ord c) - (ord 'A') + 27

-- 2413
solve2203_2 =
  sum . map (priort . head . foldl1 intersect) . chunksOf 3 . lines
  where
    priort c
      | inRange ('a', 'z') c = (ord c) - (ord 'a') + 1
      | inRange ('A', 'Z') c = (ord c) - (ord 'A') + 27


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- rock      0 defeats 2   scissors
-- paper     1 defeats 0   rock
-- scissors  2 defeats 1   paper

-- SHAPE_PTS : 1 for Rock, 2 for Paper, and 3 for Scissors
-- OUTCM_PTS : 0 if you lost, 3 if the round was a draw, and 6 if you won

-- 10718
solve2202_1 =
  sum . map score . lines
  where
    score ('A' : _ : 'X' : []) = 1 + 3
    score ('A' : _ : 'Y' : []) = 2 + 6
    score ('A' : _ : 'Z' : []) = 3 + 0

    score ('B' : _ : 'X' : []) = 1 + 0
    score ('B' : _ : 'Y' : []) = 2 + 3
    score ('B' : _ : 'Z' : []) = 3 + 6

    score ('C' : _ : 'X' : []) = 1 + 6
    score ('C' : _ : 'Y' : []) = 2 + 0
    score ('C' : _ : 'Z' : []) = 3 + 3

-- X means you need to lose
-- Y means you need to end the round in a draw
-- Z means you need to win

-- 14652
solve2202_2 =
  sum . map score . lines
  where
    score ('A' : _ : 'X' : []) = 3 + 0 -- scissors
    score ('A' : _ : 'Y' : []) = 1 + 3 -- rock
    score ('A' : _ : 'Z' : []) = 2 + 6 -- paper

    score ('B' : _ : 'X' : []) = 1 + 0 -- rock
    score ('B' : _ : 'Y' : []) = 2 + 3 -- paper
    score ('B' : _ : 'Z' : []) = 3 + 6 -- scissor

    score ('C' : _ : 'X' : []) = 2 + 0 -- paper
    score ('C' : _ : 'Y' : []) = 3 + 3 -- scissors
    score ('C' : _ : 'Z' : []) = 1 + 6 -- rock


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2022 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 72240
solve2201_1 =
  maximum . map ( sum . map toInt . lines )
  . splitOn "\n\n"

-- 210957
solve2201_2 =
  sum . take 3 . reverse . sort . map ( sum . map toInt . lines )
  . splitOn "\n\n"


-- -------------------------------------------------------------------

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
