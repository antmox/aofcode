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
  maximum . map ( sum . map toInt . lines ) . splitOn "\n\n"

-- 210957
solve2201_2 =
  sum . take 3 . reverse . sort . map ( sum . map toInt . lines ) . splitOn "\n\n"


-- -------------------------------------------------------------------

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
