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
