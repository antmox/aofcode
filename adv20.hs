#!/usr/bin/env runhaskell

import Data.Bits
import Data.Char
import Data.List
import Data.Array
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Text.Printf
import Numeric
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.MD5
import Data.List.Split
import System.Environment (getArgs)
import Test.HUnit
import Data.Foldable


-------------------------------------------------------------------
-------------------------------------------------------------------

main =
  getArgs >>= getInput >>= return . solve2001_1 >>= print
  where
    getInput (l:_) = readFile l
    getInput []    = getContents

-- readFile "inputs/2001.in" >>= return . solve2001_1

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 25
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

solve2025_1 input =
  head . drop (loop_size card) $ iterate' (one_loop door) 1
  where
    card : door : _ = map toInt . lines $ input
    one_loop sub    = flip (mod) 20201227 . (*) sub
    loop_size key   =
      length . takeWhile (/= key) $ iterate' (one_loop 7) 1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- XXXXXXXXX
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- XXX

getrule (x1 : x2 : _ : _ : "no" : _ : _ : []) = 1
getrule (x1 : x2 : _ : _ : x3 : x4 : x5 : _ : []) = 1
getrule (x1 : x2 : _ : _ : x3 : x4 : x5 : _ : x6 : x7 : x8 : _ : []) = 1

solve2007_1 =
  map (getrule . words) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 6947
solve2006_1 =
  sum . map (length . nub . concat . splitOn "\n") . splitOn "\n\n"

-- 3398
solve2006_2 =
  sum . map (length . foldl1 intersect . splitOn "\n") . splitOn "\n\n"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

seatprt min _   []      = min
seatprt min max (x : xs)
  | x == 'F' || x == 'L' = seatprt min mid xs
  | x == 'B' || x == 'R' = seatprt (mid + 1) max xs
  where mid = (div) (min + max) 2

seatdec = (\(r, s) -> (seatprt 0 127 r, seatprt 0 7 s)) . splitAt 7

seatcid r s = r * 8 + s

-- 947
solve2005_1 = maximum . map (uncurry seatcid . seatdec) . lines

-- 636
solve2005_2 input =
  head
  . filter (flip elem brd_ids . ((subtract) 1))
  . filter (flip elem brd_ids . ((+) 1))
  $ all_ids \\ brd_ids
  where
    all_ids = [seatcid r s | r <- [1..126], s <- [0..7]]
    brd_ids = map (uncurry seatcid . seatdec) . lines $ input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 196
solve2004_1 =
  length
  . filter (S.isSubsetOf (S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) . M.keysSet)
  . map (M.fromList . (\xs -> zip xs (tail xs)) . splitOne " \n:")
  . splitOn "\n\n"

--
solve2004_2 =
  length
  . filter (all (uncurry check) . M.toList)
  . filter (S.isSubsetOf (S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) . M.keysSet)
  . map (M.fromList . (\xs -> zip xs (tail xs)) . splitOne " \n:")
  . splitOn "\n\n"
  where
    check _ _ = True -- TODO XXX

    -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
    -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    -- hgt (Height) - a number followed by either cm or in:
    --     If cm, the number must be at least 150 and at most 193.
    --     If in, the number must be at least 59 and at most 76.
    -- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    -- pid (Passport ID) - a nine-digit number, including leading zeroes.
    -- cid (Country ID) - ignored, missing or not.


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

trav2003 (sx, sy) (x, y) grid
  | y >= (length grid) = 0
  | curc == '#'        = 1 + next
  | otherwise          = next
  where
    curc = grid !! y !! (mod x (length (grid !! 0)))
    next = trav2003 (sx, sy) (x + sx, y + sy) grid

-- 254
solve2003_1 = trav2003 (3, 1) (0, 0) . lines

-- 1666768320
solve2003_2 input =
  foldl (*) 1 .
  map (\slope -> trav2003 slope (0, 0) grid) $ slopes
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    grid = lines $ input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 546
solve2002_1 =
  length . filter (isvalid . splitOne " -:") . lines
  where
    isvalid ( l : h : c : s : [] ) =
      (\n -> n >= (toInt l) && n <= (toInt h)) . length . filter (flip elem c) $ s

-- 275
solve2002_2 =
  length . filter (isvalid . splitOne " -:") . lines
  where
    chinpos c s =
      ((==) c) . ((!!) s) . subtract 1 . toInt
    isvalid ( l : h : [c] : s : [] ) =
      chinpos c s l /= chinpos c s h


-- -------------------------------------------------------------------

splitOne = split . dropDelims . condense . oneOf


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 605364
solve2001_1 = foldl (*) 1 . head . filter ((== 2020) . sum) . combinations 2 . map (toInt) . lines

-- 128397680
solve2001_2 = foldl (*) 1 . head . filter ((== 2020) . sum) . combinations 3 . map (toInt) . lines


-- -------------------------------------------------------------------

combinations m xs =
  combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
