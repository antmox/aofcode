#!/usr/bin/env runhaskell

import Data.Bits
import Data.Char
import Data.List
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

-- :load adv21.hs
-- readFile "inputs/2101.in" >>= return . solve2101_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 1071734 : (
solve2103_1 =
  foldl1 (*) . map bin2dec . transpose
  . map (map fst . sortOn snd . map (\x -> (head x, length x)) . group . sort)
  . transpose . lines

-- 6124992
solve2103_2 input =

  (bin2dec $ loopr 0 0 inlst) * (bin2dec $ loopr 1 0 inlst)

  where
    inlst = lines input

    rankb lst =
      if (nb '0' lst) > (nb '1' lst) then "10" else "01"
      where nb x = length . filter (== x)

    filtr num rnk lst =
      filter ((== bit) . (!! num)) lst
      where bit = (!! rnk) . rankb . map (!! num) $ lst

    loopr rnk num lst
      | length lst == 1 = head lst
      | otherwise       =
        loopr rnk (num + 1) (filtr num rnk lst)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 1714950
solve2102_1 =
  uncurry (*) . foldl nextpos (0, 0) . map words . lines
 where
   nextpos (hpos, dpth) (dir : nb : [])
     | dir == "forward" = ((hpos + n), dpth)
     | dir == "up"      = (hpos, (dpth - n))
     | dir == "down"    = (hpos, (dpth + n))
     | otherwise        = undefined
     where n = toInt nb

-- 1281977850
solve2102_2 =
  (\(hpos, dpth, _) -> (hpos * dpth)) .
  foldl nextpos (0, 0, 0) . map words . lines
 where
   nextpos (hpos, dpth, aim) (dir : nb : [])
     | dir == "forward" = (hpos + n, dpth + aim * n, aim)
     | dir == "up"      = (hpos, dpth, aim - n)
     | dir == "down"    = (hpos, dpth, aim + n)
     | otherwise        = undefined
     where n = toInt nb


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2020 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 1676
solve2101_1 =
  nbincx . map toInt . lines
  where
    nbincx xs = length . filter (uncurry (<)) $ zip xs (tail xs)

-- 1706
solve2101_2 =
  nbinc . sumth . map toInt . lines
  where
    nbinc xs = length . filter (uncurry (<)) $ zip xs (tail xs)
    sumth xs = zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)


-- -------------------------------------------------------------------

-- combinations m xs =
--   combsBySize xs !! m
--   where
--     combsBySize = foldr f ([[]] : repeat [])
--     f x next = zipWith (++) (map (map (x:)) ([]:next)) next

toInt :: String -> Int
toInt x = read x :: Int

-- https://stackoverflow.com/a/5921593
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where c2i c = if c == '0' then 0 else 1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
