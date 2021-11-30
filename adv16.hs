#!/usr/bin/env runhaskell

import Data.Bits
import Data.Char
import Data.List
import Data.Array
import Data.Maybe
import Debug.Trace
import Text.Printf
import Numeric
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import Data.Digest.Pure.MD5
import System.Environment (getArgs)
import Test.HUnit
import Data.Foldable

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

main =
  getArgs >>= getInput >>= return . solve161_1 >>= print
  where
    getInput (l : _) = readFile l
    getInput []      = getContents

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- readFile "inputs/1501.in" >>= return . solve151_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 25
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

newval2 vs ("cpy": x1: x2: [])
  | isInteger x1            = M.insert x2 (read x1) vs
  | isJust $ M.lookup x1 vs = M.insert x2 (fromJust $ M.lookup x1 vs) vs
  | otherwise               = vs
newval2 vs ("inc": x1:     [])
  | isJust $ M.lookup x1 vs = M.adjust (+1) x1 vs
  | otherwise               = vs
newval2 vs ("dec": x1:     [])
  | isJust $ M.lookup x1 vs = M.adjust (-1+) x1 vs
  | otherwise               = vs
newval2 vs ("mul": x1: x2: x3: []) =
  M.insert x1 (v2 * v3) vs
  where
    v2 = M.findWithDefault 0 x2 vs
    v3 = M.findWithDefault 0 x3 vs
newval2 vs ("jnz": x1: x2: []) = vs
newval2 vs ("nop": [])         = vs
newval2 vs ("out": x1: [])     = nvs
  where
    nvs = M.insert "o" new vs
    old = M.findWithDefault 0 "o" vs
    val = M.findWithDefault 0 x1 vs
    new = old * 2 + val
newval2 vs x = error (show x)

newidx2 vs ("jnz": x1: x2: []) i
  | isInteger x1        = res (read x1)
  | otherwise           = res (M.findWithDefault 0 x1 vs)
  where
    v2 = if (isInteger x2) then (read x2) else (M.findWithDefault 0 x2 vs)
    res x = if x == 0 then (i + 1) else (i + v2)
newidx2 _ _ i = i + 1

newlst2 ls vs i ("tgl": x1: []) = nwl
  where
    off = M.findWithDefault 0 x1 vs
    nwl = toggle ls (off + i)
    toggle ls idx
      | idx >= length ls = ls
      | otherwise        =
        (take (idx) ls) ++ [toggli ((!!) ls idx)] ++ (drop (idx + 1) ls)
    toggli ("inc": xs) = ("dec": xs)
    toggli ("dec": xs) = ("inc": xs)
    toggli ("cpy": xs) = ("jnz": xs)
    toggli ("jnz": xs) = ("cpy": xs)
    toggli ("tgl": xs) = ("inc": xs)
newlst2 ls _ _ _ = ls

-- 0b0101010101010101010101010101010101 5726623061

-- answer part one: 198
solve1625_1 s =
  loop 0 insn
  where
    insn = map ( words ) $ lines s

    loop x insn
      | sigok == True = x
      | otherwise     = trace (show (x)) loop (x+1) insn
      where sigok = (\l -> process l (M.fromList [("a", x)]) 0) $ insn

    process ls vs i
      | length ls <= i = False
      | M.findWithDefault 0 "o" vs == 5726623061 = True
      | M.findWithDefault 0 "o" vs  > 5726623061 = False
      | otherwise = process (newlst2 ls vs i ins) (newval2 vs ins) (newidx2 vs ins i)
      where ins = (!!) ls i


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 24
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

combinations m xs =
  combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

possibleMoves grid (x, y) =
  filter isPossible [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where isPossible (x, y) = (/= '#') $ (grid !! y !! x)

visitG grid target cache (((x, y), l):vlist)
  | (== target) $ grid !! y !! x = length l
  | elem (x, y) cache            = visitG grid target cache vlist
  | otherwise                    =
      visitG grid target ((x, y):cache)
      (vlist ++ ([((nx, ny), ((x, y):l)) | (nx, ny) <- (possibleMoves grid (x, y))]))

-- answer part one: 502
solve1624_1 s =
  trace (concat . map (++"\n") $ grid)
  minimum $ map dstp pths
  where
    grid = lines s
    xmax = (-1+) $ length $ (!! 0) $ grid
    ymax = (-1+) $ length $ grid
    gcrd = [((x, y), grid !! y !! x) | x <- [0..xmax], y <- [0..ymax]]
    posx = M.fromList $ map (\(x, y) -> (y, x)) $ filter (isDigit . snd) gcrd
    cmbs = combinations 2 $ sort $ M.keys posx
    dsts = foldl (\acc cmb -> M.insert cmb (dstx grid cmb) acc) M.empty cmbs
    dstx grid (x1:x2:[]) = visitG grid x2 [] [(fromJust $ M.lookup x1 posx, [])]
    pths = map ('0':) $ permutations $ sort $ filter (/= '0') $ M.keys posx --
    dstp (x1:x2:xs) = sum (catMaybes [M.lookup (x1:x2:[]) dsts, M.lookup (x2:x1:[]) dsts]) + dstp (x2:xs)
    dstp _ = 0


-- -------------------------------------------------------------------

-- answer part two: 724
solve1624_2 s =
  trace (concat . map (++"\n") $ grid)
  minimum $ map dstp pths
  where
    grid = lines s
    xmax = (-1+) $ length $ (!! 0) $ grid
    ymax = (-1+) $ length $ grid
    gcrd = [((x, y), grid !! y !! x) | x <- [0..xmax], y <- [0..ymax]]
    posx = M.fromList $ map (\(x, y) -> (y, x)) $ filter (isDigit . snd) gcrd
    cmbs = combinations 2 $ sort $ M.keys posx
    dsts = foldl (\acc cmb -> M.insert cmb (dstx grid cmb) acc) M.empty cmbs
    dstx grid (x1:x2:[]) = visitG grid x2 [] [(fromJust $ M.lookup x1 posx, [])]
    pths = map (\x -> '0':x++"0") $ permutations $ sort $ filter (/= '0') $ M.keys posx --
    dstp (x1:x2:xs) = sum (catMaybes [M.lookup (x1:x2:[]) dsts, M.lookup (x2:x1:[]) dsts]) + dstp (x2:xs)
    dstp _ = 0


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 23
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

newval vs ("cpy": x1: x2: [])
  | isInteger x1            = M.insert x2 (read x1) vs
  | isJust $ M.lookup x1 vs = M.insert x2 (fromJust $ M.lookup x1 vs) vs
  | otherwise               = vs
newval vs ("inc": x1:     [])
  | isJust $ M.lookup x1 vs = M.adjust (+1) x1 vs
  | otherwise               = vs
newval vs ("dec": x1:     [])
  | isJust $ M.lookup x1 vs = M.adjust (-1+) x1 vs
  | otherwise               = vs
newval vs ("mul": x1: x2: x3: []) =
  M.insert x1 (v2 * v3) vs
  where
    v2 = M.findWithDefault 0 x2 vs
    v3 = M.findWithDefault 0 x3 vs
newval vs ("nop": []) = vs
newval vs _ = vs

newidx vs ("jnz": x1: x2: []) i
  | isInteger x1        = res (read x1)
  | otherwise           = res (M.findWithDefault 0 x1 vs)
  where
    v2 = if (isInteger x2) then (read x2) else (M.findWithDefault 0 x2 vs)
    res x = if x == 0 then (i + 1) else (i + v2)
newidx _ _ i = i + 1

newlst ls vs i ("tgl": x1: []) =
  nwl
  where
    off = M.findWithDefault 0 x1 vs
    nwl = toggle ls (off + i)
    toggle ls idx
      | idx >= length ls = ls
      | otherwise        =
        (take (idx) ls) ++ [toggli ((!!) ls idx)] ++ (drop (idx + 1) ls)
    toggli ("inc": xs) = ("dec": xs)
    toggli ("dec": xs) = ("inc": xs)
    toggli ("cpy": xs) = ("jnz": xs)
    toggli ("jnz": xs) = ("cpy": xs)
    toggli ("tgl": xs) = ("inc": xs)
newlst ls _ _ _ = ls

processL ls vs i
  | length ls <= i = vs
  | otherwise = processL (newlst ls vs i ins) (newval vs ins) (newidx vs ins i)
  where ins = (!!) ls i


-- answer part one: 13685
solve1623_1 =
  (\l -> processL l (M.fromList [("a", 7)]) 0) . map ( words ) . lines
--(\l -> processL l M.empty 0) . map ( words ) . lines

-- -------------------------------------------------------------------

--TODO

-- answer part two: 479010245
solve1623_2 =
  (\l -> processL l (M.fromList [("a", 12)]) 0) . map ( words ) . lines

-- -->> a! + 8645
-- 479010245


-- cpy b c   a= 0      c=11       <-    |
-- inc a     a= 1            <-    |    |
-- dec c               c=10   |    |    |
-- jnz c -2                  --    |    |

-- -> a = a + b

-- cpy a d                  d=12

-- cpy 0 a   a= 0
-- -> a = a + b                   <-
-- dec d                    d=11   |    |
-- jnz d -5                       --    |

-- cpy 0 a
-- cpy b c
-- inc a
-- dec c
-- jnz c -2
-- dec d
-- jnz d -5

-- -> a = b * d

--           a=12
-- cpy a b        b=12
-- dec b          b=11
--
-- cpy a d                  d=12       <-
-- cpy 0 a   a= 0                       |
--                                      |
-- cpy b c   a= 0      c=11       <-    |
--                                 |    |
-- inc a     a= 1            <-    |    |
-- dec c               c=10   |    |    |
-- jnz c -2                  --    |    |
--           a= 2      c= 9        |    |
--           a= 3      c= 8        |    |
--           a= 4      c= 7        |    |
--           a= 5      c= 6        |    |
--           a= 6      c= 5        |    |
--           a= 7      c= 4        |    |
--           a= 8      c= 3        |    |
--           a= 9      c= 2        |    |
--           a=10      c= 1        |    |
--           a=11      c= 0        |    |
--                                 |    |
-- dec d                    d=11   |    |
-- jnz d -5                       --    |
--           a=132     c= 0 d= 0        |
--                                      |
-- dec b           b=10                 |
-- cpy b c             c=10             |
-- cpy c d                  d=10        |
-- dec d                    d= 9  <-    |
-- inc c               c=11        |    |
-- jnz d -2                       --    |
--           a=132 b=10 c=20 d=0        |
--                                      |
-- tgl c                                |
-- cpy -16 c            c=-16           |
-- jnz 1 c                             --

-- cpy 95 c             c=95

-- jnz 91 d
-- inc a
-- inc d
-- jnz d -2
-- inc c
-- jnz c -5

-- until jnz 1 c

--  1 -> 1
--  2 -> 2      prec * 2
--  3 -> 6      prec * 3
--  4 -> 24     prec * 4
--  5 -> 120    prec * 5
--  6 -> 720    ...
--  7 -> 5040
--  8 -> 40320
--  9 -> 362680

-- to end

--  6 ->
--  7 -> 13685  ( + 8645 )
--  8 -> 48965  ( + 8645 )
--  9 -> 371525 ( + 8645 )

-- -->> a! + 8645

-- 479010245

-- cpy a b
-- dec b
-- cpy a d
-- mul a b d
-- nop
-- nop
-- nop
-- nop
-- nop
-- nop
-- dec b
-- cpy b c
-- cpy c d
-- dec d
-- inc c
-- jnz d -2
-- tgl c
-- cpy -16 c
-- jnz 1 c
-- cpy 95 c
-- jnz 91 d
-- inc a
-- inc d
-- jnz d -2
-- inc c
-- jnz c -5

-- cpy 0 a
-- cpy b c
-- inc a
-- dec c
-- jnz c -2
-- dec d
-- jnz d -5
--     a = b * d + 6 nops


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 22
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

processIn (x1:x2:x3:x4:x5:[]) =
  (coord $ drop 15 x1,
   read (init x2) :: Int,
   read (init x3) :: Int,
   read (init x4) :: Int,
   read (init x5) :: Int)
  where
    coord c =
      (x, y)
      where
        (x0, y0) = span (/='-') c
        x = read (drop 1 x0) :: Int
        y = read (drop 2 y0) :: Int

fldUsed  (c, s, u, a, p) = u

fldCrd   (c, s, u, a, p) = c

fldAvail (c, s, u, a, p) = a

-- answer part one: 985
solve1622_1 s =
  sum $ map (\a -> length (filter
                           (\b -> ((a /= b) && fldUsed(a) <= fldAvail(b)))
                           l)) $ filter ((/= 0) . fldUsed) l
  where l = map (processIn . words) . drop 2 $ lines s


-- -------------------------------------------------------------------

printGrid s =
  map printRow [0..ymax]
  where
    ch (x, y) =
      if (fldUsed rc == 0) then '_'
      else if (reg rc) then '.' else '#'
      where rc = (!! 0) $ filter (\a -> (fldCrd a) == (x, y)) l
    printRow y = [ ch (x, y) | x <- [0..xmax] ]
    xmax = maximum $ map (fst . fldCrd) l
    ymax = maximum $ map (snd . fldCrd) l
    l = map (processIn . words) . drop 2 $ lines s
    reg a = (/= 0) $ length $ filter
            (\b -> ((a /= b) && fldUsed(a) <= fldAvail(b))) l

-- TODO
-- answer part two: 179
solve1622_2 = printGrid


-- 28

-- "X..............................Y"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "..........................######"
-- "................................"
-- "............................_..."
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"
-- "................................"

-- "X...............X....X....X..._Y"
-- "................................"

--  5 ->  21
-- 10 ->  46 +25
-- 15 ->  71 +25
-- 20 ->  96 +25
-- 25 -> 121 +25
-- 30 -> 146 +25
-- 31 -> 151 +5

-- 151 + 28


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 21
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

processOp pwd ("swap":"position":x1:"with":"position":x2:[]) =
  map (swap pwd (read x1) (read x2)) [0..length pwd - 1]
  where
    swap pwd p1 p2 x
      | x == p1   = pwd !! p2
      | x == p2   = pwd !! p1
      | otherwise = pwd !! x

processOp pwd ("swap":"letter":x1:"with":"letter":x2:[]) =
  swap pwd (x1 !! 0) (x2 !! 0)
  where
    swap [] l1 l2 = []
    swap (x:xs) l1 l2
      | x == l1 = (l2:swap xs l1 l2)
      | x == l2 = (l1:swap xs l1 l2)
      | otherwise = (x:swap xs l1 l2)

processOp pwd ("rotate":x1:x2:x3:[]) =
  take ln $ drop offset $ cycle pwd
  where
    offset = if x1 == "right" then (mod (-nb) ln) else nb
    nb = read x2 :: Int
    ln = length pwd

processOp pwd ("rotate":"based":"on":"position":"of":"letter":x1:[]) =
  take ln $ drop offset $ cycle pwd
  where
    idx = elemIndex (x1 !! 0) pwd
    nbr = case idx of
           Just x    -> x
           otherwise -> 0
    offset = (mod (-(1 + if nbr >= 4 then nbr + 1 else nbr)) ln)
    ln = length pwd

processOp pwd ("reverse":"positions":x1:"through":x2:[]) =
  hd ++ rv ++ tl
  where
    p1 = read x1 :: Int
    p2 = read x2 :: Int
    hd = take p1 pwd
    tl = drop (p2 + 1) pwd
    rv = reverse $ take (p2 - p1 + 1) $ drop p1 pwd

processOp pwd ("move":"position":x1:"to":"position":x2:[]) =
  l2
  where
    p1 = read x1 :: Int
    p2 = read x2 :: Int
    l1 = take p1 pwd ++ drop (p1 + 1) pwd
    l2 = take p2 l1 ++ [pwd !! p1] ++ drop (p2) l1

scramble p s = foldl processOp p . map words $ lines s

-- answer part one: agcebfdh
solve1621_1 = scramble p
  where p = "abcdefgh"

-- -------------------------------------------------------------------

-- answer part two: afhdbegc
solve1621_2 input =
  filter (\x -> scramble x input == p) $ permutations p
  where p = "fbgdceah"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 20
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

procblackl line =
  (start, end)
  where
    start = toInt $ takeWhile (/= '-') line
    end = toInt $ tail $ dropWhile (/= '-') line
    toInt x = read x :: Int

-- answer part one: 17348574
solve1620_1 =
  find1 0 . sortOn fst . map procblackl . lines
  where
    find1 x ((l, h):xs)
      | l > x     = x
      | otherwise = find1 (max x (h + 1)) xs


-- -------------------------------------------------------------------

-- answer part two: 104
solve1620_2 =
  findN 0 0 . sortOn fst . map procblackl . lines
  where
    findN _ n [] = n
    findN x n ((l, h):xs)
      | l > x     = findN (h + 1) (n + l - x) xs
      | otherwise = findN (max x (h + 1)) n xs


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 19
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

kill2 seq =
  (Seq.><) (Seq.drop 1 nsk) (Seq.take 1 nsk)
  where
    imd = 2
    nsk = (Seq.><) (Seq.take (imd - 1) seq) (Seq.drop imd seq)

killloop fk seq
  | Seq.length seq == 1 = seq
  | otherwise = killloop fk $ fk seq

-- 3017957

-- answer part one: 1841611
solve1619_1 _ =
  killloop kill2 $ Seq.fromList $ [1..3017957]


-- -------------------------------------------------------------------

killH seq =
  (Seq.><) (Seq.drop 1 nsk) (Seq.take 1 nsk)
  where
    imd = (+ 1) $ (`div` 2) $ Seq.length seq
    nsk = (Seq.><) (Seq.take (imd - 1) seq) (Seq.drop imd seq)

-- answer part two: 1423634
solve1619_2 _ =
  killloop killH $ Seq.fromList $ [1..3017957]

-- https://en.wikipedia.org/wiki/Josephus_problem


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 18
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

nbSafe nb row =
  snd $ nrs nb row
  where
    nrs nb row = foldl (\(x, y) _ -> let nx = nr1 x in (nx, y + cnt nx))
                 (row, cnt row) [1..nb-1]
    cnt = length . filter (=='.')
    nr1 r = nr0 ("." ++ r ++ ".")
    nr0 (x1:x2:x3:xs) = (next x1 x2 x3 : nr0 (x2:x3:xs))
    nr0 _ = []
    --
    next '^' '^' '.' = '^'
    next '.' '^' '^' = '^'
    next '^' '.' '.' = '^'
    next '.' '.' '^' = '^'
    next  _   _   _  = '.'

-- phyroximetful6adimpt

-- answer part one: 1974
solve1618_1 _ =
  nbSafe 40 "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"


-- -------------------------------------------------------------------

-- answer part two: 19991126
solve1618_2 _ =
  nbSafe 400000 "^.....^.^^^^^.^..^^.^.......^^..^^^..^^^^..^.^^.^.^....^^...^^.^^.^...^^.^^^^..^^.....^.^...^.^.^^.^"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 17
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

possMoves c x y =
  map (move c (x, y)) $ filter (open c (x, y)) $ filter (poss c (x, y)) ['U', 'D', 'R', 'L']
  where
    move c (x, y) 'U' = (c ++ "U", x, y - 1)
    move c (x, y) 'D' = (c ++ "D", x, y + 1)
    move c (x, y) 'L' = (c ++ "L", x - 1, y)
    move c (x, y) 'R' = (c ++ "R", x + 1, y)

    poss c (x, y) 'U' = (y > 0)
    poss c (x, y) 'D' = (y < 3)
    poss c (x, y) 'L' = (x > 0)
    poss c (x, y) 'R' = (x < 3)

    open c (x, y) d
      | d == 'U' = (`elem` "bcdef") $ (md5sum c) !! 0
      | d == 'D' = (`elem` "bcdef") $ (md5sum c) !! 1
      | d == 'L' = (`elem` "bcdef") $ (md5sum c) !! 2
      | d == 'R' = (`elem` "bcdef") $ (md5sum c) !! 3

-- answer part one: RDDRLDRURD
solve1617_1 _ =
  visit [("qzthpkfp", 0, 0)] -- RDDRLDRURD
  where
    visit ((c, x, y):cs)
      | (x, y) == (3, 3) = dropWhile isLower c
      | otherwise        = visit (cs ++ (possMoves c x y))

-- -------------------------------------------------------------------

-- answer part two: 448
solve1617_2 _  =
  maximum $ visit2 [("qzthpkfp", 0, 0)] -- 448
  where
    visit2 [] = []
    visit2 ((c, x, y):cs)
      | (x, y) == (3, 3) = ((length $ dropWhile isLower c) : visit2 cs)
      | otherwise        = (visit2 (cs ++ (possMoves c x y)))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 16
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

dragc n x
  | length p >= n = p
  | otherwise     = dragc n p
  where
    p = x ++ "0" ++ (flipc $ reverse x)
    flipc []       = []
    flipc ('0':xs) = ('1':flipc xs)
    flipc ('1':xs) = ('0':flipc xs)

checks n x
  | even $ length c = checks n c
  | otherwise       = c
    where
      c = pairc $ take n x
      pairc (x1:x2:xs)
        | x1 == x2  = ('1':pairc xs)
        | otherwise = ('0':pairc xs)
      pairc _       = []

chkfill n x = checks n $ dragc n x

-- solve0 = chkfill 20 "10000"

-- answer part one: 01110011101111011
solve1616_1 _ =
  chkfill 272 "11110010111001001"


-- -------------------------------------------------------------------

-- answer part two: 11001111011000111
solve1616_2 _ =
  chkfill 35651584 "11110010111001001"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 15
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- Disc #1 has 13 positions; at time=0, it is at position 1.
-- Disc #2 has 19 positions; at time=0, it is at position 10.
-- Disc #3 has 3 positions; at time=0, it is at position 2.
-- Disc #4 has 7 positions; at time=0, it is at position 1.
-- Disc #5 has 5 positions; at time=0, it is at position 3.
-- Disc #6 has 17 positions; at time=0, it is at position 5.
-- Disc #7 has 11 positions; at time=0, it is at position 0.

d1 = (== 0) . (`mod` 13) . (+  1). (+ 1)
d2 = (== 0) . (`mod` 19) . (+ 10). (+ 2)
d3 = (== 0) . (`mod`  3) . (+  2). (+ 3)
d4 = (== 0) . (`mod`  7) . (+  1). (+ 4)
d5 = (== 0) . (`mod`  5) . (+  3). (+ 5)
d6 = (== 0) . (`mod` 17) . (+  5). (+ 6)
d7 = (== 0) . (`mod` 11) . (+  0). (+ 7)

fall1 t = d1 t && d2 t && d3 t && d4 t && d5 t && d6 t

-- answer part one: 376777
solve1615_1 _ =
  take 1 $ filter fall1 $ [0..]


-- -------------------------------------------------------------------

fall2 t = d1 t && d2 t && d3 t && d4 t && d5 t && d6 t && d7 t

-- answer part two: 3903937
solve1615_2 _ =
  take 1 $ filter fall2 $ [0..]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 14
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

isValidK ml i =
  case search3x (ml !! i) of
   Just x -> any id $ map (search5c x) $ map (ml !!) [i + 1..i +
                                                      1001]
   _ -> False
  where
    search3x (x1:x2:x3:xs)
      | x1 == x2 && x2 == x3 = Just x1
      | otherwise = search3x (x2:x3:xs)
    search3x (_)  = Nothing
    search5c c s =
      (>= 5) . maximum . (0:) . map length . filter ((== c) . (!! 0)) $ group s

search1st fct salt n i
  | n == 0        = (i - 1)
  | isValidK ml 0  = trace (show (n, i)) search1st fct salt (n - 1) (i + 1)
  | otherwise     = search1st fct salt n (i + 1)
  where ml = [ fct $ salt ++ show x | x <- [i..] ]

-- solve0 = search1st md5sum "abc" 64 0

-- answer part one: 16106
solve1614_1 _ =
  search1st md5sum "zpqevtbw" 64 0


-- -------------------------------------------------------------------

md5Stretch x =
  foldl (\acc _ -> md5sum acc) x [1..2017]

search2nd fct salt nb =
  search nb 0
  where
    search n i
      | n == 0       = i - 1
      | isValidK ml i = trace (show (n, i)) search (n - 1) (i + 1)
      | otherwise    = search n (i + 1)
    ml = [ fct $ salt ++ show x | x <- [1..] ]

searchAll fct salt n =
  take n $ filter (isValidK ml) [1..]
  where ml = [ fct $ salt ++ show x | x <- [1..] ]

-- solve2 = search2nd md5Stretch "zpqevtbw" 64

solve1614_2 _ = searchAll md5Stretch "zpqevtbw" 64


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 13
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

isWall c (x, y) = ((mod) n1 2 /= 0)
  where
    ns = c + x * x + 3 * x + 2 * x * y + y + y * y
    n1 = length $ filter (=='1') $ showIntAtBase 2 intToDigit ns ""

showAreaC c l =
  concat $ map ((++ "\n") . showRow) [0..rsize]
  where
    showRow y = map (\x -> showCell x y) [0..rsize]
    showCell x y
      | isWall c (x, y) = '#'
      | elem (x, y) l   = 'O'
      | otherwise       = '.'
    rsize = 40

possibleCMoves c x y =
  filter isPossible [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  where
    isPossible (x, y) = (x >= 0) && (y >= 0) && (not $ isWall c (x, y))

-- answer part one: 90
solve1613_1 _ =
  visit1 1352 (31, 39) [] [(1, 1, [])]
  where
    visit1 code target cache ((x, y, l):vlist)
      | trace (showAreaC code l) False = undefined
    visit1 code target cache ((x, y, l):vlist)
      | (x, y) == target    = length l
      | elem (x, y) cache   = visit1 code target cache vlist
      | otherwise           =
        visit1 code target ((x, y):cache)
        (vlist ++ ([(nx, ny, ((x, y):l)) | (nx, ny) <- (possibleCMoves code x y)]))


-- -------------------------------------------------------------------

-- answer part two: 135
solve1613_2 _ =
  visit2 1352 50 [] [(1, 1, 0)]
  where
    visit2 code nmax cache ((x, y, n):vlist)
      | trace (showAreaC code cache) False = undefined
    visit2 code nmax cache ((x, y, n):vlist)
      | n > nmax            = visit2 code nmax cache vlist
      | elem (x, y) cache   = visit2 code nmax cache vlist
      | otherwise           =
        visit2 code nmax ((x, y):cache) (vlist ++ ([(nx, ny, n + 1) | (nx, ny) <- (possibleCMoves code x y)]))
    visit2 code nmax cache [] = length cache


-- -------------------------------------------------------------------

-- solve0 = visit 10 (7, 4) [] [(1, 1, [])]

-- .##.##............###.....#####.#####....
-- #o######.##.####...#.##....#..#.#..#.##.#
-- .ooo##.#..#.##.#.#.######..#.##..#.####..
-- ##.ooo###......#..#..#..####.###..#..#.#.
-- .####o####..###.#.##.#.#...#..#.#.##.##..
-- .....oo######.###..###..##.#..###..##.#..
-- ..##.#oo##..#.......###.#..###......#.###
-- ##.#...oooo.##.##.#.....#....#.##.#.###..
-- .##.######o#.#.##...###.##...##.#..#..##.
-- #.##....##oo##.#.###..#.###.#.##.#..#..##
-- ##.#..#.###o#..#...#.##...#..#.#..#......
-- .######..#.o#.######.#####.#..###..##.###
-- ...#.....##o#.##..#...##.#.##.####..#.###
-- ##.#.##...#oooooo.#......#.....#.###...##
-- #..#.###.#######o########.##.#.###.##...#
-- #..#..##.#...###o##ooo#.##.#..#.....##...
-- ###.#.##.#.....#oooo#oo#.##.#.####.#.#.##
-- #.###...###.##.#.###.#o##.###...##..##.#.
-- #.....#..##.#..##..##.o.#.....#..##.#..#.
-- #..#####....#...###.#.o.##.#####.#..#..##
-- ##....#.##.###.#..#.##o#.###..#..##.#####
-- .###..####..##..#.#.##oo#ooooo#...#.#..##
-- ##.#....#.#...#..##..##ooo###o##.##..#..#
-- ...###..##.##..#.###.#.####.#o##.###.#...
-- ####.##..#######..#..#.....##ooooo#..####
-- #.....###..#......#..###.#.#.###.o#.....#
-- #.##.#..##.#..########.###.###.##o##..#.#
-- #.##..#...####....#.......#...#.#o#####.#
-- #.#.#...#....#..#.#.##..#.###..##o##...##
-- ..##.############.#.#####....#.#.oooo#.##
-- .#.#.#..##..#....##..#....##.#.#..##o#...
-- ..##.#....#.##.#.#.#.#.###.#..######o#.##
-- #.#..##.#..#.#.#.##..##..##.#.....#oo###.
-- ..#.#.##.#...#.##.#...#.#.######..#o#..##
-- ..#..#.#..###...#.##.##.##..##.#..#oooo.#
-- #..#..###.#.#.#.####.##..#.....##..##.o.#
-- ##.##.###..##..#...#.....#..######..##o##
-- .#.....#.#.#.#..##.#.##.#####..#.##.##o##
-- #.####.##..#..#.#..##.#.##..#..####.##o..
-- ##....#.#.###...#...##......###.o##oooo#.
-- .#..#.#.#.####.###.#.#####.#...#oooo##.#.

-- .##.##..oooooooooo###.....#####.#####....
-- #o######.##o####ooo#.##....#..#.#..#.##.#
-- oooo##.#..#o##.#o#o######..#.##..#.####..
-- ##oooo###..o...#oo#..#oo####.###..#..#.#.
-- o####o####..###.#o##.#o#...#..#.#.##.##..
-- ooooooo######o###oo###oo##.#..###..##.#..
-- oo##o#oo##oo#ooooooo###o#..###......#.###
-- ##.#oooooooo##o##o#ooooo#....#.##.#.###..
-- .##.######o#.#o##ooo###o##...##.#..#..##.
-- #.##....##oo##o#.###..#o###.#.##.#..#..##
-- ##.#..#.###o#oo#...#.##ooo#..#.#..#......
-- .######..#oo#o######.#####.#..###..##.###
-- ...#.....##o#o##oo#...##.#.##.####..#.###
-- ##.#.##...#ooooooo#......#.....#.###...##
-- #..#.###.#######o########.##.#.###.##...#
-- #..#..##.#...###o##ooo#.##.#..#.....##...
-- ###.#.##.#.....#oooo#oo#.##.#.####.#.#.##
-- #.###...###.##.#o###.#o##.###...##..##.#.
-- #.....#..##.#..##..##ooo#.....#..##.#..#.
-- #..#####....#...###.#ooo##.#####.#..#..##
-- ##....#.##.###.#..#.##o#.###..#..##.#####
-- .###..####..##..#.#.##oo#ooo..#...#.#..##
-- ##.#....#.#...#..##..##ooo###.##.##..#..#
-- ...###..##.##..#.###.#.####.#.##.###.#...
-- ####.##..#######..#..#.....##.....#..####
-- #.....###..#......#..###.#.#.###..#.....#
-- #.##.#..##.#..########.###.###.##.##..#.#
-- #.##..#...####....#.......#...#.#.#####.#
-- #.#.#...#....#..#.#.##..#.###..##.##...##
-- ..##.############.#.#####....#.#.....#.##
-- .#.#.#..##..#....##..#....##.#.#..##.#...
-- ..##.#....#.##.#.#.#.#.###.#..######.#.##
-- #.#..##.#..#.#.#.##..##..##.#.....#..###.
-- ..#.#.##.#...#.##.#...#.#.######..#.#..##
-- ..#..#.#..###...#.##.##.##..##.#..#.....#
-- #..#..###.#.#.#.####.##..#.....##..##...#
-- ##.##.###..##..#...#.....#..######..##.##
-- .#.....#.#.#.#..##.#.##.#####..#.##.##.##
-- #.####.##..#..#.#..##.#.##..#..####.##...
-- ##....#.#.###...#...##......###..##....#.
-- .#..#.#.#.####.###.#.#####.#...#....##.#.


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 12
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

processI ls vs i
  | length ls == i = vs
  | otherwise = processI ls (newval vs ins) (newidx vs ins i)
  where
    ins = (!!) ls i

    newval vs ("cpy": x1: x2: [])
      | all isDigit x1 = M.insert x2 (read x1) vs
      | otherwise      = M.insert x2 (M.findWithDefault 0 x1 vs) vs
    newval vs ("inc": x1:     []) =
      M.adjust (+1) x1 vs
    newval vs ("dec": x1:     []) =
      M.adjust (-1+) x1 vs
    newval vs _ = vs

    newidx vs ("jnz": x1: x2: []) i =
      if (c == 0) then (i + 1) else (i + read x2)
      where c = if (all isDigit x1) then (read x1) else (M.findWithDefault 0 x1 vs)
    newidx _ _ i = i + 1

-- answer part one: 318009
solve1612_1 =
  (\x -> processI x M.empty 0) . map ( words ) . lines


-- -------------------------------------------------------------------

-- answer part two: 9227663
solve1612_2 =
  (\x -> processI x (M.fromList [("c", 1)]) 0) . map ( words ) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 11
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

appChange elev state (nelev, move) =
  if (check nstage && check ostage) then
    let floors = [0..length state - 1]
        nstat1 = [(if (x == nelev) then (nstage) else (state  !! x)) | x <- floors]
        nstate = [(if (x ==  elev) then (ostage) else (nstat1 !! x)) | x <- floors]
    in Just (nelev, nstate) else Nothing
  where
    nstage = (state !! nelev) ++ move
    ostage = (state !! elev) \\ move
    check stg = (null ms || null gs || cr)
      where
        ms = filter (<10) stg
        gs = filter (>=10) stg
        cr = all ((`elem` gs) . (+10)) ms

possibleEMoves elev state =
  (prodx)
  where
    combs = (combinations 2 (state !! elev) ++ combinations 1 (state !! elev))
    elevs = filter (\x -> x >= 0 && x < (length state)) $ [elev + 1, elev - 1]
    prodc = [(e, m) | e <- elevs, m <- combs]
    prodx = catMaybes $ map (appChange elev state) prodc
    combinations 0 _  = [[]]
    combinations n xs = [
      xs !! i : x | i <- [0 .. (length xs) - 1],
      x <- combinations (n - 1) (drop (i + 1) xs) ]

visitFl cc ((e, s, n):vlist)
  | win s               = n
  | elem (repr e s) cc  = visitFl cc vlist
  | otherwise           =
      visitFl ((repr e s):cc) (vlist ++ ([(ne, ns, n + 1) | (ne, ns) <- (possibleEMoves e s)]))
  where
    win state       = all null $ take (length state - 1) state
    repr elev state =
      show elev ++ (concat $ map (\(x,y)-> show x ++ show y) $
                    sort $ map (\x -> (flOf x state, flOf (x+10) state)) $ allMs state)
    flOf x state    = fromJust $ findIndex ((elem) x) state
    allMs state     = filter (<10) $ concat state

state1 = [
    [ 11, 1 ],               -- 0 FLOOR-1
    [ 12, 13, 14, 15 ],      -- 1 FLOOR-2
    [ 2, 3, 4, 5 ],          -- 2 FLOOR-3
    [ ] ]                    -- 3 FLOOR-4

-- answer part one: 33
solve1611_1 _ =
  visitFl [] [(0, state1, 0)]

-- -------------------------------------------------------------------

state2 = [
    [ 11, 1, 16, 6, 17, 7 ], -- 0 FLOOR-1
    [ 12, 13, 14, 15 ],      -- 1 FLOOR-2
    [ 2, 3, 4, 5 ],          -- 2 FLOOR-3
    [ ] ]                    -- 3 FLOOR-4

-- answer part two: 57
solve1611_2 _ =
  visitFl [] [(0, state2, 0)]


-- -------------------------------------------------------------------

state0 = [
    [ 1, 2 ],  -- 0 FLOOR-1
    [ 11 ],    -- 1 FLOOR-2
    [ 12 ],    -- 2 FLOOR-3
    [ ] ]      -- 3 FLOOR-4


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 10
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 116
-- answer part two: 23903
solve1610_2 input =
  (loop bstate M.empty instrs)
  where
    (bstate, instrs) = foldl readInstr (M.empty, M.empty) . map words . lines $ input

    loop bstate outval instrs
      | null filled = (outval)
      | otherwise   = loop nstate noutvl instrs
      where
        filled = filter ( (>= 2) . length . (M.!) bstate) (M.keys bstate)
        (nstate, noutvl) = foldl (handle instrs) (bstate, outval) filled

    handle instrs (bstate, outval) nfl =
      (s3, o3)
      where
        (s1, o1) = (M.insert nfl [] bstate, outval)
        lowv0 = minimum $ (M.!) bstate nfl
        hghv0 = maximum $ (M.!) bstate nfl
        (lowo, hgho) = (M.!) instrs nfl
        (lowv, hghv) = check1 lowv0 hghv0 nfl
        (s2, o2) = update s1 o1 lowo lowv
        (s3, o3) = update s2 o2 hgho hghv

    update bstate outval ("output", n) val =
      (bstate, M.insert n val outval)
    update bstate outval ("bot",    n) val =
      (M.insert n (val:vold) bstate, outval)
      where
        vold = M.findWithDefault [] n bstate

    check1 17 61 n = trace ("[[part1-" ++ show n ++ "]]") (17, 61)
    check1 x y _ = (x, y)

    readInstr (bstate, instrs) ("value":xs) =
      (M.insert bnum (bval:vold) bstate, instrs)
      where
        bnum = (toInt $ xs!!4)
        bval = (toInt $ xs!!0)
        vold = M.findWithDefault [] bnum bstate
    readInstr (bstate, instrs) ("bot"  :xs) =
      (bstate, M.insert bnum (out1, out2) instrs)
      where
        bnum = toInt $ xs!!0
        out1 = (xs!!4, toInt $ xs!!5)
        out2 = (xs!!9, toInt $ xs!!10)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 9
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

process1 str
  | Seq.null rst1 = init
  | otherwise     = mconcat [init, rrpl, process1 rst3]
  where
    (init, rst1)  = Seq.spanl (/= '(') str
    (nums, rst2)  = Seq.spanl (/= ')') rst1
    (rlen, nrep)  = (\(x,y) -> (read $ toList x, read $ tail $ toList y)) $ Seq.spanl (/= 'x') . Seq.drop 1 $ nums
    (ssub, rst3)  = Seq.splitAt rlen . Seq.drop 1 $ rst2
    (rrpl)        = mconcat $ replicate nrep $ ssub

-- answer part one: 74532
solve169_1 =
  length . process1 . Seq.fromList . head . lines


-- -------------------------------------------------------------------

process2 str
  | Seq.null rst1 = init
  | otherwise     = mconcat [init, rrpl, process2 rst3]
  where
    (init, rst1)  = Seq.spanl (/= '(') str
    (nums, rst2)  = Seq.spanl (/= ')') rst1
    (rlen, nrep)  = (\(x,y) -> (read $ toList x, read $ tail $ toList y)) $ Seq.spanl (/= 'x') . Seq.drop 1 $ nums
    (ssub, rst3)  = Seq.splitAt rlen . Seq.drop 1 $ rst2
    (rrpl)        = mconcat $ replicate nrep $ process2 ssub

-- answer part two: 11558231665
solve169_2 =
  length . process2 . Seq.fromList . head . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

xMax = 49; yMax = 5

command2F arr ("rect":p1:[]) =
  (arr // [((i, j), 1) | i <- [0..(x - 1)], j <- [0..(y - 1)]])
  where (x, y) = (\(l1, l2) -> (toInt l1, toInt $ tail l2)) . span (/= 'x') $ p1
command2F arr ("rotate":"column":p1:"by":p2:[]) =
  (arr // [((x, j), arr ! (x, (mod (j - (toInt p2)) (yMax + 1)))) | j <- [0..yMax]])
  where x = toInt . drop 2 $ p1
command2F arr ("rotate":"row":p1:"by":p2:[]) =
  (arr // [((i, y), arr ! ((mod (i - (toInt p2)) (xMax + 1)), y)) | i <- [0..xMax]])
  where y = toInt . drop 2 $ p1

-- answer part one: 119
solve168_1 =
  sum . elems . foldl (command2F) arr . map words . lines
  where arr = array ((0,0), (xMax, yMax)) [((i, j), 0) | i <- [0..xMax], j <- [0..yMax]]


-- -------------------------------------------------------------------

-- answer part two: ZFHFSFOGPO
solve168_2 =
  disp xMax yMax . foldl (command2F) arr . map words . lines
  where
    arr = array ((0,0), (xMax, yMax)) [((i, j), 0) | i <- [0..xMax], j <- [0..yMax]]
    disp xM yM arr = concat $ map (dispRow xM arr) [0..yM]
    dispRow xM arr y = [ if arr ! (x, y) /= 0 then '.' else ' ' | x <- [0..xM]] ++ ['\n']

-- .... .... .  . ....  ... ....  ..   ..  ...   ..
--    . .    .  . .    .    .    .  . .  . .  . .  .
--   .  ...  .... ...  .    ...  .  . .    .  . .  .
--  .   .    .  . .     ..  .    .  . . .. ...  .  .
-- .    .    .  . .       . .    .  . .  . .    .  .
-- .... .    .  . .    ...  .     ..   ... .     ..


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

partSeqs sps hps [] = (sps, hps)
partSeqs sps hps ('[' : xs) =
  partSeqs sps (hps++[hyp]) (dropWhile (==']') rem)
  where (hyp, rem) = span (/= ']') xs
partSeqs sps hps (xs) =
  partSeqs (sup : sps) hps (rem)
  where (sup, rem) = span (/= '[') xs

containsABBA (x1:x2:x3:x4:xs)
  | (x1 == x4) && (x2 == x3) && (x1 /= x2) = True
  | otherwise = containsABBA (x2:x3:x4:xs)
containsABBA (_) = False

isValidTLS (sps, hps) =
  (any containsABBA sps) && (not $ any containsABBA hps)

-- answer part two: 110
solve167_1 =
  length . filter isValidTLS . map (partSeqs [] []) . lines


-- -------------------------------------------------------------------

findABAs :: Eq a => [[a]] -> [a] -> [[a]]
findABAs lst (x1:x2:x3:xs)
  | (x1 == x3) && (x1 /= x2) = (x1:x2:x3:[]) : findABAs lst (x2:x3:xs)
  | otherwise = findABAs lst (x2:x3:xs)
findABAs lst _ = lst

containsABA aba@(x1:x2:x3:[]) (y1:y2:y3:ys)
  | (x1 == y1) && (x2 == y2) && (x3 == y3) = True
  | otherwise = containsABA aba (y2:y3:ys)
containsABA aba _ = False

isValidSSL (sps, hps) =
  (not $ null abas) &&
  (any id $ concat $ map (\bab -> (map (containsABA bab) hps) ) babs)
  where
    abas = concat $ map (findABAs []) sps
    babs = map toBab abas
    toBab (x1:x2:x3:[]) = (x2:x1:x2:[])

-- answer part two: 242
solve167_2 =
  length . filter isValidSSL . map (partSeqs [] []) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: bjosfbce
solve166_1 =
  map (head . snd . last . sort . map (\x -> (length x, x)) . group . sort) .
  transpose . lines


-- -------------------------------------------------------------------

-- answer part two: veqfxzfx
solve166_2 =
  map (head . snd . head . sort . map (\x -> (length x, x)) . group . sort) .
  transpose . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

checkStr sec num =
  (>= 5) . length . takeWhile (== '0') . md5sum $ (sec ++ show num)

searchFirst sec x
  | checkStr sec (x + 1) = (x + 1)
  | otherwise = searchFirst sec (x + 1)

password1 sec =
  map (\x -> (md5sum (sec ++ show x)) !! 5) .
  take 8 . drop 1 . iterate (searchFirst sec) $ 0

-- answer part one: 4543c154
solve165_1 _ = password1 "ojvtpuvg"


-- -------------------------------------------------------------------

replPwd ltr pwd i
  | (pwd !! i) == '_' =
      ((take i pwd) ++ [ltr] ++ (drop (i + 1) pwd))
  | otherwise = pwd

password2 pwd _ _
  | not (elem '_' pwd) = pwd
password2 pwd sec nb =
  let
    indx = searchFirst sec nb
    hash = md5sum (sec ++ show indx)
    hash_6 = hash !! 5
    hash_7 = hash !! 6
    npwd =
      if (hash_6 >= '0' && hash_6 <= '7') then
        replPwd hash_7 pwd (ord(hash_6) - ord('0'))
      else pwd
  in
   password2 npwd sec (indx + 1)

-- answer part two: 1050cbbd
solve165_2 _ = password2 "________" "ojvtpuvg" 0


-- -------------------------------------------------------------------

md5sum :: [Char] -> String
md5sum = show . md5 . BS.pack . map (fromIntegral . ord)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

cmpN (x : xs) (y : ys) = compare (length xs, (0 - ord(x))) (length ys, (0 - ord(y)))

splitRname s =
  let
    (seq, chb) = span (/= '[') $ init s
    (chk) = drop 1 chb
    (sql, sqn) = break isDigit seq
    (sec) = read sqn :: Int
    (sqf) = group $ sort $ filter (/= '-') sql
    (sqs) = reverse . sortBy cmpN $ sqf
    (sq5) = concat $ map (take 1) $ take 5 sqs
  in
   (sq5, sec, chk, init sql)

isValid (a, b, c, d) = (a == c)

-- answer part one: 158835
solve164_1 = sum . map (\(_, b, _, _) -> b) . filter isValid . map splitRname . lines


-- -------------------------------------------------------------------

shiftW (_, n, _, w) = ((map (shiftC n) w), n)

shiftC n c
  | c == '-'  = ' '
  | otherwise = chr $ (mod ((ord(c) - ord('a')) + n) 26) + ord('a')

-- answer part two: 993
solve164_2 =
  filter (isPrefixOf "north" . fst) . map shiftW . filter isValid . map splitRname . lines


-- -------------------------------------------------------------------

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f (x : xs)
  | not (f x) =
      let (ht, tw) = (break f xs)
      in (x : ht) : (splitOn f tw)
  | otherwise = (splitOn f xs)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

validtri (a : b : c : []) = (c < (a + b))

-- answer part one: 917
solve163_1 =
  length . filter validtri . map (sort . map toInt . words) . lines


-- -------------------------------------------------------------------

group3 [] = []
group3 (a : b : c : xs) = ([a, b, c] : (group3 xs))

-- answer part two: 1649
solve163_2 =
  length . filter validtri . map sort . group3 . concat . transpose . map (map toInt . words) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 1 2 3
-- 4 5 6
-- 7 8 9

nextMap = M.fromList [
  ('1', M.fromList [('U', '1'), ('R', '2'), ('D', '4'), ('L', '1')]),
  ('2', M.fromList [('U', '2'), ('R', '3'), ('D', '5'), ('L', '1')]),
  ('3', M.fromList [('U', '3'), ('R', '3'), ('D', '6'), ('L', '2')]),
  ('4', M.fromList [('U', '1'), ('R', '5'), ('D', '7'), ('L', '4')]),
  ('5', M.fromList [('U', '2'), ('R', '6'), ('D', '8'), ('L', '4')]),
  ('6', M.fromList [('U', '3'), ('R', '6'), ('D', '9'), ('L', '5')]),
  ('7', M.fromList [('U', '4'), ('R', '8'), ('D', '7'), ('L', '7')]),
  ('8', M.fromList [('U', '5'), ('R', '9'), ('D', '8'), ('L', '7')]),
  ('9', M.fromList [('U', '6'), ('R', '9'), ('D', '9'), ('L', '8')])]

computedigit mp pos (x:xs) =
  computedigit mp (((M.!) ((M.!) mp pos) x)) xs
computedigit mp pos []     = pos

computecode mp pos (x : xs) =
  np : (computecode mp np xs)
  where np = computedigit mp pos x
computecode mp pos []       = []

-- answer part one: 24862
solve162_1 = computecode nextMap '5' . lines


-- -------------------------------------------------------------------

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D

nextMap2 = M.fromList [
  ('1', M.fromList [('U', '1'), ('R', '1'), ('D', '3'), ('L', '1')]),
  ('2', M.fromList [('U', '2'), ('R', '3'), ('D', '6'), ('L', '2')]),
  ('3', M.fromList [('U', '1'), ('R', '4'), ('D', '7'), ('L', '2')]),
  ('4', M.fromList [('U', '4'), ('R', '4'), ('D', '8'), ('L', '3')]),
  ('5', M.fromList [('U', '5'), ('R', '6'), ('D', '5'), ('L', '5')]),
  ('6', M.fromList [('U', '2'), ('R', '7'), ('D', 'A'), ('L', '5')]),
  ('7', M.fromList [('U', '3'), ('R', '8'), ('D', 'B'), ('L', '6')]),
  ('8', M.fromList [('U', '4'), ('R', '9'), ('D', 'C'), ('L', '7')]),
  ('9', M.fromList [('U', '9'), ('R', '9'), ('D', '9'), ('L', '8')]),
  ('A', M.fromList [('U', '6'), ('R', 'B'), ('D', 'A'), ('L', 'A')]),
  ('B', M.fromList [('U', '7'), ('R', 'C'), ('D', 'D'), ('L', 'A')]),
  ('C', M.fromList [('U', '8'), ('R', 'C'), ('D', 'C'), ('L', 'B')]),
  ('D', M.fromList [('U', 'B'), ('R', 'D'), ('D', 'D'), ('L', 'D')])
  ]

-- answer part two: 46C91
solve162_2 = computecode nextMap2 'A' . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2016 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

newpos n 'N' (x, y) = (x, y + n)
newpos n 'W' (x, y) = (x - n, y)
newpos n 'S' (x, y) = (x, y - n)
newpos n 'E' (x, y) = (x + n, y)

newdir 'N' 'R' = 'E'
newdir 'N' 'L' = 'W'
newdir 'W' 'R' = 'N'
newdir 'W' 'L' = 'S'
newdir 'S' 'R' = 'W'
newdir 'S' 'L' = 'E'
newdir 'E' 'R' = 'S'
newdir 'E' 'L' = 'N'

eastermove pos dir ((t, nb) : xs) =
  eastermove npos ndir xs
  where
    ndir = newdir dir t
    npos = newpos nb ndir pos
eastermove pos _ [] = pos

-- answer part one: 242
solve161_1 =
  (\(x, y) -> (abs x) + (abs y))
  . eastermove (0, 0) 'N'
  . map ((\(x, y) -> (head x, toInt y)) . splitAt 1) . words . filter (/= ',')


-- -------------------------------------------------------------------

newposi visited n d pos
  | S.member pos visited = (pos, visited)
  | n == 0    = (pos, visited)
  | otherwise = newposi (S.insert pos visited) (n - 1) d (newpos 1 d pos)

eastermove2 visited pos dir ((t, nb) : xs)
  | S.member npos nvisited = npos
  | otherwise = eastermove2 nvisited npos ndir xs
  where
    ndir = newdir dir t
    (npos, nvisited) = newposi visited nb ndir pos

-- answer part two: 150
solve161_2 =
  (\(x, y) -> x + y)
  . eastermove2 S.empty (0, 0) 'N'
  . map ((\(x, y) -> (head x, toInt y)) . splitAt 1) . words . filter (/= ',')


-- -------------------------------------------------------------------

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
