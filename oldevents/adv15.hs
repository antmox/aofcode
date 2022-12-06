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

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

main =
  getArgs >>= getInput >>= return . solve1519_2 >>= print
  where
    getInput (l : _) = readFile l
    getInput []      = getContents

-- readFile "inputs/1501.in" >>= return . solve151_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 25
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- Enter the code at row 3010, column 3019.

--    | 1   2   3   4   5   6
-- ---+---+---+---+---+---+---+
--  1 |  1   3   6  10  15  21
--  2 |  2   5   9  14  20
--  3 |  4   8  13  19
--  4 |  7  12  18
--  5 | 11  17
--  6 | 16

-- answer part one: 8997277
solve1525_1 _ =
  codes $ cell 3019 3010
  where
    codes x = foldl' (\acc _ -> (mod) (acc * 252533) 33554393) 20151125 [1..(x-1)]
    cell 1 1 = 1
    cell 1 y = (cell 1 (y - 1)) + (y - 1)
    cell x y = (cell (x - 1) y) + (y) + (x - 1)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 24
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

solveQuantum n b s
  | null ans  = solveQuantum (n + 1) b s
  | otherwise = minimum ans
  where
    lst = map toInt . lines $ s
    prt = div (sum lst) b
    ans = map (foldl (*) 1) . filter ((==) prt . sum) $ combinations n lst

-- answer part one: 10439961859
solve1524_1 = solveQuantum 0 3


-- -------------------------------------------------------------------

-- answer part two: 72050269
solve1524_2 = solveQuantum 0 4


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 23
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

processInstr ls vs i
  | length ls <= i = vs
  | otherwise = processInstr ls (newval vs ins) (newidx vs ins i)
  where
    ins = (!!) ls i

    newval vs ("hlf": x1:     []) =
      M.insert x1 ((div) (M.findWithDefault 0 x1 vs) 2) vs
    newval vs ("tpl": x1:     []) =
      M.insert x1 ((*) 3 (M.findWithDefault 0 x1 vs)) vs
    newval vs ("inc": x1:     []) =
      M.insert x1 ((+) 1 (M.findWithDefault 0 x1 vs)) vs
    newval vs _ = vs

    newidx vs ("jmp": x1: []) i =
      i + (readOff x1)
    newidx vs ("jie": x1: x2: []) i
      | even (M.findWithDefault 0 (init x1) vs) = i + (readOff x2)
      | otherwise = i + 1
    newidx vs ("jio": x1: x2: []) i
      | (== 1) (M.findWithDefault 0 (init x1) vs) = i + (readOff x2)
      | otherwise = i + 1
    newidx _ _ i = i + 1

    readOff ('+':xs) = (read xs :: Int)
    readOff (xs)     = (read xs :: Int)

-- answer part one: 307
solve1523_1 =
  (\l -> processInstr l M.empty 0) . map ( words ) . lines


-- -------------------------------------------------------------------

-- answer part two: 160
solve1523_2 =
  (\l -> processInstr l (M.fromList [("a", 1)]) 0) . map ( words ) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 22
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- TODO: haskell solution

-- Hit Points: 58
-- Damage: 9

-- Magic Missile costs 53 mana. It instantly does 4 damage.
-- Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
-- Shield costs 113 mana. It starts an effect that lasts for 6 turns.
--        While it is active, your armor is increased by 7.
-- Poison costs 173 mana. It starts an effect that lasts for 6 turns.
--        At the start of each turn while it is active, it deals the boss 3 damage.
-- Recharge costs 229 mana. It starts an effect that lasts for 5 turns.
--        At the start of each turn while it is active, it gives you 101 new mana.

-- manaCost, instantEffects, delayedEffects
--    damage, heal, armor, newmana
--   [(d, h, a, n, timer)]

-- spells = [
--   ( 53, ((4, 0, 0,   0), -1)), -- Magic Missile
--   ( 73, ((2, 2, 0,   0), -1)), -- Drain
--   (113, ((0, 0, 7,   0),  6)), -- Shield
--   (173, ((3, 0, 0,   0),  6)), -- Poison
--   (229, ((0, 0, 0, 101),  5))] -- Recharge

-- visit = undefined

-- allFights (hp, mana) (hb, dmg) = -- min mana cost
--   visit [((hp, mana, 0), (hb, dmg), [])]

-- 1269
-- 1309


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 21
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- Hit Points: 109
-- Damage: 8
-- Armor: 2

-- (hit, dmg, arm) player / boss
-- stats0 = ((8, 5, 5), (12, 7, 2))
-- (cost, damage, armor)

w1 = [
  (  8, 4, 0), -- dagger
  ( 10, 5, 0), -- shortsword
  ( 25, 6, 0), -- warhammer
  ( 40, 7, 0), -- longsword
  ( 74, 8, 0)] -- greataxe

w2 = [
  (  0, 0, 0), -- none
  ( 13, 0, 1), -- leather
  ( 31, 0, 2), -- chainmail
  ( 53, 0, 3), -- splintmail
  ( 75, 0, 4), -- bandedmail
  (102, 0, 5)] -- platemail

w3 = [
  (  0, 0, 0), -- none
  ( 25, 1, 0), -- damage +1
  ( 50, 2, 0), -- damage +2
  (100, 3, 0), -- damage +3
  ( 20, 0, 1), -- defense +1
  ( 40, 0, 2), -- defense +2
  ( 80, 0, 3)] -- defense +3

fight ((h1, d1, a1), (h2, d2, a2))
  | otherwise = n1 <= n2
  where
    n1 = ceiling $ (/) h2 $ max 1 $ d1 - a2
    n2 = ceiling $ (/) h1 $ max 1 $ d2 - a1

testcomb (hp, dp, ap) (hb, db, ab) x@((c1, d1, a1), (c2, d2, a2), (c3, d3, a3), (c4, d4, a4)) =
  (fight ((hp, dp + d1 + d2 + d3 + d4, ap + a1 + a2 + a3 + a4), (hb, db, ab)), c1 + c2 + c3 + c4, x)

allcombs =
  [(x1, x2, x3, x4) | x1 <- w1, x2 <- w2, x3 <- w3, x4 <- w3, x4 /= x3 || x4 == (0, 0, 0)]

-- answer part one: 111
solve1521_1 _ =
  head $ sortOn (\(x, y, z) -> y) $ filter (\(x, y, z) -> x) $ map (testcomb (100, 0, 0) input) $ allcombs
  where input = (109, 8, 2)


-- -------------------------------------------------------------------

-- answer part one: 188
solve1521_2 _ =
  last $ sortOn (\(x, y, z) -> y) $ filter (\(x, y, z) -> not x) $ map (testcomb (100, 0, 0) input) $ allcombs
  where input = (109, 8, 2)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 20
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

elfFill1 m n =
  trace (show n)
  [ (x * n, n * 10) | x <- [1..(div) m n] ]

presArr elfFill imax = accumArray (+) 0 (1, imax) (concat $ map (elfFill imax) [1..imax])

-- answer part one: 665280
solve1520_1 _ =
  head . filter ((>= targ) . snd) $
  assocs $ presArr elfFill1 1000000
  where targ = 29000000


-- -------------------------------------------------------------------

elfFill2 m n =
  trace (show n)
  [ (x * n, n * 11) | x <- [1..50], (x * n) < m ]

-- answer part one: 705600
solve1520_2 _ =
  head . filter ((>= targ) . snd) $
  assocs $ presArr elfFill2 1000000
  where targ = 29000000


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 19
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

subIndexes sub str =
  subIndexes' sub str 0
  where
    subIndexes' sub str i
      | i >= length str             = []
      | isPrefixOf sub $ drop i str = (i:(subIndexes' sub str (i + 1)))
      | otherwise                   = (subIndexes' sub str (i + 1))

allRepls str (sub, rep) =
  map replAt $ subIndexes sub str
  where replAt i = take i str ++ rep ++ drop (i + length sub) str

allTrans str =
  nub . concat . map (allRepls str)

solveTrans str trans =
  length $ allTrans str trans

getInputTrans (x1 : "=>" : x2 : []) = (x1, x2)

-- answer part one: 509
solve1519_1 input =
  solveTrans target trans
  where
    inputlines = lines input
    trans  = map (getInputTrans) . map words . init . init $ inputlines
    target = last $ inputlines


-- -------------------------------------------------------------------

-- TODO: way too long

-- visit trans target cache((n, l):vlist)
--   | trace (show (n, l)) False = undefined
visitTrans trans target cache ((n, l):vlist)
  | n == target               = l
  | length n >= length target = visitTrans trans target cache vlist
  | otherwise                 =
      visitTrans trans target
      (n:cache)
      (vlist ++ (map (\x -> (x, l + 1)) $ allTrans n trans))

-- answer part two: 195
solve1519_2 input =
  visitTrans trans target [] [("e", 0)]
  where
    inputlines = lines input
    trans  = map (getInputTrans) . map words . init . init $ inputlines
    target = last $ inputlines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 18
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

neighOn arr x y =
  length
  $ filter (\(x, y) -> arr ! (y, x) == '#')
  $ filter (\(x, y) -> x >= 0 && x <= 99 && y >= 0 && y <= 99) [
    (x - 1, y - 1),
    (x    , y - 1),
    (x + 1, y - 1),
    (x - 1, y    ),
    (x + 1, y    ),
    (x - 1, y + 1),
    (x    , y + 1),
    (x + 1, y + 1)]

nextArr arr =
  array ((0, 0), (99, 99)) [((y, x), next arr y x) | x <- [0..99], y <- [0..99]]
  where
    next arr x y
      | arr ! (y, x) == '#' =
        if n == 2 || n == 3 then '#' else '.'
      | otherwise           =
        if n == 3 then '#' else '.'
      where n = neighOn arr x y

-- answer part one: 821
solve1518_1 input =
  length $ filter (== '#') $ elems $ loop 100 arr
  where
    lns = lines input
    arr = array ((0, 0), (99, 99)) [((y, x), lns !! y !! x) | x <- [0..99], y <- [0..99]]
    loop 0 arr = arr
    loop n arr = loop (n-1) (nextArr arr)


-- -------------------------------------------------------------------

fixArr arr =
   arr // [((0, 0), '#')] // [((0, 99), '#')] // [((99, 0), '#')] // [((99, 99), '#')]

-- answer part two: 886
solve1518_2 input =
  length $ filter (== '#') $ elems $ loop 100 arr
  where
    lns = lines input
    arr = array ((0, 0), (99, 99)) [((y, x), lns !! y !! x) | x <- [0..99], y <- [0..99]]
    loop 0 arr = arr
    loop n arr = loop (n-1) (fixArr $ nextArr $ fixArr $ arr)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 17
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- -- https://wiki.haskell.org/99_questions/Solutions/26
-- combinations' 0 _  = [ [] ]
-- combinations' n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- https://rosettacode.org/wiki/Combinations#Haskell
combinations m xs = combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

allcomb l = concat $ map (\x -> combinations x l) $ [2..length l]

-- answer part one: 654
solve1517_1 =
  length . filter ((== 150) . sum) . allcomb . map toInt . lines


-- -------------------------------------------------------------------

-- answer part two: 57
solve1517_2 input =
  length $ filter ((== mnl) . length) sol
  where
    sol = filter ((== 150) . sum) . allcomb . map toInt . lines $ input
    mnl = minimum $ map length sol


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 16
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

lst = [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)
  ]

verifThing cmp mp (a, n)
  | isJust lk = cmp a (fromJust lk) n
  | otherwise = True
  where lk = M.lookup a mp

procSueLine (_:xn:xa1:xn1:xa2:xn2:xa3:xn3:[]) =
  (toInt xn, M.fromList [(xa1, toInt xn1), (xa2, toInt xn2), (xa3, toInt xn3)])
  where toInt x = read x :: Int

-- answer part one: 103
solve1516_1 =
  filter (\(n, d) -> all id $ map (verifThing cmp1 d) lst)
  . map ( procSueLine . map init . words . (++ "_"))
  . lines
  where cmp1 _ x y = x == y


-- -------------------------------------------------------------------

-- answer part two: 405
solve1516_2 =
  filter (\(n, d) -> all id $ map (verifThing cmp2 d) lst)
  . map ( procSueLine . map init . words . (++ "_"))
  . lines
  where
    cmp2 "cats" x y = x > y
    cmp2 "trees" x y = x > y
    cmp2 "pomeranians" x y = x < y
    cmp2 "goldfish" x y = x < y
    cmp2 _ x y = x == y


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 15
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- TODO: too long

-- Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
-- Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
-- Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
-- Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1

inputProperties = [
  ("Frosting",     [ 4, -2,  0,  0,  5]),
  ("Candy",        [ 0,  5, -1,  0,  8]),
  ("Butterscotch", [-1,  0,  5,  0,  6]),
  ("Sugar",        [ 0,  0, -2,  2,  1])]

sumProps (x1, x2, x3, x4) = (
  (x1, x2, x3, x4),
  (maximum $ (:[0]) $ sum $ zipWith (*) [x1, x2, x3, x4] (map ((!! 0) . snd) inputProperties)) *
  (maximum $ (:[0]) $ sum $ zipWith (*) [x1, x2, x3, x4] (map ((!! 1) . snd) inputProperties)) *
  (maximum $ (:[0]) $ sum $ zipWith (*) [x1, x2, x3, x4] (map ((!! 2) . snd) inputProperties)) *
  (maximum $ (:[0]) $ sum $ zipWith (*) [x1, x2, x3, x4] (map ((!! 3) . snd) inputProperties)))

totCals (x1, x2, x3, x4) =
  (sum $ zipWith (*) [x1, x2, x3, x4] (map ((!! 4) . snd) inputProperties))

-- answer part one: 18965440
solve1515_1 _ =
  last $ sortOn snd $ map sumProps $
  [(x1, x2, x3, x4) | x1 <- [0..100], x2 <- [0..100], x3 <- [0..100], x4 <- [0..100], x1 + x2 + x3 + x4 == 100]


-- -------------------------------------------------------------------

-- answer part two: 15862900
solve1515_2 _ =
  last $ sortOn snd $ map sumProps $ filter ((== 500) . totCals) $
  [(x1, x2, x3, x4) | x1 <- [0..100], x2 <- [0..100], x3 <- [0..100], x4 <- [0..100], x1 + x2 + x3 + x4 == 100]

-- ((24,29,31,16),18965440)
-- (68.15 secs, 40,346,530,736 bytes)

-- ((21,23,31,25),15862900)
-- (66.84 secs, 39,636,649,656 bytes)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 14
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

readSpeed smap x =
  M.insert rname rcycle smap
  where
    rname  = (!!) x 0
    rspeed = (toInt $ (!!) x 3)
    rfast  = (toInt $ (!!) x 6)
    rrest  = (toInt $ (!!) x 13)
    rcycle = (replicate rfast rspeed ++ replicate rrest 0)

getDistsAfter n smap =
  map (getDistAfter n) $ M.assocs smap
  where
    getDistAfter n (rn, rc) = (rn, sum $ take n $ cycle rc)

updateScoresAfter n smap scores =
  foldl (\scs ld -> M.insert ld (1 + M.findWithDefault 0 ld scs) scs) scores leads
  where
    dists = getDistsAfter n smap
    maxds = snd . last $ sortOn snd dists
    leads = map fst $ filter ((== maxds) . snd) dists

-- answer part one: 2655
solve1514_1 =
  sortOn snd . getDistsAfter 2503 . foldl readSpeed M.empty . map words . lines


-- -------------------------------------------------------------------

-- answer part two: 1059
solve1514_2 str =
  sortOn snd . M.assocs $ foldl (\acc x -> updateScoresAfter x smap acc) M.empty [1..2503]
  where smap = foldl readSpeed M.empty . map words $ lines str


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 13
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

getPref mp (x1:_:x2:x3:_:_:_:_:_:_:x4:[]) =
  M.insert (x1, init x4) hp mp
  where
    hn = (read x3 :: Int)
    hp = if x2 == "gain" then hn else (-hn)

hapTot mp ln =
  sum $ map (\(x, y) -> (M.findWithDefault 0 (x, y) mp)) $ l1 ++ l2
  where
    l1 = [(ln !! x, ln !! (mod (x+1) (length ln))) | x <- [0..(length ln - 1)]]
    l2 = [(y, x) | (x, y) <- l1]

-- answer part one: 709
solve1513_1 input =
  maximumBy (\(x, _) (y, _) -> compare x y) $
  map (\x -> ((hapTot mp x), x)) $
  permutations $ ls
  where
    mp = foldl getPref M.empty . map words . lines $ input
    ls = nub $ concat [[x, y] | (x, y) <- (M.keys mp)]

-- -------------------------------------------------------------------

-- answer part two: 668
solve1513_2 input =
  maximumBy (\(x, _) (y, _) -> compare x y) $
  map (\x -> ((hapTot mp x), x)) $
  permutations $ ls ++ ["Me"]
  where
    mp = foldl getPref M.empty . map words . lines $ input
    ls = nub $ concat [[x, y] | (x, y) <- (M.keys mp)]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 12
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

process1 str
  | null rst1    = []
  | otherwise    = (numb:process1 rst2)
  where
    (init, rst1) = break (`elem` ('-':['0'..'9'])) str
    (numb, rst2) = span  (`elem` ('-':['0'..'9'])) rst1

-- answer part one: 111754
solve1512_1 =
  sum . map read . concat . map process1 . lines

-- TODO: haskell part 2 (json)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 11
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

nextValid p
  | isValid np = np
  | otherwise  = nextValid np
  where
    np = nextPass p

    isValid x = chkReq0 x && chkReq1 x && chkReq2 x && chkReq3 x
      where
        chkReq0 = all id . map isAsciiLower
        chkReq1 (x1 : x2 : x3 : xs)
          | (ord x2 == (ord x1 + 1) && ord x3 == (ord x2 + 1)) = True
          | otherwise = chkReq1 (x2 : x3 : xs)
        chkReq1 _ = False
        chkReq2 = null . intersect ['i', 'o', 'l']
        chkReq3 = (>= 2) . length . filter (>=2) . map length . group

    nextPass x = reverse $ nextPass' $ reverse x
      where
        nextPass' ('z' : xs) = ('a' : nextPass' xs)
        nextPass' (x   : xs) = ((chr $ (+1) $ ord x) : xs)

-- answer part one: hepxxyzz
solve1511_1 _ = nextValid "hepxcrrq"


-- -------------------------------------------------------------------

-- answer part two: heqaabcc
solve1511_2 _ = nextValid $ solve1511_1 0


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 10
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

lookandsay =
  concat . map (\x -> chr((length x) + ord('0')) : [head x]) . group

-- answer part one: 252594
solve1510_1 = length . last . take 41 . iterate lookandsay $ "1113222113"


-- -------------------------------------------------------------------

-- answer part two: 3579328
solve1510_2 = length . last . take 51 . iterate lookandsay $ "1113222113"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 9
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

readdists ((x1 : "to" : x2 : "=" : x3 : []) : xs) =
  ((x1, x2), toInt x3) : ((x2, x1), toInt x3) : readdists xs
readdists _ = []

disttot dists (x1 : x2 : xs) =
  ((M.!) dists (x1, x2)) + (disttot dists (x2 : xs))
disttot _      _             = 0

allpathes dists = map (disttot dists) . permutations . nub $ concat [[x, y] | (x, y) <- M.keys dists]

-- answer part one: 207
solve159_1 = minimum . allpathes . M.fromList . readdists . map words . lines


-- -------------------------------------------------------------------

-- answer part two: 804
solve159_2 = maximum . allpathes . M.fromList . readdists . map words . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

memdelta []                 = 0
memdelta ('\"' : xs)        = 1 + memdelta xs
memdelta ('\\' : '\\' : xs) = 1 + memdelta xs
memdelta ('\\' : '\"' : xs) = 1 + memdelta xs
memdelta ('\\' : 'x'  : x1 : x2 : xs) = 3 + memdelta xs
memdelta (_ : xs) = memdelta xs

-- answer part one: 1371
solve158_1 = sum . map memdelta . lines


-- -------------------------------------------------------------------

encdelta []          = 2
encdelta ('\"' : xs) = 1 + encdelta xs
encdelta ('\\' : xs) = 1 + encdelta xs
encdelta (_    : xs) = encdelta xs

-- answer part two: 2117
solve158_2 = sum . map encdelta . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

getinstr (src             : "->" : dst : []) = (dst, [src])
getinstr (cmd : op1       : "->" : dst : []) = (dst, [cmd, op1])
getinstr (op1 : cmd : op2 : "->" : dst : []) = (dst, [cmd, op1, op2])

compute1 cache (oper : op1       : []) instrs =
  ((computeF1 oper) value1, newcache1)
  where
    (value1, newcache1) = (computeC cache [op1] instrs)
    computeF1 "NOT"     = (complement)

compute1 cache (oper : op1 : op2 : []) instrs =
  ((computeF2 oper) value1 value2, newcache2)
  where
    (value1, newcache1) = (computeC cache [op1] instrs)
    (value2, newcache2) = (computeC newcache1 [op2] instrs)
    computeF2 "AND"     = (.&.)
    computeF2 "OR"      = (.|.)
    computeF2 "RSHIFT"  = (shiftR)
    computeF2 "LSHIFT"  = (shiftL)

compute1 cache (op1                  : []) instrs
  | isDigit (head op1) = (toInt op1, cache)
  | otherwise = computeC cache (instrs M.! op1) instrs

computeC cache instr instrs
  | M.member instr cache = ((M.!) cache instr, cache)
  | otherwise            =
    (value, M.insert instr value newcache)
  where (value, newcache) = compute1 cache instr instrs

-- answer part one: 16076
solve157_1 = fst . computeC M.empty ["a"] . M.fromList . map (getinstr . words) . lines


-- -------------------------------------------------------------------

-- answer part two: 2797
solve157_2 = fst . computeC M.empty ["a"] .
  M.insert "b" ["16076"] .
  M.fromList . map (getinstr . words) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- TODO: too slow

-- answer part one: 377891
solve156_1 = sum . M.elems . foldl' (execinstr) grid0 . map words . lines
  where
    grid0 = M.fromList [((x, y), 0) | x <- [0..999], y <- [0..999]]

    execinstr grid ("turn" : "on"  : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) (const 1)

    execinstr grid ("turn" : "off" : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) (const 0)

    execinstr grid ("toggle"       : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) ((`mod` 2) . (+) 1)

    updategrid grid (x1, y1) (x2, y2) fct =
      foldl' (\g c -> M.adjust fct c g) grid [(x, y) | x <- [x1..x2], y <- [y1..y2]]

    coords = (\l -> (l !! 0, l !! 1)) . map toInt . splitOn (== ',')


-- -------------------------------------------------------------------

-- answer part two: 14110788
solve156_2 = sum . M.elems . foldl' (execinstr) grid0 . map words . lines
  where
    grid0 = M.fromList [((x, y), 0) | x <- [0..999], y <- [0..999]]

    execinstr grid ("turn" : "on"  : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) ((+) 1)

    execinstr grid ("turn" : "off" : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) (\v -> max 0 (v - 1))

    execinstr grid ("toggle"       : p1 : "through" : p2 : []) =
      updategrid grid (coords p1) (coords p2) ((+) 2)

    updategrid grid (x1, y1) (x2, y2) fct =
      foldl' (\g c -> M.adjust fct c g) grid [(x, y) | x <- [x1..x2], y <- [y1..y2]]

    coords = (\l -> (l !! 0, l !! 1)) . map toInt . splitOn (== ',')


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

isnicestr s
  | nbvowels s < 3  = False
  | lgestrow s < 2  = False
  | issubstr "ab" s = False
  | issubstr "cd" s = False
  | issubstr "pq" s = False
  | issubstr "xy" s = False
  | otherwise       = True
  where
    nbvowels x = length . filter (`elem` "aeiou") $ x
    lgestrow x = maximum . map ( length ) . group $ x
    issubstr x s = any (isPrefixOf x) $ tails s

-- answer part one: 236
solve155_1 = length . filter ( isnicestr ) . lines


-- -------------------------------------------------------------------

isnicerstr s =
  find2tw s && findtrw s
  where
    findprf x y (c1 : c2 : tl)
      | x == c1 && y == c2 = True
      | otherwise = findprf x y (c2 : tl)
    findprf _ _ _ = False

    find2tw (c1 : c2 : tl)
      | findprf c1 c2 tl = True
      | otherwise = find2tw (c2 : tl)
    find2tw _     = False

    findtrw (x : y : z : tl)
      | x == z    = True
      | otherwise = findtrw (y : z : tl)
    findtrw _     = False

-- answer part two: 51
solve155_2 = length . filter ( isnicerstr ) . lines


-- -------------------------------------------------------------------

check155 = runTestTT $ TestList [
  ((isnicestr "ugknbfddgicrmopn")  ~=? True),
  ((isnicestr "aaa")               ~=? True),
  ((isnicestr "jchzalrnumimnmhp")  ~=? False),
  ((isnicestr "haegwjzuvuyypxyu")  ~=? False),
  ((isnicestr "dvszwmarrgswjxmb")  ~=? False),
  --
  ((isnicerstr "qjhvhtzxzqqjkmpb") ~=? True),
  ((isnicerstr "xxyxx")            ~=? True),
  ((isnicerstr "uurcxstgmygtbstg") ~=? False),
  ((isnicerstr "ieodomkazucvgmuy") ~=? False)
  ]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

minemd5 chk key num
  | trace (show (num)) False = undefined
  | chk . md5sum $ key <> show num = num
  | otherwise = minemd5 chk key (num + 1)

-- answer part one: 117946
solve154_1 _ = minemd5 (isPrefixOf "00000") "ckczppom" 0


-- -------------------------------------------------------------------

-- answer part two: 3938038
solve154_2 _ = minemd5 (isPrefixOf "000000") "ckczppom" 0

-- checkleading0s nz =
--   (>= nz) . length . takeWhile (== '0')

-- checkmd5 ('0' : '0' : '0' : '0' : '0' : tl ) = True
-- checkmd5 _ = False

-- -------------------------------------------------------------------

check154 = runTestTT $ TestList [
  ((minemd5 (isPrefixOf "00000") "abcdef" 0)    ~=?  609043),
  ((minemd5 (isPrefixOf "00000") "pqrstuv" 0)   ~=? 1048970)
  ]


-- -------------------------------------------------------------------

md5sum :: [Char] -> String
md5sum =
  show . md5 . BS.pack . map (fromIntegral . ord)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

deliverh hset (x, y) []        = S.insert (x, y) hset
deliverh hset (x, y) ('^' : l) = deliverh (S.insert (x, y) hset) (x, y + 1) l
deliverh hset (x, y) ('>' : l) = deliverh (S.insert (x, y) hset) (x + 1, y) l
deliverh hset (x, y) ('v' : l) = deliverh (S.insert (x, y) hset) (x, y - 1) l
deliverh hset (x, y) ('<' : l) = deliverh (S.insert (x, y) hset) (x - 1, y) l

-- answer part one: 2081
solve153_1 = S.size . deliverh S.empty (0, 0)


-- -------------------------------------------------------------------

nextH (x, y) '^' = (x, y + 1)
nextH (x, y) '>' = (x + 1, y)
nextH (x, y) 'v' = (x, y - 1)
nextH (x, y) '<' = (x - 1, y)

deliverh2 hset c1 c2 [] =
  S.insert c1 (S.insert c2 hset)
deliverh2 hset c1 c2 (x1 : x2 : l) =
  deliverh2 (S.insert c1 (S.insert c2 hset)) (nextH c1 x1) (nextH c2 x2) l

-- answer part two: 2341
solve153_2 = S.size . deliverh2 S.empty (0, 0) (0, 0)


-- -------------------------------------------------------------------

check153 = runTestTT $ TestList [
  ((solve153_1 ">")          ~=?  2),
  ((solve153_1 "^>v<")       ~=?  4),
  ((solve153_1 "^v^v^v^v^v") ~=?  2),
  --
  ((solve153_2 "^v")         ~=?  3),
  ((solve153_2 "^>v<")       ~=?  3),
  ((solve153_2 "^v^v^v^v^v") ~=? 11)
  ]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

wrapp (l : w : h : []) = (2 * l * w) + (2 * w * h) + (2 * h * l) + (s1 * s2)
  where [s1, s2, _ ] = sort [l, w, h]

-- answer part one: 1606483
solve152_1 = sum . map (wrapp . map toInt . splitOn (== 'x')) . lines


-- -------------------------------------------------------------------

ribbp (l : w : h : []) = (2 * s1) + (2 * s2) + (l * w * h)
  where [s1, s2, _ ] = sort [l, w, h]

-- answer part two: 3842356
solve152_2 = sum . map (ribbp . map toInt . splitOn (== 'x')) . lines


-- -------------------------------------------------------------------

check152 = runTestTT $ TestList [
  ((wrapp [2, 3, 4])         ~=?  58),
  ((wrapp [1, 1, 10])        ~=?  43),
  --
  ((ribbp [2, 3, 4])         ~=?  34),
  ((ribbp [1, 1, 10])        ~=?  14)
  ]


-- -------------------------------------------------------------------

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f (x : xs)
  | not (f x) =
      let (ht, tw) = (break f xs)
      in (x : ht) : (splitOn f tw)
  | otherwise = (splitOn f xs)

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2015 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

movefloor []         = 0
movefloor ('(' : xs) = (1) + (movefloor xs)
movefloor (')' : xs) = (-1) + (movefloor xs)

-- answer part one: 74
solve151_1 = movefloor


-- -------------------------------------------------------------------

movebasem (-1) pos _          = pos
movebasem lev  pos ('(' : xs) = movebasem (lev + 1) (pos + 1) xs
movebasem lev  pos (')' : xs) = movebasem (lev - 1) (pos + 1) xs
movebasem _    _   []         = error "basement not visited"
movebasem _    _   _          = error "unknown instruction"

-- answer part two: 1795
solve151_2 = movebasem 0 0


-- -------------------------------------------------------------------

check151 = runTestTT $ TestList [
  ((solve151_1 "(())")         ~=?  0),
  ((solve151_1 "()()")         ~=?  0),
  ((solve151_1 "(((")          ~=?  3),
  ((solve151_1 "(()(()(")      ~=?  3),
  ((solve151_1 "))(((((")      ~=?  3),
  ((solve151_1 "())")          ~=? -1),
  ((solve151_1 "))(")          ~=? -1),
  ((solve151_1 ")))")          ~=? -3),
  ((solve151_1 ")())())")      ~=? -3),
  --
  ((solve151_2 ")")            ~=?  1),
  ((solve151_2 "()())")        ~=?  5)
  ]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
