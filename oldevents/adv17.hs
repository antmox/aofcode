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
import qualified Data.Sequence as S
import System.Environment (getArgs)
import Test.HUnit

-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

main =
  getArgs >>= getInput >>= return . solve1725_1 >>= print
  where
    getInput (l:_) = readFile l
    getInput []    = getContents


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 25
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

turingsteps state tape cursor steps
  | trace (show (steps)) False = undefined

  | steps == 0 = length . filter (==1) . M.elems $ tape

  | state == 'A' && value == 0 =
    turingsteps 'B' (write 1) (right) (nsteps)
  | state == 'A' && value == 1 =
    turingsteps 'E' (write 0) (left)  (nsteps)

  | state == 'B' && value == 0 =
    turingsteps 'C' (write 1) (left)  (nsteps)
  | state == 'B' && value == 1 =
    turingsteps 'A' (write 0) (right) (nsteps)

  | state == 'C' && value == 0 =
    turingsteps 'D' (write 1) (left)  (nsteps)
  | state == 'C' && value == 1 =
    turingsteps 'C' (write 0) (right) (nsteps)

  | state == 'D' && value == 0 =
    turingsteps 'E' (write 1) (left)  (nsteps)
  | state == 'D' && value == 1 =
    turingsteps 'F' (write 0) (left)  (nsteps)

  | state == 'E' && value == 0 =
    turingsteps 'A' (write 1) (left)  (nsteps)
  | state == 'E' && value == 1 =
    turingsteps 'C' (write 1) (left)  (nsteps)

  | state == 'F' && value == 0 =
    turingsteps 'E' (write 1) (left)  (nsteps)
  | state == 'F' && value == 1 =
    turingsteps 'A' (write 1) (right) (nsteps)

  where
    value   = M.findWithDefault 0 cursor tape
    write n = M.insert cursor n tape
    left    = cursor - 1
    right   = cursor + 1
    nsteps  = steps - 1

-- answer part one: 4387
solve1725_1 _ =
  turingsteps 'A' M.empty 0 12208951


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 24
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

visitpipe [] = []
visitpipe ((comps, bridge, port):xs)
  | trace (show (bridge)) False = undefined
  | null nbrd = (bstrength : visitpipe xs)
  | otherwise   = (visitpipe (xs ++ nbrd))
  where
    othr (a:b:_) = if (a == port) then b else a
    poss = filter (elem port) comps
    nbrd = map (\p -> ((comps \\ [p]), (p:bridge), (othr p))) poss
    bstrength = sum . map (\(a:b:_) -> a + b) $ bridge

-- answer part one: 1695
solve1724_1 input =
  maximum $ visitpipe [(comps, [], 0)]
  where comps = map (map (\x -> read x :: Int) . spliton (=='/')) . lines $ input


-- -------------------------------------------------------------------

visitpipe2 [] = []
visitpipe2 ((comps, bridge, port):xs)
  | trace (show (bridge)) False = undefined
  | null nbrd = ((blength, bstrength) : visitpipe2 xs)
  | otherwise = (visitpipe2 (xs ++ nbrd))
  where
    othr (a:b:_) = if (a == port) then b else a
    poss = filter (elem port) comps
    nbrd = map (\p -> ((comps \\ [p]), (p:bridge), (othr p))) poss
    bstrength = sum . map (\(a:b:_) -> a + b) $ bridge
    blength   = length $ bridge

-- answer part two: 1673
solve1724_2 input =
  maximum $ visitpipe2 [(comps, [], 0)]
  where comps = map (map (\x -> read x :: Int) . spliton (=='/')) . lines $ input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 23
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

excopro instr regs pc =
  case instr of
   ("set":x:y:_) -> (M.insert x (value y) regs, pc + 1)
   ("sub":x:y:_) -> (M.insert x (value x - value y) regs, pc + 1)
   ("mul":x:y:_) -> (M.insert "mul" (value "mul" + 1) (M.insert x (value x * value y) regs), pc + 1)
   ("jnz":x:y:_) -> (if value x == 0 then (regs, pc + 1) else (regs, pc + (value y)))
  where
    value x
      | isLetter $ head x = M.findWithDefault 0 x regs
      | otherwise = (read x :: Int)

loopcopro regs pc instrs
  | np < 0 || np >= (length instrs) = regs
  | otherwise = loopcopro nr np instrs
  where (nr, np) = excopro (instrs !! pc) regs pc

-- answer part one: 8281
solve1723_1 =
  (loopcopro M.empty 0) . map words . lines


-- -------------------------------------------------------------------

  -- int h = 0;
  -- for (int b = 109300; b <= 126300; b += 17) {
  --   // for (d = 2; d <= b; d++) {
  --   //   for (e = 2; e <= b; e++) {
  --   //     if ((d * e) == b) { h++; goto next; } }
  --   // }
  --   // next: ;
  --   if (!isprime(b)) h++;
  -- }

-- https://wiki.haskell.org/Testing_primality
isPrime n = n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
  where
    pfactors prs n =
      unfoldr (\(ds,n) -> listToMaybe
                          [(x, (dropWhile (< x) ds, div n x))
                          | x <- takeWhile ((<=n).(^2)) ds ++ [n | n>1], mod n x==0]) (prs,n)
    primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]

-- answer part two: 911
solve1723_2 _ =
  length . filter (not . isPrime) $ [109300,109317..126300]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 22
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

turn 'l' 'u' = 'l'
turn 'l' 'l' = 'd'
turn 'l' 'd' = 'r'
turn 'l' 'r' = 'u'

turn 'r' 'u' = 'r'
turn 'r' 'l' = 'u'
turn 'r' 'd' = 'l'
turn 'r' 'r' = 'd'

turn 'o' 'u' = 'd'
turn 'o' 'l' = 'r'
turn 'o' 'd' = 'u'
turn 'o' 'r' = 'l'

move 'u' x y = (x, y - 1, 'u')
move 'l' x y = (x - 1, y, 'l')
move 'd' x y = (x, y + 1, 'd')
move 'r' x y = (x + 1, y, 'r')

burst i n (x, y, d) gmap
  | n == 10000  = i
  | cell == '.' = burst (i+1) (n+1) (move (turn 'l' d) x y) infect
  | cell == '#' = burst (i)   (n+1) (move (turn 'r' d) x y) clean
  where
    cell = M.findWithDefault '.' (x, y) gmap
    infect = M.insert (x, y) '#' gmap
    clean  = M.insert (x, y) '.' gmap

-- answer part one: 5196
solve1722_1 input =
  burst 0 0 pos0 gmap
  where
    grid = lines input
    gmap = M.fromList $ zip [(x, y) | y <- [0..(length grid - 1)], x <- [0..(length grid - 1)]] (concat grid)
    pos0 = (div (length grid) 2, div (length grid) 2, 'u')


-- -------------------------------------------------------------------

burst2 i n (x, y, d) gmap
  | n == 10000000 = i
  | cell == '.' = burst2 (i)   (n+1) (move (turn 'l' d) x y) (modf 'W')
  | cell == 'W' = burst2 (i+1) (n+1) (move (d)          x y) (modf '#')
  | cell == '#' = burst2 (i)   (n+1) (move (turn 'r' d) x y) (modf 'F')
  | cell == 'F' = burst2 (i)   (n+1) (move (turn 'o' d) x y) (modf '.')
  where
    cell = M.findWithDefault '.' (x, y) gmap
    modf s = M.insert (x, y) s gmap
    clean  = M.insert (x, y) '.' gmap

-- answer part two: 2511633
solve1722_2 input =
  burst2 0 0 pos0 gmap
  where
    grid = lines input
    gmap = M.fromList $ zip [(x, y) | y <- [0..(length grid - 1)], x <- [0..(length grid - 1)]] (concat grid)
    pos0 = (div (length grid) 2, div (length grid) 2, 'u')


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 21
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

start = [".#.", "..#", "###"]

allvariants g0 =
  (rotations g0) ++ (rotations (reverse g0))
  where rotations = take 4 . iterate (reverse . transpose)

makerules (s1:s2:[]) =
  map (\x1 -> (x1, l2)) $ allvariants l1
  where (l1, l2) = (spliton (=='/') s1, spliton (=='/') s2)

enhance rules n grid
  | n == 0    = grid
  | otherwise = enhance rules (n-1) ngrid
  where ngrid = step rules grid

step rules grid
  | otherwise = result
  where
    result = recompose $ transf
    transf = map (\l -> M.findWithDefault l l rules) $ decomp
    decomp = decompose grid

splitSz n l =
  map (\x -> take n . drop x $ l) [(x * n) | x <- [0..(div (length l) n) - 1]]

decompose l =
  concat . map transpose . splitSz subsz $ grid
  where
    gsize = truncate . sqrt . fromIntegral . length $ l
    grid  = map (splitSz subsz) . splitSz gsize $ l
    subsz = 2 + mod (gsize) 2

recompose l =
  concat . concat . concat . map transpose . splitSz gsize $ l
  where gsize = truncate . sqrt . fromIntegral . length $ l

-- answer part one: 136
solve1721_1 input =
  length . filter (=='#') .
  enhance rules 5 . concat $ start
  where
    rules = M.fromList . concat . map (makerules . spliton (`elem` " =>")) . lines $ input


-- -------------------------------------------------------------------

-- answer part two: 1911767
solve1721_2 input =
  length . filter (=='#') .
  enhance rules 18 . concat $ start
  where
    rules = M.fromList . concat . map (makerules . spliton (`elem` " =>")) . lines $ input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 20
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>

partmove (px:py:pz:vx:vy:vz:ax:ay:az:[]) =
  ((px + vx + ax):(py + vy + ay):(pz + vz + az):(vx + ax):(vy + ay):(vz + az):ax:ay:az:[])

loopticks n parts
  | n == 1000 = parts
  | otherwise = loopticks (n + 1) newparts
  where newparts = map partmove parts

partdist (px:py:pz:_) = (abs px) + (abs py) + (abs pz)

-- answer part one: 161
solve1720_1 =
  (\l -> elemIndex (minimum l) l) .
  map partdist . loopticks 0 . map (map (\x -> read x :: Int) . spliton (`elem` "pva=<,> ")) . lines


-- -------------------------------------------------------------------

loopcollid n parts
  -- | trace (show filtered) False = undefined
  | n == 1000 = parts
  | otherwise = loopcollid (n + 1) filtered
  where
    newparts = map partmove parts
    filtered =
      (\l -> filter (\(px:py:pz:_) ->
                      (==1) . length
                      $ findIndices (\(ex:ey:ez:_) -> (px==ex && py==ey && pz==ez)) l) l)
      $ newparts

-- answer part two: 438
solve1720_2 =
  length . loopcollid 0 . map (map (\x -> read x :: Int) . spliton (`elem` "pva=<,> ")) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 19
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

--     |
--     |  +--+
--     A  |  C
-- F---|----E|--+
--     |  |  |  D
--     +B-+  +--+

nextpos x y 'u' = (x, y - 1)
nextpos x y 'd' = (x, y + 1)
nextpos x y 'r' = (x + 1, y)
nextpos x y 'l' = (x - 1, y)

nextdir x y d ls
  | (d == 'u' || d == 'd') && (ls !! y !! (x - 1) /= ' ') = (x - 1, y, 'l')
  | (d == 'u' || d == 'd') && (ls !! y !! (x + 1) /= ' ') = (x + 1, y, 'r')
  | (d == 'r' || d == 'l') && (ls !! (y - 1) !! x /= ' ') = (x, y - 1, 'u')
  | (d == 'r' || d == 'l') && (ls !! (y + 1) !! x /= ' ') = (x, y + 1, 'd')
  | otherwise = undefined

walkpath x y d ls
  | cc == '+'   = (walkpath dx dy nd ls)
  | isLetter cc = (cc:(walkpath nx ny d ls))
  | cc == ' '   = []
  | otherwise   = (walkpath nx ny d ls) -- '|' or '-'
  where
    cc = (ls !! y !! x)
    (nx, ny) = nextpos x y d
    (dx, dy, nd) = nextdir x y d ls

-- answer part one: EPYDUXANIT
solve1719_1 input =
  walkpath x0 0 'd' ls
  where
    ls = lines input
    x0 = fromJust $ findIndex (/=' ') (ls !! 0)


-- -------------------------------------------------------------------

walkpath2 x y d ls
  | cc == ' '   = 0
  | cc == '+'   = 1 + (walkpath2 dx dy nd ls)
  | otherwise   = 1 + (walkpath2 nx ny d ls)
  where
    cc = (ls !! y !! x)
    (nx, ny) = nextpos x y d
    (dx, dy, nd) = nextdir x y d ls

-- answer part two: 17544
solve1719_2 input =
  walkpath2 x0 0 'd' ls
  where
    ls = lines input
    x0 = fromJust $ findIndex (/=' ') (ls !! 0)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 18
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

exduet instr regs pc =
  case instr of
   ("snd":x:_)   -> (M.insert "lastsnd" (value x) regs, pc + 1)
   ("set":x:y:_) -> (M.insert x (value y) regs, pc + 1)
   ("add":x:y:_) -> (M.insert x (value x + value y) regs, pc + 1)
   ("mul":x:y:_) -> (M.insert x (value x * value y) regs, pc + 1)
   ("mod":x:y:_) -> (M.insert x (value x `mod` value y) regs, pc + 1)
   ("rcv":x:_)   -> (if value x == 0 then (regs, pc + 1) else (regs, -1))
   ("jgz":x:y:_) -> (if value x <= 0 then (regs, pc + 1) else (regs, pc + (value y)))
  where
    value x
      | isLetter $ head x = M.findWithDefault 0 x regs
      | otherwise = (read x :: Int)

loopduet regs pc instrs
  | np < 0 = regs
  | otherwise = loopduet nr np instrs
  where (nr, np) = exduet (instrs !! pc) regs pc

-- answer part one: 4601
solve1718_1 =
  (loopduet M.empty 0) . map words . lines


-- -------------------------------------------------------------------

exduet2 instr regs pc qrc qsn nbs =
  case instr of
   ("set":x:y:_) -> (M.insert x (value y) regs, pc + 1, qrc, qsn, nbs, True)
   ("add":x:y:_) -> (M.insert x (value x + value y) regs, pc + 1, qrc, qsn, nbs, True)
   ("mul":x:y:_) -> (M.insert x (value x * value y) regs, pc + 1, qrc, qsn, nbs, True)
   ("mod":x:y:_) -> (M.insert x (value x `mod` value y) regs, pc + 1, qrc, qsn, nbs, True)
   ("jgz":x:y:_) -> (if value x <= 0 then (regs, pc + 1, qrc, qsn, nbs, True)
                     else (regs, pc + (value y), qrc, qsn, nbs, True))
   --
   ("snd":x:_)   -> (regs, pc + 1, qrc, qsn ++ [value x], nbs + 1, True)
   ("rcv":x:_)   ->
     if (null qrc) then (regs, pc, qrc, qsn, nbs, False)
     else (M.insert x (head qrc) regs, pc + 1, tail qrc, qsn, nbs, True)
  where
    value x
      | isLetter $ head x = M.findWithDefault 0 x regs
      | otherwise = (read x :: Int)

loopduet2 regs1 pc1 qv1 nb1 regs2 pc2 qv2 nb2 instrs
  | (not e1) && (not e2) = (nn1, nn2)
  | otherwise = loopduet2 nr1 np1 nv12 nn1 nr2 np2 nv22 nn2 instrs
  where
    (nr1, np1, nv11, nv21, nn1, e1) = exduet2 (instrs !! pc1) regs1 pc1 qv1 qv2 nb1
    (nr2, np2, nv22, nv12, nn2, e2) = exduet2 (instrs !! pc2) regs2 pc2 nv21 nv11 nb2

-- answer part two: 6858
solve1718_2 =
  (loopduet2 (M.fromList [("p", 0)]) 0 [] 0 (M.fromList [("p", 1)]) 0 [] 0) . map words . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 17
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

evolveidx stp len idx =
  (+ 1) . (`mod` len) . (+ stp) $ idx

insertnel lst idx val = ys ++ [val] ++ zs
  where (ys, zs) = splitAt idx lst

spinlock lst len idx val stp
  | val == 2017 = (!!) lst (evolveidx stp len idx)
  | otherwise = spinlock newl (len + 1) newi (val + 1) stp
  where
    newi = evolveidx stp len idx
    newl = insertnel lst newi val

-- input: 382

-- answer part one: 1561
solve1717_1 =
  spinlock [0] 1 0 1 382

-- -------------------------------------------------------------------

-- answer part two: 33454823
solve1717_2 _ =
  foldl' spinlock2 (0, 0) [1..50000000]
  where
    spinlock2 (vzr, idx) val
      -- | trace (show (val, vzr, idx)) False = undefined
      | seq vzr False = undefined
      | otherwise = (newl, newi)
      where
        newi = (+ 1) . (`mod` val) . (+ 382) $ idx
        newl = if newi == 1 then val else vzr


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 16
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

dspin x = reverse . (\r -> (drop x r) ++ (take x r)) . reverse

dexch a b s = adjust (const (s !! b)) a . adjust (const (s !! a)) b $ s

dpart a b s = dexch (fromJust $ elemIndex a s) (fromJust $ elemIndex b s) s

dmove progs ('s':xs) =
  dspin (read xs :: Int) progs
dmove progs ('x':xs) =
  let l = spliton (=='/') xs
  in dexch (read $ l !! 0 :: Int) (read $ l !! 1 :: Int) progs
dmove progs ('p':a:'/':b:[]) =
  dpart a b progs

--answer part two: ionlbkfeajgdmphc
solve1716_1 =
  foldl dmove ['a'..'p'] . spliton (== ',') . head . lines


-- -------------------------------------------------------------------

parsemove ('s':xs) =
  dspin (read xs :: Int)
parsemove ('x':xs) =
  let l = spliton (=='/') xs
  in dexch (read $ l !! 0 :: Int) (read $ l !! 1 :: Int)
parsemove ('p':a:'/':b:[]) =
  dpart a b

fmove progs func = func progs

moveloop cache progs moves n
  | n == 0 = progs
  | isJust cacheent =
    moveloop cache (fromJust cacheent) moves (n - 1)
  | otherwise =
    moveloop newcache newprogs moves (n - 1)
  where
    cacheent = M.lookup progs cache
    newprogs = foldl' fmove progs moves
    newcache = M.insert progs newprogs cache

--answer part two: fdnphiegakolcmjb
solve1716_2 s =
  moveloop M.empty ['a'..'p'] moves 1000000000
  where moves = map parsemove . spliton (== ',') . head . lines $ s


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 15
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

loopvalue ml dv v0 = iterate (\p -> (mod) (p * ml) dv) $ v0

cmplow16 v1 v2
  | ((.&.) v1 0xFFFF :: Integer) == ((.&.) v2 0xFFFF :: Integer) = 1
  | otherwise = 0

-- answer part one: 592
solve1715_1 _ =
  foldl' (+) 0 . take 40000000 $ zipWith (cmplow16)
  (loopvalue 16807 2147483647 277)
  (loopvalue 48271 2147483647 349)


-- -------------------------------------------------------------------

-- answer part two: 320
solve1715_2 _ =
  foldl' (+) 0 . take 5000000 $ zipWith (cmplow16)
  (filter ((==0) . (`mod` 4)) $ loopvalue 16807 2147483647 277)
  (filter ((==0) . (`mod` 8)) $ loopvalue 48271 2147483647 349)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 14
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

knothashh s =
  concat . map (\x -> printf "%08b" x :: String) .
  densehash .
  repeatknot 64 [0..255] 0 0 . (++ [17, 31, 73, 47, 23]) . map ord $ s

knotgrid s =
  map (map ((-48+) . ord) . knothashh) .
  map ((\n -> (printf "%s-%d" s n)) :: Int -> String) $ [0..127]

-- input: wenycdww

-- answer part one: 8226
solve1714_1 input =
  sum . concat . knotgrid $ input


-- -------------------------------------------------------------------

-- :-(((((

isvalid (x, y) = (x >= 0 && y >= 0 && x <= 127 && y <= 127)
isused grid (x, y) = (grid !! x !! y) == 1
ismarked regs (x, y) = M.member (x, y) regs

visitregion []         _    regs _     = regs
visitregion ((x,y):xs) grid regs value
  | ismarked regs (x, y) = visitregion xs grid regs value
  | otherwise = visitregion (xs ++ neighs) grid nwregs value
  where
    nwregs = M.insert (x, y) value regs
    neighs =
      filter (\c -> (isvalid c) && (isused grid c) && not (ismarked regs c)) $
      [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

visitcell grid regs (x, y)
  | not $ isused grid (x, y) = regs
  | ismarked regs (x, y) = regs
  | otherwise = visitregion [(x, y)] grid regs nval
  where nval = (+1) . length . nub . M.elems $ regs

-- answer part one: 1128
solve1714_2 input
  | trace (show ("GRID", grid)) False = undefined
  | trace (show ("REGS", regs)) False = undefined
  | otherwise = length . nub . M.elems $ regs
  where
    grid = knotgrid input
    regs = foldl (visitcell grid) M.empty $ [(x, y) | x <- [0..127], y <- [0..127]]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 13
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

fastpos time range =
  ((range - 1) -) . abs $ (range - 1) - ((mod) time ((range - 1) * 2))

ftripscore (score, (time, pos)) (x:r:_) =
  (score + value, (scantime + 1, x + 1))
  where
    scantime = (time + x - pos)
    value = if ((fastpos scantime r) == 0) then (x * r) else 0

-- answer part one: 2384
solve1713_1 =
  fst . foldl ftripscore (0, (0, 0)) . map (map read . spliton (`elem` ": ")) . lines


-- -------------------------------------------------------------------

tripsafe _    _   [] = True
tripsafe time pos ((x:r:_):xs)
  | fastpos scantime r == 0 = False
  | otherwise = tripsafe (scantime + 1) (x + 1) xs
  where scantime = (time + x - pos)

findsafe d scanlist
  | (tripsafe 0 (-d) scanlist) = d
  | otherwise = findsafe (d + 1) scanlist

-- answer part two: 3921270
solve1713_2 =
  findsafe (-1) . map (map read . spliton (`elem` ": ")) . lines


-- -------------------------------------------------------------------

-- initial solution for part one

scannerpos time scanmap
  | range == 0 = (-1)
  | otherwise  = (!! time) . cycle $ [0..(range - 1)] ++ (reverse $ [1..(range-2)])
  where range  = M.findWithDefault 0 time scanmap

tripscore time scanmap
  | time > depthmax  = 0
  | scannpos == 0    = (time * range) + triptail
  | otherwise        = triptail
  where
    depthmax = head . reverse . sort . M.keys $ scanmap
    scannpos = scannerpos time scanmap
    triptail = (tripscore (time + 1) scanmap)
    range = M.findWithDefault 0 time scanmap

solve1713_3 =
  tripscore 0 . M.fromList . map ((\(x:y:_) -> (x, y)) . map (\x -> read x :: Int) . spliton (`elem` ": ")) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 12
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

visitneighs []     _ _ = []
visitneighs (x:xs) c m
  | (elem) x c = (visitneighs xs c m)
  | otherwise  = (x : (visitneighs (xs ++ (M.findWithDefault [] x m)) (x:c) m))

-- answer part one: 175
solve1712_1 =
  length . (visitneighs ["0"] []) .
  M.fromList . map (\l -> ((head l), (tail l))) . map (spliton (`elem` " <->,")) . lines


visitgroups []     gs _ = gs
visitgroups (x:xs) gs m
  | any (elem x) gs = visitgroups xs gs m
  | otherwise = visitgroups xs ((visitneighs [x] [] m):gs) m

-- answer part two: 213
solve1712_2 =
  length . (\m -> visitgroups (M.keys m) [] m) .
  M.fromList . map (\l -> ((head l), (tail l))) . map (spliton (`elem` " <->,")) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 11
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

hexmove (x, y) "n"  = (x,     y + 1)
hexmove (x, y) "ne" = (x + 1, y + 1)
hexmove (x, y) "se" = (x + 1, y)
hexmove (x, y) "s"  = (x,     y - 1)
hexmove (x, y) "sw" = (x - 1, y - 1)
hexmove (x, y) "nw" = (x - 1, y)

hexdist (x, y) = maximum [ abs x, abs y, abs $ (abs x) - (abs y) ]

-- answer part one: 687
solve1711_1 =
  hexdist . foldl hexmove (0, 0) . spliton (==',') . head . lines

-- answer part two: 1483
solve1711_2 =
  maximum . map hexdist . scanl hexmove (0, 0) . spliton (==',') . head . lines

-- http://3dmdesign.com/development/hexmap-coordinates-the-easy-way

-- Move N:         y + 1
-- Move NE: x + 1, y + 1
-- Move SE: x + 1
-- Move S:         y - 1
-- Move SW: x - 1, y - 1
-- Move NW: x - 1

-- Distance is equal to the greatest of the absolute values of:
--   the difference along the x-axis,
--   the difference along the y-axis,
--   or the difference of these two differences.


-- -------------------------------------------------------------------

spliton _ [] = []
spliton f (x:xs)
  | not (f x) =
      let (ht, tw) = (break f xs)
      in (x:ht):(spliton f tw)
  | otherwise = (spliton f xs)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 10
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

knothash l _ _ [] = l
knothash l idx skp (x:xs) = knothash inserted (idx+x+skp) (skp+1) xs
  where
    selected = reverse . take x . drop idx . cycle $ l
    inserted = insertl idx l selected
    insertl _ l []     = l
    insertl i l (x:xs) = insertl (i+1) (adjust (const x) (mod i (length l)) l) xs

-- answer part one: 52070
solve1710_1 =
  knothash [0..255] 0 0 . map ((+0) . read) . splitwith (/=',') . head . lines


-- -------------------------------------------------------------------

knothash2 l i s [] = (l, i, s)
knothash2 l idx skp (x:xs)
  | otherwise = knothash2 inserted (idx+x+skp) (skp+1) xs
  where
    selected = reverse . take x . drop idx . cycle $ l
    inserted = insertl idx l selected
    insertl _ l []     = l
    insertl i l (x:xs) = insertl (i+1) (adjust (const x) (mod i (length l)) l) xs

repeatknot 0 l _ _ _  = l
repeatknot n l i s ln = repeatknot (n-1) nl ni ns ln
  where (nl, ni, ns)  = knothash2 l i s ln

densehash [] = []
densehash l  =
  (foldl (\x y -> (xor) y x :: Int) 0 h16) : (densehash tl)
  where (h16, tl) = splitAt 16 l

-- answer part two: 7f94112db4e32e19cf6502073c66f9bb
solve1710_2 =
  concat . map (\x -> printf "%02x" x :: String) . densehash .
  repeatknot 64 [0..255] 0 0 . (++ [17, 31, 73, 47, 23]) . map ord . head . lines


-- -------------------------------------------------------------------

splitwith _ [] = []
splitwith f (x:xs)
  | (f x) =
      let (ht, tw) = (span f xs)
      in (x:ht):(splitwith f tw)
  | otherwise = (splitwith f xs)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 9
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

groupscore _    ingarb []         = []
groupscore prof ingarb ('!':x:xs) = groupscore prof ingarb xs
groupscore prof False  ('<':xs)   = groupscore prof True xs
groupscore prof False  ('{':xs)   = prof:groupscore (prof+1) False xs
groupscore prof False  ('}':xs)   = groupscore (prof-1) False xs
groupscore prof True   ('>':xs)   = groupscore prof False xs
groupscore prof ingarb (x:xs)     = groupscore prof ingarb xs

-- answer part one: 11347
solve179_1 = sum . groupscore 1 False . head . lines

-- prof is now useless here
groupsgarb _    ingarb []         = 0
groupsgarb prof ingarb ('!':x:xs) = groupsgarb prof ingarb xs
groupsgarb prof False  ('<':xs)   = groupsgarb prof True xs
groupsgarb prof False  ('{':xs)   = groupsgarb (prof+1) False xs
groupsgarb prof False  ('}':xs)   = groupsgarb (prof-1) False xs
groupsgarb prof True   ('>':xs)   = groupsgarb prof False xs
groupsgarb prof True   (x:xs)     = 1 + groupsgarb prof True xs
groupsgarb prof False  (x:xs)     = groupsgarb prof False xs

-- answer part two: 5404
solve179_2 = groupsgarb 1 False . head . lines

groupspars (n, g) _    ingarb []         = (n, g)
groupspars (n, g) prof ingarb ('!':x:xs) = groupspars (n, g) prof ingarb xs
groupspars (n, g) prof False  ('<':xs)   = groupspars (n, g) prof True xs
groupspars (n, g) prof False  ('{':xs)   = groupspars (n + prof, g) (prof + 1) False xs
groupspars (n, g) prof False  ('}':xs)   = groupspars (n, g) (prof - 1) False xs
groupspars (n, g) prof True   ('>':xs)   = groupspars (n, g) prof False xs
groupspars (n, g) prof True   (x:xs)     = groupspars (n, g + 1) prof True xs
groupspars (n, g) prof False  (x:xs)     = groupspars (n, g) prof False xs

solve179_3 = groupspars (0, 0) 1 False . head . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

evalcond (_ : _ : _ : _ : var : op : val : []) regs
  | op == "==" = (vvar == vval)
  | op == "!=" = (vvar /= vval)
  | op == "<=" = (vvar <= vval)
  | op == "<"  = (vvar <  vval)
  | op == ">=" = (vvar >= vval)
  | op == ">"  = (vvar >  vval)
  where
    vvar = (M.findWithDefault 0 var regs)
    vval = (read val :: Int)

evalop (var : op : val : _ : _ : _ : _ : []) regs
  | op == "dec" = (M.insert var (vvar - vval) regs)
  | op == "inc" = (M.insert var (vvar + vval) regs)
  where
    vvar = (M.findWithDefault 0 var regs)
    vval = (read val :: Int)

processline regs words
  | evalcond words regs = evalop words regs
  | otherwise = regs

-- answer part one: 4416
solve178_1 =
  maximum . M.elems . foldl processline3 M.empty . map words . lines


processline2 (regs, maxv) words
  | evalcond words regs = maxr $ evalop words regs
  | otherwise = (regs, maxv)
  where maxr regs = (regs, max maxv (maximum . M.elems $ regs))

-- answer part two: 5199
solve178_2 =
  snd . foldl processline2 (M.empty, 0) . map words . lines

-- alternative for part1
processline3 regs (var1 : op1 : val1 : _ : var2 : op2 : val2 : [])
  | (w2cmpop op2) (getrval var2) (w2int val2) = (M.insert var1 ((w2modop op1) (getrval var1) (w2int val1)) regs)
  | otherwise = regs
  where
    w2cmpop "=="  = (==)
    w2cmpop "!="  = (/=)
    w2cmpop "<"   = (<)
    w2cmpop ">"   = (>)
    w2cmpop "<="  = (<=)
    w2cmpop ">="  = (>=)
    w2modop "dec" = (-)
    w2modop "inc" = (+)
    getrval var   =  M.findWithDefault 0 var regs
    w2int   val   = (read val :: Int)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

calcweight allnodes node =
  (totalweight, (M.insert node totalweight weightmap))
  where
    (nodeweight, nodesuccs)  = fromJust $ M.lookup node allnodes
    (totalweight, weightmap) =
      foldl (\(n, w) (tw, mp) -> ((n + tw), (M.union w mp))) (nodeweight, M.empty)
      $ map (calcweight allnodes) nodesuccs

checkbalance allnodes node expected weightmap
  | trace (show (node, expected, computedweight, nodeweight, succsweights)) False = undefined
  -- no successors (leaf) -> not implemented
  | length nodesuccs == 0 = undefined
  -- all successors have the same weights -> problem is on the current node
  | length succsweights_freqs == 1 = (nodeweight + (expected - computedweight))
  -- otherwise -> find the wrongly balanced successor and recurse
  | otherwise = checkbalance allnodes wrongsucc wrongexpected weightmap
  where
    -- ("tknk", ["ugml","padx","fwft"])
    (nodeweight, nodesuccs) = fromJust $ M.lookup node allnodes
    computedweight = fromJust $ M.lookup node weightmap
    -- list of weight of successors [(251,"ugml"),(243,"padx"),(243,"fwft")]
    succsweights = map (\x -> (fromJust $ M.lookup x weightmap, x)) nodesuccs
    -- sorted (number of occurences, succs weights) [(251,1),(243,2)]
    succsweights_freqs = sortOn snd . frequency $ map fst $ succsweights
    -- select the successor with the unique weight (different from the others) ugml
    wrongsucc = fromJust $ M.lookup (fst . head $ succsweights_freqs) (M.fromList succsweights)
    -- and select its expected weight for good balancing 243
    wrongexpected = fst . last $ succsweights_freqs
    -- stackoverflow.com/a/22398506
    frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

-- answer part one: vgzejbd
-- answer part two: 1226

solve177 str =
  checkbalance allnodes rootnode 0 . snd $ calcweight allnodes rootnode
  where
    splitline words =
      let node   = head words
          weight = read . filter isDigit . head $ tail words
          succs  = map (filter isLetter) $ drop 3 words
      in (node, (weight, succs))
    allnodes = M.fromList . map ( splitline . words ) . lines $ str
    allsuccs = concat . map snd $ M.elems allnodes
    rootnode = head . filter (\x -> not $ (elem) x allsuccs) $ M.keys allnodes


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

adjust func index list =
  (take index list) ++ [func ((!!) list index)] ++ (drop (index + 1) list)

redistribute values index num
  | num == 0  = values
  | otherwise = redistribute (adjust (+1) index values) (mod (index + 1) (length values)) (num - 1)

reallocate values =
  redistribute newl (mod (maxi + 1) (length values)) maxv
  where
    maxv = maximum values
    maxi = fromJust $ elemIndex maxv values
    newl = adjust (const 0) maxi values

redist_loop cache nbiter values
  | elem values cache = (nbiter, (+1) . fromJust $ elemIndex values cache)
  | otherwise = redist_loop (values:cache) (nbiter+1) (reallocate values)

-- input: "4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3"
-- answer part 1: 6681
-- answer part 2: 2392
solve176 = redist_loop [] 0 . map read . words . (!! 0) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

walkmaze nbsteps position offsets
  | position >= (M.size offsets) = nbsteps
  | otherwise = walkmaze (nbsteps + 1) (position + curroff) newoffs
  where
    curroff = M.findWithDefault 0 position offsets
    newoffs = M.insert position (curroff + 1) offsets

-- answer part one: 373543
solve175_1 =
  walkmaze 0 0 . M.fromList . zip [0..] . map read . lines


-- takes forever to complete (>50s)
walkmaze2 nbsteps position offsets
  | (seq) nbsteps False = undefined
  | position >= (M.size offsets) = nbsteps
  | otherwise = walkmaze2 (nbsteps + 1) (position + curroff) newoffs
  where
    curroff = M.findWithDefault 0 position offsets
    ofdelta = if (curroff >= 3) then (-1) else 1
    newoffs = M.insert position (curroff + ofdelta) offsets

-- answer part two: 27502966
solve175_2 =
  walkmaze2 0 0 . M.fromList . zip [0..] . map read . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 451
solve174_1 = length . filter (\l -> (length l) == (length . nub $ l)) . map words . lines


-- answer part two: 223
solve174_2 = length . filter (\l -> (length l) == (length . nub . map sort $ l)) . map words . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

circnxt target num (x, y) sq d
  | num == target = (abs(x) + abs(y))
  -- change direction
  | x ==  sq && y == -sq = circnxt target (num + 1) (x + 1, y)     (sq + 1) 'U'
  | x ==  sq && y ==  sq = circnxt target (num + 1) (x - 1, y)     (sq)     'L'
  | x == -sq && y ==  sq = circnxt target (num + 1) (x,     y - 1) (sq)     'D'
  | x == -sq && y == -sq = circnxt target (num + 1) (x + 1, y)     (sq)     'R'
  -- move forward
  | d == 'U'             = circnxt target (num + 1) (x,     y + 1) (sq)     d
  | d == 'L'             = circnxt target (num + 1) (x - 1, y)     (sq)     d
  | d == 'D'             = circnxt target (num + 1) (x,     y - 1) (sq)     d
  | d == 'R'             = circnxt target (num + 1) (x + 1, y)     (sq)     d

-- input: 289326

-- answer part one: 419
solve173_1 = circnxt 289326 1 (0, 0) 0 'R'


circsum mp target num (x, y) sq d
  | trace (show (num,(x, y), sq, d, value)) False = undefined
  | value >= target = value
  | x ==  sq && y == -sq = circsum newmp target (num + 1) (x + 1, y)     (sq + 1) 'U'
  | x ==  sq && y ==  sq = circsum newmp target (num + 1) (x - 1, y)     (sq)     'L'
  | x == -sq && y ==  sq = circsum newmp target (num + 1) (x,     y - 1) (sq)     'D'
  | x == -sq && y == -sq = circsum newmp target (num + 1) (x + 1, y)     (sq)     'R'
  | d == 'U'             = circsum newmp target (num + 1) (x,     y + 1) (sq)     d
  | d == 'L'             = circsum newmp target (num + 1) (x - 1, y)     (sq)     d
  | d == 'D'             = circsum newmp target (num + 1) (x,     y - 1) (sq)     d
  | d == 'R'             = circsum newmp target (num + 1) (x + 1, y)     (sq)     d
  where
    value = sum . map (\pos -> M.findWithDefault 0 pos mp) $ [(x, y) | x <- [x-1..x+1], y <- [y-1..y+1]]
    newmp = M.insert (x, y) value mp

-- answer part two: 295229
solve173_2 = circsum (M.fromList [((0, 0), 1)]) 289326 1 (0, 0) 0 'R'


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 47136
solve172_1 = sum . map ((\l -> (maximum l) - (minimum l)) . map read . words) . lines


-- answer part two: 250
solve172_2 = sum . map ((\l -> head $ [ (div x y) | x <- l, y <- l \\ [x], mod x y == 0 ]) . map read . words) . lines


-- -------------------------------------------------------------------

check172 = runTestTT $ TestList [
  ((solve172_1 "5 1 9 5") ~=? 8),
  ((solve172_1 "7 5 3")   ~=? 4),
  ((solve172_1 "2 4 6 8") ~=? 6),
  ((solve172_1 "5 1 9 5\n7 5 3\n2 4 6 8") ~=? 18),
  --
  ((solve172_2 "5 9 2 8") ~=? 4),
  ((solve172_2 "9 4 7 3") ~=? 3),
  ((solve172_2 "3 8 6 5") ~=? 2),
  ((solve172_2 "5 9 2 8\n9 4 7 3\n3 8 6 5") ~=? 9)
  ]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2017 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 1150
solve171_1 =
  sum . map (\(x, y) -> if (x == y) then (digitToInt x) else 0) . (\s -> zip (drop 1 . cycle $ s) s) . (!! 0) . lines

-- answer part two: 1064
solve171_2 =
  sum . map (\(x, y) -> if (x == y) then (digitToInt x) else 0) . (\s -> zip (drop (div (length s) 2) . cycle $ s) s) . (!! 0) . lines


-- -------------------------------------------------------------------

sum_next_eq (x:y:xs)
  | x == y    = (digitToInt x) + tail_res
  | otherwise = tail_res
  where tail_res = sum_next_eq (y:xs)
sum_next_eq _ = 0

solve171_1bis = sum_next_eq . (\(x:xs) -> reverse $ x:(reverse $ x:xs)) . (!! 0) . lines


sum_half_eq idx lst
  | trace (show (lst, idx)) False = undefined
  | idx >= (length lst) = 0
  | (lst !! idx) == (lst !! half_idx) = (digitToInt (lst !! idx)) + tail_res
  | otherwise = tail_res
  where
    half_idx = (mod (idx + (div (length lst) 2)) (length lst))
    tail_res = sum_half_eq (idx + 1) lst

solve171_2bis = sum_half_eq 0 . (!! 0) . lines


-- -------------------------------------------------------------------

check171 = runTestTT $ TestList [
  ((solve171_1 "1122")        ~=? 3),
  ((solve171_1 "1111")        ~=? 4),
  ((solve171_1 "1234")        ~=? 0),
  ((solve171_1 "91212129")    ~=? 9),
  --
  ((solve171_2 "1212")        ~=? 6),
  ((solve171_2 "1221")        ~=? 0),
  ((solve171_2 "123425")      ~=? 4),
  ((solve171_2 "123123")      ~=? 12),
  ((solve171_2 "12131415")    ~=? 4)
  ]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
