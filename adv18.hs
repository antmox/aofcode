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
import System.Environment (getArgs)
import Test.HUnit
import Data.Foldable


-------------------------------------------------------------------
-------------------------------------------------------------------

main =
  getArgs >>= getInput >>= return . solve1820_1 >>= print
  where
    getInput (l:_) = readFile l
    getInput []    = getContents

-- readFile "inputs/180x.in" >>= return . solve18x_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 25
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

allConsts consts stars
  | null stars = consts
  | otherwise  = allConsts (newof : dfconsts) (tail stars)
  where
    (ofconsts, dfconsts) = partition (belongsto (head stars)) consts
    newof = ((head stars) : concat ofconsts)
    belongsto s = any ((<= 3) . mandist4 s)

-- answer part one: 352
solve1825_1 =
  length .
  -- list of all constellations
  allConsts [] .
  -- list of all stars
  map ((\(x1 : x2 : x3 : x4 : []) -> (x1, x2, x3, x4)) .
        map toInt . splitOn (== ',')) . lines


-- -------------------------------------------------------------------

mandist4 (x1, y1, z1, t1) (x2, y2, z2, t2) =
  (abs (x2 - x1)) + (abs (y2 - y1)) + (abs (z2 - z1)) + (abs (t2 - t1))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 24
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- TODO cleanup

-- Immune System:

groups0 =
  M.fromList [
    --           units init   hps   attack dmg & type    immunities & weaknesses
    (("ims", 1), ( 889,  12,  3275,   36, "bludgeoning", ["cold"], ["bludgeoning", "radiation"])),
    (("ims", 2), (  94,   7,  1336,  127, "bludgeoning", [], ["radiation", "cold"])),
    (("ims", 3), (1990,  20,  5438,   25, "slashing",    [], [])),
    (("ims", 4), (1211,  19,  6640,   54, "fire",        [], [])),
    (("ims", 5), (3026,  16,  7938,   26, "bludgeoning", ["cold"], ["bludgeoning"])),
    (("ims", 6), (6440,   4,  9654,   14, "fire",        [], [])),
    (("ims", 7), (2609,   3,  8218,   28, "cold",        [], ["bludgeoning"])),
    (("ims", 8), (3232,  14, 11865,   30, "slashing",    [], ["radiation"])),
    (("ims", 9), (2835,   2,  7220,   18, "bludgeoning", ["fire", "radiation"], [])),
    (("ims",10), (2570,  17,  4797,   15, "radiation",   [], ["cold"])),

    (("inf", 1), ( 333,  13, 44943,  223, "slashing",    [], ["bludgeoning"])),
    (("inf", 2), (1038,   8, 10867,   16, "cold",        ["bludgeoning", "slashing", "fire"], [])),
    (("inf", 3), (  57,   5, 50892, 1732, "cold",        [], [])),
    (("inf", 4), ( 196,   6, 36139,  334, "fire",        [], ["cold"])),
    (("inf", 5), (2886,   1, 45736,   25, "cold",        ["cold"], ["slashing"])),
    (("inf", 6), (4484,  18, 37913,   16, "fire",        ["fire", "radiation", "slashing"], ["bludgeoning"])),
    (("inf", 7), (1852,   9, 49409,   52, "radiation",   ["bludgeoning"], ["radiation"])),
    (("inf", 8), (3049,  10, 18862,   12, "fire",        [], ["radiation"])),
    (("inf", 9), (1186,  15, 23898,   34, "bludgeoning", ["fire"], [])),
    (("inf",10), (6003,  11, 12593,    2, "cold",        [], []))
    ]

--  889 units each with  3275 hit points (weak to bludgeoning, radiation; immune to cold) with an attack that does 36 bludgeoning damage at initiative 12
--   94 units each with  1336 hit points (weak to radiation, cold) with an attack that does 127 bludgeoning damage at initiative 7
-- 1990 units each with  5438 hit points with an attack that does 25 slashing damage at initiative 20
-- 1211 units each with  6640 hit points with an attack that does 54 fire damage at initiative 19
-- 3026 units each with  7938 hit points (weak to bludgeoning; immune to cold) with an attack that does 26 bludgeoning damage at initiative 16
-- 6440 units each with  9654 hit points with an attack that does 14 fire damage at initiative 4
-- 2609 units each with  8218 hit points (weak to bludgeoning) with an attack that does 28 cold damage at initiative 3
-- 3232 units each with 11865 hit points (weak to radiation) with an attack that does 30 slashing damage at initiative 14
-- 2835 units each with  7220 hit points (immune to fire, radiation) with an attack that does 18 bludgeoning damage at initiative 2
-- 2570 units each with  4797 hit points (weak to cold) with an attack that does 15 radiation damage at initiative 17

-- Infection:
--  333 units each with 44943 hit points (weak to bludgeoning) with an attack that does 223 slashing damage at initiative 13
-- 1038 units each with 10867 hit points (immune to bludgeoning, slashing, fire) with an attack that does 16 cold damage at initiative 8
--   57 units each with 50892 hit points with an attack that does 1732 cold damage at initiative 5
--  196 units each with 36139 hit points (weak to cold) with an attack that does 334 fire damage at initiative 6
-- 2886 units each with 45736 hit points (immune to cold; weak to slashing) with an attack that does 25 cold damage at initiative 1
-- 4484 units each with 37913 hit points (weak to bludgeoning; immune to fire, radiation, slashing) with an attack that does 16 fire damage at initiative 18
-- 1852 units each with 49409 hit points (immune to bludgeoning; weak to radiation) with an attack that does 52 radiation damage at initiative 9
-- 3049 units each with 18862 hit points (weak to radiation) with an attack that does 12 fire damage at initiative 10
-- 1186 units each with 23898 hit points (immune to fire) with an attack that does 34 bludgeoning damage at initiative 15
-- 6003 units each with 12593 hit points with an attack that does 2 cold damage at initiative 11

-- -------------------------------------------------------------------

-- Immune System:
-- 17 units each with 5390 hit points (weak to radiation, bludgeoning) with
--  an attack that does 4507 fire damage at initiative 2
-- 989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
--  slashing) with an attack that does 25 slashing damage at initiative 3
--
-- Infection:
-- 801 units each with 4706 hit points (weak to radiation) with an attack
--  that does 116 bludgeoning damage at initiative 1
-- 4485 units each with 2961 hit points (immune to radiation; weak to fire,
--  cold) with an attack that does 12 slashing damage at initiative 4

groups1 =
  M.fromList [
    --           units init    hps   attack dmg & type    immunities & weaknesses
    (("ims", 1), (  17,   2,   5390, 4507, "fire",        [], ["radiation", "bludgeoning"])),
    (("ims", 2), ( 989,   3,   1274,   25, "slashing",    ["fire"], ["bludgeoning", "slashing"])),
    (("inf", 1), ( 801,   1,   4706,  116, "bludgeoning", [], ["radiation"])),
    (("inf", 2), (4485,   4,   2961,   12, "slashing",    ["radiation"], ["fire", "cold"]))
  ]

-- -------------------------------------------------------------------

showGroups groups =
  concat . map ((:) '\n') . map sgr . sort . M.assocs $ groups
  where
    sgr ((g, n), (u, i, h, a, _, _, _)) =
      printf "%3s-gr%-2d | %4d units | %4d hp, %4d at, %d in" g n u h a i

selectTargets groups selection
--  | trace (show ("SELECT-1", choosers)) False = undefined
  | null choosers = selection
--  | trace (show ("SELECT-2", fchooser, ennemies)) False = undefined
  | null ennemies =
    selectTargets groups (M.insert fchooser Nothing selection)
--  | trace (show ("SELECT-3", ftrgvals)) False = undefined
--  | trace (show ("SELECT-4", fselectd)) False = undefined
  | dmg == 0  = -- trace (printf "  SEL %3s-gr%d -> Nothing" fa fn) $
      selectTargets groups (M.insert fchooser Nothing selection)
  | otherwise = -- trace (printf "  SEL %3s-gr%d -> %3s-gr%d" fa fn ea en) $
      selectTargets groups (M.insert fchooser (Just (ea, en)) selection)
  where
    selorder (n, i, _, a, _, _, _) = (n * a, i)
    choosers =
      reverse . sortOn (selorder . (M.!) groups) $
      ((M.keys groups) \\ (M.keys selection))
    fchooser @ (fa, fn) = head choosers
    ennemies = filter (\(a, n) -> a /= fa) $
      (M.keys groups) \\ (catMaybes $ M.elems selection)
    trgorder ennemy
      | elem ft ei = (0, (0, 0))
      | elem ft ew = (2 * fn * fa, (en * ea, ev))
      | otherwise  = (fn * fa, (en * ea, ev))
      where
        (fn, _, _, fa, ft, _, _) = (M.!) groups fchooser
        (en, ev, _, ea, _, ei, ew) = (M.!) groups ennemy
    ftrgvals = reverse . sort . map (\e -> (trgorder e, e)) $ ennemies
    fselectd @ ((dmg, (efp, ein)), (ea, en)) = head ftrgvals


attackTargets groups selection attackers
  | null attackers = groups
--  | trace (show ("ATTACK-1", attackers)) False = undefined
--  | trace (show ("ATTACK-2", fattacker, fdefender)) False = undefined
  | not $ M.member fattacker groups =
      attackTargets groups selection (tail attackers)
  | isNothing fdefender =
      attackTargets groups selection (tail attackers)
--  | trace (show ("ATTACK-3", fdeadunts)) False = undefined
  | (en - fdeadunts) <= 0 =
    attackTargets newgroups1 selection (tail attackers)
  | otherwise = attackTargets newgroups2 selection (tail attackers)
  where
    fattacker = head attackers
    fdefender = (M.!) selection fattacker
    fdeadunts
      | elem ft ei = min en (0)
      | elem ft ew = min en ((div) (2 * fn * fa) (eh))
      | otherwise  = min en ((div) (fn * fa) (eh))
    (fn, _, _, fa, ft, _, _) = (M.!) groups fattacker
    (en, ev, eh, ea, et, ei, ew) = (M.!) groups (fromJust fdefender)
    newgroups1 =
      M.delete (fromJust fdefender) groups
    newgroups2 =
      M.insert (fromJust fdefender) (en - fdeadunts, ev, eh, ea, et, ei, ew) groups

groupFight groups
--  | trace (showGroups groups) False = undefined
  | (length armies <= 1) =
      sum . map (\(n, _, _, _, _, _, _) -> n) . M.elems $ groups
--  | trace (show ("SELECT", selection)) False = undefined
--  | trace (show ("ATTACK", newgroups)) False = undefined
  | otherwise = groupFight newgroups
  where
    armies = nub . map fst . M.keys $ groups
    selection = selectTargets groups M.empty
    atkorder (_, i, _, _, _, _, _) = i
    attackers =
      reverse . sortOn (atkorder . (M.!) groups) $
      (M.keys groups)
    newgroups = attackTargets groups selection attackers

-- answer part one: 18532
-- solve1824_1 _ = groupFight groups0

groupFight2 groups
--  | trace (showGroups groups) False = undefined
  | (notElem "ims" armies) = 0
  | (length armies <= 1)   =
      sum . map (\(n, _, _, _, _, _, _) -> n) . M.elems $ groups
--  | trace (show ("SELECT", selection)) False = undefined
--  | trace (show ("ATTACK", newgroups)) False = undefined
  | otherwise = groupFight2 newgroups
  where
    armies = nub . map fst . M.keys $ groups
    selection = selectTargets groups M.empty
    atkorder (_, i, _, _, _, _, _) = i
    attackers =
      reverse . sortOn (atkorder . (M.!) groups) $
      (M.keys groups)
    newgroups = attackTargets groups selection attackers

testBoost groups boost
  | trace (printf "BOOST %d" boost) False = undefined
  | result == 0 = testBoost groups (boost + 1)
  | otherwise = result
  where
    appBoost (g, _) (n, v, h, a, t, i, w)
      | g == "ims" = (n, v, h, a + boost, t, i, w)
      | otherwise  = (n, v, h, a, t, i, w)
    modgroups = M.mapWithKey appBoost groups
    result    = groupFight2 modgroups

-- answer part two: 6523 (boost = 29)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 23
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 219
solve1823_1 input =
  (strongest, inrange, length inrange)
  where
    bots = map (
      (\(x : y : z : r : []) -> ((x, y, z), r)) .
        map toInt . splitOn (flip elem "<=,rpos >")) . lines $ input
    strongest @ ((xs, ys, zs), rs) = maximumOn (snd) bots
    inrange = filter ((<= rs) . mandist3 (xs, ys, zs) . fst) $ bots


-- -------------------------------------------------------------------

-- reddit.com/r/adventofcode/comments/a99n7x/2018_day_23_part_2_explanation_with_visualization/
-- reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecfmpy0

boxNIntersects bots sz (bx, by, bz) =
  ((bx, by, bz, sz),
  length . filter (\((x, y, z), r) -> ((rd x bx (bx + sz - 1)) + (rd y by (by + sz - 1)) + (rd z bz (bz + sz - 1))) <= r) $ bots)
  where rd v l h = if (v < l) then (l - v) else if (v > h) then (v - h) else 0

bestPoint bots open
  | trace (show ((bx, by, bz, sz), nb)) False = undefined
  | sz == 1   = (mandist3 (0, 0, 0) (bx, by, bz))
  | otherwise = bestPoint bots (nxtboxes ++ (open \\ [((bx, by, bz, sz), nb)]))
  where
    ((bx, by, bz, sz), nb) = maximumOn (\((x, y, z, s), n) -> (n, -(mandist3 (x, y, z) (0, 0, 0)))) open
    hs = (div) sz 2
    subboxes =
      (bx, by, bz) : (bx + hs, by, bz) : (bx, by + hs, bz) : (bx + hs, by + hs, bz) :
      (bx, by, bz + hs) : (bx + hs, by, bz + hs) : (bx, by + hs, bz + hs) : (bx + hs, by + hs, bz + hs) : []
    nxtboxes = map (boxNIntersects bots hs) subboxes

-- answer part two: 83779034
solve1823_2 input =
  bestPoint bots [boxNIntersects bots (2*po2v) (-po2v, -po2v, -po2v)]
  where
    bots = map (
      (\(x : y : z : r : []) -> ((x, y, z), r)) .
        map toInt . splitOn (flip elem "<=,rpos >")) . lines $ input
    vmax = maximum . map abs . concat . map (\((x, y, z), _) -> [x, y, z]) $ bots 
    po2v = head . dropWhile (<= vmax) $ [2 ^ x | x <- [1..]]


-- -------------------------------------------------------------------

mandist3 (x1, y1, z1) (x2, y2, z2) =
  (abs (x2 - x1)) + (abs (y2 - y1)) + (abs (z2 - z1))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 22
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

showCave lmap (tx, ty) =
  (concat . map color) . concat . map ((:) '\n') $
  [[regty (x, y) | x <- [0 .. tx]] | y <- [0 .. ty]]
  where
    regty (x, y)
      | (x, y) == (tx, ty) = 'T'
      | (x, y) == (0, 0)   = 'M'
      | otherwise = regType $ getErLevel lmap (x, y)
    color x
      | x == '|'  = printf "\ESC[1;32m\ \%c\ \\ESC[0;m" x
      | x == '='  = printf "\ESC[1;34m\ \%c\ \\ESC[0;m" x
      | x == '.'  = printf "\ESC[1;31m\ \%c\ \\ESC[0;m" x
      | otherwise = printf "%c" x

getErLevel lmap (x, y)
  | M.member (x, y) lmap = (M.!) lmap (x, y)
  | otherwise = undefined

mapELevel depth (tx, ty) =
  foldl (setErLevel (tx, ty) depth) M.empty $
  -- XXXX [(x, y) | y <- [0 .. ty], x <- [0 .. tx]]
  [(x, y) | y <- [0 .. 1000], x <- [0 .. 100]]

geoIndex target lmap (x, y)
  | (x, y) == (0, 0)   = 0
  | (x, y) == target   = 0
  | (y == 0)           = x * 16807
  | (x == 0)           = y * 48271
  | otherwise =
      (getErLevel lmap (x - 1, y)) * (getErLevel lmap (x, y - 1))

regType el
  | m == 0 = '.'
  | m == 1 = '='
  | m == 2 = '|'
  where m = ((mod) el 3)

setErLevel target depth lmap (x, y) =
  M.insert (x, y) l lmap
  where l = (mod (geoIndex target lmap (x, y) + depth) 20183)

-- depth: 510
-- target: 10,10

-- depth: 11991
-- target: 6,797

-- answer part one: 5622
solve1822_1 depth tx ty
  | trace (showCave lmap (tx, ty)) False = undefined
  | otherwise = sum $ map (flip (mod) 3) . M.elems $ lmap
  where lmap  = mapELevel depth (tx, ty)


-- -------------------------------------------------------------------

-- neither: '0'
-- torch: 'T'
-- climbing gear: 'C'

-- astar with mandist as heuristic
shortestPath lmap (tx, ty) closed open
  | trace (printf "%3d %3d %c | %3d | %3d" x y t d (d + mandist (x, y) (tx, ty))) False = undefined
  | (x, y, t) == (tx, ty, 'T') = d
  | otherwise = shortestPath lmap (tx, ty) (S.insert (x, y, t) closed) next_open
  where
    ((x, y, t), d) = minimumOn (\((x, y, t), d) -> d + mandist (x, y) (tx, ty)) open
    rt = regType $ getErLevel lmap (x, y)
    other_tool
      | rt == '.' = head $ ['C', 'T'] \\ [t]
      | rt == '=' = head $ ['C', '0'] \\ [t]
      | rt == '|' = head $ ['T', '0'] \\ [t]
    isValid rt
      | rt == '.' = t /= '0'
      | rt == '=' = t /= 'T'
      | rt == '|' = t /= 'C'
    next_cells =
      filter (isValid . regType . getErLevel lmap) .
      filter (flip M.member lmap) $
      [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    next_moves = ((x, y, other_tool), d + 7) : [((nx, ny, t), d + 1) | (nx, ny) <- next_cells]
    unqmin_nxt = M.assocs . M.fromListWith min $ (next_moves ++ open)
    next_open = filter (\((x, y, t), d) -> S.notMember (x, y, t) closed) unqmin_nxt

-- TODO: too long

-- answer part two: 1089
solve1822_2 depth tx ty
  | trace (showCave lmap (tx, ty)) False = undefined
  | otherwise =
      shortestPath lmap (tx, ty) S.empty [((0, 0, 'T'), 0)]
  where lmap  = mapELevel depth (tx, ty)
-- (1363.24 secs, 865,040,929,440 bytes)

minimumOn f = minimumBy (\x1 x2 -> compare (f x1) (f x2))
maximumOn f = maximumBy (\x1 x2 -> compare (f x1) (f x2))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 21
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

execProgX regs ipr imap
  | M.notMember pc imap = [((M.!) regs 0)]
  --  | pc == 28,
  --   trace ((printf "%2d | %s %3d %3d %3d | " pc opc ra rb rc) ++
  --          (concat . map (printf "%16d") . M.elems $ regs)) False = undefined
  | pc == 28 = ((M.!) regs 3) : execProgX nregs ipr imap
  | otherwise = execProgX nregs ipr imap
  where
    pc = (M.!) regs ipr
    (opc, ra, rb, rc) = (M.!) imap pc
    iregs   = execInstr regs opc ra rb rc
    nregs   = M.adjust (+1) ipr iregs

-- answer part one: 3909249
solve1821_1 input =
  head $ execProgX regs0 ipr imap
  where
    (ipline : inlines) = lines $ input
    ipr   = toInt . last . words $ ipline
    imap  = readInstrs M.empty 0 . map (splitOn (`elem` "[,]: ")) $ inlines
    regs0 = M.fromList $ zip [0..] [0, 0, 0, 0, 0, 0]


-- -------------------------------------------------------------------

findCycle seen (value1 : value2 : values)
  | (elem) value2 seen = value1
  | otherwise = findCycle (value2 : seen) (value2 : values)

-- answer part two: 12333799
solve1821_2 _ =
  findCycle [] $ loop21 (0 :: Int) (0 :: Int)
  where
    loop21 r2 r3 =
      (r3_2 : loop21 r2_2 r3_2)
      where
        (r2_1) = r3 .|. 0x10000
        (r2_2, r3_2) = loop22 r2_1 0x1553d2
    loop22 r2 r3
      | r2 < 256  = (r2, r3_1)
      | otherwise = loop22 r2_1 r3_1
      where
        r3_1 = (((r3 + (r2 .&. 0xff)) * 0x1016b) .&. 0xffffff)
        r2_1 = (div) r2 256


-- -------------------------------------------------------------------

-- seen = set()
-- r2 = r3 = 0
-- while True:
--     r2 = r3 | 0x10000
--     r3 = 0x1553d2
--     while True:
--         r3 = (((r3 + (r2 & 0xff)) * 0x1016b) & 0xffffff)
--         if r2 < 256: break
--         r2 = r2 / 256
--     if r3 in seen: print r3, len(seen); exit(0)
--     seen.add(r3)
--     print r3
--     if r3 == 0: break # VAL

-- # unsigned long long r0, r2, r3;
-- # int main(void) {
-- #   r3 = 0;
-- #   do {
-- #       r2 = r3 | 0x10000;
-- #       r3 = 0x1553d2;
-- #       do {
-- #           r3 = r3 + (r2 & 0xff);
-- #           r3 = r3 & 0xffffff;
-- #           r3 = r3 * 0x1016b;
-- #           r3 = r3 & 0xffffff;
-- #           if (r2 < 256) break;
-- #           r2 = r2 / 256;
-- #         }
-- #       while (1);
-- #       printf("%llu\n", r3); // return 1;
-- #     }
-- #   while (r3 != r0);
-- #   return 0;
-- # }

-- 00   seti 123 0 3        r3 <- 123
-- 01 ! bani 3 456 3        r3 <- r3 & 456
-- 02   eqri 3 72 3         r3 <- r3 == 72
-- 03 x addr 3 4 4          pc <- pc + r3
-- 04 x seti 0 0 4          pc <- 0
--
-- 05   seti 0 2 3          r3 <- 0
--
-- 06 ! bori 3 65536 2      r2 <- r3 | 65536
-- 07   seti 1397714 1 3    r3 <- 1397714
--
-- 08 ! bani 2 255 5        r5 <- r2 & 255
-- 09   addr 3 5 3          r3 <- r3 + r5
-- 10   bani 3 16777215 3   r3 <- r3 & 16777215
-- 11   muli 3 65899 3      r3 <- r3 * 65899
-- 12   bani 3 16777215 3   r3 <- r3 & 16777215
-- 13   gtir 256 2 5        r5 <- 256 > r2
-- 14 x addr 5 4 4          pc <- pc + r5
-- 15 x addi 4 1 4          pc <- pc + 1
-- 16 x seti 27 6 4         pc <- 27
-- 17   seti 0 6 5          r5 <- 0
--
-- 18 ! addi 5 1 1          r1 <- r5 + 1
-- 19   muli 1 256 1        r1 <- r1 * 256
-- 20   gtrr 1 2 1          r1 <- r1 > r2
-- 21 x addr 1 4 4          pc <- pc + r1
-- 22 x addi 4 1 4          pc <- pc + 1
-- 23 x seti 25 2 4         pc <- 25
-- 24 ! addi 5 1 5          r5 <- r5 + 1
-- 25 x seti 17 0 4         pc <- 17
-- 26 ! setr 5 7 2          r2 <- r5
-- 27 x seti 7 4 4          pc <- 7
--
-- 28 ! eqrr 3 0 5          r5 <- r3 == r0
-- 29 x addr 5 4 4          pc <- pc + r5
-- 30 x seti 5 8 4          pc <- 5


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 20
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

data RNode = RSt [Char] | RCh [[RNode]] deriving (Show)

-- get one node
--   "AA"             -> RSt "AA",  ""
--   "(AA|BB)"        -> RCh [...], ""
--   "(AA|)"          -> RCh [...], ""
getRNode str @ (x : xs)
  | isLetter x             = (RSt nd1, ftail)
  | x == '(', n == ')'     = (RCh nds, ntail)
  | x == ')'               = (RSt [],  str)
  | otherwise              =  undefined
  where
    (nd1, ftail)      = span (flip notElem "(|)$") str
    (nds, n : ntail)  = getLNodes xs

-- get a list of nodes
--   "AA|BB"          -> [RSt "AA"], "|BB"
--   "AA(CC)DD|BB"    -> [RSt "AA", RCh [...], RSt "DD"], "|BB"
getRNodes str
  | null str               = ([], str)
  | x == '|' || x == ')'   = ([], str)
  | x == '(' || isLetter x = (nd1 : ndx, ntail)
  | otherwise              = undefined
  where
    (x : _)           = str
    (nd1, ftail)      = getRNode str
    (ndx, ntail)      = getRNodes ftail

-- get a list of list of nodes
--   "AA)"            -> [[RSt "AA"]], ")"
--   "AA(CC)DD|CC"    -> [[RSt "AA", RCh [...], RSt "DD"], [RSt "CC"]], ""
getLNodes str
  | null ftail        = (nd1 : [], ftail)
  | n == ')'          = (nd1 : [], ftail)
  | n == '|'          = (nd1 : ndx, ntail)
  | otherwise         = undefined
  where
    (nd1, ftail)      = getRNodes str
    (n : ns)          = ftail
    (ndx, ntail)      = getLNodes ns

-- -------------------------------------------------------------------

-- COORD X STRING_NODE -> NEWMAP, COORD
strDest dmap (sx, sy) (c : cs)
  | c == 'N' = strNext (sx, sy - 1) cs
  | c == 'S' = strNext (sx, sy + 1) cs
  | c == 'E' = strNext (sx + 1, sy) cs
  | c == 'W' = strNext (sx - 1, sy) cs
  where
    newval = (M.findWithDefault 0 (sx, sy) dmap) + 1
    newmap newcrd  = M.insertWith min newcrd newval dmap
    strNext newcrd = strDest (newmap newcrd) newcrd
strDest dmap (sx, sy) [] = (dmap, (sx, sy))

-- COORD_LIST X ONE_NODE -> NEWMAP, NEW_COORD_LIST
nodeDests (dmap, starts) (RSt n) =
  foldl' (\(xmap, xcrds) crd ->
           let (nmap, ncrd) = strDest xmap crd n
           in  (nmap, nub $ ncrd : xcrds)
        ) (dmap, []) starts

nodeDests (dmap, starts) (RCh l) =
  foldl' (\(xmap, xcrds) ndl ->
            let (nmap, ncrds) = nodesDests (xmap, starts) ndl
            in  (nmap, nub $ ncrds ++ xcrds)
        ) (dmap, []) l

-- -- COORD_LIST X NODE_LIST -> NEWMAP, NEW_COORD_LIST
nodesDests (dmap, starts) [] = (dmap, starts)
nodesDests (dmap, starts) (n : ns) =
  nodesDests (nodeDests (dmap, starts) n) ns

-- -------------------------------------------------------------------

-- answer part one: 3465
solve1820_1 =
    last . sort . M.elems .
    -- distance map [((x, y), dist), ...]
    fst . nodesDests (M.empty, [(0, 0)]) .
    -- regnode list
    fst . getRNodes . filter (flip elem "(NS|EW)")

-- -------------------------------------------------------------------

-- answer part two: 7956
solve1820_2 =
    length . filter (>= 1000) . M.elems .
    -- distance map [((x, y), dist), ...]
    fst . nodesDests (M.empty, [(0, 0)]) .
    -- regnode list
    fst . getRNodes . filter (flip elem "(NS|EW)")


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 19
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

readInstrs imap n ((opcode : rs) : ls) =
  readInstrs nmap (n + 1) ls
  where
    (r1 : r2 : r3 : []) = map toInt rs
    (nmap) = (M.insert n (opcode, r1, r2, r3) imap)
readInstrs imap _ [] = imap

execProg regs ipr imap
  | M.notMember pc imap = regs
  | trace ((printf "%2d | %s %d %d %d | " pc opc ra rb rc) ++
           (concat . map (printf "%6d") . M.elems $ regs)) False = undefined
  | otherwise = execProg nregs ipr imap
  where
    pc = (M.!) regs ipr
    (opc, ra, rb, rc) = (M.!) imap pc
    iregs   = execInstr regs opc ra rb rc
    nregs   = M.adjust (+1) ipr iregs


-- answer part one: 920
solve1819_1 input =
  execProg regs0 ipr imap
  where
    (ipline : inlines) = lines $ input
    ipr   = toInt . last . words $ ipline
    imap  = readInstrs M.empty 0 . map (splitOn (`elem` "[,]: ")) $ inlines
    regs0 = M.fromList $ zip [0..] [1, 0, 0, 0, 0, 0]


-- -------------------------------------------------------------------

-- answer part two: 11151360
solve1819_2 _ =
  sum $ divisors 10551319
  where divisors y = [ x | x <- [1..y], (((mod) y x) == 0) ]

-- solve1819_2 input =
-- execProg regs0 ipr imap
-- where
--   (ipline : inlines) = lines $ input
--   ipr   = toInt . last . words $ ipline
--   imap  = readInstrs M.empty 0 . map (splitOn (`elem` "[,]: ")) $ inlines
--   -- regs0 = M.fromList $ zip [0..] [1, 0, 0, 0, 0, 0]
--   -- regs0 = M.fromList $ zip [0..] [0, 1, 10551319, 5, 1, 10551319]
--   regs0 = M.fromList $ zip [0..] [1, 1, 10551319, 10, 2, 10551319]


-- -------------------------------------------------------------------

-- [ r0,  r1,  r2,  r3,  r4,  r5 ]
-- [ SUM, tmp, VAL, PC,  rX,  rY ]

-- L03   mulr  4  5  1  tmp <- rX * rY
-- L04   eqrr  1  2  1  tmp <- tmp == VAL
-- L05 x addr  1  3  3  pc  <- pc + tmp   if (rX * rY) == VAL) goto L07
-- L06 x addi  3  1  3  pc  <- pc + 1     goto L08
-- L07   addr  4  0  0  sum <- rx + sum   sum <- rx + sum
-- L08   addi  5  1  5  ry  <- ry + 1     ry  <- ry + 1
-- L09   gtrr  5  2  1  tmp <- ry > val
-- L10 x addr  3  1  3  pc  <- pc + tmp   if (ry > val) goto L12
-- L11 x seti  2  1  3  pc  <- 2          goto L03

-- -- L10  while (ry <= val)
-- -- L05    if (rX * rY) == VAL) -- if x divides VAL
-- -- L07        sum <- rx + sum  --    -> add x to sum
-- -- L08    ry  <- ry + 1

-- L12   addi  4  1  4  rx  <- rx + 1    rx  <- rx + 1
-- L13   gtrr  4  2  1  tmp <- rx > val
-- L14 x addr  1  3  3  pc  <- pc + tmp  if (rx > val) EXIT
-- L15 x seti  1  3  3  pc  <- 1         goto LO2
-- L16 x mulr  3  3  3  pc  <- END
-- L01   seti 1  8  4   rx  <- 1
-- L02   seti 1  4  5   ry  <- 1

-- -- L01 rx <- 1
-- -- L02 ry <- 1
-- --
-- -- L15 while (rx <= val)
-- --
-- -- L10  while (ry <= val)
-- -- L05    if (rX * rY) == VAL)
-- -- L07        sum <- rx + sum
-- -- L08    ry  <- ry + 1
-- --
-- -- L12  rx  <- rx + 1

-- ([0,2197581,10551319,4,1,2197581],("eqrr",1,2,1),[0,0,10551319,4,1,2197581])
-- ([0,0,10551319,5,1,2197581],("addr",1,3,3),[0,0,10551319,5,1,2197581])
-- ([0,0,10551319,6,1,2197581],("addi",3,1,3),[0,0,10551319,7,1,2197581])
-- ([0,0,10551319,8,1,2197581],("addi",5,1,5),[0,0,10551319,8,1,2197582])
-- ([0,0,10551319,9,1,2197582],("gtrr",5,2,1),[0,0,10551319,9,1,2197582])
-- ([0,0,10551319,10,1,2197582],("addr",3,1,3),[0,0,10551319,10,1,2197582])
-- ([0,0,10551319,11,1,2197582],("seti",2,1,3),[0,0,10551319,2,1,2197582])

-- ([0,0,10551319,3,1,2197582],("mulr",4,5,1),[0,2197582,10551319,3,1,2197582])
-- ([0,2197582,10551319,4,1,2197582],("eqrr",1,2,1),[0,0,10551319,4,1,2197582])
-- ([0,0,10551319,5,1,2197582],("addr",1,3,3),[0,0,10551319,5,1,2197582])
-- ([0,0,10551319,6,1,2197582],("addi",3,1,3),[0,0,10551319,7,1,2197582])
-- ([0,0,10551319,8,1,2197582],("addi",5,1,5),[0,0,10551319,8,1,2197583])
-- ([0,0,10551319,9,1,2197583],("gtrr",5,2,1),[0,0,10551319,9,1,2197583])
-- ([0,0,10551319,10,1,2197583],("addr",3,1,3),[0,0,10551319,10,1,2197583])
-- ([0,0,10551319,11,1,2197583],("seti",2,1,3),[0,0,10551319,2,1,2197583])

-- ([0,0,10551319,3,1,2197583],("mulr",4,5,1),[0,2197583,10551319,3,1,2197583])
-- ([0,2197583,10551319,4,1,2197583],("eqrr",1,2,1),[0,0,10551319,4,1,2197583])
-- ([0,0,10551319,5,1,2197583],("addr",1,3,3),[0,0,10551319,5,1,2197583])
-- ([0,0,10551319,6,1,2197583],("addi",3,1,3),[0,0,10551319,7,1,2197583])
-- ([0,0,10551319,8,1,2197583],("addi",5,1,5),[0,0,10551319,8,1,2197584])
-- ([0,0,10551319,9,1,2197584],("gtrr",5,2,1),[0,0,10551319,9,1,2197584])
-- ([0,0,10551319,10,1,2197584],("addr",3,1,3),[0,0,10551319,10,1,2197584])
-- ([0,0,10551319,11,1,2197584],("seti",2,1,3),[0,0,10551319,2,1,2197584])

-- --->>> [0,1,10551319,5,1,10551319]

-- ([1,0,10551319,3,2,91230],("mulr",4,5,1),[1,182460,10551319,3,2,91230])
-- ([1,182460,10551319,4,2,91230],("eqrr",1,2,1),[1,0,10551319,4,2,91230])
-- ([1,0,10551319,5,2,91230],("addr",1,3,3),[1,0,10551319,5,2,91230])
-- ([1,0,10551319,6,2,91230],("addi",3,1,3),[1,0,10551319,7,2,91230])
-- ([1,0,10551319,8,2,91230],("addi",5,1,5),[1,0,10551319,8,2,91231])
-- ([1,0,10551319,9,2,91231],("gtrr",5,2,1),[1,0,10551319,9,2,91231])
-- ([1,0,10551319,10,2,91231],("addr",3,1,3),[1,0,10551319,10,2,91231])
-- ([1,0,10551319,11,2,91231],("seti",2,1,3),[1,0,10551319,2,2,91231])

-- ([1,0,10551319,3,2,91231],("mulr",4,5,1),[1,182462,10551319,3,2,91231])
-- ([1,182462,10551319,4,2,91231],("eqrr",1,2,1),[1,0,10551319,4,2,91231])
-- ([1,0,10551319,5,2,91231],("addr",1,3,3),[1,0,10551319,5,2,91231])
-- ([1,0,10551319,6,2,91231],("addi",3,1,3),[1,0,10551319,7,2,91231])
-- ([1,0,10551319,8,2,91231],("addi",5,1,5),[1,0,10551319,8,2,91232])
-- ([1,0,10551319,9,2,91232],("gtrr",5,2,1),[1,0,10551319,9,2,91232])
-- ([1,0,10551319,10,2,91232],("addr",3,1,3),[1,0,10551319,10,2,91232])
-- ([1,0,10551319,11,2,91232],("seti",2,1,3),[1,0,10551319,2,2,91232])

-- ([1,0,10551319,3,2,91232],("mulr",4,5,1),[1,182464,10551319,3,2,91232])
-- ([1,182464,10551319,4,2,91232],("eqrr",1,2,1),[1,0,10551319,4,2,91232])
-- ([1,0,10551319,5,2,91232],("addr",1,3,3),[1,0,10551319,5,2,91232])
-- ([1,0,10551319,6,2,91232],("addi",3,1,3),[1,0,10551319,7,2,91232])
-- ([1,0,10551319,8,2,91232],("addi",5,1,5),[1,0,10551319,8,2,91233])
-- ([1,0,10551319,9,2,91233],("gtrr",5,2,1),[1,0,10551319,9,2,91233])
-- ([1,0,10551319,10,2,91233],("addr",3,1,3),[1,0,10551319,10,2,91233])
-- ([1,0,10551319,11,2,91233],("seti",2,1,3),[1,0,10551319,2,2,91233])

-- --->>> [1,1,10551319,10,2,10551320]

-- ([1,0,10551319,3,3,38081],("mulr",4,5,1),[1,114243,10551319,3,3,38081])
-- ([1,114243,10551319,4,3,38081],("eqrr",1,2,1),[1,0,10551319,4,3,38081])
-- ([1,0,10551319,5,3,38081],("addr",1,3,3),[1,0,10551319,5,3,38081])
-- ([1,0,10551319,6,3,38081],("addi",3,1,3),[1,0,10551319,7,3,38081])
-- ([1,0,10551319,8,3,38081],("addi",5,1,5),[1,0,10551319,8,3,38082])
-- ([1,0,10551319,9,3,38082],("gtrr",5,2,1),[1,0,10551319,9,3,38082])
-- ([1,0,10551319,10,3,38082],("addr",3,1,3),[1,0,10551319,10,3,38082])
-- ([1,0,10551319,11,3,38082],("seti",2,1,3),[1,0,10551319,2,3,38082])

-- ([1,0,10551319,3,3,38082],("mulr",4,5,1),[1,114246,10551319,3,3,38082])
-- ([1,114246,10551319,4,3,38082],("eqrr",1,2,1),[1,0,10551319,4,3,38082])
-- ([1,0,10551319,5,3,38082],("addr",1,3,3),[1,0,10551319,5,3,38082])
-- ([1,0,10551319,6,3,38082],("addi",3,1,3),[1,0,10551319,7,3,38082])
-- ([1,0,10551319,8,3,38082],("addi",5,1,5),[1,0,10551319,8,3,38083])
-- ([1,0,10551319,9,3,38083],("gtrr",5,2,1),[1,0,10551319,9,3,38083])
-- ([1,0,10551319,10,3,38083],("addr",3,1,3),[1,0,10551319,10,3,38083])
-- ([1,0,10551319,11,3,38083],("seti",2,1,3),[1,0,10551319,2,3,38083])

-- ([1,0,10551319,3,3,38083],("mulr",4,5,1),[1,114249,10551319,3,3,38083])
-- ([1,114249,10551319,4,3,38083],("eqrr",1,2,1),[1,0,10551319,4,3,38083])
-- ([1,0,10551319,5,3,38083],("addr",1,3,3),[1,0,10551319,5,3,38083])
-- ([1,0,10551319,6,3,38083],("addi",3,1,3),[1,0,10551319,7,3,38083])
-- ([1,0,10551319,8,3,38083],("addi",5,1,5),[1,0,10551319,8,3,38084])
-- ([1,0,10551319,9,3,38084],("gtrr",5,2,1),[1,0,10551319,9,3,38084])
-- ([1,0,10551319,10,3,38084],("addr",3,1,3),[1,0,10551319,10,3,38084])
-- ([1,0,10551319,11,3,38084],("seti",2,1,3),[1,0,10551319,2,3,38084])


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 18
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

showLCA lca =
  (concat . map color) . concat . map ((:) '\n') $
  [[area (x, y) | x <- [0 .. cmax]] | y <- [0 .. cmax]]
  where
    cmax  = maximum . map fst . M.keys $ lca
    area (x, y) = (M.!) lca (x, y)
    --
    color x
      | x == '|'  = printf "\ESC[1;32m\ \%c\ \\ESC[0;m" x
      | x == '#'  = printf "\ESC[1;31m\ \%c\ \\ESC[0;m" x
      | otherwise = printf "%c" x

getLCA lca (x, y) (c : cs)
  | elem c ".|#" = getLCA (M.insert (x, y) c lca) (x + 1, y) cs
  | c == '\n'    = getLCA (lca) (0, y + 1) cs
getLCA lca (_, _) ([]) = lca

nextVal lca ((x, y), c)
  | c == '.', nbn '|' >= 3  = ((x, y), '|')
  | c == '.'                = ((x, y), c)
  | c == '|', nbn '#' >= 3  = ((x, y), '#')
  | c == '|'                = ((x, y), c)
  | c == '#', nbn '#' >= 1, nbn '|' >= 1 = ((x, y), '#')
  | c == '#'                = ((x, y), '.')
  where
    cmax        = maximum . map fst . M.keys $ lca
    area (x, y) = M.findWithDefault ' ' (x, y) lca
    neighs      = [area (nx, ny) | nx <- [x - 1 .. x + 1], ny <- [y - 1 .. y + 1], (nx, ny) /= (x, y)]
    nbn c       = length . filter (== c) $ neighs

nextLCA lca =
  M.fromList . map (nextVal lca) . M.assocs $ lca

evolveLCA n lca
  | trace (showLCA lca) False = undefined
  | n == 0     = nbel '|' * nbel '#'
  | otherwise  = evolveLCA (n - 1) (nextLCA lca)
  where nbel c = length . filter (== c) . M.elems $ lca

-- answer part one: 531417
solve1818_1 =
  evolveLCA 10 . getLCA M.empty (0, 0)


-- -------------------------------------------------------------------

evolveLCX lmap n lca
  | trace (showLCA lca) False = undefined
  | M.member lca lmap =
      evolveLCA ((mod) n (((M.!) lmap lca) - n)) (lca)
  | otherwise  = evolveLCX (M.insert lca n lmap) (n - 1) (nextLCA lca)
  | n == 0     = undefined

-- answer part two: 205296
solve1818_2 =
  evolveLCX M.empty 1000000000 . getLCA M.empty (0, 0)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 17
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

showScan scan water (cx, cy) =
  (concat . map color) . concat . (:) "\n" . intersperse "\n" $ [[cell (x, y) | x <- [xmin .. xmax]] | y <- [ymin .. ymax]]
  where
    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst . M.keys $ scan
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd . M.keys $ scan
    sqrs (x, y) = M.findWithDefault '.' (x, y) scan
    watr (x, y) = M.findWithDefault (sqrs (x, y)) (x, y) water
    cell (x, y) = if (x == cx && y == cy) then '?' else watr (x, y)
    --
    color x
      | (elem) x "+|~" = "\ESC[1;34m" ++ [x] ++ "\ESC[0;m"
      | (elem) x "?"   = "\ESC[1;31m" ++ [x] ++ "\ESC[0;m"
      | otherwise      = [x]

-- x=495, y=2..7
-- y=7, x=495..501

scanLine scan ((c1 : []) : c1s1 : (c2 : []) : c2s1 : c2s2 : [])
  | c1 == 'x' = M.union (M.fromList [((c1v1, y), '#') | y <- [c2v1 .. c2v2]]) scan
  | c1 == 'y' = M.union (M.fromList [((x, c1v1), '#') | x <- [c2v1 .. c2v2]]) scan
  where (c1v1 : c2v1 : c2v2 : []) = map toInt [c1s1, c2s1, c2s2]
scanLine scan (_) = scan

-- TODO - very inefficient - cells tested way too often

waterflow scan water (x, y)
  -- | trace ("\nFLOW " ++ (show (x, y)) ++ showScan scan water (x, y)) False = undefined
  -- water below limits
  | y > ymax  = (True, newwater)
  --
  | otherwise =
    let
      (flowdown, waterdown)
        | iswall      (x, y + 1) = (False, newwater)
        | isbackwater (x, y + 1) = (False, newwater)
        | isflowing   (x, y + 1) = (True,  newwater)
        | otherwise              = waterflow scan newwater (x, y + 1)

      (backwater)                =
        M.union (M.fromList $ zip ((M.keys waterdown) \\ (M.keys newwater)) (repeat '~')) newwater

      -- flowing water on left/right may become dead
      -- squares are tested just after being flooded
      -- it should not block spreading on left/right

      (flowleft, waterleft)
        | iswall      (x - 1, y) = (False, backwater)
        | isbackwater (x - 1, y) = (False, backwater)
        | isflowing   (x - 1, y) = (False, backwater) -- XXX :-(
        | otherwise              = waterflow scan backwater (x - 1, y)

      (flowright, waterright)
        | iswall      (x + 1, y) = (False, waterleft)
        | isbackwater (x + 1, y) = (False, waterleft)
        | isflowing   (x + 1, y) = (False, waterleft) -- XXX :-(
        | otherwise              = waterflow scan waterleft (x + 1, y)
    in
      (
        flowdown || flowleft || flowright,
        if flowdown then waterdown else waterright
      )
  where
    iswall      (x, y) = M.findWithDefault '.' (x, y) scan  == '#'
    isflowing   (x, y) = M.findWithDefault '?' (x, y) water == '|'
    isbackwater (x, y) = M.findWithDefault '?' (x, y) water == '~'
    --
    newwater = M.insert (x, y) '|' water
    ymax     = maximum . map snd . M.keys $ scan

-- answer part one: 31383
solve1817_1 input
  | trace (showScan scan water (500, 0)) False = undefined
  | otherwise =
      length . filter (\(_, y) -> y >= ymin && y <= ymax) . M.keys $ water
  where
    scan  = foldl' (scanLine) M.empty . map (splitOn (flip (elem) "=,. ")) . lines $ input
    water = snd $ waterflow scan M.empty (500, 0)
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd . M.keys $ scan

-- (164.07 secs, 225,487,531,952 bytes)


-- -------------------------------------------------------------------

-- answer part two: 25376
solve1817_2 input
  | trace (showScan scan water (500, 0)) False = undefined
  | otherwise = length . filter (\((_, y), c) -> c == '~' && y >= ymin && y <= ymax) . M.assocs $ water
  where
    scan  = foldl' (scanLine) M.empty . map (splitOn (flip (elem) "=,. ")) . lines $ input
    water = snd $ waterflow scan M.empty (500, 0)
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd . M.keys $ scan


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 16
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

execInstr regs opcode inA inB outC
  -- addr (add register) stores into register C the result of adding register A and register B.
  | opcode == "addr" = setC $ (+) regA regB
  -- addi (add immediate) stores into register C the result of adding register A and value B.
  | opcode == "addi" = setC $ (+) regA inB
  -- mulr (multiply register) stores into register C the result of multiplying register A and register B.
  | opcode == "mulr" = setC $ (*) regA regB
  -- muli (multiply immediate) stores into register C the result of multiplying register A and value B.
  | opcode == "muli" = setC $ (*) regA inB
  -- banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
  | opcode == "banr" = setC $ (.&.) regA regB
  -- bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
  | opcode == "bani" = setC $ (.&.) regA inB
  -- borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
  | opcode == "borr" = setC $ (.|.) regA regB
  -- bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
  | opcode == "bori" = setC $ (.|.) regA inB
  -- setr (set register) copies the contents of register A into register C. (Input B is ignored.)
  | opcode == "setr" = setC $ regA
  -- seti (set immediate) stores value A into register C. (Input B is ignored.)
  | opcode == "seti" = setC $ inA
  -- gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
  | opcode == "gtir" = setC $ if (inA > regB) then 1 else 0
  -- gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
  | opcode == "gtri" = setC $ if (regA > inB) then 1 else 0
  -- gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
  | opcode == "gtrr" = setC $ if (regA > regB) then 1 else 0
  -- eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
  | opcode == "eqir" = setC $ if (inA == regB) then 1 else 0
  -- eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
  | opcode == "eqri" = setC $ if (regA == inB) then 1 else 0
  -- eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
  | opcode == "eqrr" = setC $ if (regA == regB) then 1 else 0

  where
    setC val = M.insert outC val regs
    regA = (M.!) regs inA
    regB = (M.!) regs inB

-- Before: [3, 2, 1, 1]
-- 9 2 1 2
-- After:  [3, 2, 2, 1]

matchingOps (opC, inA, inB, outC) bregs aregs
  | otherwise = matchings
  where
    matchings = filter (\opc -> (execInstr bregs opc inA inB outC) == aregs) $
      ["addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori",
       "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr"]

checkSamples (("Before" : bregs) : (instr) : ("After" : aregs) : nexts)
  | (length $ matchingOps (i1, i2, i3, i4) regs regr) >= 3 = 1 + checkSamples nexts
  | otherwise = checkSamples nexts
  where
    (b1 : b2 : b3 : b4 : _) = map toInt bregs
    (i1 : i2 : i3 : i4 : _) = map toInt instr
    (a1 : a2 : a3 : a4 : _) = map toInt aregs
    regs = M.fromList [(0, b1), (1, b2), (2, b3), (3, b4)]
    regr = M.fromList [(0, a1), (1, a2), (2, a3), (3, a4)]
checkSamples (([]) : nexts)
  | otherwise = checkSamples nexts
checkSamples (_)
  | otherwise = 0

-- answer part one: 651
solve1816_1 =
  checkSamples . map (splitOn (`elem` "[,]: ")) . lines


-- -------------------------------------------------------------------

guessOpcodes opmap (("Before" : bregs) : (instr) : ("After" : aregs) : nexts)
  | otherwise =
      guessOpcodes (M.insertWith (intersect) i1 (matchingOps (i1, i2, i3, i4) regs regr) opmap) nexts
  where
    (b1 : b2 : b3 : b4 : _) = map toInt bregs
    (i1 : i2 : i3 : i4 : _) = map toInt instr
    (a1 : a2 : a3 : a4 : _) = map toInt aregs
    regs = M.fromList [(0, b1), (1, b2), (2, b3), (3, b4)]
    regr = M.fromList [(0, a1), (1, a2), (2, a3), (3, a4)]
guessOpcodes opmap (([]) : nexts)
  | otherwise = guessOpcodes opmap nexts
guessOpcodes opmap nexts
  | otherwise = (opmap, nexts)

execLines opmap regs ((i1 : i2 : i3 : i4 : []) : xs)
  | otherwise = execLines opmap (execInstr regs ((M.!) opmap i1) i2 i3 i4) xs
execLines _ regs ([]) = regs

findOpcodes matches opmap
  | M.null nmatches = opmap
  | otherwise = tryassocs opnum possvals
  where
    -- remove already set opcodes from possible matches list
    pmatches = M.map (\poss -> poss \\ (M.elems opmap)) matches
    -- remove already set opnums from matches list
    nmatches = M.filterWithKey (\k _ -> (notElem) k (M.keys opmap)) pmatches
    -- start with opcode with smaller number of possibilities
    (opnum, possvals) = head . sortOn (length . snd) . M.assocs $ nmatches
    -- try all (opnum, opcode) associations
    tryassocs opnum (p : ps)
      | M.null result = tryassocs opnum ps
      | otherwise     = result
      where result = findOpcodes nmatches (M.insert opnum p opmap)
    tryassocs _ []    = M.empty

-- answer part two: 706
solve1816_2 input =
  execLines opmap regs0 . map (map toInt) $ nextlines
  where
    (matches, nextlines) =
      guessOpcodes M.empty . map (splitOn (`elem` "[,]: ")) . lines $ input
    opmap =
      findOpcodes matches M.empty
    regs0 =
      M.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

-- (0,["muli","banr","bani","bori","setr","eqir","eqri"])
-- (1,["mulr","muli","banr","bani","setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"])
-- (2,["banr","gtir","gtri","gtrr","eqir","eqri","eqrr"])
-- (3,["banr","bani","gtir","gtri","gtrr","eqir","eqri","eqrr"])
-- (4,["gtir","gtri","gtrr","eqir","eqri","eqrr"])
-- (5,["setr","gtir"])
-- (6,["addr","mulr","muli","banr","bani","borr","setr","seti","gtir","gtri","gtrr","eqri"])
-- (7,["eqir","eqri","eqrr"])
-- (8,["banr","bani","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"])
-- (9,["addr","addi","mulr","muli","banr","bani","borr","bori","setr","seti","gtir","gtri","gtrr"])
-- (10,["eqri","eqrr"])
-- (11,["eqri"])
-- (12,["mulr","muli","banr","bani","borr","bori","setr","seti","eqir","eqri","eqrr"])
-- (13,["gtri","gtrr","eqir","eqri"])
-- (14,["mulr","banr","bani","seti","gtir","gtri","eqri","eqrr"])
-- (15,["gtri","eqir","eqrr"])


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 15
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

getArea wmap units (_, _) ([]) = (wmap, units)
getArea wmap units (x, y) (c : cs)
  | c == '#'  = getArea (neww c)    units   (x + 1, y) cs
  | c == '.'  = getArea (neww c)    units   (x + 1, y) cs
  | c == 'G'  = getArea (neww '.') (newu c) (x + 1, y) cs
  | c == 'E'  = getArea (neww '.') (newu c) (x + 1, y) cs
  | c == '\n' = getArea  wmap       units   (0, y + 1) cs
  | otherwise = undefined
  where
    neww c = M.insert (x, y) c wmap
    newu c = M.insert (x, y) (c, (200, 3)) units

showArea wmap units =
  (concat . map color) . concat . (:) "\n" . intersperse "\n" $ [[cell (x, y) | x <- [0 .. xmax]] | y <- [0 .. ymax]]
  where
    xmax  = maximum . map fst . M.keys $ wmap
    ymax  = maximum . map snd . M.keys $ wmap
    area (x, y) = M.findWithDefault 'X' (x, y) wmap
    cell (x, y) = fst $ M.findWithDefault (area (x, y), (0, 0)) (x, y) units
    --
    color x
      | x == 'G' = "\ESC[1;31m" ++ [x] ++ "\ESC[0;37m"
      | x == 'E' = "\ESC[1;34m" ++ [x] ++ "\ESC[0;37m"
      | otherwise = [x]

unitTurn wmap units (x, y)
  -- unit removed - already checked
  | not $ M.member (x, y) units   = undefined
  -- no more targets - already checked
  | null targets                  = undefined
  -- already in range of target
  | isInRange (x, y) targets      = attack wmap units (x, y) pow targets
  -- no reachable inrange target cell
  | null neartrg                  = units
  -- just moved on an inrange target cell
  | isInRange (nx, ny) targets    = attack wmap newunits (nx, ny) pow targets
  -- just moved toward a target cell
  | otherwise = newunits
  where
    (kind, (hitpts, pow)) = (M.!) units (x, y)
    --
    targets  = possibleTargets kind units
    trcells  = allInRange wmap units targets
    --
    neartrg  = smallestDist wmap units (x, y) trcells
    --
    trgcell  = fst . head $ neartrg
    (nx, ny) = head . sortOn swap . map (head . snd) . filter ((== trgcell) . fst) $ neartrg
    --
    newunits = M.insert (nx, ny) (kind, (hitpts, pow)) (M.delete (x, y) units)
    --
    isInRange (x, y) targets = (elem (x, y)) . concat . map inRangeCells $ targets

attack wmap units (x, y) pow targets
  -- target dies -> delete
  | hitpts <= pow = M.delete (tx, ty) units
  -- target survives -> update
  | otherwise     = M.insert (tx, ty) (kind, (hitpts - pow, tpow)) units
  where
    -- select target (hitpts then reading order)
    (tx, ty) = head .
      sortOn (\(x, y) -> (fst . snd . (M.!) units $ (x, y), y, x)) .
      filter ((elem) (x, y) . inRangeCells) $ targets
    (kind, (hitpts, tpow)) = (M.!) units (tx, ty)

smallestDist wmap units (x, y) targetcells =
  sortOn (swap . fst) $ visitcell [] [(0, [], (x, y))] Nothing
  where
    visitcell visited ((d, p, (x, y)) : cs) dist
      | (elem) (x, y) visited = visitcell (visited) cs dist
      | isJust dist, d == fromJust dist, (elem) (x, y) targetcells =
          ((x, y), p) : visitcell ((x, y):visited) cs dist
      | isNothing dist, (elem) (x, y) targetcells =
          ((x, y), p) : visitcell ((x, y):visited) cs (Just d)
      | isJust dist, d < fromJust dist =
          visitcell ((x, y):visited)
          (cs ++ (map (\c -> (d + 1, p ++ [c], c)) . filter (isOpenCell wmap units) .
                  filter (`notElem` visited) . inRangeCells $ (x, y))) dist
      | isNothing dist =
          visitcell ((x, y):visited)
          (cs ++ (map (\c -> (d + 1, p ++ [c], c)) . filter (isOpenCell wmap units) .
                  filter (`notElem` visited) . inRangeCells $ (x, y))) dist
      | otherwise =
          visitcell ((x, y):visited) cs dist
    visitcell _ [] _ = []

inRangeCells (x, y) = [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]

allInRange wmap units targets =
  sortOn swap . nub . concat . map (openInRange wmap units) $ targets
  where
    openInRange wmap units (x, y) =
      filter (isOpenCell wmap units) . inRangeCells $ (x, y)

isOpenCell wmap units (x, y)
  | (M.findWithDefault 'X' (x, y) wmap) /= '.' = False
  | (M.member (x, y) units) = False
  | otherwise = True

possibleTargets kind units =
  sortOn swap . M.keys . M.filter ((/=) kind . fst) $ units

allUnitTurns wmap units ((x, y) : us)
  -- unit removed during current round
  | not $ M.member (x, y) units   = allUnitTurns wmap units us
  -- no more targets - end of game
  | null targets                  = (units, False)
  -- else compute round and continue
  | otherwise = allUnitTurns wmap newunits us
  where
    newunits = (unitTurn wmap units (x, y))
    kind     = fst $ (M.!) units (x, y)
    targets  = possibleTargets kind units
allUnitTurns _ units ([]) = (units, True)

loopRounds wmap noelfdeath units roundnum
  | trace (showArea wmap units) False = undefined
  -- an elf died
  | noelfdeath, (nbelves units /= nbelves newunits) = 0
  -- round not ended
  | not endround = (roundnum - 1) * (sum . map (fst . snd) . M.elems $ newunits)
  -- combat continues
  | otherwise =
      (seq) newunits loopRounds wmap noelfdeath newunits (roundnum + 1)
  where
    turns = sortOn swap . M.keys $ units
    (newunits, endround) = allUnitTurns wmap units turns
    nbelves u = (length . filter (\(k, h) -> k == 'E') . M.elems $ u)

-- answer part one: 189910
solve1815_1 input =
  loopRounds wmap False units 1
  where (wmap, units) = getArea M.empty M.empty (0, 0) input


-- -------------------------------------------------------------------

tryPower wmap units pow
  | result == 0 = tryPower wmap units (pow + 1)
  | otherwise   = result
  where
    newunits = M.map (\(k, (hp, pw)) -> (k, (hp, if (k == 'E') then pow else pw))) units
    result   = loopRounds wmap True newunits 1

-- answer part two: 57820 (12 attack power)
solve1815_2 input =
  tryPower wmap units 4
  where (wmap, units) = getArea M.empty M.empty (0, 0) input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 14
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

nextScores start len recipes e1 e2
  | (Seq.length recipes) >= (start + len) =
      concat . map show . toList . Seq.take len . Seq.drop start $ recipes
  | otherwise = nextScores start len nrecipes ne1 ne2
  where
    r1 = (Seq.index) recipes e1
    r2 = (Seq.index) recipes e2
    nrecipes
      | r1 + r2 >= 10 = ((Seq.|>) ((Seq.|>) recipes 1) (r1 + r2 - 10))
      | otherwise     = ((Seq.|>) recipes (r1 + r2))
    ne1 = (`mod` (Seq.length nrecipes)) . (+ e1) . (+1) $ r1
    ne2 = (`mod` (Seq.length nrecipes)) . (+ e2) . (+1) $ r2

-- input: 554401

-- answer part one: 3610281143
solve1814_1 _ = nextScores 554401 10 (Seq.fromList [3, 7]) 0 1

-- (3.27 secs, 821,821,808 bytes)


-- -------------------------------------------------------------------

loopScores target recipes e1 e2 last7
  | ((Seq.drop 1 nlast7) == target) = (Seq.length nrecipes - 6)
  | ((Seq.take 6 nlast7) == target) = (Seq.length nrecipes - 7)
  | otherwise = loopScores target nrecipes ne1 ne2 nlast7
  where
    r1 = (Seq.index) recipes e1
    r2 = (Seq.index) recipes e2
    (nrecipes, nlast7)
      | r1 + r2 >= 10 = (((Seq.|>) ((Seq.|>) recipes 1) (r1 + r2 - 10)), ((Seq.|>) ((Seq.|>) (Seq.drop 2 last7) 1) (r1 + r2 - 10)))
      | otherwise     = (((Seq.|>) recipes (r1 + r2)),                    (Seq.|>) (Seq.drop 1 last7) (r1 + r2))
    ne1 = (`mod` (Seq.length nrecipes)) . (+ e1) . (+1) $ r1
    ne2 = (`mod` (Seq.length nrecipes)) . (+ e2) . (+1) $ r2

-- answer part two: 20211326
solve1814_2 _ =
  loopScores (Seq.fromList [5, 5, 4, 4, 0, 1]) (Seq.fromList [3, 7]) 0 1 (Seq.fromList [0, 0, 0, 0, 0, 0, 0])

-- (73.54 secs, 48,168,227,040 bytes)


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 13
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

showMaze maze carts =
  (concat . map color) . concat . (:) "\n" . intersperse "\n" $ [[cell (x, y) | x <- [0..xmax]] | y <- [0..ymax]]
  where
    xmax  = maximum . map fst . M.keys $ maze
    ymax  = maximum . map snd . M.keys $ maze
    cell (x, y) = fst $ M.findWithDefault (M.findWithDefault ' ' (x, y) maze, 'X') (x, y) carts
    color x = if (elem x "<>v^X") then "\ESC[1;31m" ++ [x] ++ "\ESC[0;37m" else [x]

getmaze (_, _) (maze) (carts) ([]) =  (maze, carts)
getmaze (x, y) (maze) (carts) (c : cs)
  | (c == ' ')      = getmaze (x + 1, y) (maze) (carts) cs
  | (c == '\n')     = getmaze (0, y + 1) (maze) (carts) cs
  | (elem) c "<>"   = getmaze (x + 1, y) (ins '-' maze) (ins (c, 'L') carts) cs
  | (elem) c "^v"   = getmaze (x + 1, y) (ins '|' maze) (ins (c, 'L') carts) cs
  | otherwise       = getmaze (x + 1, y) (ins  c  maze) (carts) cs
  where ins c mp = M.insert (x, y) c mp

nextCarts collided maze carts
  | otherwise = foldl (nextCart) carts (sortOn (swap . fst) . M.assocs $ carts)
  where
    oncollision newval Nothing = Just newval
    oncollision _ (Just _)     = collided

    nextCart carts cart @ ((x, y), (c, d))
      -- previous collisions during current tick
      | not $ M.member (x, y) carts  = carts
      | (M.!) carts (x, y) /= (c, d) = carts
      --
      | c == '>', m == '-'           = next (x + 1, y) ('>',  d )
      | c == '>', m == '/'           = next (x, y - 1) ('^',  d )
      | c == '>', m == '\\'          = next (x, y + 1) ('v',  d )
      | c == '>', m == '+', d == 'L' = next (x, y - 1) ('^', 'S')
      | c == '>', m == '+', d == 'S' = next (x + 1, y) ('>', 'R')
      | c == '>', m == '+', d == 'R' = next (x, y + 1) ('v', 'L')
      --
      | c == '<', m == '-'           = next (x - 1, y) ('<',  d )
      | c == '<', m == '/'           = next (x, y + 1) ('v',  d )
      | c == '<', m == '\\'          = next (x, y - 1) ('^',  d )
      | c == '<', m == '+', d == 'L' = next (x, y + 1) ('v', 'S')
      | c == '<', m == '+', d == 'S' = next (x - 1, y) ('<', 'R')
      | c == '<', m == '+', d == 'R' = next (x, y - 1) ('^', 'L')
      --
      | c == '^', m == '|'           = next (x, y - 1) ('^',  d )
      | c == '^', m == '/'           = next (x + 1, y) ('>',  d )
      | c == '^', m == '\\'          = next (x - 1, y) ('<',  d )
      | c == '^', m == '+', d == 'L' = next (x - 1, y) ('<', 'S')
      | c == '^', m == '+', d == 'S' = next (x, y - 1) ('^', 'R')
      | c == '^', m == '+', d == 'R' = next (x + 1, y) ('>', 'L')
      --
      | c == 'v', m == '|'           = next (x, y + 1) ('v',  d )
      | c == 'v', m == '\\'          = next (x + 1, y) ('>',  d )
      | c == 'v', m == '/'           = next (x - 1, y) ('<',  d )
      | c == 'v', m == '+', d == 'L' = next (x + 1, y) ('>', 'S')
      | c == 'v', m == '+', d == 'S' = next (x, y + 1) ('v', 'R')
      | c == 'v', m == '+', d == 'R' = next (x - 1, y) ('<', 'L')
      --
      where
        m = (M.findWithDefault ' ' (x, y) maze)
        next crd crt = (M.alter (oncollision crt) crd (M.delete (x, y) carts))

-- answer part one: 100,21
solve1813_1 =
  uncurry loop . getmaze (0, 0) M.empty M.empty
  where
    loop maze carts
      | trace (showMaze maze carts) False = undefined
      | not . null $ collided = fst . head $ collided
      | otherwise = loop maze (nextCarts (Just ('X', 'X')) maze carts)
      where collided = filter ((==) 'X' . fst . snd) . M.assocs $ carts


-- -------------------------------------------------------------------

-- answer part two: 113,109
solve1813_2 =
  uncurry loop . getmaze (0, 0) M.empty M.empty
  where
    loop maze carts
      -- | trace (showMaze maze carts) False = undefined
      | (==1) . length . M.keys $ carts   = carts
      | otherwise = loop maze (nextCarts Nothing maze carts)


-- -------------------------------------------------------------------

-- /->-\
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | v  |
-- \-+-/  \-+--/
--   \------/


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 12
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- initial state: #..#.#..##......###...###

-- ...## => #
-- ..#.. => #
-- .#... => #
-- .#.#. => #
-- .#.## => #
-- .##.. => #
-- .#### => #
-- #.#.# => #
-- #.### => #
-- ##.#. => #
-- ##.## => #
-- ###.. => #
-- ###.# => #
-- ####. => #

nextGen rules gen =
  M.fromList . map (\i -> (i, nextvl i)) $ [xmin - 2 .. xmax + 2]
  where
    xmin = minimum . M.keys . M.filter (== '#') $ gen
    xmax = maximum . M.keys . M.filter (== '#') $ gen
    neighs idx = map (\i -> M.findWithDefault '.' i gen) [idx - 2 .. idx + 2]
    nextvl idx = M.findWithDefault '.' (neighs idx) rules

showGen gen = show (map snd plist, xmin)
  where plist @ ((xmin, _) : _) = sort . M.assocs $ gen

evolveGens rules gen n
  -- | trace (showGen gen) False = undefined
  | n == 0    = gen
  | otherwise = evolveGens rules (nextGen rules gen) (n - 1)

-- answer part one: 3258
solve1812_1 input =
  sum . M.keys . M.filter (== '#') $ evolveGens rules gen0 20
  where
    inLines = lines $ input
    gen0  = M.fromList . zip [0..] . filter (`elem` "#.") . head $ inLines
    rules = M.fromList . map ((\(a:b:[]) -> (a, b !! 0)) . splitOn (`elem` " =>")) . drop 2 $ inLines


-- -------------------------------------------------------------------

genDelta rules gen0 n lastn =
  (delta, (nres genn) + delta * (lastn - n))
  where
    genn = evolveGens rules gen0 n
    genx = evolveGens rules genn 1
    nres g = sum . M.keys . M.filter (== '#') $ g
    delta  = (nres genx) - (nres genn)

-- answer part two: 3600000002022
solve1812_2 input =
  -- result already evolving at constant pace after 100 generations
  genDelta rules gen0 100 50000000000
  where
    inLines = lines $ input
    gen0  = M.fromList . zip [0..] . filter (`elem` "#.") . head $ inLines
    rules = M.fromList . map ((\(a:b:[]) -> (a, b !! 0)) . splitOn (`elem` " =>")) . drop 2 $ inLines


-- -------------------------------------------------------------------

--  0: ...#..#.#..##......###...###...........
--  1: ...#...#....#.....#..#..#..#...........
--  2: ...##..##...##....#..#..#..##..........
--  3: ..#.#...#..#.#....#..#..#...#..........
--  4: ...#.#..#...#.#...#..#..##..##.........
--  5: ....#...##...#.#..#..#...#...#.........
--  6: ....##.#.#....#...#..##..##..##........
--  7: ...#..###.#...##..#...#...#...#........
--  8: ...#....##.#.#.#..##..##..##..##.......
--  9: ...##..#..#####....#...#...#...#.......
-- 10: ..#.#..#...#.##....##..##..##..##......
-- 11: ...#...##...#.#...#.#...#...#...#......
-- 12: ...##.#.#....#.#...#.#..##..##..##.....
-- 13: ..#..###.#....#.#...#....#...#...#.....
-- 14: ..#....##.#....#.#..##...##..##..##....
-- 15: ..##..#..#.#....#....#..#.#...#...#....
-- 16: .#.#..#...#.#...##...#...#.#..##..##...
-- 17: ..#...##...#.#.#.#...##...#....#...#...
-- 18: ..##.#.#....#####.#.#.#...##...##..##..
-- 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
-- 20: .#....##....#####...#######....#.#..##.


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 11
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

powerLevel serial (x, y) =
  ((-) ((mod) ((div) ((*) ((+) ((*) rackid y) serial) rackid) 100) 10) 5)
  where rackid = ((+) x 10)

searchGrid serial size | trace (show (size)) False = undefined
searchGrid serial size =
  maximum .
  map (\c -> (sum . map (powerLevel serial) . squares $ c, c)) $
  allcorners
  where
    allcorners = [(x, y) | x <- [0 .. 300 - size], y <- [0 .. 300 - size]]
    squares (cx, cy) = [(x, y) | x <- [cx .. cx + size - 1], y <- [cy .. cy + size - 1]]

-- puzzle input : 9110

-- answer part one: 21,13
solve1811_1 _ = searchGrid 9110 3


-- -------------------------------------------------------------------

-- X . . # <- nextfv
-- . . . # </
-- . . . # </
-- . . . # </
-- $ $ $ $ <- nextfh

squareval pmap size pval (x, y)
  | (size == 0) && trace (show (x, y)) False = undefined
  | (x + size > 299) || (y + size > 299)  = []
  | otherwise = (pval + frontv, (x, y, size + 1)) : (squareval pmap (size + 1) (pval + frontv) (x, y))
  where
    frontv = (powsum nextfv) + (powsum nextfh)
    nextfv = [(x + size, fy) | fy <- [y .. y + size - 1]]
    nextfh = [(fx, y + size) | fx <- [x .. x + size]]
    powsum = sum . map (pmap M.!)

-- answer part two: 235,268,13
solve1811_2 _ =
  maximum . map (\c -> maximum . squareval powermap 0 0 $ c) $ allcells
  where
    serial   = 9110
    allcells = [(x, y) | x <- [0 .. 299], y <- [0 .. 299]]
    powermap = M.fromList . map (\c -> (c, powerLevel serial c)) $ allcells


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 10
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

showStars (time, positions) =
  (seq) (traceStars starrows) (time)
  where
    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst $ positions
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd $ positions
    --
    rowsmap  = M.fromListWith (++) . map (\(x, y) -> (y, [(x, y)])) $ positions
    rowslst  = map (\y -> map fst $ M.findWithDefault [] y rowsmap) [ymin..ymax]
    starrows = map (\row -> map (\x -> if (elem x row) then '#' else '.') [xmin..xmax]) rowslst
    --
    traceStars (r : rs) | trace (show r) False = undefined
    traceStars (r : rs) = traceStars rs
    traceStars ([])     = ()

starLoop time positions velocities
  | (surface positions) < (surface nextPositions) = (time, positions)
  | otherwise = starLoop (time + 1) nextPositions velocities
  where
    nextPositions =
      map (\((x, y), (vx, vy)) -> (x + vx, y + vy)) $ zip positions velocities
    surface positions =
      ((\l -> (maximum l - minimum l)) . map fst $ positions) *
      ((\l -> (maximum l - minimum l)) . map snd $ positions)

-- "..##....#....#..######..#.......#........####.....##....#....."
-- ".#..#...#....#.......#..#.......#.......#....#...#..#...#....."
-- "#....#..#....#.......#..#.......#.......#.......#....#..#....."
-- "#....#..#....#......#...#.......#.......#.......#....#..#....."
-- "#....#..######.....#....#.......#.......#.......#....#..#....."
-- "######..#....#....#.....#.......#.......#.......######..#....."
-- "#....#..#....#...#......#.......#.......#.......#....#..#....."
-- "#....#..#....#..#.......#.......#.......#.......#....#..#....."
-- "#....#..#....#..#.......#.......#.......#....#..#....#..#....."
-- "#....#..#....#..######..######..######...####...#....#..######"

-- answer part one: AHZLLCAL
solve1810_1 =
  showStars .
  -- (3, [(9,7), (4,0), ...])
  uncurry (starLoop 0) .
  -- ([(9,1),(7,0). ...], [(0,2),(-1,0), ...])
  unzip . map (\(x : y : vx : vy : []) -> ((x, y), (vx, vy))) .
  -- [ [9,1,0,2], [7,0,-1,0], ... ]
  map (map toInt . splitOn (not . (`elem` "0123456789-"))) .
  -- position=< 9,  1> velocity=< 0,  2> ...
  lines


-- -------------------------------------------------------------------

-- answer part two: 10333
solve1810_2 = solve1810_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 9
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

marblePlay nbplayers lastvalue =
  (marbleLoop nbplayers lastvalue (M.fromList [(0,0)]) (Seq.fromList [0]) 1)
  where

    marbleLoop nbplayers lastvalue scores circle val
      -- max value reached  -> end of game
      | val == lastvalue    = maximum . M.elems $ scores
      -- multiple of 23     -> special move & score update
      | ((mod) val 23) == 0 =
        let
          (old, nxtcircle) = Seq.splitAt 1 (shiftl (-7) circle)
          oldmarble = (Seq.index old 0)
          nplayer   = ((mod) val nbplayers)
          newscores = (M.insertWith (+) nplayer (val + oldmarble) scores)
        in
          marbleLoop nbplayers lastvalue newscores nxtcircle (val + 1)
      -- regular value      -> regular move
      | otherwise    =
        let
          nxtcircle  = ((Seq.<|) val (shiftl 2 circle))
        in
          marbleLoop nbplayers lastvalue scores nxtcircle (val + 1)

    shiftl x s =
      (uncurry (flip (Seq.><))) .
      (Seq.splitAt ((mod) x (Seq.length s))) $ s

-- 446 players; last marble is worth 71522 points

-- answer part one: 390592
solve189_1 _ = marblePlay 446 71522


-- -------------------------------------------------------------------

-- answer part two: 3277920293
solve189_2 _ = marblePlay 446 7152200


-- -------------------------------------------------------------------

-- marblePlay  9   26 -- 32
-- marblePlay 10 1618 -- 8317
-- marblePlay 13 7999 -- 146373
-- marblePlay 17 1104 -- 2764 -- XXX
-- marblePlay 21 6111 -- 54718
-- marblePlay 30 5807 -- 37305


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

data Tree = Leaf [Int] | Node ([Tree], [Int]) deriving (Show)

getelement (0 : nbmetas : tl)
  | otherwise = (Leaf (metas), tl2)
  where
    (metas, tl2) = splitAt nbmetas tl

getelement (nbnodes : nbmetas : tl)
  | otherwise = (Node (reverse nodes, metas), tl3)
  where
    (nodes, tl2) = getnodes nbnodes tl
    (metas, tl3) = splitAt nbmetas tl2

getnodes nbnodes datas
  | nbnodes == 0 = ([], datas)
  | otherwise    = (node : elems, tl2)
  where
    (node,  tl1) = (getelement datas)
    (elems, tl2) = (getnodes (nbnodes - 1) tl1)

sumtree (Leaf x) =
  sum x
sumtree (Node (nodes, x)) =
  (sum x) + sum (map sumtree nodes)

-- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

-- answer part one: 42472
solve188_1 =
  sumtree . fst . getelement . map toInt . words


-- -------------------------------------------------------------------

valtree (Leaf metas) =
  sum metas
valtree (Node (childs, metas)) =
  sum . map valtree . mapMaybe (getchild childs) $ metas
  where
    getchild childs n
      | n > length childs = Nothing
      | otherwise = Just (childs !! (n - 1))

-- answer part two: 21810
solve188_2 =
  valtree . fst . getelement . map toInt . words


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- Step M must be finished before step D can begin.
-- Step E must be finished before step Z can begin.
-- Step F must be finished before step W can begin.

availables tasks deps done =
  filter (\k -> null $ (M.findWithDefault [] k deps) \\ done) $ (tasks \\ done)

doTask tasks deps done
  | null readys = reverse done
  | otherwise   = doTask tasks deps ((head . sort $ readys) : done)
  where readys = availables tasks deps done

-- answer part one: EFHLMTKQBWAPGIVXSZJRDUYONC
solve187_1 input =
  doTask tasks deps []
  where
    deps =
      -- [ ('D',"BRJVIAM"), ... ]
      M.fromListWith (++) .
      -- [ ('D',"M"), ... ]
      map (\l -> (l !! 7 !! 0, [l !! 1 !! 0])) . map words .
      -- Step M must be finished before step D can begin.
      lines $ input
    tasks =
      nub $ (++) (concat . M.elems $ deps) (M.keys $ deps)


-- -------------------------------------------------------------------

schedTask tasks deps (workers, done, ongoing, time)
  | trace (show (time, workers)) False = undefined
  -- available workers and tasks -> schedule a task
  | not (null availWorkers) && not (null availTasks) =
    let
      schedNextTask =
        (M.insert nextWorker ((taskCost nextTask), nextTask) workers, done, (nextTask : ongoing), time)
      nextWorker    = head . sort $ availWorkers
      nextTask      = head . sort $ availTasks
      taskCost      = (+60). (subtract (ord 'A' - 1)) . ord
    in
      schedTask tasks deps (schedNextTask)
  -- work ongoing                -> go to next tick
  | any (> 0) (M.map fst workers) =
    let
      nextTick =
        ((M.map (\(l, t) -> (l - 1, t)) workers), (newDone ++ done), (ongoing \\ newDone), (time + 1))
      newDone  =
        (map snd . filter (\(x, y) -> x == 1) . M.elems $ workers)
    in
      schedTask tasks deps (nextTick)
  -- nothing ongoing / available -> end of game
  | otherwise = time
  where
    availWorkers  = M.keys . M.filter (\(x, y) -> x <= 0) $ workers
    availTasks    = availables tasks deps done \\ ongoing

-- answer part two: 1056
solve187_2 input =
  schedTask tasks deps (workers, [], [], 0)
  where
    deps =
      -- [ ('D',"BRJVIAM"), ... ]
      M.fromListWith (++) .
      -- [ ('D',"M"), ... ]
      map (\l -> (l !! 7 !! 0, [l !! 1 !! 0])) . map words .
      -- Step M must be finished before step D can begin.
      lines $ input
    tasks =
      nub $ (++) (concat . M.elems $ deps) (M.keys $ deps)
    workers =
      M.fromList $ [(x, (0, '0')) | x <- [1..5]]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

closest cells (x, y)
  | d1 == d2  = Nothing
  | otherwise = Just c1
  where ((d1, c1):(d2, c2):_) = sort . map (\c -> (mandist (x, y) c, c)) $ cells

-- answer part one: 3251
solve186_1 input
  | otherwise =
      last . sort . map length . group . sort . filter (not . (`elem` infinites)) . catMaybes $
      [closest cells (x, y) | x <- [xmin  .. xmax], y <- [ymin .. ymax]]
  where
    cells =
      map ((\(x : y : []) -> (x, y)) . map toInt . splitOn ((==) ',')) . lines $ input

    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst $ cells
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd $ cells

    infinites =
      nub . sort . catMaybes $ []
      ++ [closest cells (x, ymin) | x <- [xmin .. xmax]]
      ++ [closest cells (x, ymax) | x <- [xmin .. xmax]]
      ++ [closest cells (xmin, y) | y <- [ymin .. ymax]]
      ++ [closest cells (xmax, y) | y <- [ymin .. ymax]]


-- -------------------------------------------------------------------

inregion cells (x, y) =
  (< 10000) . sum . map (mandist (x, y)) $ cells

-- answer part two: 47841
solve186_2 input =
  length . filter (id) $
  [inregion cells (x, y) | x <- [xmin  .. xmax], y <- [ymin .. ymax]]
  where
    cells =
      map ((\(x : y : []) -> (x, y)) . map toInt . splitOn ((==) ',')) . lines $ input

    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst $ cells
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd $ cells


-- -------------------------------------------------------------------

mandist (x1, y1) (x2, y2) =
  (abs (x2 - x1)) + (abs (y2 - y1))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- simplifPoly i l
--   | (length l) <= (i + 1) = length l
--   | (i < 0)               = simplifPoly 1 l
--   | willReact l i         = simplifPoly (i - 1) (simplif l i)
--   | otherwise             = simplifPoly (i + 1) l
--   where
--     willReact l i = (== 32) . (abs) $ (ord (Seq.index l i)) - (ord (Seq.index l (i + 1)))
--     simplif   l i = (Seq.><) (Seq.take (i) l) (Seq.drop (i + 2) l)

simplifPoly ([])     (x : xs)
  | otherwise = simplifPoly [x] xs
simplifPoly (y : ys) (x : xs)
  | (== 32) . (abs) $ (ord y) - (ord x) = simplifPoly ys xs
  | otherwise = simplifPoly (x : y : ys) xs
simplifPoly (ys)     ([])
  | otherwise = length ys

-- answer part one: 11540
solve185_1 =
  simplifPoly "" . head . lines


-- -------------------------------------------------------------------

-- answer part two: 6918
solve185_2 input =
  minimum . map (simplifPoly "" . (\c -> filter (flip notElem [c, toUpper c]) polymer)) $ ['a'..'z']
  where polymer  = head . lines $ input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up

readShifts smap cgid mstart ((_ : _ : _ : _ : m : "Guard" : gid : _ : _ : []) : xs) =
  readShifts smap (toInt gid) (-1) xs
readShifts smap cgid mstart ((_ : _ : _ : _ : m : "falls" : _           : []) : xs) =
  readShifts smap cgid (toInt m) xs
readShifts smap cgid mstart ((_ : _ : _ : _ : m : "wakes" : _           : []) : xs) =
  readShifts (M.insertWith (++) cgid [mstart .. toInt m - 1] smap) cgid (-1) xs
readShifts smap _    _      [] = smap

-- answer part one: 87681
solve184_1 =
  uncurry (*) . snd . last . sort .
  map (\(g, m) -> (length m, (g, head . last . sortOn length . group . sort $ m))) .
  M.toList . readShifts M.empty (-1) (-1) .
  sort . map (splitOn (`elem` "[-]:# ")) . lines


-- -------------------------------------------------------------------

-- answer part two: 136461
solve184_2 =
  uncurry (*) . snd . last . sort .
  map (\(g, m) -> ((\l -> (length l, (g, head l))) . last . sortOn length . group . sort $ m)) .
  M.toList . readShifts M.empty (-1) (-1) .
  sort . map (splitOn (`elem` "[-]:# ")) . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

claimMap (id : l : t : w : h : []) =
  M.fromList [((x, y), id) | x <- [l..(l + w - 1)], y <- [t..(t + h - 1)]]

-- can be replaced by a fold
processClaims fabric (x : xs) =
  processClaims (M.unionWith (\_ _-> (-1)) (claimMap x) fabric) xs
processClaims fabric [] = fabric

-- answer part one: 113966
solve183_1 =
  length . filter (== (-1)) . M.elems .
  processClaims M.empty .
  map (map toInt . splitOn (`elem` "#@,:x")) . lines

input183 = unlines ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]


-- -------------------------------------------------------------------

findClaim finalmap (claim : otherclaims)
  | not overlapping = head claim
  | otherwise       = findClaim finalmap otherclaims
  where
    overlapping = (elem) (-1) . M.elems $ M.intersection finalmap (claimMap claim)

-- answer part two: 235
solve183_2 input =
  findClaim (processClaims M.empty allclaims) allclaims
  where allclaims = map (map toInt . splitOn (`elem` "#@,:x")) . lines $ input


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
-- 2018 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

grpchecksum l =
  (length . filter (elem 2) $ l) * (length . filter (elem 3) $ l)

-- answer part one: 6972
solve182_1 = grpchecksum . map (map length . group . sort) . lines


-- -------------------------------------------------------------------

-- answer part two: aixwcbzrmdvpsjfgllthdyoqe
solve182_2 =
  map fst . last . sortOn length . map (filter (uncurry (==)) . uncurry zip) . pairs . lines
  where pairs l = [(x, y) | (x : z) <- tails l, y <- z]


-- -------------------------------------------------------------------

-- tee fs = ((flip map fs) . flip ($))

-- foldl (\acc val -> newacc) acc0 [vals]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2018 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- answer part one: 454
solve181_1 = sum . map (toInt . filter (/= '+')) . lines


-- -------------------------------------------------------------------

changeFreq oldfreqs curfreq (chg : changelist)
  | S.member curfreq oldfreqs = curfreq
  | otherwise = changeFreq (S.insert curfreq oldfreqs) (curfreq + chg) changelist

-- answer part two: 566
solve181_2 = changeFreq S.empty 0 . cycle . map (toInt . filter (/= '+')) . lines


-- -------------------------------------------------------------------

check181 = runTestTT $ TestList [
  ((solve181_1 "+1 \n -2 \n +3 \n +1") ~=?  3),
  ((solve181_1 "+1 \n +1 \n +1")       ~=?  3),
  ((solve181_1 "+1 \n +1 \n -2")       ~=?  0),
  ((solve181_1 "-1 \n -2 \n -3")       ~=? -6),
  --
  ((solve181_2 "+1 \n -2 \n +3 \n +1")       ~=?  2),
  ((solve181_2 "+1 \n -1")                   ~=?  0),
  ((solve181_2 "+3 \n +3 \n +4 \n -2 \n -4") ~=? 10),
  ((solve181_2 "-6 \n +3 \n +8 \n +5 \n -6") ~=?  5),
  ((solve181_2 "+7 \n +7 \n -2 \n -7 \n -4") ~=? 14)
  ]


-- -------------------------------------------------------------------

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
