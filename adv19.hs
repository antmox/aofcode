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
  getArgs >>= getInput >>= return . solve191_1 >>= print
  where
    getInput (l:_) = readFile l
    getInput []    = getContents

-- readFile "inputs/1901.in" >>= return . solve191_1


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 13
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

runIC13 (icmp @ ICComp {prog = prog}) mp
  | state1 == End    = mp
  | state1 == Input  =
      runIC13 icn1 { inputs = [nxin] } mp
  | state1 == Output =
      runIC13 icn3 (M.insert (out1, out2) out3 mp)
  where
    (icn1 @ ICComp { state = state1, output = out1 }) = loopIC9 icmp { state = Run }
    (icn2 @ ICComp { state = state2, output = out2 }) = loopIC9 icn1 { state = Run }
    (icn3 @ ICComp { state = state3, output = out3 }) = loopIC9 icn2 { state = Run }
    xpad = fst . head . M.keys . M.filter (== 3) $ mp
    xbal = fst . head . M.keys . M.filter (== 4) $ mp
    nxin = if xpad > xbal then -1 else if xbal > xpad then 1 else 0

-- 173
solve1913_1 input =
  length . filter (== 2) . M.elems . strmap13 $ runIC13 iccmp M.empty
  where iccmp = iccomp (icprog input) []

strmap13 mp
  | trace strres False = undefined
  | otherwise          = mp
  where
    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst . M.keys $ mp
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd . M.keys $ mp
    --
    strcll (x, y) = case M.findWithDefault 0 (x, y) mp of
      { 0 -> ' '; 1 -> 'X'; 2 -> '#'; 3 -> '_'; 4 -> 'o' }
    --
    strrow y = concat $ map (\x -> [strcll (x, y)]) [0..xmax] --
    strres = concat . intersperse ['\n'] $ map strrow [ymin..ymax]

-- 8942
solve1913_2 input =
  flip (M.!) (-1, 0) . strmap13 $ runIC13 iccmp M.empty
  where iccmp = iccomp (M.insert 0 2 $ icprog input) []


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 12
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 11
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

runIC11 (icmp @ ICComp {prog = prog}) (x, y, d) mp
  | state1 == Input  =
      runIC11 icn1 { inputs = [M.findWithDefault 0 (x, y) mp] } (x, y, d) mp
  | state1 == Output =
      runIC11 icn2 (newpos . newdir . output $ icn2) (M.insert (x, y) (output icn1) mp)
  | state1 == End    = mp
  where
    (icn1 @ ICComp { state = state1 }) = loopIC9 icmp { state = Run }
    (icn2 @ ICComp { state = state2 }) = loopIC9 icn1 { state = Run }
    newdrs = M.fromList [('L',['D','U']),('R',['U','D']),('U',['L','R']),('D',['R','L'])]
    newdir t = (((M.!) newdrs d) !! t)
    newpos 'U' = (x, y - 1, 'U')
    newpos 'D' = (x, y + 1, 'D')
    newpos 'L' = (x - 1, y, 'L')
    newpos 'R' = (x + 1, y, 'R')

-- 1785
solve1911_1 input =
  length . M.keys $ runIC11 (iccomp prog []) (0, 0, 'U') M.empty
  where prog = icprog input

-- #  #   ##  ##  #      ## #### #### #  #
-- #  #    # #  # #       #    # #    #  #
-- ####    # #  # #       #   #  ###  ####
-- #  #    # #### #       #  #   #    #  #
-- #  # #  # #  # #    #  # #    #    #  #
-- #  #  ##  #  # ####  ##  #### #    #  #

-- 1785
solve1911_2 input =
  strmap11 $ runIC11 (iccomp prog []) (0, 0, 'U') (M.fromList [((0, 0), 1)])
  where prog = icprog input

strmap11 mp
  | trace strres False = undefined
  | otherwise          = 0
  where
    (xmin, xmax) = (\l -> (minimum l, maximum l)) . map fst . M.keys $ mp
    (ymin, ymax) = (\l -> (minimum l, maximum l)) . map snd . M.keys $ mp
    --
    strcll (x, y) = case M.findWithDefault 0 (x, y) mp of { 1 -> '#'; _ -> ' ' }
    --
    strrow y = concat $ map (\x -> [strcll (x, y)]) [xmin..xmax]
    strres = concat . intersperse ['\n'] $ map strrow [ymin..ymax]


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 10
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------



-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 9
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

data ICState = Run | Output | Input | End
  deriving (Eq, Show)

data ICComp = ICComp
  { prog   :: M.Map Int Int
  , pc     :: Int
  , rb     :: Int
  , inputs :: [Int]
  , output :: Int
  , state  :: ICState
  } deriving (Eq, Show)

iccomp prog inputs =
  ICComp { prog = prog, inputs = inputs, pc = 0, rb = 0, output = 0, state = Run }

icprog = M.fromList . zip [0..] . map toInt . splitOn ((==) ',')

--

stepIC9 (icmp @ ICComp {prog = prog, pc = pc, rb = rb, inputs = inputs, state = Run})
  | opc == 1  = -- ADD
      icmp { prog = (M.insert pa3 (pv1 + pv2) prog), pc = (pc + 4) }
  | opc == 2  = -- MUL
      icmp { prog = (M.insert pa3 (pv1 * pv2) prog), pc = (pc + 4) }
  --
  | opc == 5  = -- JUMP_IF_TRUE
      icmp { pc = (if (pv1 /= 0) then pv2 else (pc + 3)) }
  | opc == 6  = -- JUMP_IF_FALSE
      icmp { pc = (if (pv1 == 0) then pv2 else (pc + 3)) }
  --
  | opc == 7  = -- LESS_THAN
      icmp { prog = (M.insert pa3 (fromEnum (pv1 < pv2)) prog), pc = (pc + 4) }
  | opc == 8  = -- EQUAL
      icmp { prog = (M.insert pa3 (fromEnum (pv1 == pv2)) prog), pc = (pc + 4) }
  --
  | opc == 9  = -- ADJUST RB
      icmp { pc = (pc + 2), rb = (rb + pv1) }
  --
  | opc == 3  = -- INPUT
      case inputs of
        (inp : inps) -> icmp { prog = (M.insert pa1 inp prog), pc = (pc + 2), inputs = inps }
        []           -> icmp { state = Input }
  | opc == 4  = -- OUTPUT
      icmp { pc = (pc + 2), output = pv1, state = Output }
  | opc == 99 = -- EXIT
      icmp { state = End }
  where
    (pm3 : pm2 : pm1 : opc2 : opc1 : []) =
      map digitToInt $ (printf "%05d" ((M.!) prog (pc)) :: String)
    opc = opc2 * 10 + opc1
    --
    op1 = M.findWithDefault 0 (pc + 1) prog
    op2 = M.findWithDefault 0 (pc + 2) prog
    op3 = M.findWithDefault 0 (pc + 3) prog
    --
    prv 0 op = M.findWithDefault 0 (pra 0 op) prog
    prv 1 op = op
    prv 2 op = M.findWithDefault 0 (pra 2 op) prog
    --
    pra 0 op = op
    pra 2 op = op + rb
    --
    pv1 = prv pm1 op1
    pv2 = prv pm2 op2
    pa1 = pra pm1 op1
    pa3 = pra pm3 op3

loopIC9 (icmp @ ICComp {prog = prog})
  | state == Run = loopIC9 icnx
  | otherwise    = icnx
  where (icnx @ ICComp { state = state }) = stepIC9 icmp { state = Run }

--

runIC9 (icmp @ ICComp {prog = prog})
  | trace (printf "# PC=%05d" (pc icnx))
    False = undefined
  | state == Output && trace ("> OUT " ++ (show $ output icnx))
    False = undefined
  --
  | state == End    = undefined
  | state == Input  = undefined
  | state == Output = output icnx
  | otherwise       = runIC9 icnx
  where (icnx @ ICComp { state = state }) = stepIC9 icmp

-- 2890527621
solve199_1 input =
  runIC9 (iccomp prog [1])
  where prog = icprog input

-- 66772
solve199_2 input =
  runIC9 (iccomp prog [2])
  where prog = icprog input


-- -------------------------------------------------------------------

-- stepIC9x prog pc rb inputs
--   | opc == 1  = -- ADD
--       (M.insert pa3 (pv1 + pv2) prog, pc + 4, rb, inputs, Nothing)
--   | opc == 2  = -- MUL
--       (M.insert pa3 (pv1 * pv2) prog, pc + 4, rb, inputs, Nothing)
--   --
--   | opc == 5  = -- JUMP_IF_TRUE
--       (prog, if (pv1 /= 0) then pv2 else (pc + 3), rb, inputs, Nothing)
--   | opc == 6  = -- JUMP_IF_FALSE
--       (prog, if (pv1 == 0) then pv2 else (pc + 3), rb, inputs, Nothing)
--   --
--   | opc == 7  = -- LESS_THAN
--       (M.insert pa3 (fromEnum (pv1 < pv2)) prog, pc + 4, rb, inputs, Nothing)
--   | opc == 8  = -- EQUAL
--       (M.insert pa3 (fromEnum (pv1 == pv2)) prog, pc + 4, rb, inputs, Nothing)
--   --
--   | opc == 9  = -- ADJUST RB
--       (prog, pc + 2, rb + pv1, inputs, Nothing)
--   --
--   | opc == 3  = -- INPUT
--       case inputs of
--         (inp : inps) -> (M.insert pa1 inp prog, pc + 2, rb, inps, Nothing)
--         []           -> (prog, pc, rb, [], Nothing)
--   | opc == 4  = -- OUTPUT
--       (prog, pc + 2, rb, inputs, Just pv1)
--   | opc == 99 = -- EXIT
--       (M.empty, 0, 0, [], Nothing)
--   --
--   where
--     (pm3 : pm2 : pm1 : opc2 : opc1 : []) =
--       map digitToInt $ (printf "%05d" ((M.!) prog (pc)) :: String)
--     opc = opc2 * 10 + opc1
--     --
--     op1 = M.findWithDefault 0 (pc + 1) prog
--     op2 = M.findWithDefault 0 (pc + 2) prog
--     op3 = M.findWithDefault 0 (pc + 3) prog
--     --
--     prv 0 op = M.findWithDefault 0 (pra 0 op) prog
--     prv 1 op = op
--     prv 2 op = M.findWithDefault 0 (pra 2 op) prog
--     --
--     pra 0 op = op
--     pra 2 op = op + rb
--     --
--     pv1 = prv pm1 op1
--     pv2 = prv pm2 op2
--     pa1 = pra pm1 op1
--     pa3 = pra pm3 op3


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 8
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- 2460
solve198_1 input =
  n2 * n3
  where
    layers = chunksof (25 * 6) . head . lines $ input
    sorted = sortOn head . map (map length . group . sort) $ layers
    (n1 : n2 : n3 : []) = head $ sorted

-- LRFKO
solve198_2 input =
  trace (concat . map (strrow) . chunksof 25 $ pixels) 0
  where
    chunks = 25 * 6
    layers = chunksof chunks . map digitToInt . head . lines $ input
    pixels = map (head . dropWhile (== 2)) . transpose $ layers
    strrow []       = "\n"
    strrow (1 : cs) = "# " ++ strrow cs
    strrow (0 : cs) = "  " ++ strrow cs


-- -------------------------------------------------------------------

chunksof n l
  | null l    = []
  | n <= 0    = undefined
  | otherwise = (take n l) : (chunksof n (drop n l))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 7
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

stepIC7 prog pc inputs
  | trace (printf "# PC=%05d OPC=%05d" pc ((M.!) prog (pc))) False = undefined
  | opc == 1  = -- ADD
      (stepIC7 (M.insert op3 (ov1 + ov2) prog) (pc + 4) inputs)
  | opc == 2  = -- MUL
      (stepIC7 (M.insert op3 (ov1 * ov2) prog) (pc + 4) inputs)
  | opc == 3  = -- INPUT
      case inputs of
        []           -> ((prog), (pc), [], Nothing)
        (inp : inps) -> (stepIC7 (M.insert op1 inp prog) (pc + 2) inps)
  | opc == 4  = -- OUTPUT
      ((prog), (pc + 2), inputs, Just ov1)
  | opc == 5  = -- JUMP_IF_TRUE
      (stepIC7 (prog) (if (ov1 /= 0) then ov2 else (pc + 3)) inputs)
  | opc == 6  = -- JUMP_IF_FALSE
      (stepIC7 (prog) (if (ov1 == 0) then ov2 else (pc + 3)) inputs)
  | opc == 7  = -- LESS_THAN
      (stepIC7 (M.insert op3 (if (ov1 < ov2) then 1 else 0) prog) (pc + 4) inputs)
  | opc == 8  = -- EQUAL
      (stepIC7 (M.insert op3 (if (ov1 == ov2) then 1 else 0) prog) (pc + 4) inputs)
  | opc == 99 = -- EXIT
      (M.empty, 0, [], Nothing)
  where
    (pm3 : pm2 : pm1 : opc2 : opc1 : []) =
      map digitToInt $ (printf "%05d" ((M.!) prog (pc)) :: String)
    opc = opc2 * 10 + opc1
    op1 = (M.!) prog (pc + 1)
    op2 = (M.!) prog (pc + 2)
    op3 = (M.!) prog (pc + 3)
    ov1 = if (pm1 == 1) then (op1) else ((M.!) prog op1)
    ov2 = if (pm2 == 1) then (op2) else ((M.!) prog op2)

runIC71 prog (s0:s1:s2:s3:s4:[]) =
  out4
  where
    (np0, npc0, _, Just out0) = stepIC7 prog 0 [s0, 0]
    (np1, npc1, _, Just out1) = stepIC7 prog 0 [s1, out0]
    (np2, npc2, _, Just out2) = stepIC7 prog 0 [s2, out1]
    (np3, npc3, _, Just out3) = stepIC7 prog 0 [s3, out2]
    (np4, npc4, _, Just out4) = stepIC7 prog 0 [s4, out3]

-- 368584
solve197_1 input =
  maximum . map (runIC71 prog) . permutations $ [0..4]
  where
    prog = M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines $ input

loopIC7 p0 pc0 p1 pc1 p2 pc2 p3 pc3 p4 pc4 inp0
  | M.null np4 = inp0
  | otherwise  =
    loopIC7 np0 npc0 np1 npc1 np2 npc2 np3 npc3 np4 npc4 (fromJust out4)
  where
    (np0, npc0, _, Just out0) = stepIC7 p0 pc0 [inp0]
    (np1, npc1, _, Just out1) = stepIC7 p1 pc1 [out0]
    (np2, npc2, _, Just out2) = stepIC7 p2 pc2 [out1]
    (np3, npc3, _, Just out3) = stepIC7 p3 pc3 [out2]
    (np4, npc4, _, out4) = stepIC7 p4 pc4 [out3]

runIC72 prog (s0:s1:s2:s3:s4:[]) =
  loopIC7 np0 npc0 np1 npc1 np2 npc2 np3 npc3 np4 npc4 0
  where
    (np0, npc0, _, _) = stepIC7 prog 0 [s0]
    (np1, npc1, _, _) = stepIC7 prog 0 [s1]
    (np2, npc2, _, _) = stepIC7 prog 0 [s2]
    (np3, npc3, _, _) = stepIC7 prog 0 [s3]
    (np4, npc4, _, _) = stepIC7 prog 0 [s4]

-- 35993240
solve197_2 input =
  maximum . map (runIC72 prog) . permutations $ [5..9]
  where
    prog = M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines $ input


-- -------------------------------------------------------------------

-- new version after day 9

loopIC972x (icmp @ ICComp {prog = prog})
  | state icnx /= Run = icnx
  | otherwise = loopIC972x icnx
  where icnx = stepIC9 icmp { state = Run }

reloopIC972x inp0 (icmp0 : icmp1 : icmp2 : icmp3 : icmp4 : [])
  | state icnx4 == End = output icnx4
  | otherwise = reloopIC972x (output icnx4) [icnx0, icnx1, icnx2, icnx3, icnx4]
  where
    icnx0 = loopIC972x (icmp0 {inputs = [inp0]})
    icnx1 = loopIC972x (icmp1 {inputs = [(output icnx0)]})
    icnx2 = loopIC972x (icmp2 {inputs = [(output icnx1)]})
    icnx3 = loopIC972x (icmp3 {inputs = [(output icnx2)]})
    icnx4 = loopIC972x (icmp4 {inputs = [(output icnx3)]})

solve197_2x input =
  maximum . map
  ( reloopIC972x 0 . -- amplifier loop
    map (loopIC972x . iccomp prog . (:[])) -- init ics
  ) . permutations $ [5..9]
  where
    prog = icprog input


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 6
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

allorbits dorbits obj
  | nxt == "COM" = nxt : []
  | otherwise    = nxt : (allorbits dorbits nxt)
  where nxt = (M.!) dorbits obj

-- 140608
solve196_1 input =
  sum . map (length . allorbits dorbits) . M.keys $ dorbits
  where
    dorbits = M.fromList . map (
      (\(x : y :_) -> (y, x)) . splitOn (== ')')) . lines $ input

-- 337
solve196_2 input = -- symmetric difference
  (length (youorbs \\ sanorbs)) + (length (sanorbs \\ youorbs))
  where
    dorbits = M.fromList . map (
      (\(x : y :_) -> (y, x)) . splitOn (== ')')) . lines $ input
    youorbs = allorbits dorbits "YOU"
    sanorbs = allorbits dorbits "SAN"


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 5
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

execIntCode5 pc input prog
  | trace (printf "# PC=%05d OPC=%05d" pc ((M.!) prog (pc))) False = undefined
  | opc == 1  = -- ADD
      execIntCode5 (pc + 4) (input) (M.insert op3 (ov1 + ov2) prog)
  | opc == 2  = -- MUL
      execIntCode5 (pc + 4) (input) (M.insert op3 (ov1 * ov2) prog)
  | opc == 3  = -- INPUT
      execIntCode5 (pc + 2) (input) (M.insert op1 (input) prog)
  | opc == 4  = -- OUTPUT
      trace (printf "> %d" ov1) execIntCode5 (pc + 2) (input) (prog)
  | opc == 5  = -- JUMP_IF_TRUE
      execIntCode5 (if (ov1 /= 0) then ov2 else (pc + 3)) (input) (prog)
  | opc == 6  = -- JUMP_IF_FALSE
      execIntCode5 (if (ov1 == 0) then ov2 else (pc + 3)) (input) (prog)
  | opc == 7  = -- LESS_THAN
      execIntCode5 (pc + 4) (input) (M.insert op3 (if (ov1 < ov2) then 1 else 0) prog)
  | opc == 8  = -- EQUAL
      execIntCode5 (pc + 4) (input) (M.insert op3 (if (ov1 == ov2) then 1 else 0) prog)
  | opc == 99 = -- EXIT
      0
  where
    (pm3 : pm2 : pm1 : opc2 : opc1 : []) =
      map digitToInt $ (printf "%05d" ((M.!) prog (pc)) :: String)
    opc = opc2 * 10 + opc1
    op1 = (M.!) prog (pc + 1)
    op2 = (M.!) prog (pc + 2)
    op3 = (M.!) prog (pc + 3)
    ov1 = if (pm1 == 1) then (op1) else ((M.!) prog op1)
    ov2 = if (pm2 == 1) then (op2) else ((M.!) prog op2)

-- 11193703
solve195_1 =
  execIntCode5 0 1 .
  M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines

-- 12410607
solve195_2 =
  execIntCode5 0 5 .
  M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 4
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

pwdRuleG = (> 1) . maximum . map length . group

pwdRuleD x = (== x) . sort $ x

-- 495
solve194_1 =
  length . filter (all id . sequence [pwdRuleG, pwdRuleD] . show)
  $ [367479 .. 893698]

pwdRuleG2 x = (elem 2) . map length . group $ x

-- 305
solve194_2 =
  length . filter (all id . sequence [pwdRuleG2, pwdRuleD] . show)
  $ [367479 .. 893698]

-- puzzle input: 367479-893698


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 3
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

seglocs (x, y) (d : ls)
  | d == 'U' = [(x, y - d) | d <- [1 .. l]]
  | d == 'D' = [(x, y + d) | d <- [1 .. l]]
  | d == 'R' = [(x + d, y) | d <- [1 .. l]]
  | d == 'L' = [(x - d, y) | d <- [1 .. l]]
  where l = toInt ls

wirelocs (x, y) (z : zs)
  | otherwise = slocs : (wirelocs (last slocs) zs)
  where slocs = seglocs (x, y) z
wirelocs _ [] = []

-- 627
solve193_1 =
  minimum . map (mandist (0, 0)) . S.toList .
  foldr1 S.intersection .
  map (S.fromList . concat . wirelocs (0, 0) . splitOn (== ',')) . lines

-- -------------------------------------------------------------------

wireinter ( ls1 : ls2 : [] ) =
  minimum .
  map ( \p -> fromJust (elemIndex p ls1) + fromJust (elemIndex p ls2) + 2) .
  S.toList $ S.intersection (S.fromList ls1) (S.fromList ls2)

-- 13190
solve193_2 =
  wireinter . map (concat . wirelocs (0, 0) . splitOn (== ',')) . lines


-- -------------------------------------------------------------------

mandist (x1, y1) (x2, y2) =
  (abs (x2 - x1)) + (abs (y2 - y1))


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
--
-- 2019 DAY 2
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

execIntCode pc prog
  | op0 == 1  =
      execIntCode (pc + 4) (M.insert op3 (op1 + op2) prog)
  | op0 == 2  =
      execIntCode (pc + 4) (M.insert op3 (op1 * op2) prog)
  | op0 == 99 = prog
  where
    op0 = (M.!) prog (pc)
    op1 = (M.!) prog $ (M.!) prog (pc + 1)
    op2 = (M.!) prog $ (M.!) prog (pc + 2)
    op3 = (M.!) prog (pc + 3)

-- 4576384
solve192_1 =
  flip (M.!) 0 . execIntCode 0 .
  M.insert 2 2 . M.insert 1 12 .
  M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines


-- -- -------------------------------------------------------------------

checkICInputs ((noun, verb) : xs) prog
  | res == 19690720 = 100 * noun + verb
  | otherwise = checkICInputs xs prog
  where
    res = flip (M.!) 0 . execIntCode 0 .
      M.insert 2 verb .
      M.insert 1 noun $ prog
checkICInputs [] prog = undefined

-- 5398
solve192_2 =
  checkICInputs [(x, y) | x <- [0..100], y <- [0..100]] .
  M.fromList . zip [0..] . map toInt . splitOn ((==) ',') . head . lines


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
-- 2019 DAY 1
--
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

fuelReq = subtract 2 . flip div 3

-- 3429947
solve191_1 = sum . map (fuelReq . toInt) . lines


-- -------------------------------------------------------------------

totalFuel m
  | f > 0     = f + totalFuel f
  | otherwise = 0
  where f = fuelReq m

-- 5142043
solve191_2 = sum . map (totalFuel . toInt) . lines

-- -------------------------------------------------------------------

toInt :: String -> Int
toInt x = read x :: Int


-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------
-- -------------------------------------------------------------------

-- elm lua ruby caml julia
