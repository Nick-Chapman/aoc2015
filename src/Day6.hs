module Day6 (main) where

import Prelude hiding (head)
import Misc (check,head)
import Par4 (parse,Par,separated,nl,alts,key,int,lit)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day6.input"

  let steps1 = part1 inp
  --mapM_ print steps1
  let _res1 = head (reverse steps1)
  print ("day6, part1", check 543903 $ _res1)

  let steps2 = part2 inp
  --mapM_ print steps2
  let _res2 = head (reverse steps2)
  print ("day6, part2", check 14687245 $ _res2)


type Instruction = (Mode,Range)
data Mode = On | Off | Toggle deriving Show
type Range = (Pos,Pos)
type Pos = (Int,Int)

gram :: Par [Instruction]
gram = separated nl instruction
  where
    instruction = do m <- mode; r <- range; pure (m,r)
    range = do a <- pos; key " through "; b <- pos; pure (a,b)
    pos = do i <- int; lit ','; j <- int; pure (i,j)
    mode =
      alts [ do key "toggle "; pure Toggle
           , do key "turn on "; pure On
           , do key "turn off "; pure Off ]

part1 :: [Instruction] -> [Int]
part1 xs = loop initG xs
  where
    loop g = \case
      [] -> [count g]
      (mode,range):is -> count g : loop (foldr (action mode) g (expand range)) is

    action :: Mode -> Pos -> Grid -> Grid
    action = \case On -> on; Off -> off; Toggle -> toggle

expand :: Range -> [Pos]
expand ((x1,y1),(x2,y2)) =
  if x1>x2 || y1>y2 then error "oops" else
    [ (x,y) | x <- [x1..x2], y <- [y1..y2] ]

type Grid = Set Pos
initG :: Grid
on :: Pos -> Grid -> Grid
off :: Pos -> Grid -> Grid
toggle :: Pos -> Grid -> Grid
count :: Grid -> Int
initG = Set.empty
on p s = Set.insert p s
off p s = Set.delete p s
toggle p s = if Set.member p s then off p s else on p s
count = Set.size


part2 :: [Instruction] -> [Int]
part2 xs = loop initG2 xs
  where
    loop g = \case
      [] -> [count2 g]
      (mode,range):is -> count2 g : loop (foldr (action mode) g (expand range)) is

    action :: Mode -> Pos -> Grid2 -> Grid2
    action = \case On -> on2; Off -> off2; Toggle -> toggle2

type Grid2 = Map Pos Int
initG2 :: Grid2
on2 :: Pos -> Grid2 -> Grid2
off2 :: Pos -> Grid2 -> Grid2
toggle2 :: Pos -> Grid2 -> Grid2
count2 :: Grid2 -> Int
initG2 = Map.empty
on2 p g = Map.alter (\case Nothing -> Just 1; Just n -> Just (n+1)) p g
off2 p g = Map.update (\n -> if n<1 then Nothing else Just (n-1)) p g
toggle2 p g = Map.alter (\case Nothing -> Just 2; Just n -> Just (n+2)) p g
count2 g = sum (Map.elems g)
