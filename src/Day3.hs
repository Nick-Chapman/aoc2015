module Day3 (main) where

import Misc (check,nub)
import Par4 (parse,Par,many,alts,lit)

main :: IO ()
main = do
  let sam1 = parse gram ">"
  let sam2 = parse gram "^>v<"
  let sam3 = parse gram "^v^v^v^v^v"
  inp <- parse gram <$> readFile "input/day3.input"

  print ("day3, part1 (sample1)", check 2 $ part1 sam1)
  print ("day3, part1 (sample2)", check 4 $ part1 sam2)
  print ("day3, part1 (sample3)", check 2 $ part1 sam3)
  print ("day3, part1", check 2592 $ part1 inp)

  let sam1' = parse gram "^v"

  print ("day3, part2 (sample1)", check 3 $ part2 sam1')
  print ("day3, part2 (sample2)", check 3 $ part2 sam2)
  print ("day3, part2 (sample3)", check 11 $ part2 sam3)
  print ("day3, part2", check 2360 $ part2 inp)

data Dir = U | D | L | R

gram :: Par [Dir]
gram = many (alts [ do lit '<'; pure L
                  , do lit '>'; pure R
                  , do lit '^'; pure U
                  , do lit 'v'; pure D ])

part1 :: [Dir] -> Int
part1 = length . nub . locations (0,0)

part2 :: [Dir] -> Int
part2 xs = do
  let (ys,zs) = split xs
  length $ nub $ (locations (0,0) ys ++ locations (0,0) zs)

split :: [a] -> ([a],[a])
split = \case
  [] -> ([],[])
  x:xs -> let (ys,zs) = split xs in (x:zs,ys)

type Pos = (Int,Int)

locations :: Pos -> [Dir] -> [Pos]
locations p = \case
  [] -> [p]
  d:ds -> p : locations (move p d) ds

move :: Pos -> Dir -> Pos
move (x,y) = \case
  U -> (x,y+1)
  D -> (x,y-1)
  L -> (x-1,y)
  R -> (x+1,y)
