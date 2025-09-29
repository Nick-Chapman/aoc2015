module Day9 (main) where

import Data.List (permutations)
import Data.Map (Map)
import Data.Map qualified as Map
import Misc (check,tail,nub)
import Par4 (parse,Par,separated,nl,key,int,word)
import Prelude hiding (tail)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day9.sam"
  inp <- parse gram <$> readFile "input/day9.input"
  print ("day9, part1 (sample)", check 605 $ part1 sam)
  print ("day9, part1", check 141 $ part1 inp)
  print ("day9, part2 (sample)", check 982 $ part2 sam)
  print ("day9, part2", check 736 $ part2 inp)
  where
    part1 = minimum . distances
    part2 = maximum . distances

type Edge = (String,String,Int)

gram :: Par [Edge]
gram = separated nl edge
  where edge = do a <- word; key " to "; b <- word; key " = "; n <- int; pure (a,b,n)

distances :: [Edge] -> [Int]
distances edges = [ pathCost order | order <- permutations nodes ]
  where
    nodes :: [String]
    nodes = nub [ x | (a,b,_) <- edges, x <- [a,b] ]

    tab :: Map (String,String) Int
    tab = Map.fromList [ (p,n) | (a,b,n) <- edges, p <- [(a,b),(b,a)] ]

    dist :: (String,String) -> Int
    dist k = maybe err id $ Map.lookup k tab where err = error $ show ("dist",k)

    pathCost :: [String] -> Int
    pathCost xs = sum [ dist (a,b) | (a,b) <- zip xs (tail xs) ]
