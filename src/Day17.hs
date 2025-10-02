module Day17 (main) where

import Misc (check)
import Par4 (parse,separated,int,nl)

main :: IO ()
main = do
  let sam = [20, 15, 10, 5, 5]
  inp <- parse (separated nl int) <$> readFile "input/day17.input"
  print ("day17, part1 (sample)", check 4 $ length $ part1 25 sam)
  print ("day17, part1", check 1304 $ length $ part1 150 inp)
  print ("day17, part2 (sample)", check 3 $ length $ part2 25 sam)
  print ("day17, part2", check 18 $ length $ part2 150 inp)

part1 :: Int -> [Int] -> [[Int]]
part1 tot xs = [ pick | pick <- combs xs, sum pick == tot ]

part2 :: Int -> [Int] -> [[Int]]
part2 tot xs = do
  let picks = [ pick | pick <- combs xs, sum pick == tot ]
  let smallest = minimum [ length xs | xs <- picks ]
  [ pick | pick <- picks, length pick == smallest ]

combs :: [a] -> [[a]]
combs = \case
  [] -> [[]]
  x:xs -> [ a | ys <- combs xs, a <- [ys,x:ys] ]
