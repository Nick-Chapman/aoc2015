module Day2 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,lit,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day2.sample"
  inp <- parse gram <$> readFile "input/day2.input"
  print ("day2, part1 (sample)", check [58,43] $ part1 sam)
  print ("day2, part1", check 1598415 $ sum $ part1 inp)
  print ("day2, part2 (sample)", check [34,14] $ part2 sam)
  print ("day2, part2", check 3812909 $ sum $ part2 inp)

type Box = (Int,Int,Int)

gram :: Par [Box]
gram = separated nl box
  where
    box = do
      a <- int
      lit 'x'
      b <- int
      lit 'x'
      c <- int
      pure (a,b,c)

part1 :: [Box] -> [Int]
part1 = map paper
  where
    paper (a,b,c) = do
      let ab = a*b
      let ac = a*c
      let bc = b*c
      2*(ab+ac+bc) + minimum [ab,ac,bc]

part2 :: [Box] -> [Int]
part2 = map paper
  where
    paper (a,b,c) = do
      2 * minimum [a+b,a+c,b+c] + a*b*c
