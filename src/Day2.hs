module Day2 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,lit,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day2.sample"
  inp <- parse gram <$> readFile "input/day2.input"
  print ("day2, part1 (sample)", check [58,43] $ part1 sam)
  print ("day2, part1", check 1598415 $ sum $ part1 inp)

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
