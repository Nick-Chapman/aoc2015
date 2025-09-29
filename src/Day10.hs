module Day10 (main) where

import Misc (check,head)
import Par4 (parse,many,digit)
import Prelude hiding (head)

main :: IO ()
main = do
  let inp = parse (many digit) "1113122113"
  print ("day10, part1", check 360154 $ compute 40 inp)
  print ("day10, part2", check 5103798 $ compute 50 inp)

compute :: Int -> [Int] -> Int
compute n xs = length $ head $ drop n $ iterate step xs

step :: [Int] -> [Int]
step xs = reps xs >>= \(n,x) -> [n,x]

reps :: Eq a => [a] -> [(Int,a)]
reps = \case
  [] -> []
  x:xs -> loop 1 x xs
    where
      loop n last = \case
        [] -> [(n,last)]
        x:xs ->
          if x==last
          then loop (n+1) x xs
          else (n,last) : loop 1 x xs
