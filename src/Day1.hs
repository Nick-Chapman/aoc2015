module Day1 (main) where

import Misc (check)
import Par4 (parse,Par,many,alts,lit)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day1.input"
  print ("day1, part1", check 138 $ part1 inp)
  print ("day1, part2", check 1771 $ part2 inp)

data Paren = Open | Close

gram :: Par [Paren]
gram = many paren
  where paren = alts [ do lit '('; pure Open
                     , do lit ')'; pure Close
                     ]

part1 :: [Paren] -> Int
part1 xs = sum [ case x of Open -> 1; Close -> -1 | x <- xs ]

part2 :: [Paren] -> Int
part2 xs = loop 1 0 xs
  where
    loop :: Int -> Int -> [Paren] -> Int
    loop p h = \case
      [] -> undefined
      Open:xs -> loop (p+1) (h+1) xs
      Close:xs -> if h == 0 then p else loop (p+1) (h-1) xs
