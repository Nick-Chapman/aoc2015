module Day5 (main) where

import Misc (check,collate)
import Data.List (sort)
main :: IO ()
main = do
  inp <- lines <$> readFile "input/day5.input"

  print ("day5, nice1 example 1", check True $ nice1 "ugknbfddgicrmopn")
  print ("day5, nice1 example 2", check True $ nice1 "aaa")
  print ("day5, nice1 example 3", check False $ nice1 "jchzalrnumimnmhp")
  print ("day5, nice1 example 4", check False $ nice1 "haegwjzuvuyypxyu")
  print ("day5, nice1 example 5", check False $ nice1 "dvszwmarrgswjxmb")
  print ("day5, part1", check 236 $ part1 inp)

  print ("day5, nice2 example 1", check True $ nice2 "qjhvhtzxzqqjkmpb")
  print ("day5, nice2 example 2", check True $ nice2 "xxyxx")
  print ("day5, nice2 example 3", check False $ nice2 "uurcxstgmygtbstg")
  print ("day5, nice2 example 4", check False $ nice2 "ieodomkazucvgmuy")

  print ("day5, part2", check 51 $ part2 inp)

    where
      part1 xs = length (filter nice1 xs)
      part2 xs = length (filter nice2 xs)

nice1 :: String -> Bool
nice1 s = threeVowels && hasDoubleLetter && not containsBad
  where
    pairs = case s of [] -> []; s@(_:s') -> zip s s'

    threeVowels = length [ () | c <- s, isVowel c ] >=3
      where isVowel c = c `elem` "aeiou"

    hasDoubleLetter = length [ () | (c,d) <- pairs, c ==d ] >=1

    containsBad = length [ () | (c,d) <- pairs, isBad [c,d] ] >=1
      where isBad sub = sub `elem` ["ab","cd","pq","xy"]

nice2 :: String -> Bool
nice2 string = condA && condB
  where
    pairs = case string of [] -> []; s@(_:s') -> zip s s'

    condA =
      any nonOverlappedPair
      [ sort ps | (_,ps) <- collate (zip pairs [1::Int ..]) ]

    nonOverlappedPair :: [Int] -> Bool
    nonOverlappedPair = \case
      [] -> error "nonOverlappedPair"
      [_] -> False
      [p1,p2] -> if p2-p1 >= 2 then True else False
      _ -> True

    condB =
      any sepBy1char
      [ sort ps | (_,ps) <- collate (zip string [1::Int ..]) ]

    sepBy1char :: [Int] -> Bool
    sepBy1char = \case
      [] -> error "sepBy1char"
      xs@(_:xs') -> any (\(p1,p2) -> p2 == p1+2) (zip xs xs')
