module Day8 (main) where

import Misc (check)

main :: IO ()
main = do
  sam <- lines <$> readFile "input/day8.sam"
  inp <- lines <$> readFile "input/day8.input"
  print ("day8, part1 (sample)", check 12 $ part1 sam)
  print ("day8, part1", check 1350 $ part1 inp)
  print ("day8, part2 (sample)", check 19 $ part2 sam)
  print ("day8, part2", check 2085 $ part2 inp)

part1 :: [String] -> Int
part1 = sum . map waste1

waste1 :: String -> Int
waste1 = \case
  '"':s -> loop s
  _ -> error "a"
  where
    loop = \case
      [] -> error "b"
      ['"'] -> 2
      '\\':'\\':s -> 1 + loop s
      '\\':'"':s -> 1 + loop s
      '\\':'x':_:_:s -> 3 + loop s
      '\\':_ -> error "c"
      _:s -> loop s

part2 :: [String] -> Int
part2 = sum . map waste2

waste2 :: String -> Int
waste2 s = 2 + loop s
  where
    loop = \case
      [] -> 0
      '\\':s -> 1 + loop s
      '"':s -> 1 + loop s
      _:s -> loop s
