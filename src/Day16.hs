module Day16 (main) where

import Misc (check)
import Par4 (Par,parse,separated,int,word,key,nl)
import Data.Map qualified as Map
import Data.Map (Map)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day16.input"
  print ("day16, part1", check [373] $ part1 inp)
  print ("day16, part2", check [260] $ part2 inp)

type Aunt = Map String Int

gram :: Par [(Int,Aunt)]
gram = separated nl aunt
  where
    aunt = do
      key "Sue "; i <- int; key ": "; xs <- separated (key ", ") fact
      pure (i, Map.fromList xs)
    fact = do f <- word; key ": "; n <- int; pure (f,n)

part1 :: [(Int,Aunt)] -> [Int]
part1 xs =
  [ i
  | (i,memory) <- xs
  , all (\(key,val) ->
            case (Map.lookup key memory) of
              Just v -> v==val
              Nothing -> True
        ) evidence
  ]

part2 :: [(Int,Aunt)] -> [Int]
part2 xs =
  [ i
  | (i,memory) <- xs
  , all (\(key,val) ->
            case (Map.lookup key memory) of
              Just v -> func key v val
              Nothing -> True
        ) evidence
  ]
  where
    func = \case
      "cats" -> (>)
      "trees" -> (>)
      "pomeranians" -> (<)
      "goldfish" -> (<)
      _ -> (==)

evidence :: [(String,Int)]
evidence =
  [ ("children", 3)
  , ("cats", 7)
  , ("samoyeds", 2)
  , ("pomeranians", 3)
  , ("akitas", 0)
  , ("vizslas", 0)
  , ("goldfish", 5)
  , ("trees", 3)
  , ("cars", 2)
  , ("perfumes", 1) ]
