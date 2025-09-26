module Day7 (main) where

import Data.Bits ((.|.),(.&.),complement,shiftL,shiftR)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word16)
import Misc (check)
import Par4 (parse,Par,separated,nl,alts,key,int,word)

main :: IO ()
main = do
  _sam <- parse gram <$> readFile "input/day7.sam"
  inp <- parse gram <$> readFile "input/day7.input"
  print ("day7, part1", check 46065 $ part1 inp)
  print ("day7, part2", check 14134 $ part2 inp)

type Node = (Func,Id)
data Func = Lit Value | OneAnd Id | AND Id Id | OR Id Id | Lshift Id Int | Rshift Id Int | Not Id | Pass Id deriving Show
type Id = String
type Value = Word16

part1 :: [Node] -> Value
part1 nodes = eval "a"
  where
    tab :: Map Id Value
    tab = Map.fromList [ (x, compute f) | (f,x) <- nodes ]
    eval :: Id -> Value
    eval x = maybe err id $ Map.lookup x tab where err = error $ show ("eval",x)
    compute :: Func -> Value
    compute = \case
      Lit x -> x
      OneAnd y -> 1 .&. eval y
      AND x y -> eval x .&. eval y
      OR x y -> eval x .|. eval y
      Lshift x n -> eval x `shiftL` n
      Rshift x n -> eval x `shiftR` n
      Not x -> complement (eval x)
      Pass x -> eval x

part2 :: [Node] -> Value
part2 nodes = do
  let v = part1 nodes
  part1 ([ n | n@(_,x) <- nodes, x /= "b" ] ++ [ (Lit v, "b") ])

gram :: Par [Node]
gram = separated nl node
  where
    node = do
      f <- func
      key " -> "
      x <- id
      pure (f,x)
    id = word
    func = alts
      [ do
          n <- int
          alts [ do key " AND "; y <- id; pure (OneAnd y)
               , pure (Lit (fromIntegral n))
               ]
      , do
          key "NOT "
          Not <$> id
      , do
          x <- id
          alts [ do
                   f <- alts [ do key " AND "; pure AND
                             , do key " OR "; pure OR
                             ]
                   y <- id
                   pure (f x y)
               , do
                   f <- alts [ do key " LSHIFT "; pure Lshift
                             , do key " RSHIFT "; pure Rshift
                             ]
                   n <- int
                   pure (f x n)
               , do
                   pure (Pass x)
               ]
      ]
