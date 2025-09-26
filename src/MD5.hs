module MD5 (md5) where

import Data.Hash.MD5 qualified as MD5_reference

import Data.Bits (Bits,rotateL,(.|.),(.&.),complement,shiftL,shiftR)
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word32)
import Prelude hiding (and,or,not)
import Text.Printf (printf)
import qualified Data.Bits as Bits (xor)

myassert :: String -> Bool -> a -> a
myassert why b x = if b then x else error why

type Byte = Word8
byteOfChar :: Char -> Byte
byteOfChar = c2w

mkByte :: Int -> Byte
mkByte = fromIntegral

newtype W32 = X Word32 deriving (Eq,Bits,Num)

and,or,xor,plus :: W32 -> W32 -> W32
leftRotate :: W32 -> Int -> W32
not :: W32 -> W32

and = (.&.)
or = (.|.)
xor = Bits.xor
plus = (+)
leftRotate = rotateL
not = complement

makeW32 :: (Byte,Byte,Byte,Byte) -> W32
makeW32 (a,b,c,d) =
  fromIntegral d `shiftL` 24
  + fromIntegral c `shiftL` 16
  + fromIntegral b `shiftL` 8
  + fromIntegral a

splitW32 :: W32 -> (Byte,Byte,Byte,Byte)
splitW32 (X w) =
  ( fromIntegral (w `mod` 256)
  , fromIntegral ((w `shiftR` 8) `mod` 256)
  , fromIntegral ((w `shiftR` 16) `mod` 256)
  , fromIntegral ((w `shiftR` 24) `mod` 256)
  )

instance Show W32 where
  show x = do
    let (a,b,c,d) = splitW32 x
    printf "%02x%02x%02x%02x" a b c d -- little endian

kTable :: [W32]
kTable =
  [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
  , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
  , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
  , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
  , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
  , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
  , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
  , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
  , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
  , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
  , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
  , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
  , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
  , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
  , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
  , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
  ]

type Func = (W32,W32,W32) -> W32

fTable :: [Func]
fTable =
  replicate 16 f ++ replicate 16 g ++ replicate 16 h ++ replicate 16 i
  where
    f,g,h,i :: Func
    f (b,c,d) = (b `and` c) `or` ((not b) `and` d)
    g (b,c,d) = (d `and` b) `or` ((not d) `and` c)
    h (b,c,d) = b `xor` c `xor` d
    i (b,c,d) = c `xor` (b `or` (not d))

sTable :: [Int]
sTable =
  [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
  , 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
  , 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
  , 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ]

data State = State (W32,W32,W32,W32)
instance Show State where show (State (a,b,c,d)) = show (a,b,c,d)

state0 :: State
state0 = State (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)

data W512 = SixteenWords [W32] -- an input package of 16x 32-bit words

instance Show W512 where show (SixteenWords xs) = show xs

makeIP :: [W32] -> W512
makeIP xs = myassert "makeIP" (length xs == 16) $ SixteenWords xs

pick :: W512 -> Int -> W32
pick (SixteenWords xs) i = xs !! (i `mod` 16)

pickInputsInDefinedOrder :: W512 -> [W32]
pickInputsInDefinedOrder ip =
  round1 ++ round2 ++ round3 ++ round4
  where
    round1 = [ pick ip i       | i <- [0..15] ]
    round2 = [ pick ip (5*i+1) | i <- [0..15] ]
    round3 = [ pick ip (3*i+5) | i <- [0..15] ]
    round4 = [ pick ip (7*i)   | i <- [0..15] ]

chunk4 :: [Byte] -> [W32]
chunk4 = \case
  [] -> []
  a:b:c:d:xs -> makeW32 (a,b,c,d) : chunk4 xs
  _ -> error "chunk4"

-- TODO: make this work for longer messages
padSingleIP :: [Byte] -> W512
padSingleIP bs = do
  let len = length bs
  if len > 55 then error "too big" else do
    let size = len * 8 -- size in bits
    let sizeByteL = mkByte (size `mod` 256)
    let sizeByteH = mkByte ((size `shiftR` 8) `mod` 256)
    let sizeWordL = makeW32 (sizeByteL,sizeByteH,0,0)
    let sizeWordH = makeW32 (0,0,0,0)
    let bs' = bs ++ [mkByte 128] ++ replicate (55 - len) byte0
    myassert (show ("b56",length bs')) (length bs' == 56) $
      makeIP (chunk4 bs' ++ [sizeWordL,sizeWordH])
  where
    byte0 = mkByte 0

data Quad = Quad (W32,Func,W32,Int)

makeQuadTable :: [W32] -> [Quad]
makeQuadTable iTable =
  [ Quad(i,f,k,s) | (i,(f,k,s)) <- zip iTable (zip3 fTable kTable sTable) ]

plusState :: State -> State -> State
plusState (State (a,b,c,d)) (State (w,x,y,z)) =
  State (a `plus` w, b `plus` x, c `plus` y, d `plus` z)

computeNewB :: State -> Quad -> W32
computeNewB (State(a,b,c,d)) (Quad (i,f,k,s)) =
  b `plus` ( (f(b,c,d) `plus` a `plus` k `plus` i)  `leftRotate` s)

sixtyFourSteps :: State -> W512 -> State
sixtyFourSteps state inputPackage =
  let inputEachUsed4time = pickInputsInDefinedOrder inputPackage in
  let qs = makeQuadTable inputEachUsed4time in
  myassert "q64" (length qs == 64) $
  state `plusState` loop state qs
  where
    loop :: State -> [Quad] -> State
    loop s@(State(_a,b,c,d)) = \case
      [] -> s
      q:qs -> do
        let b' = computeNewB s q
        let s' = State(d,b',b,c)
        loop s' qs

prettyPrint :: State -> String
prettyPrint (State (a,b,c,d)) = printf "%s%s%s%s" (show a) (show b) (show c) (show d)

md5_mine :: String -> String
md5_mine string = do
  let ip = padSingleIP (map byteOfChar string)
  let finalState = sixtyFourSteps state0 ip
  prettyPrint finalState

md5_reference :: String -> String
md5_reference s = MD5_reference.md5s (MD5_reference.Str s)

md5 :: String -> String
md5 s = do
  let _a = md5_reference s
  let _b = md5_mine s
  --if (_a /= _b) then error (show ("md5(ref!=mine)",s,_a,_b)) else
  _b
