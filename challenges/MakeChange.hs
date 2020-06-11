-- https://maxfieldchen.com/posts/2020-05-23-Dynamically-Flunking-Code-Interview-Haskell.html
import qualified Debug.Trace as T
  ( trace
  )

makeChange :: [Int] -> Int -> Int
makeChange coins total = last memo where
  initMemo = 1 : replicate total 0
  memo = foldr buildRow initMemo coins
  buildRow coin prevRow = newRow where
    -- newRow = zipWith (+) withCoin (T.trace (show ("prevRow: ", prevRow)) prevRow)
    newRow = zipWith (+) withCoin prevRow
    withCoin = replicate coin 0 ++ newRow


makeChange' :: [Int] -> Int -> [[Int]]
makeChange' coins total = memo where
  initMemo = 1 : replicate total 0
  memo = scanl buildRow initMemo coins
  buildRow prevRow coin = newRow where
    newRow = zipWith (+) withCoin prevRow
    withCoin = replicate coin 0 ++ newRow

buildRow' prevRow coin = newRow where
  newRow = zipWith (+) withCoin prevRow
  withCoin = replicate coin 0 ++ newRow

builtRow = newRow where
  newRow = zipWith (+) (replicate 1 0 ++ newRow) [1,0,0,0,0,0]


builtRow' = zipWith (+) (replicate 1 0 ++ builtRow') [1,0,0,0,0,0]
builtRow'' = zipWith (+) (replicate 2 0 ++ builtRow'') [1,1,1,1,1,1]
-- [0,0] [1,1]
-- [0,0,1,1] [1,1]

-- builtRow'' = zipWith (+) (0:builtRow'') [1,0,0,0,0,0]
-- using simple-reflect
-- let builtRow''' = zipWith (+) (z:builtRow') [a,b,c,d,e,f]  in builtRow''' :: [Expr] -- jon1 for a good time
-- [z + a,z + a + b,z + a + b + c,z + a + b + c + d,z + a + b + c + d + e,z + a...
