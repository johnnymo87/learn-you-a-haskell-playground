import qualified Data.Map as M
  ( fromList
  , fromListWith
  , insert
  , lookup
  , Map
  , size
  )

import qualified Data.Char as C
  ( digitToInt
  , isDigit
  )

phoneBookToMap :: Ord k => [(k, String)] -> M.Map k [String]
phoneBookToMap book = M.fromListWith (++) $ map (\(k, v) -> (k, [v])) book

phoneBook = phoneBookToMap $
  [ ("betty", "555-2938")
  , ("betty", "342-2492")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("patsy", "943-2929")
  , ("patsy", "827-9162")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  , ("penny", "555-2111")
  ]

string2digit = map C.digitToInt . filter C.isDigit

--findKey :: Eq k => k -> [(k, v)] -> Maybe v
--findKey _ [] = Nothing
--findKey key ((k,v):xs)
--    | key == k  = Just v
--    | otherwise = findKey key xs
--findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
