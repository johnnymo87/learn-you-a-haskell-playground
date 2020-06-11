import qualified Data.List as L (
  group,
  find,
  isPrefixOf,
  nub,
  sort,
  tails,
  words)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . L.nub

wordNums :: String -> [(String, Int)]
wordNums = (map (\ws -> (head ws, length ws)) . L.group . L.sort . L.words)

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` hayStack = any (needle `L.isPrefixOf`) $ L.tails hayStack
