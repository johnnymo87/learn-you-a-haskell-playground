import qualified Data.Char as C (
  chr,
  digitToInt,
  ord)
import qualified Data.List as L (find)

encode :: Int -> String -> String
encode offset = map $ C.chr . (+ offset) . C.ord

decode :: Int -> String -> String
decode = encode . negate

digitsSumTo :: Int -> Maybe Int
digitsSumTo n = L.find ((== n) . sumOfDigits) [1..]
    where sumOfDigits = sum . map C.digitToInt . show
