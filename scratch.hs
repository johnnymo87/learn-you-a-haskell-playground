seven :: Int -> String
seven 7 = "LUCKY NUMBER SEVEN"
seven x = "sorry pal, it's not " ++ show x

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

initials :: String -> String -> String
initials (f:_) (l:_) = f : ['.',' ',l,'.']

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

--rpn (l:r:o:rest) = if o `elem` [+, *] then rpn (o l r):rest else rpn l:(rpn r:o:rest)
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
-- reverse' xs = last xs : reverse (init xs)
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

-- repeat' :: [a] -> [a]
-- repeat' [] = []
-- repeat' (x:xs) = x : repeat' (xs ++ [x])

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
-- elem' x (y:ys) = if x == y then True else elem' x ys
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys

-- [8,4,9,2]
-- [4,2,9,8,2,9]
qs :: (Ord a) => [a] -> [a]
qs []     = []
qs (x:xs) =
    let lessers  = [y | y <- xs, y <= x]
        greaters = [y | y <- xs, y >  x]
    in qs lessers ++ [x] ++ qs greaters

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a

sumOfOdds :: Int
sumOfOdds = sum $ takeWhile (< 10000) $ filter odd $ map (^2) [1..]

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd  n = n:collatz (n * 3 + 1)

longCollatz :: Int
longCollatz = length $ filter (\xs -> length xs > 15) $ map collatz [1..100]

sum'' :: Num a => [a] -> a
-- sum'' xs = foldl (\acc x -> acc + x) 0 xs
sum'' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

elem'' :: (Eq a, Foldable t) => a -> t a -> Bool
elem'' y = foldr (\x acc -> x == y || acc) False

maximum' :: (Ord a, Foldable t) => t a -> a
maximum' = foldl1 max

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

and' :: [Bool] -> Bool
and' = foldr (&&) True
-- works on `and' $ False : (repeat True)`!

smallRoots = takeWhile (<1000) $ scanl1 (+) (map sqrt [1..])
