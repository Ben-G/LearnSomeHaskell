compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' x = compare 100 x

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  
  
divideByTen = (/10)

divideByTen' :: (Floating a) => a -> a
divideByTen' = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x
    
-- Simpler implementation of flip:

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

-- Quicksort with filter

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted
  
largestDivisible :: (Integral a) => a -> a
largestDivisible x = head (filter p [x, x-1..])
  where p x = x `mod` 3829 == 0
  
filterWithWhere = head(filter p [10, 9..])
  where p x = x == 2 
  
-- Collatz sequence

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 +1)

chainsWithLength :: Int -> Int -> Int
chainsWithLength x y = length (filter p [chain n | n <- [1..x]])
  where p c = length c > y

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  