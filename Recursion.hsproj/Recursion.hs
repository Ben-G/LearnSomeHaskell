-- Recursive maximum function

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs
  
-- Recursive maximum function using "max"

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "No maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- Replicate function, e.g. replicate 4 2 -> [2,2,2,2]

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 =[]
  | otherwise = x:replicate' (n-1) x
  
-- Take first n elements from list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
  
-- Pattern matching without 'otherwise' means fallthrough in case we don't match
take' _ [] = []
take' n (x: xs) = x:take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
--repeat' x = [x] ++ repeat' x
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs
 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a >= x]
  in smallerSorted ++ [x] ++ biggerSorted
 