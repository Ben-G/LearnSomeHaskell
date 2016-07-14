-- Pattern matching with function bodies:
-- Depending on which pattern is matched, a different body will be executed

lucky :: (Integral a) => a -> String
lucky 7 = "You're lucky!"
-- Match all patterns and bind to `x`
lucky x = "You're out of luck"



sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)

addVectors (a,b) (c,d) = (a+c, b+d)

-- Tuple accessor methods
--- _ means we ignore this type variable
first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c) -> b
second (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c

head' :: [a] -> a
head' [] = error "There's no head in an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements " ++ show x ++ " " ++ show y 
tell (x:_) = "The list is long."


-- Recursive length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + (length' xs)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)


capital :: String -> String
capital "" = "Empty String"
-- @ is called 'patterns'
capital all@(x:xs) = "The first letter of" ++ all ++ " is " ++ show x

-- Pipes are called 'guards'
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
      | bmi <= 18.5 = "You're underweight, you emo, you!"
      | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
      | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
      | otherwise   = "You're a whale, congratulations!" 
      
max' :: (Ord a) => a -> a -> a
max' a b 
  | a > b = a
  | otherwise = b

-- Guards can also be written in-line, albeit less readable
max'' :: (Ord a) => a -> a -> a
max'' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ
  
-- Using where statements we can create local variables for use within the guards. Indentations of all variable definition need to be aligned

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
      | bmi <= skinny = "You're underweight, you emo, you!"
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
      | bmi <= fat = "You're fat! Lose some weight, fatty!"  
      | otherwise   = "You're a whale, congratulations!"
      where bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0
            
-- We can also use pattern matching instead:

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
      | bmi <= skinny = "You're underweight, you emo, you!"
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
      | bmi <= fat = "You're fat! Lose some weight, fatty!"  
      | otherwise   = "You're a whale, congratulations!"
      where bmi = weight / height ^ 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Another example of `where`

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
        
-- Even though param pattern matching would have been easier here:
        
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

-- Using let statements

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea
  
-- Use let with list comprehension
calcBmisLet :: (RealFloat a) => [(a, a)] -> [a]
calcBmisLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
-- In example above:
-- `bmi` is output function (everything before the pipe)

calcBmisLetOnlyOverweight :: (RealFloat a) => [(a, a)] -> [a]
calcBmisLetOnlyOverweight xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- `bmi` cannot be used within `(w, h) <- xs`, since this is defined before the let binding

-- Pattern matching:
-- Pattern matching for function arguments is just syntactical sugar for
-- regular pattern matching

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x
                       

-- Inline pattern matching as part of other expressions
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."
                                               [x,y] -> "a two element list."   
                                               xs -> "a longer list." 
-- Because pattern matching in function definitions is syntactivc sugar,
-- we can also use where clause                                               
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
          
-- Or plain old argument matching
describeList'' :: [a] -> String
describeList'' [] = "Empty List"
describeList'' [x,y] = "Two Element List" 
