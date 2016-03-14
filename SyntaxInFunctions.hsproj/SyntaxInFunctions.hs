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
