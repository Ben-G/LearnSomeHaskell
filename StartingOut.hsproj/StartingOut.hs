doubleMe x = x + x
doubleUs x y = x*2 + y*2  
doubleSmallNumber x = if x > 100 
                  then x
                  else x*2  
                  
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1 

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--- Replace every element in the list with `1`, then add them together to get the size of the list
length' xs = sum [1 | _ <- xs]

--- Filter non uppercase characters
removeNonUpCaseString str = [ c | c <- str, c `elem` ['A'..'Z']]
