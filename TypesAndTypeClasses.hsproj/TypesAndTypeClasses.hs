removeNonUppercaseChars :: String -> String
removeNonUppercaseChars st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c