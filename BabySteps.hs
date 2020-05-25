doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = 
  if x > 100
    then x
    else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' xs = sum [1 | _ <- xs]

xxs = 
  [[1,3,5,2,3,1,2,4,5],
  [1,2,3,4,5,6,7,8,9],
  [1,2,4,2,1,6,3,1,3,2,3,6]]

thing = 
  [ [ x | x <- xs, even x ] 
  | xs <- xxs ]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise = "You're a whale, congratulations!" 

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
  | bmi <= 18.5 = "You're underweight, you emo, you!"  
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise = "You're a whale, congratulations!"  
  where bmi = weight / height ^ 2

bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height  
  | bmi <= skinny = "You're underweight, you emo, you!"  
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= fat = "You're fat! Lose some weight, fatty!"  
  | otherwise = "You're a whale, congratulations!"  
  where bmi = weight / height ^ 2  
        skinny = 18.5  
        normal = 25.0  
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname  
        (l:_) = lastname

initials' :: String -> String -> String  
initials' (firstInitial:_) (lastInitial:_) = [firstInitial] ++ ". " ++ [lastInitial] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
