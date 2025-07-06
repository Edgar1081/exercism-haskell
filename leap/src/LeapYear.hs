module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = case isBy4 year of
                    False -> False
                    True -> case isBy100 year of
                      True -> isBy400 year
                      False -> True
                      
isBy100 :: Integer -> Bool
isBy100 y = mod y 100 == 0

isBy400 :: Integer -> Bool
isBy400 y = mod y 400 == 0

isBy4 :: Integer -> Bool
isBy4 y = mod y 4 == 0
