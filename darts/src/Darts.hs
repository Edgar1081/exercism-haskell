module Darts (score) where

score :: Float -> Float -> Int
score x y = 1

zero :: Float -> Maybe Int
zero d = case d > 10 of
           True -> Just 0
           _ -> Nothing

one :: Float -> Maybe Int
one d = case d > 5 of
           True -> Just 1
           _ -> Nothing

five :: Float -> Maybe Int
five d = case d > 1 of
           True -> Just 5
           _ -> Nothing
           
ten :: Float -> Maybe Int
ten d = case d < 1 of
           True -> Just 10
           _ -> Nothing

points :: Float -> Maybe Int
points d = do
        outside <- zero d
        outer <- one d
        middle <- five d
        inner <- ten d

firstJust :: [Maybe a] -> Maybe a
firstJust =
          
square :: Float -> Float
square = (^2)

hip :: Float -> Float -> Float
hip x y = sqrt $ (square . abs) x + (square . abs) y
