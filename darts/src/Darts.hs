module Darts (score) where
import Data.Monoid

score :: Float -> Float -> Int
score x y = (points . hip x) y

zero :: Float ->  Int
zero d = case d > 10 of
           True -> 0
           _ -> 0

one :: Float ->  Int
one d = case d > 5 && d <= 10 of
           True -> 1
           _ -> 0

five :: Float ->  Int
five d = case d > 1 && d <= 5 of
           True -> 5
           _ -> 0
           
ten :: Float ->  Int
ten d = case d <= 1 of
           True -> 10
           _ -> 0

points :: Float -> Int
points = getSum . foldMap (Sum .) [zero, one, five, ten]
          
square :: Float -> Float
square = (^2)

hip :: Float -> Float -> Float
hip x y = sqrt $ (square . abs) x + (square . abs) y
