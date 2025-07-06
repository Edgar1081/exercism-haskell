module SpaceAge (Planet(..), ageOn) where
import Data.Maybe
import Data.List

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving (Show, Eq)

ageOn :: Planet -> Float -> Float
ageOn planet = snd . (fromJust . filterScale planet) . allScales

filterScale :: Planet -> [(Planet, Float)] -> Maybe (Planet, Float)
filterScale planet = find ((== planet) . fst)

round2decimals :: Float -> Float
round2decimals f = fromIntegral (round (f * 100)) / 100

allScales :: Float -> [(Planet, Float)]
allScales f = do
            ps <- scales
            [(fst ps, (round2decimals . snd ps . earthYears) f)]
           
  where scales = [(Mercury, mercuryScale), (Venus,venusScale),
                  (Earth, earthScale), (Mars,marsScale),
                  (Jupiter, jupiterScale), (Saturn,saturnScale),
                  (Uranus,uranusScale), (Neptune, neptuneScale)]
             
earthYears :: Float -> Float
earthYears y = y / 31557600

earthScale :: Float -> Float
earthScale = (*) 1

mercuryScale :: Float -> Float
mercuryScale = (/ 0.2408467)

venusScale :: Float -> Float
venusScale = (/ 0.61519726)

marsScale :: Float -> Float
marsScale = (/ 1.8808158)

jupiterScale :: Float -> Float
jupiterScale = (/ 11.862615)

saturnScale :: Float -> Float
saturnScale = (/ 29.447498)

uranusScale :: Float -> Float
uranusScale = (/ 84.016846)

neptuneScale :: Float -> Float
neptuneScale = (/ 164.79132)
