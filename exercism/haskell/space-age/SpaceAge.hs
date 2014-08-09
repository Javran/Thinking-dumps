module SpaceAge
    ( Planet(..)
    , ageOn
    )
where

data Planet
    = Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

ageInEarthYear :: Planet -> Float
ageInEarthYear p = case p of
    Earth   -> 1
    Mercury -> 0.2408467
    Venus   -> 0.61519726
    Mars    -> 1.8808158
    Jupiter -> 11.862615
    Saturn  -> 29.447498
    Uranus  -> 84.016846
    Neptune -> 164.79132

ageInSec :: Planet -> Float
ageInSec = (* 31557600) . ageInEarthYear

ageOn :: Planet -> Float -> Float
ageOn p = ( / ageInSec p)
