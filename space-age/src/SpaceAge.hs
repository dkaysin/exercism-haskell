module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn p s = s / 31557600 / period p

period :: Planet -> Float
period Mercury = 0.2408467
period Venus   = 0.61519726
period Earth   = 1.0
period Mars    = 1.8808158
period Jupiter = 11.862615
period Saturn  = 29.447498
period Uranus  = 84.016846
period Neptune = 164.79132
period _       = 1.0
