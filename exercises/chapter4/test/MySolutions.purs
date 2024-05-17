module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Person (Person)
import Data.Picture (Shape(..), Point)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n == k = 1
             | n < k = 0
             | otherwise = factorial n / ((factorial k) * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = (binomial (n - 1) k) + (binomial (n - 1) (k - 1))

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: cityA } } { address: { city: cityB } } = cityA == cityB

fromSingleton :: forall a . a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _ = x

circleAtOrigin :: Shape
circleAtOrigin = Circle { x: 0.0, y: 0.0 } 10.0

origin :: Point
origin = { x: 0.0, y: 0.0 }

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (r * 2.0)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
doubleScaleAndCenter (Line p1 p2) = Line {x: 0.0 - x, y: 0.0 - y} {x: x, y: y}
  where
      x = abs (p2.x - p1.x)
      y = abs (p2.y - p1.y)
doubleScaleAndCenter (Text _ t) = Text origin t

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)
