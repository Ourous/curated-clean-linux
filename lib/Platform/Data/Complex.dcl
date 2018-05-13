definition module Data.Complex

import StdOverloaded
import StdClass

:: Complex a = (:+) infixl 6 a a

class pi a :: a
class Geo a | sin a & cos a & tan a
class ArcGeo a | asin a & acos a & atan a
class HypGeo a | asinh a & acosh a & atanh a & sinh a & cosh a & tanh a
class AllGeo a | Geo a & ArcGeo a & HypGeo a

instance pi Real
instance pi (Complex a) | pi a & zero a

instance + (Complex a) | + a

instance - (Complex a) | - a
instance zero (Complex a) | zero a

instance * (Complex a) | MultDiv a & PlusMin a
instance / (Complex a) | MultDiv a & PlusMin a
instance one (Complex a) | one a & zero a

instance == (Complex a) | == a

instance abs (Complex a) | sqrt a & * a & PlusMin a
instance ~ (Complex a) | ~ a

instance fromInt (Complex a) | fromInt a & zero a
instance fromReal (Complex a) | fromReal a & zero a
instance fromString (Complex a) | fromString a
instance toString (Complex a) | toString a

instance ln (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance exp (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a & exp a
instance sqrt (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a

instance sin (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance cos (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance tan (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance asin (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance acos (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance atan (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance sinh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance cosh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance tanh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a
instance asinh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance acosh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a
instance atanh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a

realPart :: (Complex a) -> a
imagPart :: (Complex a) -> a

mkPolar :: a a -> Complex a | cos a & sin a & * a
cis :: a -> Complex a | cos a & sin a
polar :: (Complex a) -> (a, a) | Ord a & Eq a & atan a & sqrt a & MultDiv a & PlusMin a & pi a
magnitude :: (Complex a) -> a | sqrt a & * a & + a
phase :: (Complex a) -> a | Ord a & Eq a & atan a & MultDiv a & PlusMin a & pi a

conjugate :: (Complex a) -> Complex a | ~ a
