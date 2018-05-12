implementation module Data.Complex

import StdBool
import StdClass
import StdInt
import StdMisc
import StdOverloaded
import StdReal
import StdString

from Text import class Text(split,textSize), instance Text String

:: Complex a = (:+) infixl 6 a a

instance pi Real where
	pi = 3.14159265358979323846264338327950288419716939937510582097494459230781

instance pi (Complex a) | pi a & zero a where pi = pi :+ zero

instance + (Complex a) | + a where
	(+) (a :+ b) (c :+ d) = (a + c) :+ (b + d)

instance - (Complex a) | - a where
	(-) (a :+ b) (c :+ d) = (a - c) :+ (b - d)

instance zero (Complex a) | zero a where zero = zero :+ zero

instance * (Complex a) | MultDiv a & PlusMin a where
	(*) (a :+ b) (c :+ d) = (a*c-b*d) :+ (b*c-a*d)

instance / (Complex a) | MultDiv a & PlusMin a where
	(/) (a :+ b) (c :+ d) = (a*c+b*d)/(c*c+d*d) :+ (b*c-a*d)/(c*c+d*d)

instance one (Complex a) | one a & zero a where
	one = one :+ zero

instance == (Complex a) | == a where
	(==) (a :+ b) (c :+ d) = a == c && c == d
	
instance abs (Complex a) | sqrt a & * a & PlusMin a where
	abs z = magnitude z :+ zero

instance ~ (Complex a) | ~ a where
	~ (x :+ y) = ~x :+ ~y

instance fromInt (Complex a) | fromInt a & zero a where
	fromInt i = (fromInt i) :+ zero

instance fromReal (Complex a) | fromReal a & zero a where
	fromReal r = (fromReal r) :+ zero

instance fromString (Complex a) | fromString a where
	fromString s = case split "+" s of
		[a,b] = case b % ((textSize b) - 1, textSize b) of
			"i" = fromString a :+ fromString b
			_ = abort "Couldn't parse Complex number"
		_ = abort "Couldn't parse Complex number"

instance toString (Complex a) | toString a where
	toString (x :+ y) = toString x +++ "+" +++ toString y +++ "i"

instance ln (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
	ln z = ln (magnitude z) :+ phase z

instance exp (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a & exp a where
	exp (x:+y) = let expx = exp x in expx * cos y :+ expx * sin y

instance sqrt (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
    sqrt z=:(x :+ y)
	| x == zero && y == zero = zero
                             = u :+ if (y < zero) (~v) v
    	where 
			(u,v) = if (x < zero) (v`,u`) (u`,v`)
        	v`    = abs y / (u`*two)
        	u`    = sqrt ((magnitude z + abs x) / two)

//	Trigonometrical Functions:
instance sin (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
	sin (x :+ y) = (sin x * cosh y) :+ (cos x * sinh y)

instance cos (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
	cos (x :+ y) = (cos x * cosh y) :+ ~ (sin x * sinh y)

instance tan (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
	tan (x :+ y) = (sinx * coshy :+ cosx * sinhy) / 
			((cosx * coshy :+ ~sinx * sinhy))
		where 
			sinx  = sin x
			cosx  = cos x
			sinhy = sinh y
			coshy = cosh y

instance asin (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    asin z=:(x:+y) = y` :+ ~x`
		where
			(x` :+ y`) = ln ((~y:+x) + sqrt (one - z*z))

instance acos (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    acos z = y`` :+ (~x``)
		where
			(x`` :+ y``) = ln (z + (~y`:+x`))
			(x` :+ y`)   = sqrt (one - z*z)

instance atan (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    atan z=:(x :+ y) = y` :+ ~x`
		where
			(x` :+ y`) = ln (((one-y) :+x ) / sqrt (one + z*z))

instance sinh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
    sinh (x:+y) = cos y * sinh x :+ sin y * cosh x

instance cosh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
    cosh (x:+y) = cos y * cosh x :+ sin y * sinh x

instance tanh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a where
    tanh (x:+y) = (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
		where 
			siny  = sin y
			cosy  = cos y
			sinhx = sinh x
			coshx = cosh x

instance asinh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    asinh z = ln (z + sqrt (one + z*z))

instance acosh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    acosh z = ln (z + (z+one) * sqrt ((z-one)/(z+one)))

instance atanh (Complex a) | Ord a & Eq a & AllGeo a & Arith a & pi a & sqrt a & ln a where
    atanh z = (one/two) * ln ((one+z) / (one-z))

realPart :: (Complex a) -> a
realPart (x :+ _) = x

imagPart :: (Complex a) -> a
imagPart (_ :+ x) = x

mkPolar :: a a -> Complex a | cos a & sin a & * a
mkPolar r theta = r * cos theta :+ r * sin theta

cis :: a -> Complex a | cos a & sin a
cis theta = cos theta :+ sin theta

polar :: (Complex a) -> (a, a) | Ord a & Eq a & atan a & sqrt a & MultDiv a & PlusMin a & pi a
polar z = (magnitude z, phase z)

magnitude :: (Complex a) -> a | sqrt a & * a & + a
magnitude (x :+ y) = sqrt (x*x + y*y)

phase :: (Complex a) -> a | Ord a & Eq a & atan a & MultDiv a & PlusMin a & pi a
phase (x :+ y)
| x > zero              = atan (y/x)
| x < zero && y >= zero = atan (y/x) + pi
| x < zero && y < zero  = atan (y/x) - pi
| x == zero && y > zero = pi / two
| x == zero && y < zero = pi / two
                        = undef

conjugate :: (Complex a) -> Complex a | ~ a
conjugate (x :+ y) = x :+ (~y)

//Ugly
two :: a | one a & + a
two = one + one
