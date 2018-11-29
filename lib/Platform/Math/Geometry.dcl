definition module Math.Geometry

from StdEnv import class ==, class <, class +, class -, class sign

//* The constant pi.
pi :== 3.14159265359

:: Angle
  = Deg !Real
  | Rad !Real

rad :: !Real -> Angle
deg :: !Real -> Angle

toDeg     :: !Angle -> Real
toRad     :: !Angle -> Real
normalize :: !Angle -> Angle

instance == Angle
instance < Angle
instance + Angle
instance - Angle
instance sign Angle
