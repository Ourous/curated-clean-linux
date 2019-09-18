definition module Math.Geometry

/**
 * This module provides geometry related functions.
 *
 * @property-bootstrap
 *	   import StdReal, StdInt
 *	   derive class Gast Angle
 *
 *	   // there is a tradeoff between the largest degree value allowed and the error
 *	   angleInvariant :: Angle -> Property
 *	   angleInvariant angle = prop (abs (toDeg angle) <= 1.0E6)
 *
 *	   (~~) infix 4
 *	   (~~) x y :== approxEqual 1.0E-11 x y
 */

from StdEnv import class ==, class <, class +, class -, class sign, class abs

//* The constant pi.
pi :== 3.14159265359

/**
 * An angle.
 */
:: Angle
  = Deg !Real //* An angle in degree representation
  | Rad !Real //* An angle in radian representation

/**
 * An angle corresponding to the given radian representation.
 *
 * @param the radian representation
 * @result the corresponding angle
 * @property correct angle from radian: A.r :: Real:
 *     not (isNaN r) ==> r =.= toRad (rad r)
 */
rad :: !Real -> Angle

/**
 * An angle corresponding to the given degree representation.
 *
 * @param the degree representation
 * @result the corresponding angle
 * @property correct angle from degrees: A.d :: Real:
 *     not (isNaN d) ==> d =.= toDeg (deg d)
 */
deg :: !Real -> Angle

/**
 * The degree representation of a given angle.
 *
 * @param the angle
 * @result the angle's degree representation
 * @property degrees from angle agree with radians: A.angle :: Angle:
 *     toDeg angle ~~ toDeg (rad (toRad angle))
 * @precondition angleInvariant angle
 */
toDeg :: !Angle -> Real

/**
 * The radian representation of a given angle.
 *
 * @param the angle
 * @result the angle's radian representation
 * @property radians from angle agree with degrees: A.angle :: Angle:
 *     toRad angle ~~ toRad (deg (toDeg angle))
 * @precondition angleInvariant angle
 */
toRad :: !Angle -> Real

/**
 * Normalizes an angle.
 *
 * @param the angle to normalize
 * @result the normalized angle
 * @property normalized degree range: A.angle :: Angle:
 *     0.0 <=. degNorm /\ degNorm <=. 360.0
 *     with
 *         degNorm = toDeg (normalize angle)
 * @property normalized radian range: A.angle :: Angle:
 *     0.0 <=. radNorm /\ radNorm <=. 2.0 * pi
 *     with
 *         radNorm = toRad (normalize angle)
 * @property idempotence: A.angle :: Angle:
 *     normalize angle =.= normalize (normalize angle)
 * @precondition angleInvariant angle
 */
normalize :: !Angle -> Angle

instance == Angle
instance < Angle
instance + Angle
instance - Angle
instance sign Angle
