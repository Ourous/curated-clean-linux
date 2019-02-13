implementation module Graphics.Scalable.Extensions

import Graphics.Scalable.Image
import StdEnum, StdList

/**	circular r a imgs = image:
	displays @imgs along an arc of radius @r, starting at angle @a.
*/
circular :: !Span !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
        [(~r *. cos angle,~r *. sin angle) \\ i <- [0.0, sign_a ..], angle <- [i*alpha - 0.5*pi]]
        [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
        (Host (empty (r *. 2) (r *. 2)))              // BUG: using NoHost creates incorrect image (offset to left)
