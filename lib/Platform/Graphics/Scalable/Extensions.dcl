definition module Graphics.Scalable.Extensions

/**	This module extends Graphics.Scalable.Image
*/
import Graphics.Scalable.Image

/**	circular r a imgs = image:
	displays @imgs along an arc of radius @r, starting at angle @a.
*/
circular :: !Span !Real ![Image m] -> Image m
