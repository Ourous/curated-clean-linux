definition module Graphics.Scalable.Image

/**
 * A proposal for a compositional image library for defining SVG-images.
 *	Peter Achten
 *	Jurrien Stutterheim
 *	Rinus Plasmeijer
 */

import Math.Geometry
import Graphics.Scalable.Types
from Graphics.Scalable.Internal.Image` import :: Image`

:: Image m :== Image` m

/**
 * empty w h = image:
 *		`image` has no visible content, and a span box that is (`maxSpan [zero,w]`) wide and (`maxSpan [zero,h]`) high.
 */
empty      :: !Span !Span -> Image m

/**
 * text font txt = image:
 *		`image` renders `txt`, using `font` in which `font.fontysize` is set to be at least `zero`.
 *		The span box of `image` is as wide as the rendered text and as high as the corrected `font.fontysize`.
 *		The client selects a font that matches `font` best.
 */
text       :: !FontDef !String -> Image m

/**
 * circle d = image:
 *		`image` renders a circle of diameter (`maxSpan [zero,d]`), hence the span box of `image` is (`maxSpan [zero,d]`) wide and high.
 */
circle     :: !Span -> Image m

/**
 * ellipse w h = image:
 *		`image` renders an ellipse that is (`maxSpan [zero,w]`) wide and (`maxSpan [zero,h]`) high, which is also the span box of `image`.
 */
ellipse    :: !Span !Span -> Image m

/**
 * square edge = image:
 *		`image` renders a square with edges of size (`maxSpan [zero,edge]`), which is also the span box of `image`.
 */
square     :: !Span -> Image m

/**
 * rect w h = image:
 *		`image` renders a rectangle that is (`maxSpan [zero,w]`) wide and (`maxSpan [zero,h]`) high, which is also the span box of `image`.
 */
rect       :: !Span !Span -> Image m

/**
 * xline w = image:
 *		`image` renders a horizontal line that is (`abs w`) wide and has `zero` height.
 *		If `w` is negative, then the start/end of the line is at the right/left, and it is at the left/right otherwise.
 *		This is relevant when using the `LineStartMarker`, `LineMidMarker`, `LineEndMarker` attributes.
 */
xline      :: !Span -> Image m

/**
 * yline h = image:
 *		`image` renders a vertical line that has `zero` width and is (`abs h`) high.
 *		If `h` is negative, then the start/end of the line is at the bottom/top, and it is at the top/bottom otherwise.
 *		This is relevant when using the `LineStartMarker`, `LineMidMarker`, `LineEndMarker` attributes.
 */
yline      :: !Span -> Image m

/**
 * line w h = image:
 *		`image` renders a line that connects the opposite corners of a span box that is (`abs w`) wide and (`abs h`) high.
 *		If `w` is negative, then the start/end of the line is at the right/left, and it is at the left/right otherwise.
 *		If `h` is negative, then the start/end of the line is at the bottom/top, and it is at the top/bottom otherwise.
 *		This is relevant when using the `LineStartMarker`, `LineMidMarker`, `LineEndMarker` attributes.
 */
line       :: !Span !Span -> Image m

/**
 * polyline []:
 *		yields a run-time error because the list must contain at least 2 elements.
 * polyline [_]:
 *		yields a run-time error because the list must contain at least 2 elements.
 * polyline offsets = image:
 *		`image` renders the sequence of lines that connect the points in `offsets`: 
 *			`offsets!!0` with `offsets!!1`, `offsets!!1` with `offsets!!2`, ...
 *		The span box of `image` is as wide as the bounding box of `offsets`.
 *		When using the `LineStartMarker`, `LineMidMarker`, `LineEndMarker` attributes, these points determine the direction of the lines.
 */
polyline   :: ![ImageOffset] -> Image m

/**
 * polygon []:
 *		yields a run-time error because the list must contain at least 3 elements.
 * polygon [_]:
 *		yields a run-time error because the list must contain at least 3 elements.
 * polygon [_,_]:
 *		yields a run-time error because the list must contain at least 3 elements.
 * polygon offsets = image:
 *		`image` renders the sequence of lines that connect the points in `offsets` (with n = `length offsets`): 
 *			`offsets!!0` with `offsets!!1`, `offsets!!1` with `offsets!!2`, ..., `offsets!!(n-1)` with `offsets!!0`
 *		The span box of `image` is as wide as the bounding box of `offsets`.
 *		When using the `LineStartMarker`, `LineMidMarker`, `LineEndMarker` attributes, these points determine the direction of the lines.
 */
polygon    :: ![ImageOffset] -> Image m

/**
 * rotate angle image = image`:
 *		`image`` renders `image` after rotating it over `angle`. 
 *		The span box of `image`` is equal to the span box of `image`. 
 */
rotate     :: !Angle !(Image m) -> Image m

/**
 * flipx image = image`:
 *		`image`` renders `image` after flipping all x-coordinates.
 *		The span box of `image`` is equal to the span box of `image`.
 */
flipx      :: !(Image m) -> Image m

/**
 * flipy image = image`:
 *		`image`` renders `image` after flipping all y-coordinates.
 *		The span box of `image`` is equal to the span box of `image`.
 */
flipy      :: !(Image m) -> Image m

/**
 * fit w h image = image`:
 *		`image`` renders `image` after making sure that the span box of `image` is resized to have width 
 *		(`maxSpan [zero,w]`) and height (`maxSpan [zero,h]`).
 *		The span box of `image`` has width (`maxSpan [zero,w]`) and height (`maxSpan [zero,h]`).
 */
fit        :: !Span !Span !(Image m) -> Image m

/**
 * fitx w image = image`:
 *		`image`` renders `image` after making sure that the span box of `image` is resized to have width
 *		(`maxSpan [zero,w]`). The height is adjusted proportionally. These are also the sizes of the span
 *		box of `image``.
 */
fitx       :: !Span !(Image m) -> Image m

/**
 * fity h image = image`:
 *		`image`` renders `image` after making sure that the span box of `image` is resized to have height
 *		(`maxSpan [zero,h]`). The width is adjusted proportionally. These are also the sizes of the span
 *		box of `image``.
 */
fity       :: !Span !(Image m) -> Image m

/**
 * scale fx fy image = image`:
 *		`image`` renders `image` after scaling the width of `image` with factor (axSpan [zero,fx]`) and the
 *		height of `image` with factor (`maxSpan [zero,fy]`). These are also the sizes of the span box of `image``.
 */
scale      :: !Real !Real !(Image m) -> Image m

/**
 * scalex fx image = image`:
 *		`image`` renders `image` after scaling the width of `image` with factor (`maxSpan [zero,fx]`). 
 *		This is also the width of the span box of `image``. The height of the span box of `image`` is the same
 *		as the height of the span box of `image`.
 */
scalex     :: !Real !(Image m) -> Image m

/**
 * scaley fy image = image`:
 *		`image`` renders `image` after scaling the height of `image` with factor (`maxSpan [zero,fy]`). 
 *		This is also the height of the span box of `image``. The width of the span box of `image`` is the same
 *		as the width of the span box of `image`.
 */
scaley     :: !Real !(Image m) -> Image m

/**
 * skewx angle image = image`:
 *		`image`` renders `image` after skewing it along the x-axis over `angle`. This does not alter the
 *		y-coordinates of the elements of `image`.
 *		The span box of `image`` is the same as the span box of `image`.
 */
skewx      :: !Angle !(Image m) -> Image m

/**
 * skewy angle image = image`:
 *		`image`` renders `image` after skewing it along the y-axis over `angle`. This does not alter the
 *		x-coordinates of the elements of `image`.
 *		The span box of `image`` is the same as the span box of `image`.
 */
skewy      :: !Angle !(Image m) -> Image m

/**
 * overlay aligns offsets images NoHost = image`:
 *		`image`` stacks `images`, with the last element of `images` closest to the viewer.
 *		The span box of `image`` is the bounding box of the span boxes of `images`.
 *		The i-th alignment in `aligns` is applied to the i-th image in `images`. Alignments are relative to the span 
 *		box of `image``. The default alignment is `(AtLeft,AtTop)`.
 *		The i-th offset in `offsets` is applied to the i-th image in `images` and do not affect the span box of `image``.
 * overlay aligns offsets images (Host image) = image`:
 *		`image`` stacks `images`, with the last element of `images` closest to the viewer and `image` the furthest away.
 *		The span box of `image`` is the span box of `image`.
 *		The i-th alignment in `aligns` is applied to the i-th image in `images`. Alignments are relative to the span 
 *		box of `image``. The default alignment is `(AtLeft,AtTop)`.
 *		The i-th offset in `offsets` is applied to the i-th image in `images` and do not affect the span box of `image`.
 */
overlay    :: ![XYAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m

/**
 * collage offsets images host = image`:
 *		`image`` is the overlay of `images` with the given `offsets` and `host` and default alignment `(AtLeft,AtTop)`.
 */
collage    :: ![ImageOffset] ![Image m] !(Host m) -> Image m

/**
 * beside aligns widths height offsets images host = image`:
 *		`image`` is the single row of `images` with the given `aligns` and constraints of column widths (`widths`) and
 *		height (`height`) and `host`.
 */
beside     :: ![YAlign] ![Span] !(Maybe Span) ![ImageOffset] ![Image m] !(Host m) -> Image m

/**
 * above aligns heights width offsets images host = image`:
 *		`image`` is the single column of `images` with the given `aligns` and constraints of row heights (`heights`) and
 *		width (`width`) and `host`.
 */
above      :: ![XAlign] ![Span] !(Maybe Span) ![ImageOffset] ![Image m] !(Host m) -> Image m

/**
 * grid dimension layout aligns widths heights offsets images host = image`:
 *		`image`` stacks `images`, with the last element of `images` closest to the viewer. 
 *		In addition, it places the `images` in a grid structure: 
 *			if `dimension` is (`Rows`    n), then the `images` are grouped in n rows; 
 *			if `dimension` is (`Columns` n), then the `images` are grouped in n columns.
 *		Any remaining image is just `(empty zero zero)`.
 *		The order of placing in the grid structure is determined by `layout` = (major,x,y):
 *			if major is `ColumnMajor`, then the images are placed column by column, and row by row in case of `RowMajor`;
 *			if x     is `LeftToRight`, then the images are placed left to right, and right to left in case of `RightToLeft`;
 *			if y     is `TopToBottom`, then the images are placed top to bottom, and bottom to top in case of `BottomToTop`.
 *		The i-th alignment in `aligns` is applied to the i-th image in `images`. Alignments are relative to the column width
 *		and row height of the grid location the image is placed in. The default alignment is `(AtLeft,AtTop)`.
 *		The column widths can be constrained via `widths` and the row heights via `heights`. The default column width is
 *		determined by the widest image in that column, and the default height is determined by the highest image in that row.
 *		The i-th offset in `offsets` is applied to the i-th image in `images` and do not affect the span box of `image``.
 *		If `host` is `NoHost`, then the span box of @image` is the bounding box of the entire grid.
 *		If `host` is (`Host` image), then the span box of `image`` is the span box of image.
 */
grid       :: !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image m] !(Host m) -> Image m

:: Host  m = NoHost | Host (Image m)

class tuneImage attr :: !(Image m) !(attr m) -> Image m

(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr
tuneIf         :: !Bool !(Image m) !(attr m) -> Image m | tuneImage attr

:: NoAttr          m = NoAttr
:: DashAttr        m = { dash        :: ![Int]    }
:: FillAttr        m = { fill        :: !SVGColor }
:: LineEndMarker   m = { endmarker   :: !Image m  }
:: LineMidMarker   m = { midmarker   :: !Image m  }
:: LineStartMarker m = { startmarker :: !Image m  }
:: MaskAttr        m = { mask        :: !Image m  }
:: OpacityAttr     m = { opacity     :: !Real     }
:: StrokeAttr      m = { stroke      :: !SVGColor }
:: StrokeWidthAttr m = { strokewidth :: !Span     }
:: XRadiusAttr     m = { xradius     :: !Span     }
:: YRadiusAttr     m = { yradius     :: !Span     }

instance tuneImage NoAttr, DashAttr, FillAttr, LineEndMarker, LineMidMarker, LineStartMarker, MaskAttr, OpacityAttr, 
                   StrokeAttr, StrokeWidthAttr, XRadiusAttr, YRadiusAttr, 
                   OnClickAttr, OnNClickAttr, OnMouseDownAttr, OnMouseUpAttr, OnMouseOverAttr, OnMouseMoveAttr, OnMouseOutAttr, DraggableAttr

class margin a where margin :: !a !(Image m) -> Image m

/**
 * margin a image = image`:
 *		adds margin `a` to all directions (north, east, south, west) of `image`.
 */
instance margin Span

/**
 * margin (a,b) image = image`:
 *		adds margin `a` to north and south, and adds margin `b` to east and west of `image`.
 */
instance margin (!Span, !Span)

/**
 * margin (a,b,c) image = image`:
 *		adds margin `a` to north, `c` to south, and `b` to east and west of `image`.
 */
instance margin (!Span, !Span, !Span)

/**
 * margin (a,b,c,d) image = image`:
 *		adds margin `a` to north, `b` to east, `c` to south, and `d` to west of `image`.
 */
instance margin (!Span, !Span, !Span, !Span)

tag        :: !*ImageTag !(Image m) -> Image m
tagWithSrc :: !*TagSource !(Image m) -> *(!(!Image m, !ImageTag), !*TagSource)
