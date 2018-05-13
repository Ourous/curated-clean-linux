implementation module Graphics.Scalable.Types

import Data.Maybe
import Data.List
import Data.GenEq
from Text.HTML import :: SVGColor (..)
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import StdBool, StdInt, StdReal, StdString
import Graphics.Scalable.Internal.Types

normalFontDef :: !String !Real -> FontDef // (normalFontDef family size) sets all other fields to "normal"
normalFontDef family size
  = {fontfamily = family, fontysize = size, fontstretch = "normal", fontstyle = "normal", fontvariant = "normal", fontweight = "normal"}

instance == FontDef    where  == fd1 fd2 = fd1 === fd2
instance <  FontDef    where  <  fd1 fd2 =  fd1.fontfamily  < fd2.fontfamily
                                         || fd1.fontysize   < fd2.fontysize
                                         || fd1.fontstretch < fd2.fontstretch
                                         || fd1.fontstyle   < fd2.fontstyle
                                         || fd1.fontvariant < fd2.fontvariant
                                         || fd1.fontweight  < fd2.fontweight
derive gEq FontDef
import Text.GenPrint
derive gPrint FontDef
instance toString FontDef where toString font = printToString font

derive gEq LineMarkerPos
instance == LineMarkerPos  where == lm1 lm2 = lm1 === lm2

instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r, g, b} = SVGRGB r g b

instance zero RGB where zero = { r = 0, g = 0, b = 0 }
