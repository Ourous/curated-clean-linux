implementation module Graphics.Scalable.Types

import Data.Maybe
import Data.List
import Data.GenEq
from Text.HTML import :: SVGColor (..)
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import Text.GenPrint
import StdBool, StdInt, StdReal, StdString
import Graphics.Scalable.Internal.Types

:: FontDef :== FontDef`

normalFontDef :: !String !Real -> FontDef // (normalFontDef family size) sets all other fields to "normal"
normalFontDef family size
  = { fontfamily`  = family
    , fontysize`   = to2dec (max zero size)  // make sure the size can be converted correctly to the client and vice versa
    , fontstretch` = "normal"
    , fontstyle`   = "normal"
    , fontvariant` = "normal"
    , fontweight`  = "normal"
    }

setfontfamily :: !String !FontDef -> FontDef
setfontfamily family fontdef = setfontfamily` family fontdef

setfontysize :: !Real !FontDef -> FontDef
setfontysize ysize fontdef = setfontysize` ysize fontdef

setfontstretch :: !String !FontDef -> FontDef
setfontstretch stretch fontdef = setfontstretch` stretch fontdef

setfontstyle :: !String !FontDef -> FontDef
setfontstyle style fontdef = setfontstyle` style fontdef

setfontvariant :: !String !FontDef -> FontDef
setfontvariant variant fontdef = setfontvariant` variant fontdef

setfontweight :: !String !FontDef -> FontDef
setfontweight weight fontdef = setfontweight` weight fontdef

getfontfamily :: !FontDef -> String
getfontfamily fontdef = getfontfamily` fontdef

getfontysize :: !FontDef -> Real
getfontysize fontdef = getfontysize` fontdef

getfontstretch :: !FontDef -> String
getfontstretch fontdef = getfontstretch` fontdef

getfontstyle :: !FontDef -> String
getfontstyle fontdef = getfontstyle` fontdef

getfontvariant :: !FontDef -> String
getfontvariant fontdef = getfontvariant` fontdef

getfontweight :: !FontDef -> String
getfontweight fontdef = getfontweight` fontdef

instance == FontDef    where  == fd1 fd2 = fd1 === fd2
instance <  FontDef    where  <  fd1 fd2 =  fd1.fontfamily`  < fd2.fontfamily`
                                         || fd1.fontysize`   < fd2.fontysize`
                                         || fd1.fontstretch` < fd2.fontstretch`
                                         || fd1.fontstyle`   < fd2.fontstyle`
                                         || fd1.fontvariant` < fd2.fontvariant`
                                         || fd1.fontweight`  < fd2.fontweight`
instance toString FontDef where toString font = printToString font

derive gEq LineMarkerPos
instance == LineMarkerPos  where == lm1 lm2 = lm1 === lm2

instance toSVGColor String where toSVGColor name = SVGColorText name
instance toSVGColor RGB    where toSVGColor {RGB | r, g, b} = SVGRGB r g b

instance zero RGB where zero = { r = 0, g = 0, b = 0 }
