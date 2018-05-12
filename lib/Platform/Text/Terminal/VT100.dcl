definition module Text.Terminal.VT100

from StdOverloaded import class zero
from Text.HTML import :: HtmlTag
from Data.Map import :: Map

:: VT100Settings =
	{ cols    :: Int
	, rows    :: Int
	, tabstop :: Int
	, cssmap  :: Map Int (String, String)
	}

instance zero VT100Settings

/*
 * @param Terminal settings
 * @return Render as HTML
 */
vt100render :: VT100Settings -> (String -> HtmlTag)
