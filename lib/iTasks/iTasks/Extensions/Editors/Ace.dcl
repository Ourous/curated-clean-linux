definition module iTasks.Extensions.Editors.Ace

/**
 * Integration of Cloud9 Ace code editor
 */

import iTasks
import iTasks.UI.Editor
import Data.Maybe

/**
 * A drop-in replacement for textArea using Ace.
 */
aceTextArea :: Editor String

/**
 * An Ace editor with more fine-grained control
 */
:: AceState =
	{ value     :: !String         //* The string in the editor
	, cursor    :: !(!Int,!Int)    //* The location of the cursor (<row>,<column>)
	, selection :: !Maybe AceRange //* A text selection is delimited by this position and the cursor position
	, disabled  :: !Bool           //* Disallow editing
	}
:: AceRange =
	{ start :: !(!Int,!Int)
	, end   :: !(!Int,!Int)
	}
:: AceOptions =
	{ theme :: !String //* The Ace theme to use
	, mode  :: !String //* The Ace highlight mode to use
	}

derive class iTask AceState, AceRange

derive gEditor AceOptions
derive gEq AceOptions
derive gText AceOptions
derive gDefault AceOptions
derive JSONEncode AceOptions
derive JSONDecode AceOptions

aceEditor :: Editor (!AceOptions,!AceState)
