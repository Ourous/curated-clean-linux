definition module iTasks

/**
* Main iTask module exporting all end user iTask modules 
*/
import	
    // iTasks engine
        iTasks.Engine				
    // iTasks API
    ,   iTasks.SDS.Definition
    ,   iTasks.SDS.Sources.Core
    ,   iTasks.SDS.Sources.Store
    ,   iTasks.SDS.Sources.System
    ,   iTasks.SDS.Combinators.Core
    ,   iTasks.SDS.Combinators.Common

    ,   iTasks.WF.Definition
    ,   iTasks.WF.Derives
    ,   iTasks.WF.Tasks.Core
    ,   iTasks.WF.Tasks.SDS
    ,   iTasks.WF.Tasks.IO
    ,   iTasks.WF.Tasks.System
    ,   iTasks.WF.Tasks.Interaction
    ,   iTasks.WF.Combinators.Core
    ,   iTasks.WF.Combinators.SDS
    ,   iTasks.WF.Combinators.Tune
    ,   iTasks.WF.Combinators.Overloaded
    ,   iTasks.WF.Combinators.Common

	//  Custom task GUI's
    ,   iTasks.UI.Editor.Controls
    ,   iTasks.UI.Editor.Containers
    ,   iTasks.UI.Editor.Modifiers

	//	Miscellaneous machinery
	,	Text.GenJSON							// JSON is used for serializing/deserializing strings
	,   iTasks.UI.Prompt 					// Standard for creating prompts
	,   iTasks.UI.Layout.Common 			// Standard layout patterns
	
	//	API extensions for user  & workflow management
	,	iTasks.Extensions.Admin.UserAdmin
	,	iTasks.Extensions.Admin.WorkflowAdmin
	
	//StdEnv modules
	,	StdInt
	,	StdBool
	,	StdString
	,	StdList
	,	StdOrdList
	,	StdTuple
	,	StdEnum
	,	StdOverloaded

//JSON(En|De)code for Dynamic and (->)
from iTasks.Internal.Serialization import generic JSONEncode, generic JSONDecode

from StdFunc import id, const, o
from Data.List import instance Functor []
