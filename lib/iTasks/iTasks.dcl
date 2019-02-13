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
	// Distributed iTasks
	,   iTasks.Internal.Distributed.Domain

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

from iTasks.Internal.SDSService import sdsServiceTask

from iTasks.Internal.SDS import instance Identifiable SDSSource, instance Readable SDSSource,instance Writeable SDSSource,instance Modifiable SDSSource,instance Registrable SDSSource,instance Identifiable SDSLens,instance Readable SDSLens,instance Writeable SDSLens,instance Modifiable SDSLens,instance Registrable SDSLens,instance Identifiable SDSCache,instance Readable SDSCache,instance Writeable SDSCache,instance Modifiable SDSCache,instance Registrable SDSCache,instance Identifiable SDSSequence,instance Readable SDSSequence,instance Writeable SDSSequence,instance Modifiable SDSSequence,instance Registrable SDSSequence,instance Identifiable SDSSelect,instance Readable SDSSelect,instance Writeable SDSSelect,instance Modifiable SDSSelect,instance Registrable SDSSelect,instance Identifiable SDSParallel,instance Readable SDSParallel,instance Writeable SDSParallel,instance Modifiable SDSParallel,instance Registrable SDSParallel,instance Identifiable SDSRemoteService,instance Readable SDSRemoteService,instance Writeable SDSRemoteService,instance Modifiable SDSRemoteService,instance Registrable SDSRemoteService,instance Identifiable SDSRemoteSource,instance Readable SDSRemoteSource,instance Writeable SDSRemoteSource,instance Modifiable SDSRemoteSource,instance Registrable SDSRemoteSource, instance Identifiable SDSDebug, instance Readable SDSDebug, instance Writeable SDSDebug, instance Registrable SDSDebug, instance Modifiable SDSDebug

from StdFunc import id, const, o
from Data.List import instance Functor []
