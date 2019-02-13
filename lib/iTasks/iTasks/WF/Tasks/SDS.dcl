definition module iTasks.WF.Tasks.SDS
/**
* This module provides the core tasks for accessing shared data sources.
*/
from iTasks.WF.Definition import :: Task, class iTask
import iTasks.SDS.Definition

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq
from Data.Maybe import :: Maybe
from StdOverloaded import class toString

:: SharedException		= SharedException !String

derive class iTask SharedException
instance toString SharedException

/**
* Reads shared data once.
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*
* @gin-title Read shared
* @gin-icon shared_read
*/
get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w

/**
* Writes shared data.
*
* @param Value: A value to write
* @param Shared: A shared reference
* @return The value written
* @throws SharedException
*
* @gin-title Write shared
* @gin-icon shared_update
*/
set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds

/**
* Updates shared data in one atomic operation.
*
* @param Shared: A shared reference
* @param Update function: A function modifying the shared value
* @return The value written
* @throws SharedException
*
* @gin-title Update shared
* @gin-icon shared_update
*/
upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds

/**
* Reads shared data continously
*
* @param Shared: A shared reference
* @return The value read
* @throws SharedException
*
* @gin-title Read shared
* @gin-icon shared_read
*/
watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds


