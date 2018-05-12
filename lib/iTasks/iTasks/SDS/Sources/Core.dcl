definition module iTasks.SDS.Sources.Core
/*
* This module provides the builtin shared sources
*/
from iTasks.SDS.Definition import :: SDS
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError, :: MaybeErrorString
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode 

// constant share from which you always read the same value
constShare :: !a -> SDS p a ()

// null share to which you can write anything
nullShare :: SDS p () a

// Useful placeholder when you need a share don't intent to use it
unitShare :: SDS () () ()

// Random source
randomInt :: SDS () Int ()

// world function share
worldShare :: (p *World -> *(MaybeErrorString r,*World)) (p w *World -> *(MaybeErrorString (),*World)) -> SDS p r w 

// Share that maps to the plain contents of a file on disk
// When the file does not exist on reading it returns nothing. By writing Nothing you can remove the file
fileShare :: SDS FilePath (Maybe String) (Maybe String)

// Share that maps to a file encoded as JSON 
jsonFileShare :: SDS FilePath (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|} a

// memory share (essentially a global variable)
memoryShare :: SDS String (Maybe a) (Maybe a) | TC a

// Share that maps to a file that holds a serialized graph representation of the value
graphFileShare :: SDS FilePath (Maybe a) (Maybe a)

// Directory
directoryListing :: SDS FilePath [String] ()
