definition module iTasks.SDS.Sources.Core
/*
* This module provides the builtin shared sources
*/
import iTasks.SDS.Definition
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError, :: MaybeErrorString
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode 

// constant share from which you always read the same value
constShare :: !a -> SDSSource p a ()

// null share to which you can write anything
nullShare :: SDSSource p () a

// Useful placeholder when you need a share don't intent to use it
unitShare :: SimpleSDSSource ()

// Random source
randomInt :: SDSSource () Int ()

// Random string (the parameters determines its length)
randomString :: SDSSource Int String ()

// world function share
worldShare :: (p *World -> *(MaybeErrorString r,*World)) (p w *World -> *(MaybeErrorString (),*World)) -> SDSSource p r w

// Share that maps to the plain contents of a file on disk
// When the file does not exist on reading it returns nothing. By writing Nothing you can remove the file
fileShare :: SDSSource FilePath (Maybe String) (Maybe String)

// Share that maps to a file encoded as JSON 
jsonFileShare :: SDSSource FilePath (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|} a

// memory share (essentially a global variable)
memoryShare :: SDSSource String (Maybe a) (Maybe a) | TC a

// Share that maps to a file that holds a serialized graph representation of the value
graphFileShare :: SDSSource FilePath (Maybe a) (Maybe a)

// Directory
directoryListing :: SDSSource FilePath [String] ()
