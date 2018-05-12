implementation module iTasks.Extensions.JSONFile

import StdBool, StdList, StdFile, StdArray, System.FilePath, System.File, Data.Error, Text.GenJSON, StdString
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.TaskStore

:: JSONParseException = CannotParse !String
instance toString JSONParseException
where
	toString (CannotParse msg) = msg

CHUNK_SIZE :== 1048576 // 1M

importJSONFile :: !FilePath -> Task a | iTask a
importJSONFile filename = mkInstantTask eval
where
	eval taskId iworld = readJSON taskId filename fromJSON iworld

importJSONDocument  :: !Document -> Task a | iTask a
importJSONDocument {Document|documentId} = mkInstantTask eval
  where
  eval taskId iworld
    # (filename, iworld) = documentLocation documentId iworld
    # (mbstr, iworld)    = loadDocumentContent documentId iworld
    = case mbstr of
        Just str -> case fromJSON (fromString str) of
                      Just a -> (Ok a, iworld)
                      _      -> (parseException filename, iworld)
        _ -> (openException filename, iworld)

importJSONFileWith :: !(JSONNode -> Maybe a) !FilePath -> Task a | iTask a
importJSONFileWith parsefun filename = mkInstantTask eval
where
	eval taskId iworld = readJSON taskId filename parsefun iworld
	
createJSONFile :: !String a -> Task Document | iTask a
createJSONFile filename content = mkInstantTask eval
where
	eval taskId iworld
		# (mbDoc,iworld)	= createDocument filename "text/json" (toString (toJSON content)) iworld
		= case mbDoc of
			Ok doc	    = (Ok doc, iworld)
			Error e     = (Error (dynamic e,toString e),iworld)

exportJSONFile :: !FilePath a -> Task a | iTask a
exportJSONFile filename content = exportJSONFileWith toJSON filename content

exportJSONFileWith :: !(a -> JSONNode) !FilePath a -> Task a | iTask a
exportJSONFileWith encoder filename content = mkInstantTask eval
where
	eval taskId iworld = fileTask taskId filename content (writeJSON encoder) iworld

fileTask taskId filename content f iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (Ok content, {IWorld|iworld & world = world})

readJSON taskId filename parsefun iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= case (parsefun (fromString content)) of
		Just a
          = (Ok a, {IWorld|iworld & world = world})
		Nothing
          = (parseException filename, {IWorld|iworld & world = world})

readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)

writeAll content file
	= fwrites content file

writeJSON encoder content file
	= fwrites (toString (encoder content)) file

ioException s
	# e = FileException s IOError
	= Error (dynamic e, toString e)
openException s	
	# e = FileException s CannotOpen
	= Error (dynamic e, toString e)
closeException s
	# e = FileException s CannotClose
	= Error (dynamic e, toString e)

parseException s
	# e = CannotParse ("Cannot parse JSON file " +++ s)
	= Error (dynamic e, toString e)

