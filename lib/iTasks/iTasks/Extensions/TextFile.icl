implementation module iTasks.Extensions.TextFile

import StdBool, StdList, StdFile, StdArray, System.FilePath, Text, System.File, Data.Error, StdString
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.TaskStore

CHUNK_SIZE :== 1048576 // 1M

importTextFile :: !FilePath -> Task String
importTextFile filename = mkInstantTask eval
where
	eval taskId iworld = fileTaskRead taskId filename readAll iworld
	
exportTextFile :: !FilePath !String -> Task String
exportTextFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTaskWrite taskId filename content writeAll iworld

fileTaskWrite taskId filename content f iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FWriteData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# file				= f content file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (Ok content, {IWorld|iworld & world = world})

fileTaskRead taskId filename f iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (res,file)		= f file
	# (ok,world)		= fclose file world
	| not ok			= (closeException filename,{IWorld|iworld & world = world})
	= (Ok res, {IWorld|iworld & world = world})
	
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
