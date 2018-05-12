implementation module iTasks.Extensions.CSVFile

import StdBool, StdList, System.FilePath, Text, Text.CSV, System.File, Data.Error 
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.TaskStore

importCSVFile :: !FilePath -> Task [[String]]
importCSVFile filename = mkInstantTask eval
where
	eval taskId iworld = fileTaskRead taskId filename readCSVFile iworld

importCSVDocument :: !Document -> Task [[String]]
importCSVDocument {Document|documentId} = mkInstantTask eval
where
	eval taskId iworld
		# (filename,iworld) = documentLocation documentId iworld
		= fileTaskRead taskId filename readCSVFile iworld

importCSVFileWith :: !Char !Char !Char !FilePath -> Task [[String]]
importCSVFileWith delimitChar quoteChar escapeChar filename = mkInstantTask eval
where
	eval taskId iworld = fileTaskRead taskId filename (readCSVFileWith delimitChar quoteChar escapeChar) iworld

importCSVDocumentWith :: !Char !Char !Char !Document -> Task [[String]]
importCSVDocumentWith delimitChar quoteChar escapeChar {Document|documentId} = mkInstantTask eval
where
	eval taskId iworld
		# (filename,iworld) = documentLocation documentId iworld
		= fileTaskRead taskId filename (readCSVFileWith delimitChar quoteChar escapeChar) iworld


createCSVFile :: !String ![[String]] -> Task Document
createCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
        # csv = join "\n" (map (join ",") content)
		# (mbDoc,iworld)	= createDocument filename "text/csv" csv iworld
		= case mbDoc of
			Ok doc	= (Ok doc, iworld)
			Error e	= (Error (dynamic e,toString e),iworld)
			
exportCSVFile :: !FilePath ![[String]] -> Task [[String]]
exportCSVFile filename content = mkInstantTask eval
where
	eval taskId iworld = fileTaskWrite taskId filename content writeCSVFile iworld

exportCSVFileWith :: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
exportCSVFileWith delimitChar quoteChar escapeChar filename content = mkInstantTask eval
where
	eval taskId iworld = fileTaskWrite taskId filename content (writeCSVFileWith delimitChar quoteChar escapeChar) iworld

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
