implementation module iTasks.Extensions.Document

import iTasks.WF.Definition
import iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers

import iTasks.Internal.Task, iTasks.Internal.IWorld, iTasks.Internal.TaskStore
import StdBool, StdString, StdFile, StdArray, StdInt

import Text.GenJSON, Text.Encodings.MIME, Text.HTML, System.FilePath, System.File, System.OSError, Data.Error
import qualified Data.Map as DM
from StdFunc import const

CHUNK_SIZE :== 1048576 // 1M

//* Documents
gText{|Document|} _ (Just val)
	| val.Document.size == 0			= ["No Document"]
	| otherwise							= [val.Document.name]
gText{|Document|} _ Nothing             = [""]

gEditor {|Document|} = selectByMode viewDocument editDocument editDocument
where
	viewDocument = comapEditorValue toView htmlView 
	where
		toView {Document|contentUrl,name} = ATag [HrefAttr contentUrl, TargetAttr "_blank"] [Text name]

	editDocument = bijectEditorValue toView fromView documentField
	where
		toView {Document|documentId,contentUrl,name,mime,size} = (documentId,contentUrl,name,mime,size)
		fromView (documentId,contentUrl,name,mime,size) = {Document|documentId=documentId,contentUrl=contentUrl,name=name,mime=mime,size=size}

derive JSONEncode		Document
derive JSONDecode		Document
derive gDefault			Document
derive gEq				Document

instance toString Document
where
	toString doc = ""
	
instance == Document
where
	(==) doc0 doc1 = doc0.documentId == doc1.documentId

instance toString FileException
where
	toString (FileException path error) = case error of
		CannotOpen	= "Cannot open file '" +++ path +++ "'"
		CannotClose	= "Cannot close file '" +++ path +++ "'"
		IOError		= "Error reading/writing file '" +++ path +++ "'"

derive class iTask	FileException, FileError

importDocument :: !FilePath -> Task Document
importDocument filename = mkInstantTask eval
where
	eval taskId iworld =  readDocument taskId filename iworld
	
	readDocument taskId filename iworld=:{IWorld|current={taskTime},world}
	# (ok,file,world)	= fopen filename FReadData world
	| not ok			= (openException filename,{IWorld|iworld & world = world})
	# (content,file)	= readAll file
	# (ok,world)		= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})
	# name					= dropDirectory filename
	# mime					= extensionToMimeType (takeExtension name)
	# (mbDocument,iworld)	= createDocument name mime content {IWorld|iworld & world = world}
	= case mbDocument of
		(Ok document) 	= (Ok document, iworld)
		(Error e)		= (Error (dynamic e,toString e),iworld)

	readAll file
	# (chunk,file) = freads file CHUNK_SIZE
	| size chunk < CHUNK_SIZE
		= (chunk,file)
	| otherwise
		# (rest,file) = readAll file
		= (chunk +++ rest,file)


exportDocument :: !FilePath !Document -> Task Document
exportDocument filename document = mkInstantTask eval
where
	eval taskId iworld = writeDocument taskId filename document iworld

writeDocument taskId filename document iworld
	# (mbContent,iworld=:{IWorld|current={taskTime},world})
							= loadDocumentContent document.Document.documentId iworld
	| isNothing mbContent	= (ioException filename, {IWorld|iworld & world = world})
	# (ok,file,world)		= fopen filename FWriteData world
	| not ok				= (openException filename,{IWorld|iworld & world = world})
	# file					= fwrites (fromJust mbContent) file
	# (ok,world)			= fclose file world
	| not ok				= (closeException filename,{IWorld|iworld & world = world})	
	= (Ok document, {IWorld|iworld & world = world})

ioException s
	# e = FileException s IOError
	= Error (dynamic e, toString e)
openException s	
	# e = FileException s CannotOpen
	= Error (dynamic e, toString e)
closeException s
	# e = FileException s CannotClose
	= Error (dynamic e, toString e)
