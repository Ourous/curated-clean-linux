implementation module iTasks.Extensions.Files

import StdFile, StdArray, StdFunctions

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Data.Error, Text
import Data.Functor
import Data.List
import Data.Tree
import System.Directory
import System.FilePath
import System.File
import System.Time
import qualified Control.Monad as CM
import qualified System.File as SF
import qualified System.Directory as SD

import iTasks

deleteFile :: !FilePath -> Task ()
deleteFile path = accWorldError ('SF'.deleteFile path) snd

moveFile :: !FilePath !FilePath -> Task ()
moveFile srcPath dstPath = accWorldError ('SF'.moveFile srcPath dstPath) snd

copyFile :: !FilePath !FilePath -> Task ()
copyFile srcPath dstPath = accWorldError (copyFile` srcPath dstPath) id

copyFile` :: !FilePath !FilePath !*World -> (MaybeError String (), !*World)
copyFile` srcPath dstPath w
	# (ok, srcFile, w) = fopen srcPath FReadText w
	| not ok = (Error ("cannot open " +++ srcPath), w)
	# (ok, dstFile, w) = fopen dstPath FWriteText w
	| not ok = (Error ("cannot open " +++ dstPath), w)
	# (srcFile, dstFile) = actuallyCopy srcFile dstFile
	# (ok, w) = fclose srcFile w
	| not ok = (Error ("cannot close " +++ srcPath), w)
	# (ok, w) = fclose dstFile w
	| not ok = (Error ("cannot close " +++ dstPath), w)
	= (Ok (), w)
where
	actuallyCopy :: !*File !*File -> (!*File, !*File)
	actuallyCopy src dst
		# (end, src) = fend src
		| end = (src, dst)
		# (s, src) = freads src 65536
		# dst = dst <<< s
		= actuallyCopy src dst

createDirectory :: !FilePath !Bool -> Task ()
createDirectory path False = accWorldError ('SD'.createDirectory path) snd
createDirectory path True = accWorldError (createWithParents path) id
where
	createWithParents path world = create [] (split {pathSeparator} path) world

	create _ [] world = (Ok (),world)
	create [] ["":rest] world = create [""] rest world //Special case for absolute paths
	create base [dir:rest] world
		# next = base ++ [dir]
		# path = join {pathSeparator} next
		# (exists,world) = 'SF'.fileExists path world
		| exists = create next rest world //This part exists, continue
		| otherwise = case 'SD'.createDirectory path world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = create next rest world

deleteDirectory :: !FilePath !Bool -> Task ()
deleteDirectory path False = accWorldError ('SD'.removeDirectory path) snd
deleteDirectory path True = accWorldError (deleteDirectoryRecursive path) id

deleteDirectoryRecursive path world = case 'SD'.readDirectory path world of
	(Error e,world) = (Error (snd e), world)
	(Ok content,world) = case deleteContent content world of
		(Error e,world) = (Error e,world)
		(Ok (),world) = case 'SD'.removeDirectory path world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = (Ok (),world)
where
	deleteContent [] world = (Ok (),world)
	deleteContent [".":rest] world = deleteContent rest world
	deleteContent ["..":rest] world = deleteContent rest world
	deleteContent [entry:rest] world = case getFileInfo (path </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world)
		| directory = case deleteDirectoryRecursive (path </> entry) world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = deleteContent rest world
		| otherwise = case 'SF'.deleteFile (path </> entry) world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = deleteContent rest world

copyDirectory :: !FilePath !FilePath -> Task ()
copyDirectory  srcPath dstPath = accWorldError (copyDirectory` srcPath dstPath) id

copyDirectory` srcPath dstPath world = case readDirectory srcPath world of
		(Error e,world) = (Error (snd e), world)
		(Ok content,world) = case 'SD'.createDirectory dstPath world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = copyContent content world
where
	copyContent [] world  = (Ok (),world)
	copyContent [".":rest] world = copyContent rest world
	copyContent ["..":rest] world = copyContent rest world
	copyContent [entry:rest] world = case getFileInfo (srcPath </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world)
			| directory = case copyDirectory` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error e,world)
				(Ok (),world) = copyContent rest world
			| otherwise = case copyFile` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error (toString e), world)
				(Ok (),world) = copyContent rest world


//Why is this necessary?!?!?!?
derive class iTask RTree, FileInfo, Tm

selectFileTree :: !Bool !Bool !FilePath [FilePath]-> Task [FilePath]
selectFileTree exp multi root initial
	= accWorld (readDirectoryTree root Nothing) @ numberTree
	>>- \tree->editSelection [SelectMultiple multi,selectOption] tree
		[i\\(i, (f, _))<-leafs tree | elem f initial]
where
	selectOption = SelectInTree
		(\tree->[{foldTree (fp2cn exp) tree & label=root}])
		(\tree sel->[f\\(i, (f, _))<-leafs tree | isMember i sel])

selectFileTreeLazy :: !Bool !FilePath -> Task [FilePath]
selectFileTreeLazy multi root = accWorld (readDirectoryTree root (Just 1)) >>- \tree->
	withShared tree \stree->let numberedtree = mapRead numberTree stree in
	withShared [] \ssel->
	editSharedSelectionWithShared [SelectMultiple multi,selOpt] numberedtree ssel
	-|| whileUnchanged (ssel >*< numberedtree) (\(sel, tree)->case sel of
		[i] = case find ((==)i o fst) (leafs tree) of
			Just (i, (fp, Ok {directory=True}))
				= accWorld (readDirectoryTree fp (Just 1))
				@ flip (mergeIn i) tree
				>>- \newtree->set ([], newtree) (ssel >*< stree) @? const NoValue
			_ = unstable ()
		_ = unstable ()
	)
	@ map (fst o snd)
where
	mergeIn j newtree = foldTree \(i, t) cs->if (i == j) newtree (RNode t cs)

	unstable a = treturn a @? \(Value a _)->Value a False

	selOpt :: SelectOption (RTree (Int, (FilePath, MaybeOSError FileInfo))) (Int, (FilePath, MaybeOSError FileInfo))
	selOpt = SelectInTree
		(\tree->[{foldTree (fp2cn True) tree & label=root}])
		(\tree sel->[t\\t=:(i, _)<-leafs tree | isMember i sel])

fp2cn :: Bool (Int, (FilePath, MaybeOSError FileInfo)) [ChoiceNode] -> ChoiceNode
fp2cn exp (i, (fp, mfi)) cs =
	{ id=i
	, label=dropDirectory fp +++ if (isError mfi) (" (" +++ snd (fromError mfi) +++ ")") ""
	, icon=icon mfi
	, expanded=exp
	, children=cs
	}
where
	icon (Ok {directory=True}) = Just "folder"
	icon (Ok _) = Just "document"
	icon _ = Just "document-error"

numberTree :: ((RTree a) -> RTree (Int, a))
numberTree = flip evalState zero o foldTree \a cs->
	(\lvs i->RNode (i, a) lvs) <$> 'CM'.sequence cs <*> getState <* modify inc
