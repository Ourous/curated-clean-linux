implementation module iTasks.Extensions.FileDialog

import iTasks
import Data.Maybe, Data.Either, Data.Tuple, System.Directory, System.FilePath, System.File, Text, System.OS
import iTasks.SDS.Definition
import iTasks.Internal.SDS
import iTasks.UI.Layout
from iTasks.Internal.IWorld import :: IWorld{world}
from Data.Map import unions

derive class iTask FileInfo, Tm

editFilePath :: String Action (Maybe FilePath) -> Task (Maybe FilePath)
editFilePath title action initialPath 
	=   (determineInitialDir initialPath
	>>- \(initDir,initFile) ->
		withShared (initDir,initFile)
		\sSelection ->
			((navigateUp sSelection <<@ ApplyLayout navigateUpLayout)
			 ||-
			 (chooseFromCurrentDirectory sSelection <<@ ApplyLayout fileListLayout)
			 ||-
			 editFilename sSelection
			) >>* [OnAction ActionCancel (always (return Nothing))
				  ,OnAction action (ifValue (isJust o snd) (\(dir,Just file) -> return (Just (dir </> file))))
				  ]

		) <<@ InWindow <<@ Title title
where
	determineInitialDir :: (Maybe FilePath) -> Task (FilePath, Maybe String)
	determineInitialDir Nothing = accWorldError getCurrentDirectory snd @ (\d -> (d,Nothing))
 	determineInitialDir (Just path)
		= accWorldError (canonicalizePath path) snd
		>>- \fullPath ->
			 accWorldError (getFileInfo fullPath) snd 
		@ \{FileInfo|directory} -> if directory (fullPath,Nothing) (takeDirectory fullPath,Just (dropDirectory fullPath))

	navigateUp :: (Shared (FilePath,Maybe String)) -> Task (FilePath, Maybe String)
	navigateUp sSelection
   		= editSharedChoiceWithShared () [ChooseFromDropdown fst] (ancestorDirectories sSelection) (selection sSelection)

	chooseFromCurrentDirectory :: (Shared (FilePath,Maybe String))-> Task (FilePath, Maybe String)
	chooseFromCurrentDirectory sSelection
		= editSharedChoiceWithShared () [ChooseFromList view] (filesInCurDir sSelection) (selection sSelection)
	where
		view (path,Nothing) = "[DIR] " +++ dropDirectory path
		view (path,Just filename) = "[FILE] " +++ filename

	selection sds = mapReadWrite (Just, const) sds
 
	editFilename :: (Shared (FilePath, Maybe String))  -> Task (FilePath,Maybe String)
	editFilename sSelection = updateSharedInformation () [UpdateAs snd (\(d,_) f -> (d,f))] sSelection

	fileListLayout = setUIAttributes (unions [sizeAttr FlexSize (ExactSize 200),minWidthAttr (ExactBound 400)])
	navigateUpLayout = layoutSubUIs SelectChildren (setUIAttributes (widthAttr FlexSize))

ancestorDirectories :: (SDS () (FilePath,Maybe String) (FilePath,Maybe String)) 
	-> SDS () [(FilePath,Maybe String)] (FilePath,Maybe String)
ancestorDirectories	sds = mapRead (ancestors o fst) sds
where
	ancestors "" = [("/",Nothing)]
	ancestors path = [(path,Nothing):ancestors (takeDirectory path)]

filesInCurDir :: (SDS () (FilePath,Maybe String) (FilePath,Maybe String)) -> SDS () [(FilePath,Maybe String)] ()
filesInCurDir selection 
	= sdsSequence "filesIn" id (\() (p,_) -> p) (\() _ = Right snd)
		(SDSWriteConst (\_ _ -> Ok Nothing)) (SDSWriteConst (\_ _ -> Ok Nothing)) selection directoryListingWithDir

//Files are listed as (<current dir>,Just <file name>)
//Directories are listed as (<child dir>, Nothing)

directoryListingWithDir :: SDS FilePath [(FilePath,Maybe String)] () 
directoryListingWithDir = mapRead (map combine) directoryListingWithInfo
where
	combine (parentDir,filename,{FileInfo|directory})
		| directory = (parentDir </> filename, Nothing)
		| otherwise = (parentDir, Just filename)

//UTIL
//Entries are: (Dir, Filename, Fileinfo)
directoryListingWithInfo :: SDS FilePath [(FilePath,String,FileInfo)] ()
directoryListingWithInfo = createReadOnlySDSError read
where
	read path iworld = case readDirectory path iworld of
		(Error (_,e),iworld) = (Error (exception (e +++ " : " +++ path)),iworld)
		(Ok files,iworld) = readInfo [(path,f) \\ f <- files | f <> "." && f <> ".."] iworld

	readInfo [] iworld = (Ok [],iworld)
	readInfo [(d,f):fs] iworld=:{IWorld|world} = case getFileInfo (d</>f) world of
		(Error (_,e),world) = (Error (exception (e +++ " : " +++ f)),{IWorld|iworld & world=world})
		(Ok i,world) = case readInfo fs {IWorld|iworld & world=world} of
			(Error e, iworld) = (Error e,iworld)
			(Ok fis, iworld) = (Ok [(d,f,i):fis],iworld)

isAbsolute :: FilePath -> Bool
isAbsolute path = IF_WINDOWS (indexOf ":\\" path == 1) (startsWith "/" path)

canonicalizePath :: FilePath *World -> *(!MaybeOSError FilePath,!*World) 
canonicalizePath path world 
	| isAbsolute path = (Ok path,world)
	| otherwise = case getCurrentDirectory world of
		(Ok curDir,world) = (Ok (curDir </> path),world)
		(Error e,world) = (Error e, world)

