implementation module iTasks.Extensions.Development.CleanCode
import iTasks

viewCleanModule :: FilePath String -> Task ()
viewCleanModule baseDir moduleName
    = viewInformation "TODO" [] (baseDir,moduleName) @! ()
    
import System.Directory, System.File


/**
* Given a list of absolute path names, offers a tree structure choice to select a file 
* @param  absolute paths of directories to search through
* @param  only show files with given extensions, all files are shown if this list is empty
* @return selected file and the absolute path directory name it is found 
*/

chooseFile :: [FilePath] [FileExtension] -> Task (FilePath,FilePath)
chooseFile paths extensions
	=					accWorld (getFilesInDir paths extensions) 
		>>- \tree ->	enterChoice [Att (Title "Select File"), Att IconEdit] [ChooseWith (ChooseFromTree (\list _ -> toChoiceTree list))] (treeToList tree [])
		@? adjust
where
	toChoiceTree :: [(Int,(FilePath,[FilePath],FilePath))] -> [ChoiceTree FilePath]
	toChoiceTree []  							= []
	toChoiceTree [(i,(path,[],fileName)):next]
	= [{label = fileName, icon = Nothing, value = ChoiceNode i, type = LeafNode}:toChoiceTree next]
	toChoiceTree [(i,(path,[dir:dirs],fileName)):next]
	= [{label = dir, icon = Nothing, value = GroupNode dir, type = CollapsedNode (toChoiceTree inDir`)}:toChoiceTree outDir]
	where
		(inDir,outDir) = span (\(_,(_,dirs,_)) -> if (not (isEmpty dirs)) (hd dirs == dir) False) next
		inDir` = [(i,(path,dirs,fileName)):[(j,(path,tl dirs,name)) \\ (j,(path,dirs,name)) <- inDir]]		

	adjust (Value (path,dirs,fileName) stab)
	| fileName == "" = NoValue
	= Value (foldl (</>) path dirs,fileName) stab
	adjust NoValue   = NoValue

	treeToList :: [(FilePath,[TreeNode FilePath])] [FilePath] -> [(FilePath,[FilePath],FilePath)]
	treeToList [] 								      dirs = []
	treeToList [(path,[Leaf file:files]):tree] 	 	  dirs = [(path,dirs,file): treeToList [(path,files)] dirs] ++ treeToList tree []
	treeToList [(path,[Node dir childs :files]):tree] dirs = treeToList [(path,childs)] (dirs++[dir]) ++ treeToList [(path,files)] dirs ++ treeToList tree []
	treeToList [_:tree] 				      		  dirs = treeToList tree []

getFilesInDir :: [FilePath] [FileExtension] !*World -> ([(FilePath,[TreeNode FilePath])],*World)
getFilesInDir [] extensions w = ([],w)
getFilesInDir [path:paths] extensions w
# (treeFiles,w)	= getTree (takeDirectory path) [dropDirectory path] w
# (ntrees,w)	= getFilesInDir paths extensions w
= ([(takeDirectory path,treeFiles):ntrees],w)
where
    getTree absolutePath [] w   = ([],w)
    getTree absolutePath [fileName:fileNames] w
    # absoluteFileName          = absolutePath </> fileName
    # (mbInfo,w)                = getFileInfo absoluteFileName w
    | isError mbInfo            = getTree absolutePath fileNames w
    | (fromOk mbInfo).directory // file is directory name
        # (filesInDir,w)        = getFilesInPath absoluteFileName w
        # (dirNodes,w)          = getTree absoluteFileName filesInDir w
        # (filesNodes,w)		= getTree absolutePath fileNames w
        = case dirNodes of
            [] -> (filesNodes,w)
            _  -> ([Node fileName dirNodes:filesNodes],w)
    | isEmpty extensions ||    isMember (snd (splitExtension fileName)) extensions
        # (treeNodes,w)         = getTree absolutePath fileNames w
        = ([Leaf fileName:treeNodes],w)    
    = getTree absolutePath fileNames w    
        
getFilesInPath :: !FilePath !*World -> ([FilePath],!*World)
getFilesInPath path w
# (mbFiles,w)        = readDirectory path w
| isError mbFiles    = ([],w)
= ([name \\ name <- fromOk mbFiles | name <> "." && name <> ".."],w)
 
readDir :: !FilePath !*World -> ([FilePath],!*World)
readDir path w
# (mbInfo,w)                 = getFileInfo path w
| isError mbInfo             = ([],w)
| (fromOk mbInfo).directory = getFilesInPath path w
