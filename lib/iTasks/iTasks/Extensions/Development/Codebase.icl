implementation module iTasks.Extensions.Development.Codebase
import iTasks
import iTasks.UI.Editor.Controls
import StdArray, System.FilePath, System.File, System.Directory, Text, StdFile, Data.List, Data.Tree, Data.Error, Data.Maybe

derive class iTask SourceTree, SourceTreeSelection, ModuleType, Extension
instance == Extension where (==) x y = x === y

instance toString Extension
where
	toString Dcl = ".dcl"
	toString Icl = ".icl"

moduleList :: SDSSource FilePath [(ModuleName,ModuleType)] ()
moduleList = worldShare read write
where
	read path world = case scanPaths [path] world of
		(Ok paths,world) = (Ok (determineModules path paths), world)
		(Error e,world) = (Error (snd e), world)

	write path () world = (Ok (),world)

	scanPaths [] world = (Ok [],world)
	scanPaths [p:ps] world = case getFileInfo p world of
		(Error e,world) = (Error e,world)
		(Ok info,world)
			| not info.directory
	 			= case scanPaths ps world of
					(Error e,world) = (Error e,world)
					(Ok filesps,world)
						| include p = (Ok ([p:filesps]),world)
                                    = (Ok filesps,world)
				= case readDirectory p world of
					(Error e,world) = (Error e,world)
					(Ok files,world)
						= case scanPaths [p </> name \\ name <- files | not (exclude name)] world of
							(Error e,world) = (Error e,world)
							(Ok filesp,world) = case scanPaths ps world of
								(Error e,world) = (Error e,world)
								(Ok filesps,world) = (Ok (filesp++filesps),world)
					
    //Include
    include p = let ext = takeExtension p in ext == "icl" || ext == "dcl"
       
	//We can skip directories that we know don't contain Clean modules
	exclude p = startsWith "." p || p == "Clean System Files" || p == "WebPublic"

    determineModules root paths = mods (sort pathsWithoutRoot)
	where
		//The module name is determined only from the part of the path without the root directory
		pathsWithoutRoot = [subString (textSize root + 1) (textSize s) s \\ s <- paths]

		mods [] = []
		mods [p1,p2:ps]
			# p1name = dropExtension p1
			# p2name = dropExtension p2
			| p1name == p2name && takeExtension p1 == "dcl" && takeExtension p2 == "icl"
				= [(moduleName p1name,AuxModule):mods ps]
			| takeExtension p1 == "icl"
				= [(moduleName p1name,MainModule):mods [p2:ps]]
			    = mods [p2:ps]
		mods [p1:ps]
			| takeExtension p1 == "icl"
				= [(moduleName (dropExtension p1),MainModule):mods ps]
			    = mods ps

		mods paths = [(p,MainModule) \\ p <- paths]
		moduleName p = replaceSubString {pathSeparator} "." p
	

moduleDefinition :: SDSLens (FilePath,ModuleName) [String] [String]
moduleDefinition = mapReadWrite mapToLines Nothing (sdsTranslate "moduleDefinition" (\(p,m) -> modulePath p m "dcl") (removeMaybe (Just "") fileShare))

moduleImplementation :: SDSLens (FilePath,ModuleName) [String] [String]
moduleImplementation = mapReadWrite mapToLines Nothing (sdsTranslate "moduleImplementation" (\(p,m) -> modulePath p m "icl") (removeMaybe (Just "") fileShare))

moduleDocumentation :: SDSLens (FilePath,ModuleName) [String] [String]
moduleDocumentation = mapReadWrite mapToLines Nothing (sdsTranslate "moduleDocumentation" (\(p,m) -> modulePath p m "md") (removeMaybe (Just "") fileShare))

mapToLines = (split "\n",\w _ -> Just (join "\n" w))

modulePath path name ext = path </> addExtension (replaceSubString "." {pathSeparator} name) ext

toModuleSelectTree :: [(ModuleName,ModuleType)] -> [(ChoiceNode)]
toModuleSelectTree modules = foldl addModule [] [(i,name,type) \\(name,type) <- modules & i <- [0..]]
where
	addModule tree (i,name,type) = insert i type (split "." name) tree

	insert i type [s] [t:ts]
		| s == t.ChoiceNode.label= [{ChoiceNode|t & id = i}:ts]
                                 = [t:insert i type [s] ts]
	insert i type [s:ss] [t:ts]
		| s == t.ChoiceNode.label= [{ChoiceNode|t & children = insert i type ss t.ChoiceNode.children}:ts]
                                 = [t:insert i type [s:ss] ts]
	insert i type [s] [] = [{id=i,label=s,icon=Nothing,expanded=False,children=[]}]
	insert i type [s:ss] [] = [{ChoiceNode|id= -1,label=s,icon=Nothing,expanded=False,children = insert i type ss []}]
	
rescanCodeBase :: CodeBase -> Task CodeBase
rescanCodeBase codebase
    =   allTasks [ accWorld (findModulesForTree tree)
                 @ (\modules -> {SourceTree|tree & modules=modules})
                 \\ tree <- codebase]

navigateCodebase :: CodeBase -> Task SourceTreeSelection
navigateCodebase codebase
    = enterChoice () [/* ChooseWith (ChooseFromTree (groupModules (sourceTreeRoots codebase)))*/] (modulesOf codebase)
where
    modulesOf codebase
        = flatten [[SelSourceTree name rootPath:[moduleSelection modName modType modPath \\ (modName,modType,modPath) <- modules]] \\ {SourceTree|name,rootPath,modules} <- codebase]

    moduleSelection modName MainModule modPath = SelMainModule modName modPath
    moduleSelection modName AuxModule modPath = SelAuxModule modName modPath
/*
    sourceTreeRoots codebase
        = flatten (map roots codebase)
    where
        roots {SourceTree|name,rootPath,subPaths=[]}  = [(name,rootPath)]
        roots {SourceTree|name,rootPath,subPaths}     = [(name,rootPath </> sub) \\sub <- subPaths]


    groupModules roots options expanded = sortByLabel (foldl insert [] options)
    where
        //Add a new source tree
	    insert nodeList (i,m=:(SelSourceTree name rootNode))
            = nodeList ++ [{ChoiceTree|label=name,icon=Just "sourcetree",value=ChoiceNode i, type = ifExpandedChoice i expanded []}]
        //Find the node that holds the tree to which this module belongs, and add it there
        insert nodeList (i,m) = insert` (sourceTreeOf m roots) (split "." (moduleName m)) (i,m) nodeList

        insert` Nothing _ _ nodeList = nodeList
        insert` _ _ _ [] = []
	    insert` (Just treeName) moduleSplit (i,m) [n=:{ChoiceTree|label}:ns]
            | label == treeName = [{ChoiceTree|n & type = case n.ChoiceTree.type of
                                        ExpandedNode nodes = ExpandedNode (insert`` moduleSplit (i,m) nodes)
                                        CollapsedNode nodes = CollapsedNode (insert`` moduleSplit (i,m) nodes)
                                   }:ns]
            | otherwise         = [n:insert` (Just treeName) moduleSplit (i,m) ns]
        where
            insert`` [] (i,m) nodeList = nodeList
            //Search
            insert`` path=:[nodeP:pathR] (i,m) [node=:{ChoiceTree|label=nodeL,value}:nodesR]
                | nodeP == nodeL
                    # type = ifExpandedChoice i expanded (insert`` pathR (i,m) (choiceTreeChildren node))
                    | pathR =:[]
                        = [{ChoiceTree|node & value = ChoiceNode i, icon = Just (moduleIcon m), type = type}:nodesR]
                    = [{ChoiceTree|node & type = type}:nodesR]
                | otherwise         = [node:insert`` path (i,m) nodesR]
		    insert`` path=:[nodeP:pathR] (i,m) []
                | pathR =:[]
                    = [{ChoiceTree|label=nodeP,icon=Just (moduleIcon m),value=ChoiceNode i, type= LeafNode}]
                | otherwise
                    = [{ChoiceTree|label=nodeP,icon=Nothing,value=GroupNode (moduleName m), type= ifExpandedGroup (moduleName m) expanded (insert`` pathR (i,m) [])}] 
        moduleName (SelMainModule name _) = name
        moduleName (SelAuxModule name _) = name

        modulePath (SelMainModule _ path) = path
        modulePath (SelAuxModule _ path) = path

        moduleIcon (SelMainModule _ _) = "mainmodule"
        moduleIcon (SelAuxModule _ _) = "auxmodule"

        sourceTreeOf m roots
            = case [name \\ (name,path) <- roots | startsWith path (modulePath m)] of
                [x:_] = Just x
                _     = Nothing

    sortByLabel nodes = map sortChildren (sortBy ordering nodes)
    where
        ordering a b = a.ChoiceTree.label < b.ChoiceTree.label

        sortChildren node=:{ChoiceTree|type=ExpandedNode children} = {node & type = ExpandedNode (sortByLabel children)}
        sortChildren node=:{ChoiceTree|type=CollapsedNode children} = {node & type = CollapsedNode (sortByLabel children)}
        sortChildren node = node
*/

lookupModule :: ModuleName CodeBase -> Maybe (ModuleName,ModuleType,FilePath)
lookupModule module [] = Nothing
lookupModule module [t=:{SourceTree|modules}:ts]
    = maybe (lookupModule module ts) Just (find ((==) module o fst3) modules)

listFilesInCodeBase :: CodeBase -> [CleanFile]
listFilesInCodeBase codeBase
    = flatten [	[(rootPath, modName, Icl) \\ (modName,_,_)         <- modules] ++
    			[(rootPath, modName, Dcl) \\ (modName,AuxModule,_) <- modules]
	    	  \\ {SourceTree|rootPath,modules} <- codeBase]

    //TODO Also add dcl files

cleanFilePath :: CleanFile -> FilePath
cleanFilePath (baseDir,modName,ext) = foldl (</>) baseDir (split "." modName) +++ toString ext

getModuleType :: ModuleName CodeBase -> Maybe ModuleType
getModuleType modName [] = Nothing
getModuleType modName [{SourceTree|modules}:ts] = maybe (getModuleType modName ts) Just (search modules)
where
    search [] = Nothing
    search [(m,t,p):ms]
        | modName == m  = Just t
                        = search ms

codeBaseToCleanModuleNames :: CodeBase -> [CleanModuleName]
codeBaseToCleanModuleNames codeBase
    = flatten [[(foldl (</>) rootPath (split "." modName), modName) \\ (modName,modType,modPath) <- modules] \\ {SourceTree|rootPath,modules} <- codeBase]

dirsOfTree :: !SourceTree -> [FilePath]
dirsOfTree {SourceTree|rootPath,subPaths=[]} = [rootPath]
dirsOfTree {SourceTree|rootPath,subPaths}    = [rootPath </> subPath \\ subPath <- subPaths]

findModulesForTree :: !SourceTree !*World -> ([(ModuleName,ModuleType,FilePath)],*World)
findModulesForTree tree w
    # (files,w) = foldr addDir ([],w) (dirsOfTree tree)
    = find [] files w
where
    addDir dir (files,w)
        # (filesInDir,w) = getFilesInPath dir w
        = ([dir </> file \\ file <- filesInDir] ++ files,w)

    find modBase [] w = ([],w)
    find modBase [f:fs] w
        # (mbInfo,w)                = getFileInfo f w
        | isError mbInfo            = find modBase fs w
        | (fromOk mbInfo).directory
            # (filesInDir,w)        = getFilesInPath f w
            # (subModules,w)        = find [dropExtension (dropDirectory f):modBase] [f </> file \\ file <- filesInDir] w
            # (fsModules,w)         = find modBase fs w
            = (subModules ++ fsModules,w)
        # (fileName,ext)            = splitExtension (dropDirectory f)
        # (fsModules,w)             = find modBase fs w
        | ext == "icl"
            = (addModule (dropExtension f) (toModuleName fileName modBase) False fsModules, w)
        | ext == "dcl"
            = (addModule (dropExtension f) (toModuleName fileName modBase) True fsModules, w)
        = (fsModules,w)

    addModule path modName isAux []
        = [(modName,if isAux AuxModule MainModule,path)]
    addModule path modName isAux [(m,MainModule,p):ms]
        | modName == m && isAux = [(m,AuxModule,p):ms]
                                = [(m,MainModule,p):addModule path modName isAux ms]
    addModule path modName isAux [(m,type,p):ms]
        | modName == m          = [(m,type,p):ms]
                                = [(m,type,p):addModule path modName isAux ms]


    toModuleName fileName modBase = join "." (reverse [fileName:modBase])

:: FileExtension :== String

getFilesInDir :: [FilePath] [FileExtension] !Bool !*World -> ([(FilePath,RForest FilePath)],*World)
getFilesInDir [] extensions showExtension w = ([],w)
getFilesInDir [path:paths] extensions showExtension w
# (treeFiles,w)	= getTree (takeDirectory path) [dropDirectory path] w
# (ntrees,w)	= getFilesInDir paths extensions showExtension w
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
            _  -> ([RNode fileName dirNodes:filesNodes],w)
    | isEmpty extensions || isMember (snd (splitExtension fileName)) extensions
        # (treeNodes,w)         = getTree absolutePath fileNames w
		# name 					= if showExtension fileName (dropExtension fileName)
        = ([RNode name []:treeNodes],w)
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
= ([],w)

