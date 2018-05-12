implementation module Sapl.Linker.LazyLinker

import StdEnv, Data.Maybe, Text
import Sapl.SaplTokenizer, Sapl.Linker.SaplLinkerShared, Text.StringAppender

import qualified Data.Map as DM

from Sapl.FastString import charIndexBackwards
from Data.Set import newSet

import System.FilePath, System.File, System.Directory, Data.Error
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

// Module name -> file name
:: ModuleMap :== Map String String

// (normal module map, builtin module map, ...)
:: LoaderState    :== (ModuleMap, ModuleMap, Warnings, IdGenerator)
:: LoaderStateExt :== (LoaderState, FuncTypeMap, SkipSet)

handlerr (Error (c, str)) = abort ("LazyLinker.icl: " +++ str)
handlerr (Ok a) = a

isDirectory :: !String !*World -> *(!Bool, !*World)
isDirectory path world
	= case getFileInfo path world of
		(Ok fi, world)   = (fi.FileInfo.directory, world)
		(Error _, world) = (False, world)

fileList :: !FilePath (FilePath -> Bool) !*World -> *(![FilePath], ![FilePath], !*World)
fileList path ffilter world 
	# (fs, world) = readDirectory path world
	| isOk fs
		= perFile path (fromOk fs) [] [] world
		// skip the error if the directory is not exists
		= ([],[],world)
where	
	// basePath, dirlist to process, module list, override module list, world
	perFile _ [] ms os world
		= (ms, os, world)
	perFile basePath [f:fs] ms os world
		| f == "." || f == ".."
			= perFile path fs ms os world
		| f == "_override_"
			# (res, world) = readDirectory fullPath world
			# os2 = filter ffilter (handlerr res)
			# os2 = map (\m = fullPath </> (dropDirectory m)) os2
			= perFile path fs ms (os++os2) world
		| otherwise
			= case isDirectory fullPath world of
				(True,  world) 
					# (ms2, os2, world) = fileList fullPath ffilter world
					= perFile path fs (ms++ms2) (os++os2) world
				(False, world)
					| ffilter f 
						= perFile path fs (ms++[fullPath]) os world
						= perFile path fs ms os world				
	where
		fullPath = basePath </>	f

generateLoaderState :: ![String] ![String]  ![String] !*World -> *(LoaderStateExt, !*World)
generateLoaderState dirs mods exclude world 
		
	# (ms, os, world) = foldl (\(ms,os,w) module_directory -> 
		let (ms2,os2,w2) = findModules module_directory w in (ms++ms2,os++os2,w2)) ([],[],world) dirs

	// Add individual modules (extension doesn't matter in this case)
	# ms = toPairTopLevel "" mods ++ ms

	# omap = 'DM'.fromList os
	# mmap = 'DM'.fromList ms
	# mmap = 'DM'.delList exclude mmap 
		
	// If an override doesn't actually override anything, move it to normal modules	
    # onlyoverride = 'DM'.toList ('DM'.delList (map fst ('DM'.toList mmap)) omap)
	# omap = 'DM'.delList (map fst onlyoverride) omap
	# mmap = 'DM'.putList onlyoverride mmap

	= (((mmap, omap, [], 0), 'DM'.newMap, newSet), world)

where
	findModules module_directory world
		# (ms, os, world) = fileList module_directory (\f -> endsWith ".sapl" f) world
		= (toPair module_directory ms, toPair module_directory os, world)

	// -I
	toPair module_directory ms = zip2 (map ((toModuleName module_directory) o dropExtension) ms) ms
	// -i
	toPairTopLevel module_directory ms = zip2 (map ((toModuleName module_directory) o dropExtension o dropDirectory) ms) ms 

	toModuleName module_directory path = join "." moduleDirs`
	where
		relativeDir = if(module_directory == "") path (subString (size module_directory + 1) (size path) path)
		dirs = split (toString pathSeparator) relativeDir
		moduleDirs = filter (not o (==) "_override_") dirs
		// drop filename from module name: Adjoxo;Main
		moduleDirs` = (init moduleDirs) ++ [last (split ";" (last moduleDirs))]

getWarnings :: !LoaderStateExt -> [String]
getWarnings ((_, _, ws, _), _, _) = ws

linkByExpr :: !LoaderStateExt !StringAppender !String !*World -> *(!LoaderStateExt, !StringAppender, !String, !*World)
linkByExpr (ls,lmap,ss) a expr world 
	# maindeps = generate_dependencies (tokens expr) []
	# (lmap, (_, ls), expra, world) = substitute_macros lmap maindeps (lazy_loader, ls) expr newAppender world

	# (lmap, ss, (_, ls), a, world) 
				= foldl (\(lmap, ss, loader, a, world) d = generate_source lmap ss loader d a world) 
					    (lmap, ss, (lazy_loader, ls), a, world) maindeps

	= ((ls,lmap,ss), a, toString expra, world)

where
	getModuleName name
		# (ok, pos) = charIndexBackwards name (size name - 1) '.'
		| ok
			= name % (0,pos-1)
			= ""

	/* Load a given function (LoaderFunction LoaderState, see SaplLinkerShared)
	 *
	 * @param ls loader state (module map, built-in module map, warning messages, id generator)
	 * @param fn function name to be loaded
	 * @param lmap line map
	 */
	lazy_loader :: LoaderState String FuncTypeMap *World -> *(Maybe LineType, FuncTypeMap, LoaderState, *World)
	lazy_loader ls=:(mmap, bmmap, messages, id) fn lmap world 
		# line = 'DM'.get fn lmap
		| isJust line
			= (line, lmap, ls, world)

			// try to load the module
			# m = getModuleName fn
			| size m == 0 // the function name doesn't contain module name
				= (Nothing, lmap, ls, world)

				// is it already loaded?
				# (mpath, mmap) = 'DM'.delU m mmap 
				| isNothing mpath
					= (Nothing, lmap, ls, world)
					# (lmap, id, messages, world) = read_module (fromJust mpath) lmap messages id world
				
					// read built-in module if avalaible
					# (bmpath, bmmap) = 'DM'.delU m bmmap
					# (lmap, id, messages, world) = 
						if (isJust bmpath)
							(read_module (fromJust bmpath) lmap messages id world)
							(lmap, id, messages, world)
				
					// try to get the line information again
					= ('DM'.get fn lmap, lmap, (mmap,bmmap,messages,id), world)
