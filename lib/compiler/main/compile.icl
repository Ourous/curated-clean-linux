implementation module compile

import StdEnv
import frontend
import backendinterface
import filesystem, CoclSystemDependent
import compilerSwitches

from CoclSystemDependent import DirectorySeparator, ensureCleanSystemFilesExists

baseName :: {#Char} -> {#Char}
baseName path
	=	last (splitBy DirectorySeparator path)

directoryName :: {#Char} -> {#Char}
directoryName path
	=	foldr (\p ps -> p +++ {DirectorySeparator} +++ ps) "" (init (splitBy DirectorySeparator path))
	
splitBy :: Char {#Char} -> [{#Char}]
splitBy char string
	=	splitBy` 0 0
	where
		splitBy` frm to
			| to >= stringSize
				=	[string % (frm, to-1)]
			| string.[to] == char
				=	[string % (frm, to-1) : splitBy` (to+1) (to+1)]
			// otherwise
				=	splitBy` frm (to+1)
		stringSize
			=	size string

make_clean_system_files_dir_and_file_name :: !{#Char} -> (!{#Char},!{#Char})
make_clean_system_files_dir_and_file_name dir_and_module_name
	# last_dir_separator_i = find_last_dir_separator_i dir_and_module_name
	# last_dot_i = find_last_dot_i last_dir_separator_i dir_and_module_name
	| last_dot_i<0
		= ("Clean System Files",dir_and_module_name)
	# module_name = dir_and_module_name % (last_dot_i+1,size dir_and_module_name-1)
	| last_dir_separator_i<0
		# subdir_name = {let c=dir_and_module_name.[i] in if (c=='.') DirectorySeparator c \\ i<-[0..last_dot_i-1] }
		= (subdir_name +++ {DirectorySeparator} +++ "Clean System Files", module_name)
	# dir = dir_and_module_name % (0,last_dir_separator_i)
	| last_dot_i==last_dir_separator_i
		= (dir +++ "Clean System Files", module_name)
		# subdir_name = {let c=dir_and_module_name.[i] in if (c=='.') DirectorySeparator c \\ i<-[last_dir_separator_i+1..last_dot_i-1] }
		= (dir +++ subdir_name +++ {DirectorySeparator} +++ "Clean System Files", module_name)
where
	find_last_dir_separator_i s
		= find_last_dir_separator_i s (size s-1)
	where
		find_last_dir_separator_i s i
			| i>=0 && s.[i]<>DirectorySeparator
				= find_last_dir_separator_i s (i-1)
				= i

	find_last_dot_i last_dir_separator_i s
		= find_last_dot_i s (size s-1)
	where
		find_last_dot_i s i
			| i>last_dir_separator_i && s.[i]<>'.'
				= find_last_dot_i s (i-1)
				= i

openTclFile :: !String !String !*File !*Files -> (!Bool, !Optional .File, !*File, !*Files)
openTclFile mod_dir mod_name error files
	= open_file_in_clean_system_files_folder mod_dir mod_name ".tcl" FWriteData error files

open_file_in_clean_system_files_folder :: !String !String !String !Int !*File !*Files -> (!Bool, !Optional .File, !*File, !*Files)
open_file_in_clean_system_files_folder mod_dir mod_name file_extension file_mode error files
	# icl_mod_pathname = mod_dir +++ {DirectorySeparator} +++ mod_name;
	# (csf_directory_path,file_name) = make_clean_system_files_dir_and_file_name icl_mod_pathname
	# file_path = csf_directory_path +++ {DirectorySeparator} +++ file_name +++ file_extension
	# (opened, tcl_file, files)
		= fopen file_path file_mode files
	| opened
		= (True, Yes tcl_file, error, files)
	// try again after creating Clean System Files folder
	# (ok, files)
		= ensureCleanSystemFilesExists csf_directory_path files
	| not ok
		# error = fwrites ("can't create folder \"" +++ csf_directory_path +++"\"\n") error
		= (False, No, error, files)
	# (opened, tcl_file, files)
		= fopen file_path file_mode files
	| not opened
		# error = fwrites ("couldn't open file \"" +++ file_path +++ "\"\n") error
		= (False, No, error, files)
	= (True, Yes tcl_file, error, files)

closeTclFile :: !*(Optional *File) *Files -> *(!Bool,*Files)
closeTclFile (Yes tcl_file) files
	= fclose tcl_file files
closeTclFile _ files
	= (True,files);

::	CoclOptions =
	{	moduleName:: {#Char}
	,	pathName ::{#Char}
	,	outputPathName ::{#Char}
	,	errorPath:: {#Char}
	,	errorMode::	Int
	,	outPath:: {#Char}
	,	outMode::	Int
	,	searchPaths:: SearchPaths
	,	listTypes :: ListTypesOption
	,	fusion_options :: !FusionOptions
	,	compile_for_dynamics	:: !Bool
	,	dump_core				:: !Bool
	,	compile_with_generics   :: !Bool
	}

StdErrPathName :== "_stderr_"
StdOutPathName :== "_stderr_"

InitialCoclOptions =
	{	moduleName=	""
	,	pathName=	""
	,	outputPathName=	""
	,	errorPath=	StdErrPathName
	,	errorMode=	FWriteText
	,	outPath=	StdErrPathName
	,	outMode=	FWriteText
	,	searchPaths=	{sp_locations = [], sp_paths = []}
	,	listTypes = {lto_showAttributes = True, lto_listTypesKind = ListTypesNone}
	,	fusion_options = {compile_with_fusion = False, generic_fusion = False, strip_unused = False}
	,	compile_for_dynamics	= False
	,	dump_core				= False
	,	compile_with_generics 	= True 
	}

:: DclCache = {
	dcl_modules::!{#DclModule},
	functions_and_macros::!.{#.{#FunDef}},
	predef_symbols::!.PredefinedSymbols,
	hash_table::!.HashTable,
	heaps::!.Heaps
 };

empty_cache :: !*SymbolTable -> *DclCache
empty_cache symbol_heap
	# heaps = {hp_var_heap = newHeap, hp_expression_heap = newHeap, hp_type_heaps = {th_vars = newHeap, th_attrs = newHeap}, hp_generic_heap = newHeap}
	# (predef_symbols, hash_table) = buildPredefinedSymbols (newHashTable symbol_heap)
	= {dcl_modules={},functions_and_macros={},predef_symbols=predef_symbols,hash_table=hash_table,heaps=heaps}

compile :: ![{#Char}] !*DclCache !*Files -> (!Bool,!*DclCache,!*Files)
compile args cache files
	# (args_without_modules,modules,cocl_options) = parseCommandLine args InitialCoclOptions
	= compile_modules modules 0 cocl_options args_without_modules cache files;

// WARNING:
// if you add an option which is not supported by the backend, then you should remove it from
// the first list in the tuple returned by parseCommandLine
parseCommandLine :: [{#Char}] CoclOptions -> ([{#Char}],[{#Char}],CoclOptions)
parseCommandLine [] options
	=	([],[],options)
parseCommandLine [arg1=:"-o", outputPathName : args] options=:{searchPaths}
        =       parseCommandLine args {options & outputPathName = outputPathName}
parseCommandLine [arg1=:"-P", searchPathsString : args] options=:{searchPaths}
// RWS, voor Maarten +++	=	parseCommandLine args {options & searchPaths = {searchPaths & sp_paths = splitPaths searchPathsString}}
	# (args,modules,options) =	parseCommandLine args {options & searchPaths.sp_paths = splitPaths searchPathsString}
	= ([arg1,searchPathsString:args],modules,options)
parseCommandLine [arg1=:"-RO", outPath : args] options
	# (args,modules,options)=	parseCommandLine args {options & outPath = stripQuotes outPath, outMode = FWriteText}
	= ([arg1,outPath:args],modules,options)
parseCommandLine [arg1=:"-RAO", outPath : args] options
	# (args,modules,options)=	parseCommandLine args {options & outPath = stripQuotes outPath, outMode = FAppendText}
	= ([arg1,outPath:args],modules,options)
parseCommandLine [arg1=:"-RE", errorPath : args] options
	# (args,modules,options)=	parseCommandLine args {options & errorPath = stripQuotes errorPath, errorMode = FWriteText}
	= ([arg1,errorPath:args],modules,options)
parseCommandLine [arg1=:"-RAE", errorPath : args] options
	# (args,modules,options)=	parseCommandLine args {options & errorPath = stripQuotes errorPath, errorMode = FAppendText}
	= ([arg1,errorPath:args],modules,options)
/* RWS FIXME: "-id" option is only used for the Mac version
   and should be moved elsewhere
*/
parseCommandLine ["-id",compiler_id_string : args] options
	# compiler_id=toInt compiler_id_string
	| set_compiler_id compiler_id==compiler_id
		= parseCommandLine args options
parseCommandLine [arg1=:"-dynamics":args] options
	// generates for each .icl module a .tcl file (which contains the type information for that module)
	# (args,modules,options) = parseCommandLine args {options & compile_for_dynamics = True}
	= ([arg1:args],modules,options)
parseCommandLine [arg1=:"-fusion":args] options
	// switch on fusion transformations
	# (args,modules,options) = parseCommandLine args {options & fusion_options.compile_with_fusion = True}
	= ([arg1:args],modules,options)
parseCommandLine [arg1=:"-generic_fusion":args] options
	# (args,modules,options) = parseCommandLine args {options & fusion_options.generic_fusion = True}
	= ([arg1:args],modules,options)
parseCommandLine [arg1=:"-dump":args] options
	= parseCommandLine args {options & dump_core = True}
parseCommandLine [arg1=:"-strip":args] options
	= parseCommandLine args {options & fusion_options.strip_unused = True}
parseCommandLine ["-generics":args] options
	// enable generics
	= parseCommandLine args {options & compile_with_generics = True}
parseCommandLine ["-lattr":args] options
	= parseCommandLine args {options & listTypes.lto_showAttributes = False}
parseCommandLine ["-lt":args] options
	= parseCommandLine args {options & listTypes.lto_listTypesKind = ListTypesInferred}
parseCommandLine ["-lset":args] options
	= parseCommandLine args {options & listTypes.lto_listTypesKind = ListTypesStrictExports}
parseCommandLine ["-lat":args] options
	= parseCommandLine args {options & listTypes.lto_listTypesKind = ListTypesAll}
parseCommandLine [arg : args] options
	| arg.[0] == '-'
		# (args,modules,options)=	parseCommandLine args options
		= ([arg:args],modules,options)
	// otherwise
		# (args,modules,options) = parseCommandLine args options
		= (args,[arg : modules],options);

stripExtension :: {#Char} {#Char} -> {#Char}
stripExtension extension string
	| stringSize >= extensionSize && (string % (stringSize-extensionSize, stringSize-1)) == extension
		=	string % (0, stringSize-extensionSize-1)
	// otherwise
		=	string
	where
		stringSize
			=	size string
		extensionSize
			=	size extension

stripQuotes :: {#Char} -> {#Char}
stripQuotes string
	| stringSize > 1 && string.[0] == '"' && string.[stringSize-1] == '"'
		=	string % (1, stringSize-2)
	// otherwise
		=	string
	where
		stringSize
			=	size string

splitPaths :: {#Char} -> [{#Char}]
splitPaths paths
	=	[path +++ {DirectorySeparator} \\ path <- splitBy PathSeparator paths]

compile_modules [module_:modules] n_compiles cocl_options args_without_modules cache files
	# cocl_options = prependModulePath {cocl_options & pathName=stripExtension ".icl" (stripQuotes module_)}
		with
		// RWS +++ hack, both module name and file path should be passed to frontEndInterface
		prependModulePath options=:{pathName, searchPaths}
			=	{	options
				&	moduleName = baseName pathName
					// RWS, voor Maarten +++				,	searchPaths = {searchPaths & sp_paths = [directoryName pathName : searchPaths.sp_paths]}
//				,	searchPaths = [directoryName pathName : searchPaths]
				}
	# (ok,cache,files)
		= compileModule cocl_options (args_without_modules++[module_]) cache files;
	| ok
		= compile_modules modules (n_compiles+1) cocl_options args_without_modules cache files;
	// otherwise
		= (ok,cache,files);
compile_modules [] n_compiles cocl_options args_without_modules cache files
	= (True,cache,files);

openPath :: {#Char} Int *Files -> (Bool, *File, *Files)
openPath path mode files
	| path == StdErrPathName
		=	(True, stderr, files)
	| path == StdOutPathName
		# (io, files)
			=	stdio files
		=	(True, io, files)
	// otherwise
		=	fopen path mode files

compileModule :: CoclOptions [{#Char}] *DclCache *Files -> (!Bool,!*DclCache,!*Files)
compileModule options backendArgs cache=:{dcl_modules,functions_and_macros,predef_symbols,hash_table,heaps} files	
	# (opened, error, files)
		=	openPath options.errorPath options.errorMode files
	| not opened
		=	abort ("couldn't open error file \"" +++ options.errorPath +++ "\"\n")
	# (opened, out, files)
		=	openPath options.outPath options.outMode files
	| not opened
		=	abort ("couldn't open out file \"" +++ options.outPath +++ "\"\n")
	# (opt_file_dir_time,files) = fopenInSearchPaths options.moduleName ".icl" options.searchPaths FReadData fmodificationtime files
	# (opt_file_dir_time, mbModPath) = 
			case opt_file_dir_time of
				Yes (_,mod_path,_) = (opt_file_dir_time, Yes mod_path)
				No				   = (opt_file_dir_time, No)
	# (optional_tcl_opened, tcl_file, error, files)
		= case mbModPath of
			Yes mod_path
				| options.compile_for_dynamics
					-> openTclFile mod_path options.moduleName error files
			_
				-> 	(True,No,error,files)
 	| not optional_tcl_opened
		# (closed, files) = fclose out files
		| not closed
			=	abort ("couldn't close stdio")
		# (closed, files) = fclose error files
		| not closed
			=	abort ("couldn't close out file \"" +++ options.outPath +++ "\"\n")
		=	(False, cache, files)
	# (io, files) = stdio files
	# ({boxed_ident=moduleIdent}, hash_table) = putIdentInHashTable options.moduleName (IC_Module NoQualifiedIdents) hash_table
	# list_inferred_types
		=	if (options.listTypes.lto_listTypesKind == ListTypesInferred)
				(Yes options.listTypes.lto_showAttributes)
				No
	# (optionalSyntaxTree,cached_functions_and_macros,cached_dcl_mods,main_dcl_module_n,predef_symbols, hash_table, files, error, io, out,tcl_file,heaps)
		= frontEndInterface opt_file_dir_time
			{feo_up_to_phase=FrontEndPhaseAll
			,feo_generics=options.compile_with_generics
			,feo_fusion=options.fusion_options
			} moduleIdent options.searchPaths dcl_modules functions_and_macros list_inferred_types predef_symbols hash_table fmodificationtime files error io out tcl_file heaps 

	# unique_copy_of_predef_symbols={predef_symbol\\predef_symbol<-:predef_symbols}
	# (closed, files)
		= closeTclFile tcl_file files
	| not closed
		=   abort ("couldn't close tcl file \"" +++ options.pathName +++ "tcl\"\n")
	# (closed, files)
		=	fclose io files
	| not closed
		=	abort ("couldn't close stdio")
	# var_heap=heaps.hp_var_heap
	  hp_type_heaps=heaps.hp_type_heaps
	  type_var_heap=hp_type_heaps.th_vars
	  attrHeap=hp_type_heaps.th_attrs
	# (success,functions_and_macros,var_heap,type_var_heap,attrHeap,error,out)
		= case optionalSyntaxTree of
			Yes syntaxTree
				# functions_and_macros = syntaxTree.fe_icl.icl_functions
				# (success, var_heap, type_var_heap, attrHeap, error, out)
				 	 = backEndInterface outputPath (map appendRedirection backendArgs) options.listTypes options.outPath predef_symbols syntaxTree main_dcl_module_n
				 	 					var_heap type_var_heap attrHeap error out
				-> (success,functions_and_macros,var_heap,type_var_heap,attrHeap,error,out)
				with
					appendRedirection arg
						= case arg of
							"-RE"
								-> "-RAE"
							"-RO"
								-> "-RAO"
							arg
								->	arg
			No
				-> (False,{},var_heap,type_var_heap,attrHeap,error,out)
		with
/*
			outputPath
				=	if (options.outputPathName == "")
						(directoryName options.pathName +++ "Clean System Files" +++ {DirectorySeparator} +++ baseName options.pathName)
						options.outputPathName
*/
			outputPath
	//				=	/* directoryName options.pathName +++ "Clean System Files" +++ {DirectorySeparator} +++ */ baseName options.pathName
				=	baseName options.pathName
	# heaps = {heaps & hp_var_heap=var_heap, hp_type_heaps = {th_vars=type_var_heap, th_attrs=attrHeap}}
	# (closed, files) = fclose out files
	| not closed
		=	abort ("couldn't close out file \"" +++ options.outPath +++ "\"\n")
	# (closed, files)
		=	fclose error files
	| not closed
		=	abort ("couldn't close error file \"" +++ options.errorPath +++ "\"\n")
	| success
		# dcl_modules={{dcl_module \\ dcl_module<-:cached_dcl_mods} & [main_dcl_module_n].dcl_has_macro_conversions=False}
		# cache={dcl_modules=dcl_modules,functions_and_macros=cached_functions_and_macros,predef_symbols=unique_copy_of_predef_symbols,hash_table=hash_table,heaps=heaps}
		= (success,cache,files)
		# cache={dcl_modules=cached_dcl_mods,functions_and_macros=cached_functions_and_macros,predef_symbols=unique_copy_of_predef_symbols,hash_table=hash_table,heaps=heaps}
		= (success,cache,files)
