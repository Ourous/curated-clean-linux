implementation module Directory

import	StdEnv
import code from "cDirectory."
import StdMaybe
from StdLibMisc import :: Date (..), :: Time (..)

getDirectoryContents::	!Path !*env -> (!(!DirError, [DirEntry]), !*env)	| FileSystem env
getDirectoryContents path files
	#	(platformId, files)		= getPlatformIdC dummy files
	|	isBadPath path platformId
		= ((BadName, fail self), files)
	#	(path_string, files)	= pathToPD_String path files
		(errCode, files)		= findFirstFileC path_string files
		(result, files)			= case errCode of
									M_NoDirError
										#	(entries, files) = getDirEntries platformId [] files
										-> ((NoDirError, entries), files)
									1	-> ((NoDirError, []), files)
									_	-> ((toDirError [M_BadName, M_DoesntExist, M_NoPermission] errCode,fail self),
											files)
		files					= closeSearchC files
	= (result, files)
  where
	self = "getDirectoryContents"
	
getDirEntries	::	!Int [DirEntry] !*env -> (![DirEntry], !*env)	| FileSystem env
getDirEntries platform akku files
	#	(entry, files) = getDirEntry True platform files
		(errCode, files) = findNextFileC dummy files
	| 	errCode<>M_NoDirError
		= (reverse [entry:akku], files)
	=  getDirEntries platform [entry:akku] files

getDirEntry	::	!Bool !Int !*env -> (DirEntry, !*env)	| FileSystem env
getDirEntry also_get_file_name platform files
	#	((fileName, fileSizeTuple, lastModifiedTuple, isDirectory, isReadOnly), files)
			= getCommonFileInfoC also_get_file_name files
		(fileSizeLow, fileSizeHigh)	= fileSizeTuple
		fileSize	= (fileSizeLow, fileSizeHigh)		// packBigInt True { fileSizeLow, fileSizeHigh } // MdM
		lastModified= DT_tuple_to_record lastModifiedTuple
		pi_fileInfo	=	{	fileSize	= fileSize
						,	lastModified= lastModified
						,	isDirectory	= isDirectory
						,	isReadOnly	= isReadOnly
						}
	|	platform == MacPlatform
		#	((macCreationTimeTuple, macBackupTimeTuple, macIsHidden, macCreator, 
			  macFileType, macFDFlags), files)
				= getMacFileInfoC files
			macFurtherInfo	= if isDirectory	Directory
												(File (macCreator, macFileType))
			macFileInfo	=	{	macCreationTime		= DT_tuple_to_record macCreationTimeTuple
							,	macBackupTime		= DT_tuple_to_record macBackupTimeTuple
							,	macIsHidden			= macIsHidden
							,	macFDFlags			= macFDFlags
							,	macFurtherInfo		= macFurtherInfo
							}
		= ({fileName=fileName, fileInfo={pi_fileInfo=pi_fileInfo, pd_fileInfo=Mac macFileInfo}}, files)
	|	platform == WindowsPlatform
		#	((winFileAttributes, winCreationTimeTuple, winLastAccessTimeTuple, winDOSName, winIsHidden), files)
				= getWindowsFileInfoC files
			windowsFileInfo		=	{	winCreationTime		= DT_tuple_to_record winCreationTimeTuple
									,	winLastAccessTime	= DT_tuple_to_record winLastAccessTimeTuple
									,	winDOSName			= winDOSName
									,	winIsHidden			= winIsHidden
									,	winIsArchiveFile	= (FILE_ATTRIBUTE_ARCHIVE bitand winFileAttributes) <> 0
									,	winIsSystemFile		= (FILE_ATTRIBUTE_SYSTEM bitand winFileAttributes) <> 0
									}
		= ({fileName=fileName, fileInfo={pi_fileInfo=pi_fileInfo, pd_fileInfo=Windows windowsFileInfo}}, files)
	|	platform == UnixPlatform
		#	((unixModeBitsField, unixOwnerUserId, unixOwnerGroupId, unixLastAccessTimeTuple), files)
			 	= getUnixFileInfoC files
			unixFileInfo = 	{	unixModeBitsField	= unixModeBitsField
							,	unixOwnerUserId		= unixOwnerUserId
							,	unixOwnerGroupId	= unixOwnerGroupId
							,	unixLastAccessTime	= DT_tuple_to_record unixLastAccessTimeTuple
							}
		= ({fileName=fileName, fileInfo={pi_fileInfo=pi_fileInfo, pd_fileInfo=Unix unixFileInfo}}, files)

DT_tuple_to_record	::	DateTimeTuple -> DateTime
DT_tuple_to_record ((year, month, day, dayNr), (hours, minutes, seconds))
	#	date	= { year=year, month=month, day=day, dayNr=dayNr }
		time	= { hours=hours, minutes=minutes, seconds=seconds }
	 =(date, time)

isBadPath path platformId
	= let (diskName, steps) = get_disk_and_steps path
		  bad_chars = bad_path_chars platformId
	  in diskNameIsBad bad_chars diskName platformId || aStepIsBad bad_chars steps
	  
  where
	get_disk_and_steps (AbsolutePath diskname steps) = (diskname,steps)
	get_disk_and_steps (RelativePath steps) = ("", steps)

	diskNameIsBad bad_chars diskName WindowsPlatform
		| size diskName>2 && diskName.[0]=='\\' && diskName.[1]=='\\' // Windows network path
			= isBadString bad_chars diskName 2
		// further with next alternative
	diskNameIsBad bad_chars diskName _
		= isBadString bad_chars diskName 0
		
	aStepIsBad _ []
		= False
	aStepIsBad bad_chars [step : steps]
		= stepIsBad bad_chars step || aStepIsBad bad_chars steps
	  where
		stepIsBad _ PathUp
			= False
		stepIsBad bad_chars (PathDown string)
			= isBadString bad_chars string 0

	isBadString bad_chars string i
		| i>=size string
			= False
		# char = string.[i]
		= isMember char bad_chars || isBadString bad_chars string (i+1)
		
listToString :: [Char] -> String
listToString l
	= { ch \\ ch<-l}

fail function_name = abort ("fatal error: tried to evaluate an error value returned by function "+++function_name+++".")

isHidden			::	!DirEntry	   -> Bool
isHidden {fileName, fileInfo={pd_fileInfo=Unix _}}
	= size fileName==0 || fileName.[0]=='.'
isHidden {fileInfo={pd_fileInfo=Windows {winIsHidden}}}
	= winIsHidden
isHidden {fileInfo={pd_fileInfo=Mac {macIsHidden}}}
	= macIsHidden
	

createDirectory		::	!Path  !*env -> (!DirError, !*env)				| FileSystem env
createDirectory path env
	#	(platformId, env)		= getPlatformIdC dummy env
	|	isBadPath path platformId
		= (BadName, env)
	# (path_string, env) = pathToPD_String path env
	  (errCode, env) = createDirectoryC (path_string+++"\0") env
	= (toDirError [M_NoDirError, M_DoesntExist, M_BadName, M_NotEnoughSpace, M_AlreadyExists,
				   M_NoPermission] errCode, env)

getFileInfo			::	!Path !*env	-> (!(!DirError, FileInfo), !*env)	| FileSystem env
getFileInfo (AbsolutePath _ []) files
		= ((BadName, fail "getFileInfo"), files)
getFileInfo path files
	#	(platformId, files)	= getPlatformIdC dummy files
	|	isBadPath path platformId
		= ((BadName, fail self), files)
	#	(path_string, files)= pathToPD_String path files
		(errCode, files)	= findSingleFileC (path_string+++"\0") files
		(result, files)
			= case errCode of
				M_NoDirError#	(entry, files)	= getDirEntry False platformId files
							-> ((NoDirError, entry.fileInfo), files)
				_			-> ((toDirError [M_DoesntExist, M_BadName, M_NoPermission, M_OtherDirError]
											errCode, fail self)
							   , files)
		files				= closeSingleSearchC files
	= (result, files)
  where
	self = "getFileInfo"

getFileName			::	!Path !*env	-> (!(!DirError, String), !*env)	| FileSystem env
getFileName (AbsolutePath _ []) files
		= ((BadName, fail "getFileName"), files)
getFileName path files
	#	(platformId, files)	= getPlatformIdC dummy files
	|	isBadPath path platformId
		= ((BadName, fail self), files)
	#	(path_string, files)= pathToPD_String path files
		(errCode, files)	= findSingleFileC (path_string+++"\0") files
		(result, files)
			= case errCode of
				M_NoDirError#	(entry, files)	= getDirEntry True platformId files
							-> ((NoDirError, entry.fileName), files)
				_			-> ((toDirError [M_DoesntExist, M_BadName, M_NoPermission, M_OtherDirError]
											errCode, fail self)
							   , files)
		files				= closeSingleSearchC files
	= (result, files)
  where
	self = "getFileName"

fmove				::	!MoveMode !Path !Path !*env -> (!DirError, !*env)			| FileSystem env
fmove moveMode p_fromm p_to env
	#	(platformId, env) = getPlatformIdC dummy env
	|	isBadPath p_fromm platformId || isBadPath p_to platformId
		= (BadName, env)
	#	overwrite = case moveMode of
						OverwriteFile -> True
						DontOverwrite -> False
		(ps_fromm, env) = pathToPD_String p_fromm env
		(ps_to, env) = pathToPD_String p_to env
	= case platformId of
		UnixPlatform# (errCode, env) = fmoveC overwrite (ps_fromm+++"\0") (ps_to+++"\0") env
					-> (toDirError fmoveRestrictions errCode, env)
		MacPlatform	-> macMove overwrite (ps_fromm,p_fromm) (ps_to,p_to) env
		WindowsPlatform
					#	(file_would_be_copied, env)
							= on_different_disks p_fromm p_to env
					|	file_would_be_copied
						-> (MoveAcrossDisks, env)
					# (errCode, env) = fmoveC overwrite (ps_fromm+++"\0") (ps_to+++"\0") env
					-> (toDirError fmoveRestrictions errCode, env)
  where
	fmoveRestrictions = [M_NoDirError, M_DoesntExist, M_BadName, M_NotEnoughSpace, M_AlreadyExists, M_NoPermission,
						 M_MoveIntoOffspring, M_MoveAcrossDisks, M_OtherDirError]
	on_different_disks (RelativePath _) (RelativePath _) env
		= (False, env)
	on_different_disks p_fromm p_to env
		// for windows only
		#	(diskname_fromm	, env)	= diskname_of p_fromm env
			(diskname_to	, env)	= diskname_of p_to env
		= (diskname_fromm<>diskname_to, env)
	diskname_of (RelativePath _) env
		#	(AbsolutePath diskname _, env) = getCurrentDirectory env
		= (diskname.[0], env)
	diskname_of (AbsolutePath diskname _) env
		= (diskname.[0], env)

	macMove :: !Bool !(!String, !Path) !(!String, !Path) !*env -> (!DirError, !*env)	| FileSystem env
	macMove overwrite (ps_fromm,p_fromm) (ps_to,p_to) env
		#	((err, to_fileInfo), env) = getFileInfo p_to env
		|	err<>NoDirError
			# (errCode, env) = macMoveWithFreeTarget (ps_fromm,p_fromm) (ps_to,p_to) env
			= (toDirError fmoveRestrictions errCode, env)
		#	{pi_fileInfo={isDirectory}} = to_fileInfo
		|	isDirectory || not overwrite
			= (AlreadyExists, env)
		#	((err, _), env) = getFileInfo p_fromm env
		|	err<>NoDirError
			= (err, env)
		#	(errCode, env) = fremoveC ps_to env
		|	errCode<>M_NoDirError
			= (toDirError fmoveRestrictions errCode, env)
		# (errCode, env) = macMoveWithFreeTarget (ps_fromm,p_fromm) (ps_to,p_to) env
		= (toDirError fmoveRestrictions errCode, env)
	  where
		macMoveWithFreeTarget :: !(!String, !Path) !(!String, !Path) !*env -> (!ErrCode, !*env)	| FileSystem env
		macMoveWithFreeTarget (ps_fromm,p_fromm) (ps_to,p_to) env
			#	env = env
				mb_split_fromm	= splitPath p_fromm
				mb_split_to	= splitPath p_to
			|	isNothing mb_split_fromm || isNothing mb_split_to
				= (M_OtherDirError, env)
			#	(Just (p_fromm_up, filename_fromm))	= mb_split_fromm
				(Just (p_to_up, filename_to))	= mb_split_to
				(ps_to_up   , env) = pathToPD_String p_to_up    env
				(errCode, env) = macMoveC ps_fromm ps_to_up env
				env = env
			= case errCode of
				M_NoDirError
					-> macRename (appendPath p_to_up filename_fromm) filename_to env
				M_AlreadyExists
					#	env = env
					#	p_mixed = appendPath p_fromm_up filename_to
						(exists, env) = doesExist p_mixed env
					|	not exists
						-> macMoveVia filename_fromm p_fromm_up filename_to ps_to_up env
					#	(temp_name, env)
							= mac_search_temporary_name p_fromm_up p_to_up filename_fromm Nothing Nothing 0 env
						(errCode2, env) = macMoveVia filename_fromm p_fromm_up temp_name ps_to_up env
						env = env
					| errCode2==M_NoDirError
						-> (errCode2, env)
					-> (errCode2, snd (macRename (appendPath p_to_up temp_name) filename_to env))
				_	-> (errCode, env)
		macMoveVia :: !String !Path !String !String !*env -> (!ErrCode, !*env)	| FileSystem env
		macMoveVia filename_fromm p_fromm_up filename_via ps_to_up env
			// before: p_fromm_up:filename_fromm exists
			// afterwards: ps_to_up:filename_via exists (or not ok)
			#	(errCode, env) = macRename (appendPath p_fromm_up filename_fromm) filename_via env
			|	errCode<>M_NoDirError
				= (errCode, env)
			#	(ps_via, env) = pathToPD_String (appendPath p_fromm_up filename_via) env
				(errCode2, env) = macMoveC ps_via ps_to_up env
			|	errCode2==M_NoDirError // no error
				= (errCode2, env)
			= macRename (appendPath p_fromm_up filename_via) filename_fromm env
		mac_search_temporary_name :: !Path !Path !String !(Maybe [DirEntry])
									 !(Maybe [DirEntry]) Int !*env -> (!String, !*env)	| FileSystem env
		mac_search_temporary_name dir_path_1 dir_path_2 filename mb_dir1 mb_dir2 i env
			// search a filename, that is free in dir_path_1 and dir_path_2 and that sounds like filename
			|	i==25
				# ((err1, dir1), env) = getDirectoryContents dir_path_1 env
				  ((err2, dir2), env) = getDirectoryContents dir_path_1 env
				  mb_dir1 = toMaybe err1 dir1
				  mb_dir2 = toMaybe err2 dir2
				= mac_search_temporary_name dir_path_1 dir_path_2 filename mb_dir1 mb_dir2 (inc i) env
			#	i_string = toString i
				i = min (size filename) (max_mac_filename_length-(size i_string))
				filename1 = (filename % (0, i-1))+++i_string
				(exists1, env) = doesExist` mb_dir1 dir_path_1 filename1 env
			|	exists1
				= mac_search_temporary_name dir_path_1 dir_path_2 filename mb_dir1 mb_dir2 (inc i) env
			#	(exists2, env) = doesExist` mb_dir2 dir_path_2 filename1 env
			|	exists2
				= mac_search_temporary_name dir_path_1 dir_path_2 filename mb_dir1 mb_dir2 (inc i) env
			= (filename1, env)
		  where
			doesExist` Nothing dir_path filename env
				= doesExist (appendPath dir_path filename) env
			doesExist` (Just dir_entries) _ filename env
				= (isMember filename [fileName \\ {fileName}<-dir_entries], env)
			max_mac_filename_length = 31
			toMaybe NoDirError dir = Just dir
			toMaybe _ _ =  Nothing
		macRename :: !Path !String !*env -> (!ErrCode, !*env)	| FileSystem env
		macRename path filename_after env
			#	(Just (path_up, _)) = splitPath path
				(ps_source, env) = pathToPD_String path env
				(ps_dest, env) = pathToPD_String (appendPath path_up filename_after) env
			= macRenameC ps_source ps_dest env
		splitPath (RelativePath [])
			= Nothing
		splitPath (RelativePath l)
			= case last l of
				PathUp			-> Nothing
				PathDown name	-> Just (RelativePath (l % (0, (length l) - 2)), name)
		splitPath (AbsolutePath _ [])
			= Nothing
		splitPath (AbsolutePath diskname l)
			= case last l of
				PathUp			-> Nothing
				PathDown name	-> Just (AbsolutePath diskname (l % (0, (length l) - 2)), name)
		appendPath :: !Path !String -> Path
		appendPath (RelativePath path) path_step			= RelativePath (path++[PathDown path_step])
		appendPath (AbsolutePath diskname path) path_step	= AbsolutePath diskname (path++[PathDown path_step])
		doesExist path env
			#	((err, _), env) = getFileInfo path env
			= (err==NoDirError, env)


fremove				::	!Path !*env -> (!DirError, !*env)			| FileSystem env
fremove path env
	#	(platformId, env) = getPlatformIdC dummy env
	|	isBadPath path platformId
		= (BadName, env)
	# (path_string, env) = pathToPD_String path env
	  (errCode, env) = fremoveC (path_string+++"\0") env
	= (toDirError [M_NoDirError, M_DoesntExist, M_BadName, M_NoPermission, M_NotYetRemovable] errCode, env)
	
getCurrentDirectory	::	!*env		   -> (!Path, !*env)				| FileSystem env
getCurrentDirectory env
	# (platformId, env) = getPlatformIdC dummy env
	| platformId==MacPlatform
		= mac_loop 0 [] env
	= unix_or_windows_loop (createArray 32 ' ') env
  where
	mac_loop 1 [disk_name] env
		= (AbsolutePath disk_name [], env)
	mac_loop 1 [disk_name:dirs] env
		= (AbsolutePath disk_name (map PathDown dirs), env)
	mac_loop dirID accu env
		# ((parentDirID, dir_name), env) = get_mac_dir_parent_and_name_C dirID env
		= mac_loop parentDirID [dir_name:accu] env
	unix_or_windows_loop s env
		#!s_size = size s
		# ((errCode, path_string), env) = getCurrentDirectoryC s env
		| errCode==fatalError
			= abort "StdDirectory: a fatal error occured during execution of \"getCurrentDirectory\""
		| errCode==ok
			# ((_, path), env) = pd_StringToPath path_string env
			= (path, env)
		// otherwise: string was too small
		= unix_or_windows_loop (createArray (2*s_size) ' ') env
	ok = 0
	fatalError = 2

setCurrentDirectory	::	!Path  !*env -> (!DirError, !*env)				| FileSystem env
setCurrentDirectory path env
	#	(platformId, env) = getPlatformIdC dummy env
	|	isBadPath path platformId
		= (BadName, env)
	# (path_string, env) = pathToPD_String path env
	  (errCode, env) = setCurrentDirectoryC (path_string+++"\0") env
	= (toDirError [M_NoDirError, M_DoesntExist, M_BadName, M_NoPermission] errCode, env)
	
encodeUnixModeBits	:: !UnixModeBitsField -> UnixAccessRights
encodeUnixModeBits unixModeBits
	=	{	ownerRight  =	{	mayRead		= bitTest S_IRUSR unixModeBits
							,	mayWrite	= bitTest S_IWUSR unixModeBits
							,	mayExecute	= bitTest S_IXUSR unixModeBits
							}
		,	groupRight  =	{	mayRead		= bitTest S_IRGRP unixModeBits
							,	mayWrite	= bitTest S_IWGRP unixModeBits
							,	mayExecute	= bitTest S_IXGRP unixModeBits
							}
		,	othersRight  =	{	mayRead		= bitTest S_IROTH unixModeBits
							,	mayWrite	= bitTest S_IWOTH unixModeBits
							,	mayExecute	= bitTest S_IXOTH unixModeBits
							}
		}

bitTest	mask bitField	= (mask bitand bitField) <> 0



S_IRUSR	:==	00400		/* read permission: owner */
S_IWUSR	:==	00200		/* write permission: owner */
S_IXUSR	:==	00100		/* execute permission: owner */
S_IRGRP	:==	00040		/* read permission: group */
S_IWGRP	:==	00020		/* write permission: group */
S_IXGRP	:==	00010		/* execute permission: group */
S_IROTH	:==	00004		/* read permission: other */
S_IWOTH	:==	00002		/* write permission: other */
S_IXOTH	:==	00001		/* execute permission: other */

pd_StringToPath :: !String !*env -> (!(!Bool, Path), !*env) | FileSystem env
pd_StringToPath "" env
	= ((True, RelativePath []), env)
pd_StringToPath pd_string env
	# (platformId, env)	= getPlatformIdC dummy env
	= (pd_string_to_pi_path [ch \\ ch<-:pd_string] platformId, env)
  where
	pd_string_to_pi_path pd_list MacPlatform
		| not (isMember ':' pd_list)
			= (True, RelativePath [PathDown (listToString pd_list)])
		| pd_list==[':']
			= (True, RelativePath [])
		# reverse_parts = splitInParts ':' pd_list
		  // reverse_parts has at least two elements
		  parts = remove_last_empty_and_reverse reverse_parts
		| isEmpty (hd parts)
			= (True, RelativePath (map emptyToUp (tl parts)))
		= (True, AbsolutePath (listToString (hd parts)) (map emptyToUp (tl parts)))
	  where
		emptyToUp []	= PathUp
		emptyToUp x		= PathDown (listToString x)
	pd_string_to_pi_path pd_list WindowsPlatform
		# reverse_parts = splitInParts '\\' pd_list
		  parts = remove_last_empty_and_reverse reverse_parts
		| length parts>=2 && isEmpty (hd parts) && isEmpty (hd (tl parts))
			// parse as a network path
			| length parts==2
				= (False, fail "pd_StringToPath")
			// length parts>2!
			= (True, AbsolutePath ("\\\\"+++(toString (parts!!2))) [pointsToUp part \\ part <- (tl (tl (tl parts)))])
		# until_slash = takeWhile ((<>)'\\') pd_list
		  (until_double_colon, from_double_colon) = span ((<>)':') pd_list
		| length until_double_colon<length until_slash
			// absolute path
			# diskName = listToString until_double_colon
			| isEmpty (tl from_double_colon) || hd (tl from_double_colon)<>'\\'
				// the ':' was not followed by a '\'
				= (False, fail "pd_StringToPath")
			= (True, AbsolutePath diskName (map pointsToUp (tl parts)))
		= (True, RelativePath (map pointsToUp parts))
	pd_string_to_pi_path pd_list UnixPlatform
		# reverse_parts = splitInParts '/' pd_list
		  parts = remove_last_empty_and_reverse reverse_parts
		| hd pd_list=='/'
			= (True, AbsolutePath "" (map pointsToUp (tl parts)))
		= (True, RelativePath (map pointsToUp parts))
	splitInParts delimiter l
		= split_in_parts delimiter l [] []
	  where
		split_in_parts delimiter [] partAccu partsAccu
			= [reverse partAccu :partsAccu]
		split_in_parts delimiter [h:t] partAccu partsAccu
			| h==delimiter
				= split_in_parts delimiter t [] [reverse partAccu:partsAccu]
			= split_in_parts delimiter t [h:partAccu] partsAccu
	remove_last_empty_and_reverse reverse_parts
		| isEmpty (hd reverse_parts)
			= reverse (tl reverse_parts)
		= reverse reverse_parts
	pointsToUp ['..']	= PathUp
	pointsToUp x		= PathDown (listToString x)
	
pathToPD_String	::	!Path !*env -> (!String,  !*env)	| FileSystem env 
pathToPD_String (AbsolutePath diskName path) env
	#	(platformId, env)	= getPlatformIdC dummy env
		header = case platformId of
					MacPlatform		-> diskName+++":"
					WindowsPlatform	| size diskName>=2 && diskName.[0]=='\\' && diskName.[1]=='\\'
										-> diskName+++"\\"
									-> diskName+++":\\"
					UnixPlatform	-> "/"
	|	isEmpty path
			= (header, env)
	= (header+++relative_path_to_string path platformId, env)
pathToPD_String (RelativePath path) env
	#	(platformId, env)	= getPlatformIdC dummy env
	|	isEmpty path
		= (if (platformId==MacPlatform) ":" ".", env)
	|	platformId==MacPlatform && length path==1
		= (result, env)
		with result	= case hd path of
						PathUp		-> "::"
						PathDown p	-> p
	#	relative_path_string = relative_path_to_string path platformId
	|	platformId==MacPlatform
			= (":"+++relative_path_string, env)
	= (relative_path_string, env)

relative_path_to_string path platformId
	// only for (length path>0)
	#	result = foldl1 (addDelimiter (delimiter platformId)) (map (replaceUp (up platformId)) path)
	= case (platformId, last path) of
			(MacPlatform, PathUp)	-> result+++":"
			_						-> result
  where		
	up MacPlatform		= ""
	up WindowsPlatform	= ".."
	up UnixPlatform		= ".."
	
	replaceUp _ (PathDown x)	= x
	replaceUp x PathUp			= x
	
delimiter MacPlatform		= ":"
delimiter WindowsPlatform	= "\\"
delimiter UnixPlatform		= "/"
	
bad_path_chars MacPlatform		= [':']
bad_path_chars WindowsPlatform	= ['\\','/','?','*']
bad_path_chars UnixPlatform		= ['/']
	
addDelimiter delimiter left right
	= left+++delimiter+++right

foldl1 op [h:t]
	= foldl op h t

dummy = 0

getDiskNames		::			 !*env -> ([DiskName], !*env)				| FileSystem env
getDiskNames files
	#	(platformId, files) = getPlatformIdC dummy files
	= case platformId of
		UnixPlatform	-> ([""],files)
		WindowsPlatform	#	(bits, files) = get_windows_disk_available_bits_C dummy files
							pairs = [ ((bits>>i) bitand 1, intToChar i) \\ i<-[0..25] ]
						-> ([ toString diskChar \\ (available, diskChar)<-pairs | available<>0 ], files)
		MacPlatform		-> loop 1 [] files
  where
	loop i accu files
		#	(diskName, files) = getMacDiskNameC i files
		|	diskName==""
			= (reverse accu, files)
		= loop (inc i) [diskName:accu] files
	intToChar i
		= toChar (i+a)
	a = toInt 'A'

toDirError restriction errCode
	| isMember errCode restriction
		= toDirError` errCode
	= OtherDirError

// MdM -- updated to Clean 2.0 syntax
instance == DirError
  where
	(==) NoDirError			NoDirError			= True
	(==) DoesntExist		DoesntExist			= True
	(==) BadName			BadName				= True
	(==) NotEnoughSpace		NotEnoughSpace		= True
	(==) AlreadyExists		AlreadyExists		= True
	(==) NoPermission		NoPermission		= True
	(==) MoveIntoOffspring	MoveIntoOffspring	= True
	(==) MoveAcrossDisks	MoveAcrossDisks		= True
	(==) NotYetRemovable	NotYetRemovable		= True
	(==) OtherDirError		OtherDirError		= True
	(==) _					_					= False
// ... MdM

toDirError` M_NoDirError		= NoDirError
toDirError` M_OtherDirError		= OtherDirError
toDirError` M_DoesntExist		= DoesntExist
toDirError` M_BadName			= BadName
toDirError` M_NotEnoughSpace 	= NotEnoughSpace
toDirError` M_AlreadyExists 	= AlreadyExists
toDirError` M_NoPermission		= NoPermission
toDirError` M_MoveIntoOffspring = MoveIntoOffspring
toDirError` M_MoveAcrossDisks 	= MoveAcrossDisks
toDirError` M_NotYetRemovable	= NotYetRemovable

M_NoDirError		:==  0
M_OtherDirError		:== -1
M_DoesntExist		:== -2
M_BadName			:== -3
M_NotEnoughSpace	:== -4
M_AlreadyExists		:== -5
M_NoPermission		:== -6
M_MoveIntoOffspring	:==	-7
M_MoveAcrossDisks	:==	-8
M_NotYetRemovable	:== -9

////////////////////////// low level stuff ////////////////////////////////

findFirstFileC	::	!String !*env	-> (!ErrCode, !*env)
findFirstFileC _ _
	= code
		{
			ccall findFirstFileC "S:I:A"
		}

findNextFileC	::	!Int !*env	-> (!ErrCode, !*env)
// first parameter is a dummy
findNextFileC _ _
	= code
		{
			ccall findNextFileC "I:I:A"
		}

getCommonFileInfoC	::	!Bool !*env
				->	(!(!String, !(!Int, !Int), !DateTimeTuple, !Bool, !Bool), !*env)
getCommonFileInfoC _ _
	= code
		{
			ccall getCommonFileInfoC "I:VSIIIIIIIIIII:A"
		}

getUnixFileInfoC	::	!*env
					->	(!(!Int, !Int, !Int, !DateTimeTuple), !*env)
getUnixFileInfoC _
	= code
		{
			ccall getUnixFileInfoC ":VIIIIIIIIII:A"
		}

getWindowsFileInfoC	::	!*env
					->	(!(!Int, !DateTimeTuple, !DateTimeTuple, !String, !Bool), !*env)
getWindowsFileInfoC _
	= code
		{
			ccall getWindowsFileInfoC ":VIIIIIIIIIIIIIIISI:A"
		}					

getMacFileInfoC	::	!*env
				->	(!(!DateTimeTuple, !DateTimeTuple, !Bool, !String, !String, !Int),!*env)
getMacFileInfoC _
	= code
		{
			ccall getMacFileInfoC ":VIIIIIIIIIIIIIIISSI:A"
		}

closeSearchC ::	!*env -> *env
closeSearchC _
	= code
		{
			ccall closeSearchC ":V:A"
		}

findSingleFileC	::	!String !*env	-> (!ErrCode, !*env)
findSingleFileC _ _
	= code
		{
			ccall findSingleFileC "S:I:A"
		}

closeSingleSearchC ::	!*env -> *env
closeSingleSearchC _
	= code
		{
			ccall closeSingleSearchC ":V:A"
		}

createDirectoryC :: !String !*env -> (!ErrCode, !*env)
createDirectoryC _ _
	= code
		{
			ccall createDirectoryC "S:I:A"
		}

fmoveC	::	!Bool !String !String !*env -> (!ErrCode, !*env)
fmoveC _ _ _ _
	= code
		{
			ccall fmoveC "ISS:I:A"
		}


macMoveC	::	!String !String !*env -> (!ErrCode, !*env)
macMoveC _ _ _
	= code
		{
			ccall macMoveC "SS:I:A"
		}

macRenameC	::	!String !String !*env -> (!ErrCode, !*env)
macRenameC _ _ _
	= code
		{
			ccall macRenameC "SS:I:A"
		}


fremoveC	::	!String  !*env -> (!ErrCode, !*env)
fremoveC _ _
	= code
		{
			ccall fremoveC "S:I:A"
		}

getCurrentDirectoryC :: !*String !*env -> (!(!Int, !String), !*env)
// Int result: 0=ok, 1="string was too small", 2="fatal error"
getCurrentDirectoryC s env
	= (getCurrentDirectory_SE s s, env)

getCurrentDirectory_SE	:: !String !string		   -> (!Int, !string)
// SE stands for "sideeffect"
getCurrentDirectory_SE _ _
	= code
		{
			ccall getCurrentDirectory_SE "S:I:A"
		}

get_mac_dir_parent_and_name_C :: !Int !*env -> (!(!Int, !String), !*env)
get_mac_dir_parent_and_name_C dirID env
	= code
		{
			ccall get_mac_dir_parent_and_name_C "I:VIS:A"
		}

setCurrentDirectoryC :: !String !*env -> (!ErrCode, !*env)
setCurrentDirectoryC _ _
	= code
		{
			ccall setCurrentDirectoryC "S:I:A"
		}

getPlatformIdC :: !Int !*env -> (!Int, !*env)
getPlatformIdC _ _
	= code
		{
			ccall getPlatformIdC "I:I:A"
		}

get_windows_disk_available_bits_C :: !Int !*env -> (!Int, !*env)
get_windows_disk_available_bits_C _ _
	= code
		{
			ccall get_windows_disk_available_bits_C "I:I:A"
		}

getMacDiskNameC :: !Int !*env -> (!String, !*env)
getMacDiskNameC diskIndex _
	= code
		{
			ccall getMacDiskNameC "I:VS:A"
		}


UnixPlatform	:== 0	// C functions rely on these values
WindowsPlatform	:== 1
MacPlatform		:== 2

FILE_ATTRIBUTE_SYSTEM		:==	0x00000004
FILE_ATTRIBUTE_ARCHIVE		:==	0x00000020

::	ErrCode			:== Int	// <>0 <=> error
::	DateTimeTuple	:== (!DateTuple, !TimeTuple)
::	DateTuple		:== (!Int, !Int, !Int, !Int)
::	TimeTuple		:== (!Int, !Int, !Int)

::	Path
	=	RelativePath [PathStep]
	|	AbsolutePath DiskName [PathStep]

::	PathStep	= PathUp | PathDown String
::	DiskName	:== String

::	DirError = NoDirError | DoesntExist | BadName | NotEnoughSpace | AlreadyExists | NoPermission
				| MoveIntoOffspring | MoveAcrossDisks | NotYetRemovable | OtherDirError

::	DirEntry
	=	{	fileName		::	!String
		,	fileInfo		::	!FileInfo
		}

::	FileInfo
	=	{	pi_fileInfo		::	!PI_FileInfo
		,	pd_fileInfo		::	!PD_FileInfo
		}

::	PI_FileInfo							//	platform independent information
	=	{	fileSize		::	!(!Int, !Int)		// MdM
		,	lastModified	::	!DateTime
		,	isDirectory		::	!Bool
		,	isReadOnly		::	!Bool
		}

::	PD_FileInfo	:== PlatformDependent UnixFileInfo WindowsFileInfo MacFileInfo
											//	platform dependent information
::	PlatformDependent unix windows mac
	=	Unix	unix
	|	Windows	windows
	|	Mac		mac

::	UnixFileInfo
	= 	{	unixModeBitsField	::	!UnixModeBitsField
		,	unixOwnerUserId		::	Int
		,	unixOwnerGroupId	::	Int
		,	unixLastAccessTime	::	!DateTime
		}
		
::	UnixModeBitsField	:==	Int

::	UnixAccessRights
	=	{	ownerRight			::	UnixRight
		,	groupRight			::	UnixRight
		,	othersRight			::	UnixRight
		}

::	UnixRight
	=	{	mayRead				::	!Bool
		,	mayWrite			::	!Bool
		,	mayExecute			::	!Bool
		}
		
::	WindowsFileInfo
	=	{	winCreationTime		::	!DateTime
		,	winLastAccessTime	::	!DateTime
		,	winDOSName			::	!String
		,	winIsHidden			::	!Bool
		,	winIsArchiveFile	::	!Bool
		,	winIsSystemFile		::	!Bool
		}

::	MacFileInfo
	=	{	macCreationTime		::	!DateTime
		,	macBackupTime		::	!DateTime
		,	macIsHidden			::	!Bool
		,	macFDFlags			::	!Int	//	finder information from FInfo/DInfo record
		,	macFurtherInfo		::	!ForFile MacFurtherInfo
		}

::	MacFurtherInfo	:==	(!MacCreator, !MacFileType)

::	MacCreator	:==	String		//	always four characters
::	MacFileType	:==	String		//	always four characters

::	MoveMode	= OverwriteFile | DontOverwrite
::	DateTime	:==	(!Date, !Time)

::	ForFile x
	=	File x
	|	Directory

