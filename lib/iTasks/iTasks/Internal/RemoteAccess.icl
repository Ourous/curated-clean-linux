implementation module iTasks.Internal.RemoteAccess

import StdString, StdMisc, StdFile, StdBool, StdArray
import Text

import iTasks.Engine
import iTasks.WF.Definition
import iTasks.Internal.Task
import iTasks.Internal.IWorld

import Data.Maybe, Internet.HTTP, Text.URI, Data.Error
import System.OS, System.FilePath, System.File

from System.Process			import qualified ::ProcessHandle, runProcess, checkProcess,callProcess
from System.Process			import :: ProcessHandle(..)

from iTasks.Extensions.Document import :: FileException(..), instance toString FileException

CHUNK_SIZE :== 1048576 // 1M

fileTask filename f iworld=:{IWorld|current={taskTime},world}
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

openException s	
	# e = FileException s CannotOpen
	= Error (dynamic e, toString e)
closeException s
	# e = FileException s CannotClose
	= Error (dynamic e, toString e)

httpRequest_server :: !HTTPMethod !URI !String !*IWorld -> *(!HTTPResponse, !*IWorld)
httpRequest_server method uri request iworld=:{IWorld|current={taskTime},options={tempDirPath},world}
		# infile  = tempDirPath </> (mkFileName "request")
		# outfile = tempDirPath </> (mkFileName "response")
		# hfile = tempDirPath </> (mkFileName "response-header")		
		# (res,world) = writeFile infile request world
		| isError res
			= abort "httpRequest_server: infile creation error"
		# cmd	= IF_POSIX_OR_WINDOWS "/usr/bin/curl" ("Tools" </> "Curl" </> "curl.exe" )
		# args	=	[ options
						, "--data-binary"
						, "@" +++ infile
						, "-s"
						, "-o"
						, outfile
						, "-D" 
						, hfile						
						, url
						]
		# (res,world) = 'System.Process'.callProcess cmd args Nothing world
		| isError res
			= abort "httpRequest_server: callProcess failed"
		# (res1, iworld) = fileTask outfile readAll {IWorld|iworld & world = world}
		| isError res1
			= abort "httpRequest_server: reading output file failed"
		# (res2, iworld) = fileTask hfile readAll iworld
		| isError res2
			= abort "httpRequest_server: reading header output file failed"			
		= case parseResponse (fromOk res2 +++ fromOk res1) of
			(Just res) = (res, iworld)
			Nothing    = abort "httpRequest_server: invalid response"	
where	
	mkFileName :: !String -> String
	mkFileName part = toString taskTime +++ "-rpc-" +++ part

	url = toString uri

	options	= case method of
		HTTP_GET	= "--get"
		HTTP_POST 	= ""
		HTTP_PUT	= "-XPUT"

httpRequest :: !HTTPMethod !URI !(Maybe String) !IWorld -> (!HTTPResponse, !IWorld)
httpRequest method uri mbBody iworld=:{onClient = True}
	= httpRequest_client (toString method) (toString uri) mbBody iworld

httpRequest method uri Nothing iworld
	= httpRequest_server method uri "" iworld
httpRequest method uri (Just body) iworld
	= httpRequest_server method uri body iworld

// For easy override on the client, dont touch it!
httpRequest_client method url mbBody iworld = undef

