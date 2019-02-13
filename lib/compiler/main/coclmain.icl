implementation module coclmain

import StdEnv
import StdDebug
import ArgEnv
import set_return_code
import CoclSystemDependent

import compile,utilities

coclMain :: ![{#Char}] !*World -> *World
coclMain testArgs world
	# world
		=	set_return_code 0 world
	# (commandArgs, world)
		=	getCommandArgs (tl [arg \\ arg <-: getCommandLine]) testArgs world 
	# (symbol_table,world)
		= init_identifiers newHeap world
	# (success, world)
		=	accFiles (compiler symbol_table) world
	=	set_return_code (if success 0(-1)) world
	where
		getCommandArgs :: [{#Char}] [{#Char}] *World -> ([{#Char}], *World)
		getCommandArgs [] testArgs world
			=	getArgs testArgs world
		getCommandArgs realArgs _ world
			=	getArgs realArgs world

		getArgs :: [{#Char}] *World -> ([{#Char}], *World)
		getArgs ["--dump-args" : commandArgs] world
			# (opened, file, world)
				=	fopen CoclArgsFile FWriteText world
			| not opened
				=	abort ("--dump-args " +++ CoclArgsFile +++ " could not be opened\n")
			# file
				=	foldSt (\s -> fwritec '\n' o fwrites s) commandArgs file
			# (closed, world)
				=	fclose file world
			| not closed
				=	abort ("--dump-args " +++ CoclArgsFile +++ " could not be closed\n")
			=	(commandArgs, world)
		getArgs ["--restore-args"] world
			# (opened, file, world)
				=	fopen CoclArgsFile FReadText world
			| not opened
				=	abort ("--restore-args " +++ CoclArgsFile +++ " could not be opened\n")
			# (commandArgs, file)
				=	readArgs [] file
			# (closed, world)
				=	fclose file world
			| not closed
				=	abort ("--restore-args " +++ CoclArgsFile +++ " could not be closed\n")
			=	(commandArgs, world)
			where
				readArgs :: [{#Char}] *File -> ([{#Char}], *File)
				readArgs reversedArgs file
					# (arg, file)
						=	freadline file
					| arg == ""
						=	(reverse reversedArgs, file)
					// otherwise
						=	readArgs [chopNewline arg : reversedArgs] file

				chopNewline :: {#Char} -> {#Char}
				chopNewline s
					| s.[n-1] == '\n'
						=	s % (0, n-2)
					// otherwise
						=	s
					where
						n
							=	size s
		getArgs commandArgs world
			=	(commandArgs, world)

CoclArgsFile :== "coclargs.txt"

// Unix
compile2 args (cache, files)
	# (r, cache, files)
		=	compile args cache files
	= (r, (cache, files))

compiler symbol_table files
	# dcl_cache = empty_cache symbol_table
	# (r,(_,files))
		=	compiler_loop compile2 (dcl_cache, files)
	= (r, files)
