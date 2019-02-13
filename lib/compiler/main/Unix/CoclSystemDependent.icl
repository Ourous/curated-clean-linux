// this is for Unix
implementation module CoclSystemDependent

import StdEnv
import StdDebug
import ArgEnv
import ipc
from filesystem import ensureDirectoryExists

import code from "cDirectory.o"
import code from "ipc_c.o"

PathSeparator
	:==	':'
DirectorySeparator
	:== '/'

SystemDependentDevices :: [a]
SystemDependentDevices
		=	[]

SystemDependentInitialIO :: [a]
SystemDependentInitialIO
		=	[]

set_compiler_id :: Int -> Int
set_compiler_id compiler_id = compiler_id

ensureCleanSystemFilesExists :: !String !*Files -> (!Bool, !*Files)
// returned bool: now there is such a subfolder
ensureCleanSystemFilesExists path env
	= ensureDirectoryExists path env

	
compiler_loop :: ([{#Char}] *st -> *(Bool, *st)) *st -> (!Bool, !*st)
compiler_loop compile compile_state
	| length commandArgs==3 && commandArgs!!0=="--pipe"
		# commands_name= (commandArgs!!1);
		# results_name= (commandArgs!!2);
		= (True,compile_loop compile commands_name results_name compile_state)
		# (r,compile_state)=compile commandArgs compile_state
		= (r,compile_state)
	where
		commandArgs
			=	tl [arg \\ arg <-: getCommandLine]
// ... Unix

string_to_args string
	= string_to_args 0;
	where
		l=size string;
		
		string_to_args i
			# end_spaces_i=skip_spaces i;
			| end_spaces_i==l
				= []
			| string.[end_spaces_i]=='"'
				# next_double_quote_i=skip_to_double_quote (end_spaces_i+1)
				| next_double_quote_i>=l
					= [string % (end_spaces_i,l-1)]
					# arg=string % (end_spaces_i+1,next_double_quote_i-1);
					= [arg : string_to_args (next_double_quote_i+1)];
				# space_i=skip_to_space (end_spaces_i+1)
				| space_i>=l
					= [string % (end_spaces_i,l-1)]
					# arg=string % (end_spaces_i,space_i-1);
					= [arg : string_to_args (space_i+1)];

		skip_spaces i
			| i>=l
				= l;
			# c=string.[i];
			| c==' ' || c=='\t'
				= skip_spaces (i+1);
				= i;

		skip_to_space i
			| i>=l
				= l;
			# c=string.[i];
			| c==' ' || c=='\t'
				= i;
				= skip_to_space (i+1);

		skip_to_double_quote i
			| i>=l
				= l;
			# c=string.[i];
			| c=='"'
				= i;
				= skip_to_double_quote (i+1);


compile_loop :: ([{#Char}] *st -> *(Bool, *st)) {#Char} {#Char} *st -> *st
compile_loop compile commands results compile_state
	# r=open_pipes commands results;
	| r<>0
		= abort ("compile_loop\n");
	=	compile_files compile compile_state

compile_files :: ([{#Char}] *st -> *(Bool, *st)) *st -> *st
compile_files compile compile_state
	# n = get_command_length;
	| n==(-1)
		= abort "compile_files 1";
	# string=createArray n '\0';
	# r=get_command string;
	| r<>0
		= abort ("compile_files 2 ");
	# args=string_to_args (string % (0,size string-2))
	= case args of
		["cocl":cocl_args]
			# (ok,compile_state)=compile cocl_args compile_state
			# result=if ok 0(-1);
			# r=send_result result
			| r<>0
				-> abort "compile_files 3";
				-> compile_files compile compile_state
		["quit"]
			-> /* trace_n "quiting" */ compile_state;
		_
				-> abort "compile_files 4"
