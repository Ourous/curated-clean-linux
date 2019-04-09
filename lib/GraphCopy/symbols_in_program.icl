implementation module symbols_in_program;

// for ELF

import StdEnv;
import _SystemStrictLists;

:: SectionHeaders = {
	symbol_table_offset :: !Int,
	symbol_table_size :: !Int,
	string_table_section_n :: !Int,
	first_non_local_symbol :: !Int,
	string_table_offset :: !Int,
	string_table_size :: !Int
   };

:: Symbol = { symbol_name :: !String, symbol_value :: !Int};

exported_clean_symbol :: !Int !{#Char} -> Bool;
exported_clean_symbol i s
	| i==0
		= False;
	| s.[i]=='e' && s.[i+1]=='_' && s.[i+2]=='_'
		= True;
	| s.[i]=='_' && s.[i+1]=='_'
		| s.[i+2]=='S' && s.[i+3]=='T' && s.[i+4]=='R' && s.[i+5]=='I' && s.[i+6]=='N' && s.[i+7]=='G' &&
		  s.[i+8]=='_' && s.[i+9]=='_' && s.[i+10]=='\0'
			= True;
		| s.[i+2]=='A' && s.[i+3]=='R' && s.[i+4]=='R' && s.[i+5]=='A' && s.[i+6]=='Y' &&
		  s.[i+7]=='_' && s.[i+8]=='_' && s.[i+9]=='\0'
			= True;
		| s.[i+2]=='C' && s.[i+3]=='o' && s.[i+4]=='n' && s.[i+5]=='s'
			| s.[i+6]=='\0'
				= True;
			| s.[i+6]=='i' || s.[i+6]=='c' || s.[i+6]=='r' || s.[i+6]=='b' || s.[i+6]=='f'
				| s.[i+7]=='\0'
					= True;
				| s.[i+7]=='t' && s.[i+8]=='s' && s.[i+9]=='\0'
					= True;
				= False;
			| s.[i+6]=='a' && s.[i+7]=='\0'
				= True;
			= False;
		| s.[i+2]=='N' && s.[i+3]=='i' && s.[i+4]=='l' && s.[i+5]=='\0'
			= True;
		| s.[i+2]=='T' && s.[i+3]=='u' && s.[i+4]=='p' && s.[i+5]=='l' && s.[i+6]=='e' && s.[i+7]=='\0'
			= True;
			= False;
	| IF_INT_64_OR_32
		(s.[i]=='d' && s.[i+1]=='I' && s.[i+2]=='N' && s.[i+3]=='T' && s.[i+4]=='\0')
		(s.[i]=='I' && s.[i+1]=='N' && s.[i+2]=='T' && s.[i+3]=='\0')
		= True;
	| s.[i]=='C' && s.[i+1]=='H' && s.[i+2]=='A' && s.[i+3]=='R' && s.[i+4]=='\0'
		= True;
	| s.[i]=='R' && s.[i+1]=='E' && s.[i+2]=='A' && s.[i+3]=='L' && s.[i+4]=='\0'
		= True;
	| s.[i]=='B' && s.[i+1]=='O' && s.[i+2]=='O' && s.[i+3]=='L' && s.[i+4]=='\0'
		= True;
	| s.[i]=='A' && s.[i+1]=='R' && s.[i+2]=='R' && s.[i+3]=='A' && s.[i+4]=='Y' && s.[i+5]=='\0'
		= True;
	| s.[i]=='n' && s.[i+1]=='_' && s.[i+2]=='_'
		| s.[i+3]=='S' && s.[i+4]=='_' && s.[i+5]=='P' && s.[i+6]>='1' && s.[i+6]<='6' && s.[i+7]=='\0'
			= True;
		| s.[i+3]=='C' && s.[i+4]=='o' && s.[i+5]=='n' && s.[i+6]=='s'
			| s.[i+7]=='s'
				| s.[i+8]=='\0'
					= True;
				| s.[i+8]=='t' && s.[i+9]=='s' && s.[i+10]=='\0'
					= True;
					= False;
			| s.[i+7]=='t' && s.[i+8]=='s' && s.[i+9]=='\0'
				= True;
				= False;
			= False;
		= False;

skip_to_null_char i s
	| i<size s && s.[i]<>'\0'
		= skip_to_null_char (i+1) s;
		= i;

string_from_string_table i s
	# e = skip_to_null_char i s;
	= s % (i,e-1);

freadi64 :: !*File -> (!Bool,!Int,!*File);
freadi64 exe_file
	# (ok, i1, exe_file) = freadi exe_file;
	| not ok
		= (False, 0, exe_file);
	# (ok, i2, exe_file) = freadi exe_file;
	| not ok
		= (False, 0, exe_file);
	= (True, (i2 << 32) + i1, exe_file);

read_section_headers :: !Int !Int !SectionHeaders !*File -> (!SectionHeaders,!*File);
read_section_headers section_n n_section_headers section_headers exe_file
	| section_n<n_section_headers
		# (ok,i0,exe_file) = freadi exe_file;
		# (ok,section_type,exe_file) = freadi exe_file;
		# (ok,sh_flags,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		# (ok,sh_addr,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		# (ok,sh_offset,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		# (ok,sh_size,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		# (ok,sh_link,exe_file) = freadi exe_file;
		# (ok,sh_info,exe_file) = freadi exe_file;
		# (ok,sh_addralign,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		# (ok,sh_entsize,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
		| section_type==2
			# section_headers & symbol_table_offset = sh_offset, symbol_table_size = sh_size,
								string_table_section_n = sh_link, first_non_local_symbol = sh_info;
			= read_section_headers (section_n+1) n_section_headers section_headers exe_file;
		| section_type==3
			| section_n==section_headers.string_table_section_n
				# section_headers & string_table_offset = sh_offset, string_table_size = sh_size;
				= read_section_headers (section_n+1) n_section_headers section_headers exe_file;
				= read_section_headers (section_n+1) n_section_headers section_headers exe_file;
			= read_section_headers (section_n+1) n_section_headers section_headers exe_file;
	= (section_headers,exe_file);

read_symbol_table symbol_n symbol_table_size string_table symbols exe_file
	= IF_INT_64_OR_32 
		(read_symbol_table64 symbol_n symbol_table_size string_table symbols exe_file) 
		(read_symbol_table32 symbol_n symbol_table_size string_table symbols exe_file);

read_symbol_table32 symbol_n symbol_table_size string_table symbols exe_file
	| symbol_n<<4 < symbol_table_size
		# (ok,i0,exe_file) = freadi exe_file;
		# (ok,i1,exe_file) = freadi exe_file;
		# (ok,i2,exe_file) = freadi exe_file;
		#! (ok,i3,exe_file) = freadi exe_file;
		| not (exported_clean_symbol i0 string_table)
			= read_symbol_table32 (symbol_n+1) symbol_table_size string_table symbols exe_file;
		# object = i3 bitand 0xf;
		  bind = (i3>>4) bitand 0xf;
		| (object==1 /*OBJECT*/ || object==2 /*FUNC*/ || object==0 /*NOTYPE*/) &&
		  bind==1 /* GLOBAL */
			# symbol_name = string_from_string_table i0 string_table;
			# symbols = [(symbol_name,i1):symbols];
			= read_symbol_table32 (symbol_n+1) symbol_table_size string_table symbols exe_file;
		= read_symbol_table32 (symbol_n+1) symbol_table_size string_table symbols exe_file;
	= (symbols,exe_file);

read_symbol_table64 symbol_n symbol_table_size string_table symbols exe_file
	| symbol_n*24 < symbol_table_size
		# (ok,st_name,exe_file) = freadi exe_file;
		# (ok,i1,exe_file) = freadi exe_file;
		# (ok,st_value,exe_file) = freadi64 exe_file;
		#! (ok,st_size,exe_file) = freadi64 exe_file;
		| not (exported_clean_symbol st_name string_table)
			= read_symbol_table64 (symbol_n+1) symbol_table_size string_table symbols exe_file;
		# object = i1 bitand 0xf;
		  bind = (i1>>4) bitand 0xf;
		| (object==1 /*OBJECT*/ || object==2 /*FUNC*/ || object==0 /*NOTYPE*/) &&
		  bind==1 /* GLOBAL */
			# symbol_name = string_from_string_table st_name string_table;
			# symbols = [(symbol_name,st_value):symbols];
			= read_symbol_table64 (symbol_n+1) symbol_table_size string_table symbols exe_file;
		= read_symbol_table64 (symbol_n+1) symbol_table_size string_table symbols exe_file;
	= (symbols,exe_file);

read_symbols :: !{#Char} !*Files -> (!{#Symbol},!*Files);
read_symbols file_name files
	# (ok,exe_file,files) = fopen file_name FReadData files;
	| not ok
		= abort ("Could not open file "+++file_name);
	# (ok,c,exe_file) = freadc exe_file
	| not ok || c<>'\x7f'
		= abort "Not an ELF file (error in header)";
	# (ok,c,exe_file) = freadc exe_file
	| not ok || c<>'E'
		= abort "Not an ELF file (error in header)";
	# (ok,c,exe_file) = freadc exe_file
	| not ok || c<>'L'
		= abort "Not an ELF file (error in header)";
	# (ok,c,exe_file) = freadc exe_file
	| not ok || c<>'F'
		= abort "Not an ELF file (error in header)";

	# (ok,exe_file) = fseek exe_file (IF_INT_64_OR_32 40 32) FSeekSet;
	# (ok,section_headers_offset,exe_file) = IF_INT_64_OR_32 (freadi64 exe_file) (freadi exe_file);
	# (ok,exe_file) = fseek exe_file (IF_INT_64_OR_32 60 48) FSeekSet;
	| not ok
		= abort "fseek failed";
	# (ok,i ,exe_file) = freadi exe_file;
	# n_section_headers = i bitand 0xffff;
	# string_table_section_header_n = (i>>16) bitand 0xffff;
	# (ok,exe_file) = fseek exe_file section_headers_offset FSeekSet;
	| not ok
		= abort "fseek failed";
	# section_headers = {	symbol_table_offset = 0,
							symbol_table_size = 0,
							string_table_section_n = -1,
							first_non_local_symbol = 0,
							string_table_offset = 0,
							string_table_size = 0
				 		};
	# (section_headers,exe_file)
		= read_section_headers 0 n_section_headers section_headers exe_file;
	# (ok,exe_file) = fseek exe_file section_headers.string_table_offset FSeekSet;
	| not ok
		= abort "fseek failed";
	# (string_table,exe_file) = freads exe_file section_headers.string_table_size;
	| size string_table<>section_headers.string_table_size
		= abort "reading symbol table failed";
	# section_n = section_headers.first_non_local_symbol;
	# offset = section_headers.symbol_table_offset + (IF_INT_64_OR_32 (section_n*24) (section_n<<4));
	# (ok,exe_file) = fseek exe_file offset FSeekSet;
	| not ok
		= abort "fseek failed";
	# (symbols,exe_file)
		= read_symbol_table section_n section_headers.symbol_table_size string_table [] exe_file;
	# symbols = sortBy (\(s1,_) (s2,_) -> s1<s2) symbols;
	# symbols = {#{symbol_name=s,symbol_value=v} \\ (s,v)<-symbols};
	# (ok,files) = fclose exe_file files;
	= (symbols,files);

get_symbol_value :: !{#Char} !{#Symbol} -> Int;
get_symbol_value symbol_name symbols
	= find_symbol 0 (size symbols) symbol_name symbols;
{
	find_symbol :: !Int !Int !{#Char} !{#Symbol} -> Int;
	find_symbol left right s symbols
		| left<right
			# m = left+((right-left)>>1);
			# s_name = symbols.[m].symbol_name;
			| s==s_name
				= symbols.[m].symbol_value;
			| s<s_name
				= find_symbol left m s symbols;
				= find_symbol (m+1) right s symbols;
			= -1;
}

