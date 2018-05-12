implementation module Text.CSV

import StdEnv
import Data.Maybe
import Text

BUFF_SIZE :== 1024

readCSVRecord :: !*File -> (!Maybe [String],!*File)
readCSVRecord file = readCSVRecordWith ',' '"' '\\' file

readCSVRecordWith :: !Char !Char !Char !*File -> (!Maybe [String],!*File)
readCSVRecordWith delimitChar quoteChar escapeChar file
	// Read until line delimiter or end of file
	# (remain,fields,file) = read "" 0 0 False False [] file
	| remain == -1
		= (Nothing,file)
	| otherwise
		// Reset file position to start of new record
		# (_,file)			= fseek file (~ remain) FSeekCur
		= (Just (reverse fields),file)
where
	read :: !{#Char} !Int !Int !Bool !Bool ![{#Char}] !*File -> (!Int,![{#Char}],!*File)
	read buffer start cur quoted escape fields file
		//End of buffer before record ends (add more to the buffer)
		| cur == size buffer
			# (chunk,file) = freads file BUFF_SIZE
			| size chunk == 0 //EOF
				//When nothing was read at all, return -1 to indicate this
				| cur == 0 		= (-1,[],file)
				//An eof if fields have been read already is treated as end of a record
				| otherwise		= (0,[field:fields], file)
			| otherwise
				= read (buffer +++ chunk) start cur quoted escape fields file
		//End of field
		| buffer.[cur] == delimitChar && not quoted && not escape
			= read buffer next next quoted False [field:fields] file 
		//End of line
		| (buffer.[cur] == '\n' || buffer.[cur] == '\r') && not quoted
			= (remain, [field:fields], file)
		//Start of a quoted field
		| buffer.[cur] == quoteChar && cur == start
			= read buffer start next True False fields file
		//End of a quoted field
		| buffer.[cur] == quoteChar && quoted && not escape
			= read buffer start next False False fields file
		//Trigger escape
		| buffer.[cur] == escapeChar && not escape
			= read buffer start next False True fields file
		//Simply advance the cursor
		| otherwise
			= read buffer start next quoted False fields file
	where
		next		= inc cur
		field		= if isQuoted quotedField normalField
		isQuoted	= buffer.[start] == quoteChar && buffer.[cur - 1] == quoteChar 
		normalField = unescape (buffer % (start, cur - 1))
		quotedField = unescape (buffer % (start + 1, cur - 2))
		
		remain		= size buffer - next
		
		//Remove all {escapeChar,quoteChar} and {escapeChar,escapeChar} combinations from a string
		unescape s	= {c \\ c <- (unescape` [u \\ u <-: s])}
		where
			unescape` [char1,char2:rest] 
				| char1 == escapeChar	= unescape` [char2:rest]
										= [char1:unescape` [char2:rest]]
			unescape` [char]			= [char]
			unescape` []				= []
			
readCSVFile :: !*File -> (![[String]],!*File)
readCSVFile file = readCSVFileWith ',' '"' '\\' file

readCSVFileWith :: !Char !Char !Char !*File -> (![[String]],!*File)
readCSVFileWith delimitChar quoteChar escapeChar file
	# (mbRec,file) = readCSVRecordWith delimitChar quoteChar escapeChar file
	= case mbRec of
		Nothing	= ([],file)
		Just rec
			# (recs,file) = readCSVFileWith delimitChar quoteChar escapeChar file
			= ([rec:recs],file)


writeCSVRecord :: ![String] !*File -> *File
writeCSVRecord fields file = writeCSVRecordWith ',' '"' '\\' fields file

writeCSVRecordWith :: !Char !Char !Char ![String] !*File -> *File
writeCSVRecordWith delimitChar quoteChar escapeChar fields file
	= fwrites line file
where
	line			= (join {delimitChar} [{quoteChar} +++ escape field +++ {quoteChar} \\ field <- fields]) +++ "\r\n"
	escape s		= {c \\ c <- flatten [escape` u \\ u <-: s]}
	where
		escape` c
			| c == escapeChar	= [escapeChar,escapeChar]
			| c == quoteChar	= [escapeChar,quoteChar]
								= [c]
		
writeCSVFile :: ![[String]] !*File -> *File
writeCSVFile fields file = writeCSVFileWith ',' '"' '\\' fields file

writeCSVFileWith :: !Char !Char !Char ![[String]] !*File -> *File
writeCSVFileWith delimitChar quoteChar escapeChar fields file = foldl (flip (writeCSVRecordWith delimitChar quoteChar escapeChar)) file fields
