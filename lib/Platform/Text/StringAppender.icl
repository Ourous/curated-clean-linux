implementation module Text.StringAppender

import StdString, StdArray, StdInt, StdFile, StdList
import Data.Error, System.File

:: StringAppender = { elements 		:: [String]
		 		    , full_length 	:: Int
		 		    }

newAppender :: StringAppender		  
newAppender = {full_length = 0, elements = []}		  
		   
append :: !StringAppender a -> StringAppender | toString a
append appender a 
	#! str = toString a
	#! new_length = appender.full_length + (size str)
	= {full_length = new_length, elements = [str:appender.elements]} 
	
concat_rev :: ![String] !Int -> String
concat_rev xs full_length = concat` xs (createArray full_length '\0') 0
where
	concat` []     dst _		= dst
	concat` [x:xs] dst offset	= concat` xs (copyChars (full_length - offset - (size x)) 0 (size x) x dst) (offset + size x)
	
	copyChars offset i num src dst
	| i == num		= dst
	| otherwise		= copyChars offset (i+1) num src {dst & [offset + i] = src.[i]}	
	
joinList :: !String ![a] !StringAppender -> StringAppender | toString a
joinList sep [t] a = append a t
joinList sep [t:ts] a = joinList sep ts (append (append a t) sep)
joinList sep [] a = a	

intoFile :: !StringAppender !*File -> (!MaybeError FileError (), !*File)
intoFile {elements} file = foldl wrt (Ok (), file) (reverse elements)
where
	wrt (Ok (), file) str 
		# file = fwrites str file
		# (error, file) = ferror file
		| error
			= (Error IOError, file)
			= (Ok (), file)
	wrt (e, file) _ = (e, file)

instance toString StringAppender			   
where
	toString appender = concat_rev appender.elements appender.full_length

instance Appendable String
where
	(<++) a b = append a b

instance Appendable Int
where
	(<++) a b = append a b

instance Appendable Real
where
	(<++) a b = append a b

instance Appendable (StringAppender -> StringAppender)
where
	(<++) a f = f a
