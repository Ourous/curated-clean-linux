implementation module Database.SQL.RelationalMapping

import StdEnv, StdGeneric
import Data.Maybe, Database.SQL, Text

//Wrapper functions
mapRead :: !ref !*cur -> (!(Maybe MappingError), !(Maybe val), !*cur) | relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapRead ref cursor
	# (mbErr,_,_,tokens,cursor)		= relMap{|*|} RelMapInit 0 (Just ref) [] [] cursor
	| isJust mbErr		= (mbErr, Nothing, cursor)
	# (mbErr,mbVal,_,tokens,cursor)	= relMap{|*|} RelMapRead 0 Nothing [] tokens cursor
	| isEmpty tokens	= (mbErr, mbVal, cursor)
						= (mbErr, Nothing, cursor)
mapCreate :: !val !*cur -> (!(Maybe MappingError), !(Maybe ref), !*cur) | relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapCreate val cursor
	# (mbErr,_,_,tokens,cursor)		= relMap{|*|} RelMapCreate 0 (Just val) [] [] cursor
	| isJust mbErr		= (mbErr, Nothing, cursor)
	# (mbErr,mbRef,_,tokens,cursor)	= relMap{|*|} RelMapRead 0 Nothing [] tokens cursor
	| isEmpty tokens	= (mbErr, mbRef, cursor)
						= (mbErr, Nothing, cursor)

mapUpdate :: !val !*cur -> (!(Maybe MappingError), !(Maybe ref), !*cur) | relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapUpdate val cursor
	# (mbErr,_,_,tokens,cursor)		= relMap{|*|} RelMapUpdate 0 (Just val) [] [] cursor
	| isJust mbErr		= (mbErr, Nothing, cursor)
	# (mbErr,mbRef,_,tokens,cursor)	= relMap{|*|} RelMapRead 0 Nothing [] tokens cursor
	| isEmpty tokens	= (mbErr, mbRef, cursor)
						= (mbErr, Nothing, cursor)

mapDelete :: !ref !*cur -> (!(Maybe MappingError), !(Maybe val), !*cur) | relMap{|*|} ref & relMap{|*|} val & SQLCursor cur & bimap{|*|} cur
mapDelete ref cursor
	# (mbErr,_,_,tokens,cursor)		= relMap{|*|} RelMapInit 0 (Just ref) [] [] cursor
	| isJust mbErr		= (mbErr, Nothing, cursor)
	# (mbErr,mbVal,_,tokens,cursor)	= relMap{|*|} RelMapDelete 0 Nothing [] tokens cursor
	| isEmpty tokens	= (mbErr, mbVal, cursor)
						= (mbErr, Nothing, cursor)
// Error toString
instance toString MappingError
where
	toString (TypeError e)		= "TypeError " +++ e
	toString (DatabaseError e)	= "DatabaseError " +++ toString e


// Types used by the generic function that does the mapping

// The separator used in representation type fieldnames
FIELD_SEPARATOR	:== "_"

// Token type that is used in token lists with which reading and writing to
// a database can be viewed as parsing/unparsing of a stream of tokens
:: RelMapToken	= RelMapValue SQLValue				//Plain value, index in the stream determines to which field it is mapped
				| RelMapTerminator					//Terminator token, indicates the end of a list of values
				| RelMapOverride String SQLValue	//Field override. If there is such a token in the stream for a given field,
													//it's value is used instead of the 'normal' value in the stream

// Mode type which determines what we want the generic function to do
:: RelMapMode	= RelMapCreate	// C
				| RelMapRead	// R
				| RelMapUpdate	// U
				| RelMapDelete	// D
				| RelMapInfo	// Find info by traversing the structure of the type
				| RelMapInit	// Reduce a value to a flat list of tokens to start reading or deleting an entry.

// Information about the structure of types
:: RelMapFieldInfo =	{ fld_table		:: String			//The database table in which this value is stored
						, fld_select	:: Maybe String		//The database field in which this value is stored
						, fld_match		:: Maybe String		//The database field on which can be matched to find the right database record
						, rec_table		:: String			//The database table of the key field of the parent record
						, rec_key		:: String			//The database field of the key field of the parent record
						, val_list		:: Bool				//Are dealing with one, or with a set of values
						, val_maybe		:: Bool				//Is the value optional
						, val_fields	:: [RelMapFieldInfo]//Information about the fields if this value is a record
						, val_id		:: Bool				//Is the field an ID type or an entity record
						}

// Some of the modes need to go in multiple passes over the data structure
:: RelMapPass		:== Int

//The real generic magic!
generic	relMap t :: !RelMapMode !RelMapPass !(Maybe t) ![RelMapFieldInfo] ![RelMapToken] !*cur -> (!(Maybe MappingError), !(Maybe t), ![RelMapFieldInfo], ![RelMapToken], !*cur) | SQLCursor cur

relMap{|Int|} 		RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|Int|} 		RelMapRead		_	_			info	[RelMapValue (SQLVInteger x):xs]	cursor = (Nothing, Just x, info, xs, cursor)
relMap{|Int|} 		RelMapInit		_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, [RelMapValue (SQLVInteger x):tokens], cursor)
relMap{|Int|} 		RelMapCreate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVInteger x)], cursor)
relMap{|Int|} 		RelMapUpdate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVInteger x)], cursor)
relMap{|Int|} 		RelMapDelete	_	_			info	[RelMapValue (SQLVInteger x):xs]	cursor = (Nothing, Just x, info, xs, cursor)
relMap{|Int|}		_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

relMap{|Real|}		RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|Real|}		RelMapRead		_	_			info	[RelMapValue (SQLVReal x):xs]		cursor = (Nothing, Just x, info, xs, cursor)
relMap{|Real|}		RelMapInit		_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, [RelMapValue (SQLVReal x):tokens], cursor)
relMap{|Real|}		RelMapCreate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVReal x)], cursor)
relMap{|Real|}		RelMapUpdate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVReal x)], cursor)
relMap{|Real|}		RelMapDelete	_	_			info	[RelMapValue (SQLVReal x):xs]		cursor = (Nothing, Just x, info, xs, cursor)
relMap{|Real|}		_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

relMap{|Bool|}		RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|Bool|}		RelMapRead		_	_			info	[RelMapValue (SQLVInteger x):xs]	cursor = (Nothing, Just (x <> 0), info, xs, cursor)
relMap{|Bool|}		RelMapInit		_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, [RelMapValue (SQLVInteger (if x 1 0)):tokens], cursor)
relMap{|Bool|}		RelMapCreate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVInteger (if x 1 0))], cursor)
relMap{|Bool|}		RelMapUpdate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVInteger (if x 1 0))], cursor)
relMap{|Bool|}		RelMapDelete	_	_			info	[RelMapValue (SQLVInteger x):xs]	cursor = (Nothing, Just (x <> 0), info, xs, cursor)
relMap{|Bool|} 		_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

relMap{|Char|} 		RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|Char|} 		RelMapRead		_	_			info	[RelMapValue (SQLVChar x):xs]		cursor = (Nothing, Just (hd (fromString x)), info, xs, cursor)
relMap{|Char|} 		RelMapInit		_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, [RelMapValue (SQLVChar (toString [x])):tokens], cursor)
relMap{|Char|} 		RelMapCreate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVChar (toString [x]))], cursor)
relMap{|Char|} 		RelMapUpdate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVChar (toString [x]))], cursor)
relMap{|Char|} 		RelMapDelete	_	_			info	[RelMapValue (SQLVChar x):xs]		cursor = (Nothing, Just (hd (fromString x)), info, xs, cursor)
relMap{|Char|} 		_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

relMap{|String|} 	RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|String|}	RelMapRead		_	_			info	[RelMapValue (SQLVVarchar x):xs]	cursor = (Nothing, Just x, info, xs, cursor)
relMap{|String|}	RelMapInit		_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, [RelMapValue (SQLVVarchar x):tokens], cursor)
relMap{|String|}	RelMapCreate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVVarchar x)], cursor)
relMap{|String|}	RelMapUpdate	_	(Just x)	info	tokens								cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue (SQLVVarchar x)], cursor)
relMap{|String|}	RelMapDelete	_	_			info	[RelMapValue (SQLVVarchar x):xs]	cursor = (Nothing, Just x, info, xs, cursor)
relMap{|String|}	_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

relMap{|UNIT|}		RelMapInfo		_	_			info	tokens								cursor = (Nothing, Nothing, [emptyInfo], tokens, cursor)
relMap{|UNIT|}		RelMapRead		_	_			info	tokens								cursor = (Nothing, Just UNIT, info, tokens, cursor)
relMap{|UNIT|}		RelMapDelete	_	_			info	tokens								cursor = (Nothing, Just UNIT, info, tokens, cursor)
relMap{|UNIT|}		_				_	_			info	tokens								cursor = (Nothing, Nothing, info, tokens, cursor)

//Default function for EITHER 
relMap{|EITHER|} fl fr mode pass Nothing info tokens cursor
	= case fl mode pass Nothing info tokens cursor of
		(mbErr,Just x, ixs, xs, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (LEFT x), ixs, xs, cursor) 					
		(mbErr,Nothing, _, _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= case fr mode pass Nothing info tokens cursor of
				(mbErr,Just x, ixs, xs, cursor)
					| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Just (RIGHT x), ixs, xs, cursor)
				(mbErr,Nothing, _,  _, cursor)
					| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Nothing, info, tokens, cursor)

relMap{|EITHER|} fl fr mode pass (Just (LEFT x)) info tokens cursor
	# (mbErr,_,info,tokens,cursor) = fl mode pass (Just x) info tokens cursor
	| isJust mbErr	= (mbErr,Nothing,[],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)
relMap{|EITHER|} fl fr mode pass (Just (RIGHT x)) info tokens cursor
	# (mbErr,_,info,tokens,cursor) = fr mode pass (Just x) info tokens cursor
	| isJust mbErr	= (mbErr,Nothing,[],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

//Special PAIR for RelMapInfo mode. Just combines the two info lists
relMap{|PAIR|} fx fy RelMapInfo _ _ info tokens cursor
	# (mbErr,_, infox, _, cursor)			= fx RelMapInfo 0 Nothing info tokens cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	# (mbErr,_, infoy, _, cursor)			= fy RelMapInfo 0 Nothing info tokens cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	= (Nothing, Nothing, infox ++ infoy, tokens, cursor)

//Default function for PAIR
relMap{|PAIR|} fx fy mode pass Nothing info tokens cursor
	# (mbErr, resx, infox, tokx, cursor)	= fx mode pass Nothing info tokens cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	# (mbErr, resy, infoy, toky, cursor)	= fy mode pass Nothing infox tokx cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	| isJust resx && isJust resy			= (Nothing, Just (PAIR (fromJust resx) (fromJust resy)), infoy, toky, cursor)
											= (Nothing, Nothing, infoy, toky, cursor)

relMap{|PAIR|} fx fy mode pass (Just (PAIR x y)) info tokens cursor
	# (mbErr,_, infox, tokx, cursor)		= fx mode pass (Just x) info tokens cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	# (mbErr,_, infoy, toky, cursor)		= fy mode pass (Just y) infox tokx cursor
	| isJust mbErr							= (mbErr,Nothing,[],[],cursor)
	= (Nothing, Nothing, infoy, toky, cursor)

//Special CONS for RelMapInfo mode.
relMap{|CONS of d|} f RelMapInfo _ _ info tokens cursor
	| not (isEmpty d.gcd_type_def.gtd_conses)
		= (Nothing, Nothing, [{emptyInfo & val_fields = [setFldInfo desc.gcd_name emptyInfo \\ desc <- d.gcd_type_def.gtd_conses] , val_id = isID d.gcd_name }], tokens, cursor)
	| otherwise
		# (mbErr, _, info, _, cursor)		= f RelMapInfo 0 Nothing info tokens cursor
		| isJust mbErr						= (mbErr,Nothing,[],[],cursor)
		= (Nothing, Nothing, info, tokens, cursor)

//Special CONS for RelMapRead mode.
relMap{|CONS of d|} f RelMapRead _ _ info tokens cursor
	| not (isEmpty d.gcd_type_def.gtd_conses)
		# (_,_,info,_,cursor)	= f RelMapInfo 0 Nothing info tokens cursor							//Extract info about the fields in the record
		# info					= map (setRecInfo info) info										//Add type info about this record
		# (mbErr,tokx,cursor)	= readRecord info (hd tokens) cursor								//Read the extra tokens for this record
		| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
		# tokens				= tokx ++ (tl tokens)												//Replace the head of the token list by the extra tokens
		= case f RelMapRead 0 Nothing info tokens cursor of
			(mbErr, Nothing, _, _, cursor)
				| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Nothing, info, tokens, cursor)
			(mbErr, Just x, _, xs, cursor)
				| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Just (CONS x), info, xs, cursor)
	| otherwise
		= case f RelMapRead 0 Nothing info tokens cursor of
			(mbErr, Nothing, _, _, cursor)
				| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Nothing, info, tokens, cursor)
			(mbErr,Just x, _, xs, cursor)
				| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Just (CONS x), info, xs, cursor)

//Special CONS for RelMapCreate mode.
relMap{|CONS of d|} f RelMapCreate pass (Just (CONS x)) info tokens cursor 
	| not (isEmpty d.gcd_type_def.gtd_conses) && not (isID d.gcd_name)
		# (mbErr,_,info,_,cursor)	= f RelMapInfo 0 (Just x) info [] cursor							//Find type info of the individual fields
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# info						= map (setRecInfo info) info 									//Add type info about this record
		# (overrides,tokens)		= takeOverrides tokens
		# (mbErr,_,_,tokx1,cursor)	= f RelMapCreate 1 (Just x) info [] cursor						//First pass: Prepare the tokenstream
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,id,cursor)			= insertRecord info tokx1 overrides cursor						//Write the values in this record to the database
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,_,_,tokx2,cursor)	= f RelMapCreate 2 (Just x) info (mkOverrides info id) cursor		//Second pass: Recursive create for incoming pointers
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (_,tokx2)					= takeOverrides tokx2											//Remove the overrides used in the second pass
		# tokx						= mergeTokens info tokx1 tokx2									//Merge the tokens from both passes
		# (mbErr,cursor)			= linkRecords info tokx id cursor								//Add the link records
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, info, overrides ++ tokens ++ [id], cursor)
	| otherwise
		# (mbErr,_,_,tokens,cursor) = f RelMapCreate pass (Just x) info tokens cursor
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, info, tokens, cursor)

//Special CONS for RelMapUpdate mode.
relMap{|CONS of d|} f RelMapUpdate pass (Just (CONS x)) info tokens cursor
	| not (isEmpty d.gcd_type_def.gtd_conses) && not (isID d.gcd_name)
		# (mbErr,_,info,_,cursor)	= f RelMapInfo 0 (Just x) info [] cursor							//Find type info of the individual fields
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# info						= map (setRecInfo info) info									//Add type info about this record
		# (overrides,tokens)		= takeOverrides tokens
		# (mbErr,_,_,tokx1,cursor)	= f RelMapUpdate 1 (Just x) info [] cursor						//First pass: Prepare the tokenstream
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,orig,cursor)		= readRecord info (hd tokx1) cursor								//Read the original values
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,id,cursor)			= updateOrInsertRecord info tokx1 overrides cursor				//Update the values in this record in the database
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,_,_,tokx2,cursor)	= f RelMapUpdate 2 (Just x) info (mkOverrides info id) cursor		//Second pass: Recursive update for incoming pointers
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (_,tokx2)					= takeOverrides tokx2											//Remove the overrides used in the second pass
		# tokx						= mergeTokens info tokx1 tokx2									//Merge the tokens from both passes
		# (mbErr,cursor)			= relinkRecords info tokx id cursor								//Update, or add the link records
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# tokx						= zipTokens info orig tokx										//Zip the original values with the updated ones
		# (mbErr,cursor)			= unlinkDirectRecords info tokx cursor
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# (mbErr,_,_,_,cursor)		= f RelMapUpdate 3 (Just x) info tokx cursor						//Garbage collect, delete removed entities in the database
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		= (Nothing,Nothing, info, overrides ++ tokens ++ [id], cursor)
	| otherwise
		# (mbErr,_,_,tokens,cursor) = f RelMapUpdate pass (Just x) info tokens cursor
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, info, tokens, cursor)

//Special CONS for RelMapDelete mode.
relMap{|CONS of d|} f RelMapDelete pass _ info tokens cursor
	| not (isEmpty d.gcd_type_def.gtd_conses) && not (isID d.gcd_name)
		# (mbErr,_,info,_,cursor)	= f RelMapInfo 0 Nothing [] tokens cursor							//Extract info about the fields in the record
		| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
		# info						= map (setRecInfo info) info									//Add type info about this record
		# id						= hd tokens
		| pass == 0 //Read and delete
			# (mbErr,tokx,cursor)	= readRecord info id cursor										//Read the extra tokens for this record
			| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
			# tokens				= tokx ++ (tl tokens)											//Replace the head of the token list by the extra tokens
			# (mbErr,cursor)		= unlinkRecords info id cursor									//Unlink indirect pointed fields
			| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
			= case f RelMapDelete 0 Nothing info tokens cursor of
				(mbErr, Nothing, _, _, cursor)
						| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Nothing, info, tokens, cursor)
				(mbErr, Just x, _, xs, cursor)
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						# (mbErr,cursor)		= deleteRecord info id cursor
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						# (mbErr,_,_,_,cursor)	= f RelMapDelete 2 Nothing info tokens cursor			//Garbage collect
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Just (CONS x), info, xs, cursor)
		| pass == 1 //Only read
			# (mbErr,tokx,cursor)	= readRecord info id cursor										//Read the extra tokens for this record
			| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
			# tokens				= tokx ++ (tl tokens)											//Replace the head of the token list by the extra tokens
			# (mbErr,cursor)		= unlinkRecords info id cursor									//Unlink indirect pointed fields
			| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
			= case f RelMapDelete 0 Nothing info tokens cursor of
				(mbErr, Nothing, _, _, cursor)
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Nothing, info, tokens, cursor)
				(mbErr, Just x, _, xs, cursor)
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						# (mbErr,_,_,_,cursor)	= f RelMapDelete 2 Nothing info tokens cursor			//Garbage collect
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Just (CONS x), info, xs, cursor)
		| pass == 2 //Garbage collection: construct and delete
			= case f RelMapDelete 1 Nothing info tokens cursor of
				(mbErr, Nothing, _, _, cursor)
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Nothing, info, tokens, cursor)
				(mbErr, Just x, _, xs, cursor)
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						# (mbErr,cursor)		= deleteRecord info id cursor
						| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
						= (Nothing, Just (CONS x), info, xs, cursor)
		| otherwise
			= (Nothing, Nothing, info, tokens, cursor)
	| otherwise
		= case f RelMapDelete pass Nothing info tokens cursor of
			(mbErr, Nothing, _, _, cursor)
				| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Nothing, info, tokens, cursor)
			(mbErr, Just x, _, xs, cursor)
				| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Just (CONS x), info, xs, cursor)

//Default function for CONS
relMap{|CONS|} f mode pass Nothing info tokens cursor
	= case f mode pass Nothing info tokens cursor of
		(mbErr, Nothing, _, _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr, Just x, ixs, xs, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (CONS x), ixs, xs, cursor)

relMap{|CONS|} f mode pass (Just (CONS x)) info tokens cursor
	# (mbErr,_,info,tokens,cursor)	= f mode pass (Just x) info tokens cursor
	| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

//Special FIELD for RelMapInfo mode.
relMap{|FIELD of d|} f RelMapInfo _ _ info tokens cursor 
	= case f RelMapInfo 0 Nothing info tokens cursor of
		(mbErr,_, [x:xs], _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, [setFldInfo d.gfd_name x:xs], tokens, cursor)
		(mbErr,_, info, _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)

//Special FIELD for RelMapInit mode.
relMap{|FIELD of d|} f RelMapInit _ (Just (FIELD x)) info tokens cursor
	| d.gfd_index == 0
		# (mbErr,_,info,tokens,cursor)	= f RelMapInit 0 (Just x) info tokens cursor
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, info, tokens, cursor)
	| otherwise
		= (Nothing, Nothing, info, tokens, cursor)

//Special FIELD for RelMapCreate mode.
relMap{|FIELD|} f RelMapCreate 1 (Just (FIELD x)) [i:is] tokens cursor
	| store i	
		# (mbErr,_,_,tokens,cursor)		= f RelMapCreate 1 (Just x) [] tokens cursor
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, is, tokens, cursor)
	| otherwise
		= (Nothing, Nothing, is, tokens, cursor)

relMap{|FIELD|} f RelMapCreate 2 (Just (FIELD x)) [i:is] tokens cursor
	| store i
		= (Nothing, Nothing, is, tokens, cursor)
	| otherwise
		# (mbErr,_,_,tokens,cursor)		= f RelMapCreate 2 (Just x) [] tokens cursor
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, is, tokens, cursor)

//Special FIELD for RelMapUpdate mode.
relMap{|FIELD|} f RelMapUpdate 1 (Just (FIELD x)) [i:is] tokens cursor
	| store i	
		# (mbErr,_,_,tokens,cursor)		= f RelMapUpdate 1 (Just x) [] tokens cursor
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, is, tokens, cursor)
	| otherwise
		= (Nothing, Nothing, is, tokens, cursor)

relMap{|FIELD|} f RelMapUpdate 2 (Just (FIELD x)) [i:is] tokens cursor
	| store i
		= (Nothing, Nothing, is, tokens, cursor)
	| otherwise
		# (mbErr,_,_,tokens,cursor)		= f RelMapUpdate 2 (Just x) [] tokens cursor
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, is, tokens, cursor)

relMap{|FIELD|} f RelMapUpdate 3 (Just (FIELD x)) [i:is] tokens cursor
	# (orig,tokens)	= takeTokens i.val_list tokens
	# (new,tokens)	= takeTokens i.val_list tokens
	| i.val_list
		# (mbErr,_,_,_,cursor)			= f RelMapDelete 0 Nothing [] (removedTokens orig new) cursor 
		| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
		= (Nothing, Nothing, is, tokens, cursor)
	| i.val_maybe
		| (isNullValue (hd new)) && (not (isNullValue (hd orig)))
			# (mbErr,_,_,_,cursor)		= f RelMapDelete 0 Nothing [] orig cursor 
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, is, tokens, cursor)
		| otherwise
			= (Nothing, Nothing, is, tokens, cursor)
	| otherwise
		= (Nothing, Nothing, is, tokens, cursor)

//Special FIELD for RelMapDelete mode.
relMap{|FIELD of d|} f RelMapDelete 0 _ [i:is] tokens cursor
	| isOutPointer i
		= case f RelMapDelete 1 Nothing [] tokens cursor of
			(mbErr,Nothing,_,_,cursor)
				| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Nothing, is, tokens, cursor)
			(mbErr,Just x,_,xs,cursor)
				| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Just (FIELD x), is, xs, cursor)
	| otherwise
		= case f RelMapDelete 0 Nothing [] tokens cursor of
			(mbErr,Nothing,_,_,cursor)
				| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Nothing, is, tokens, cursor)
			(mbErr,Just x,_,xs,cursor)
				| isJust mbErr			= (mbErr,Nothing, [],[],cursor)
				= (Nothing, Just (FIELD x), is, xs, cursor)

relMap{|FIELD of d|} f RelMapDelete pass _ [i:is] tokens cursor
	= case f RelMapDelete pass Nothing [] tokens cursor of
		(mbErr,Nothing,_,_,cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, is, tokens, cursor)
		(mbErr,Just x,_,xs,cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (FIELD x), is, xs, cursor)

//Default function for FIELD
relMap{|FIELD|} f mode pass Nothing info tokens cursor 
	= case f mode pass Nothing info tokens cursor of
		(mbErr,Nothing, _, _, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr,Just x, ixs, xs, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (FIELD x), ixs, xs, cursor)

relMap{|FIELD|} f mode pass (Just (FIELD x)) info tokens cursor
	# (mbErr,_, info, tokens, cursor)	= f mode pass (Just x) info tokens cursor
	| isJust mbErr						= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

//Special OBJECT for RelMapInfo mode.
relMap{|OBJECT|} f RelMapInfo pass Nothing info tokens cursor
	# (mbErr,_, info, _, cursor)		= f RelMapInfo 0 Nothing info tokens cursor
	| isJust mbErr						= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

//Default function for OBJECT
relMap{|OBJECT|} f mode pass Nothing info tokens cursor 
	# (mbErr, res, info, tokens, cursor)	= f mode pass Nothing info tokens cursor
	| isJust mbErr							= (mbErr,Nothing, [],[],cursor)
	| isJust res							= (Nothing, Just (OBJECT (fromJust res)), info, tokens, cursor)
											= (Nothing, Nothing, info, tokens, cursor)

relMap{|OBJECT|} f mode pass (Just (OBJECT x)) info tokens cursor
	# (mbErr,_, info, tokens, cursor)		= f mode pass (Just x) info tokens cursor
	| isJust mbErr							= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

//Bullocks case, just because the compiler doesn't see this will never happen
relMap{|OBJECT|} _ _ _ _ info tokens cursor = (Nothing, Nothing, info, tokens, cursor)

//We don't use arrays in the representation types
relMap{|{}|} f mode pass mb_val info tokens cursor	= (Just (TypeError "Arrays are not representation types"), Nothing, info, tokens, cursor)
relMap{|{!}|} f mode pass mb_val info tokens cursor	= (Just (TypeError "Arrays are not representation types"), Nothing, info, tokens, cursor)

//The maybe type is mapped to NULL values in the database
//A NULL value maps to Nothing. A non-NULL value to a Just.
relMap{|Maybe|} f RelMapRead _ _ info [RelMapValue (SQLVNull):xs] cursor = (Nothing, Just Nothing, info, xs, cursor)
relMap{|Maybe|} f RelMapRead _ _ info tokens cursor
	= case f RelMapRead 0 Nothing info tokens cursor of
		(mbErr, Nothing, _, _, cursor)
			| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr, Just x, _, xs, cursor)
			| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (Just x), info, xs, cursor)

relMap{|Maybe|} f RelMapInfo _ _ info tokens cursor
	= case f RelMapInfo 0 Nothing info tokens cursor of
		(mbErr,_, [x:xs], _, cursor)
			| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, [{x & val_maybe = True}:xs], tokens, cursor)
		(mbErr,_, info, _, cursor)
			| isJust mbErr					= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)

relMap{|Maybe|} f RelMapInit _ (Just Nothing) info tokens cursor = (Nothing, Nothing, info, [RelMapValue SQLVNull:tokens], cursor)
relMap{|Maybe|} f RelMapInit _ (Just (Just x)) info tokens cursor
	# (mbErr, _,info, tokens, cursor)	= f RelMapInit 0 (Just x) info tokens cursor
	| isJust mbErr						= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

relMap{|Maybe|} f RelMapCreate _ (Just Nothing) info tokens cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue SQLVNull], cursor)
relMap{|Maybe|} f RelMapCreate _ (Just (Just x)) info tokens cursor
	# (mbErr, _,_, tokens, cursor)		= f RelMapCreate 0 (Just x) info tokens cursor
	| isJust mbErr						= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

relMap{|Maybe|} f RelMapUpdate _ (Just Nothing) info tokens cursor = (Nothing, Nothing, info, tokens ++ [RelMapValue SQLVNull], cursor)
relMap{|Maybe|} f RelMapUpdate _ (Just (Just x)) info tokens cursor
	# (mbErr,_,_, tokens, cursor)		= f RelMapUpdate 0 (Just x) info tokens cursor
	| isJust mbErr						= (mbErr,Nothing, [],[],cursor)
	= (Nothing, Nothing, info, tokens, cursor)

relMap{|Maybe|} f RelMapDelete _ _ info [RelMapValue (SQLVNull):xs] cursor = (Nothing, Just Nothing, info, xs, cursor)
relMap{|Maybe|} f RelMapDelete pass _ info tokens cursor
	= case f RelMapDelete pass Nothing info tokens cursor of
		(mbErr, Nothing, _, _, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr, Just x, _, xs, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Just (Just x), info, xs, cursor)

relMap{|Maybe|} f  _ _ _ info tokens cursor = (Nothing, Nothing, info, tokens, cursor)

//The list type is used for one-to-many and many-to-many relations
//in the database. To construct a list of values, it expects the values
//concatenated in the stream followed by a terminator token.
relMap{|[]|} f RelMapRead _ _ info [RelMapTerminator:xs] cursor = (Nothing, Just [], info, xs, cursor)
relMap{|[]|} f RelMapRead _ _ info tokens cursor
	= case f RelMapRead 0 Nothing info tokens cursor of
		(mbErr,Nothing, _, _, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr,Just x, _, xs, cursor)
			| isJust mbErr				= (mbErr,Nothing, [],[],cursor)
			= case relMap{|*->*|} f RelMapRead 0 Nothing info xs cursor of
				(mbErr, Nothing, _, _, cursor)
					| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Nothing, info, tokens, cursor)
				(mbErr, Just y, _, ys, cursor)
					| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Just [x:y], info, ys, cursor)

//Info case sets the list flag to true
relMap{|[]|} f RelMapInfo _ _ info tokens cursor
	= case f RelMapInfo 0 Nothing info tokens cursor of
		(mbErr, _, [x:xs], _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, [{x & val_list = True}:xs], tokens, cursor)
		(mbErr, _, info, _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)


//Create case does create for each member of the list and adds a terminator token 
//to the token stream.
relMap{|[]|} f RelMapCreate _ (Just list) info tokens cursor
	# (mbErr, tokens, cursor) = doList list tokens cursor
	= (mbErr, Nothing, info, tokens, cursor)
where
	doList [] tokens cursor = (Nothing, tokens ++ [RelMapTerminator], cursor)
	doList [x:xs] tokens cursor
		# (mbErr, _, _, tokx, cursor)	= f RelMapCreate 0 (Just x) [] tokens cursor
		| isJust mbErr		= (mbErr,[],cursor)
		= doList xs tokx cursor

//Update case
relMap{|[]|} f RelMapUpdate _ (Just list) info tokens cursor
	# (mbErr, tokens, cursor) = doList list tokens cursor
	= (mbErr, Nothing, info, tokens, cursor)
where
	doList [] tokens cursor = (Nothing, tokens ++ [RelMapTerminator], cursor)
	doList [x:xs] tokens cursor
		# (mbErr, _, _, tokx, cursor)	= f RelMapUpdate 0 (Just x) [] tokens cursor
		| isJust mbErr		= (mbErr,[],cursor)
		= doList xs tokx cursor

//Delete case
relMap{|[]|} f RelMapDelete _ _ info [RelMapTerminator:xs] cursor = (Nothing, Just [], info, xs, cursor)
relMap{|[]|} f RelMapDelete pass _ info tokens cursor
	= case f RelMapDelete pass Nothing info tokens cursor of
		(mbErr, Nothing, _, _, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= (Nothing, Nothing, info, tokens, cursor)
		(mbErr, Just x, _, xs, cursor)
			| isJust mbErr		= (mbErr,Nothing, [],[],cursor)
			= case relMap{|*->*|} f RelMapDelete pass Nothing info xs cursor of
				(mbErr, Nothing, _, _, cursor)
					| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Nothing, info, tokens, cursor)
				(mbErr, Just y, _, ys, cursor)
					| isJust mbErr	= (mbErr,Nothing, [],[],cursor)
					= (Nothing, Just [x:y], info, ys, cursor)

//Default function for lists
relMap{|[]|} f mode pass (Just list) info tokens cursor
	# (mbErr, tokens, cursor) = doList mode pass list tokens cursor
	= (mbErr, Nothing, info, tokens, cursor)
where
	doList mode pass [] tokens cursor = (Nothing, [RelMapTerminator], cursor)
	doList mode pass [x:xs] tokens cursor
		# (mbErr, _,info, tokx, cursor)	= f mode pass (Just x) info tokens cursor
		| isJust mbErr	= (mbErr, [], cursor)
		# (mbErr, tokxs, cursor)		= doList mode pass xs tokens cursor
		= (mbErr, tokx ++ tokxs, cursor)

relMap{|[]|} f  _ _ _ info tokens cursor = (Nothing, Nothing, info, tokens, cursor)

// --- Helper functions for RelMapInfo mode --- //

//Creates a default info record
emptyInfo :: RelMapFieldInfo 
emptyInfo = {fld_table = undef, fld_select = Nothing, fld_match = Nothing, rec_table = undef, rec_key = undef, val_list = False, val_maybe = False, val_fields = [], val_id = False}

//Sets the information that is encoded in the field name in an info record
setFldInfo :: !String !RelMapFieldInfo -> RelMapFieldInfo
setFldInfo desc info 
	# parts = split FIELD_SEPARATOR desc
	//Determine the table
	# table = hd parts
	# parts = tl parts
	//Determine the select columns
	# select = takeWhile ((<>) "ofwhich") parts		//Collect all elements before "ofwhich"
	# select = if (isEmpty select) Nothing (Just (hd select))
	# match = (dropWhile ((<>) "ofwhich") parts)	//Collect all elements after and including "ofwhich"
	# match = if (isEmpty match) match (tl match)	//Strip the "ofwhich" element from the select list
	# match = if (isEmpty match) Nothing (Just (hd match))
	= {info & fld_table = table, fld_select = select, fld_match = match}

//Copies the table and key from the first field in a set to the info record
setRecInfo :: ![RelMapFieldInfo] !RelMapFieldInfo -> RelMapFieldInfo
setRecInfo fields info = {info & rec_table = getTable fields, rec_key = getKey fields}

// --- Helper functions for RelMapRead mode --- //

readRecord :: [RelMapFieldInfo] RelMapToken !*cur -> (Maybe MappingError, [RelMapToken], *cur) | SQLCursor cur
readRecord [] id cursor = (Nothing, [], cursor)
readRecord [i:is] id cursor
	# (mbErr, cursor)			= execute (mkSelectQuery i) [fromValue id] cursor 
	| isJust mbErr				= (Just (DatabaseError (fromJust mbErr)), [], cursor)
	# (mbErr, rows, cursor)		= fetchAll cursor
	| isJust mbErr				= (Just (DatabaseError (fromJust mbErr)), [], cursor)
	#  tokx						= map RelMapValue (flatten rows)
	# (mbErr,tokxs,cursor)		= readRecord is id cursor
	| isJust mbErr				= (mbErr, [], cursor)
	| i.val_list	= (Nothing, tokx ++ [RelMapTerminator] ++ tokxs, cursor)
					= (Nothing, tokx ++ tokxs, cursor)

// --- Helper functions for RelMapCreate mode --- //

insertRecord :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] *cur -> (Maybe MappingError, RelMapToken,*cur) | SQLCursor cur
insertRecord info values overrides cursor 
	# info				= filter store info 
	# stmt				= mkInsertQuery info
	# values			= appOverrides info values overrides
	# keyval			= hd values
	# (mbErr,cursor)	= execute stmt (map fromValue values) cursor
	| isJust mbErr		= (Just (DatabaseError (fromJust mbErr)), keyval, cursor)
	| isZeroValue keyval	
		# (mbErr, idval, cursor) = fetchIDToken cursor
		| isJust mbErr		= (Just (DatabaseError (fromJust mbErr)), keyval, cursor)
		= (Nothing, idval, cursor)
	| otherwise
		= (Nothing, keyval, cursor)

//Checks the link records and adds new links where necessary
linkRecords :: [RelMapFieldInfo] [RelMapToken] RelMapToken !*cur -> (Maybe MappingError, *cur ) | SQLCursor cur
linkRecords [] tokens id cursor = (Nothing, cursor)
linkRecords [i:is] tokens id cursor
	# (values,tokens)	= takeTokens i.val_list tokens
	| isIndirect i
		# (mbErr, cursor) = linkRecord i (map fromValue (init values)) (fromValue id) cursor
		| isJust mbErr = (mbErr, cursor)
		= linkRecords is tokens id cursor
	| otherwise
		= linkRecords is tokens id cursor
where
	linkRecord info [] id cursor = (Nothing, cursor)
	linkRecord info [x:xs] id cursor
		# (mbErr, cursor)	= execute (mkLinkQuery info) [x,id] cursor //Create the link
		| isJust mbErr = (Just (DatabaseError (fromJust mbErr)), cursor)
		= linkRecord info xs id cursor

// --- Helper functions for RelMapUpdate mode --- //

updateOrInsertRecord :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] *cur -> (Maybe MappingError, RelMapToken, *cur) | SQLCursor cur
updateOrInsertRecord info values overrides cursor
	# (mbErr,upd,id,cursor)	= updateRecord info values overrides cursor
	| isJust mbErr		= (mbErr,id,cursor)
	| upd				= (Nothing,id,cursor)
	| otherwise			= insertRecord info values overrides cursor

updateRecord :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] *cur -> (Maybe MappingError, Bool, RelMapToken, *cur) | SQLCursor cur
updateRecord info values overrides cursor 
	# info						= filter store info 
	# stmt						= mkUpdateQuery info
	# values					= appOverrides info values overrides
	# idval						= hd values
	# (mbErr,cursor)			= execute stmt (map fromValue (values ++ [hd values]) ) cursor
	| isJust mbErr				= (Just (DatabaseError (fromJust mbErr)),False, idval, cursor)	
	# (mbErr,updated,cursor)	= numRows cursor
	| isJust mbErr				= (Just (DatabaseError (fromJust mbErr)),False, idval, cursor)
	| updated == 0				= (Nothing, False, idval, cursor)
								= (Nothing, True, idval, cursor)

//Checks the link records and adds new links where necessary
relinkRecords :: [RelMapFieldInfo] [RelMapToken] RelMapToken !*cur -> (Maybe MappingError, *cur) | SQLCursor cur
relinkRecords [] tokens id cursor = (Nothing, cursor)
relinkRecords [i:is] tokens id cursor
	# (values,tokens)	= takeTokens i.val_list tokens
	| isIndirect i
		# (mbErr, cursor) = relinkRecord i (map fromValue (init values)) (fromValue id) cursor
		| isJust mbErr	= (mbErr, cursor)
		# (mbErr, cursor) = cleanlinkRecord i (map fromValue (init values)) (fromValue id) cursor
		| isJust mbErr	= (mbErr, cursor)
		= relinkRecords is tokens id cursor
	| otherwise
		= relinkRecords is tokens id cursor
where
	relinkRecord info [] id cursor = (Nothing, cursor)
	relinkRecord info [x:xs] id cursor
		# (mbErr, cursor)		= execute (mkCheckLinkQuery info) [x,id] cursor //Check if the link already exists
		| isJust mbErr			= (Just (DatabaseError (fromJust mbErr)), cursor)
		# (mbErr, num, cursor)	= numRows cursor
		| isJust mbErr			= (Just (DatabaseError (fromJust mbErr)), cursor)
		| num > 0		
			= relinkRecord info xs id cursor
		| otherwise
			# (mbErr, cursor)	= execute (mkLinkQuery info) [x,id] cursor //Create the link
			| isJust mbErr			= (Just (DatabaseError (fromJust mbErr)), cursor)
			= relinkRecord info xs id cursor

	cleanlinkRecord info values id cursor
		# (mbErr, cursor)		= execute (mkCleanLinkQuery info values) [id:values] cursor
		| isJust mbErr			= (Just (DatabaseError (fromJust mbErr)), cursor)
								= (Nothing, cursor)

unlinkDirectRecords :: [RelMapFieldInfo] [RelMapToken] !*cur -> (Maybe MappingError, *cur) | SQLCursor cur
unlinkDirectRecords [] tokens cursor = (Nothing, cursor)
unlinkDirectRecords [i:is] tokens cursor
	# (orig,tokens)	= takeTokens i.val_list tokens
	# (new,tokens)	= takeTokens i.val_list tokens
	| isDirect i && i.val_id
		= case (map fromValue (init (removedTokens orig new))) of
			[]			= unlinkDirectRecords is tokens cursor
			values
				# (mbErr, cursor)	= execute (mkUnlinkDirectQuery i values) values cursor
				| isJust mbErr		= (Just (DatabaseError (fromJust mbErr)), cursor)
				= unlinkDirectRecords is tokens cursor
	| otherwise
		= unlinkDirectRecords is tokens cursor


// --- Helper functions for RelMapDelete mode --- //

deleteRecord :: [RelMapFieldInfo] RelMapToken !*cur -> (Maybe MappingError, *cur ) | SQLCursor cur
deleteRecord info id cursor
	# (mbErr,cursor)	= execute (mkDeleteQuery (hd info)) [fromValue id] cursor
	| isJust mbErr		= (Just (DatabaseError (fromJust mbErr)),cursor)
	= (Nothing, cursor)

unlinkRecords :: [RelMapFieldInfo] RelMapToken !*cur -> (Maybe MappingError, *cur) | SQLCursor cur
unlinkRecords [] id cursor = (Nothing, cursor)
unlinkRecords [i:is] id cursor
	| isIndirect i
		# (mbErr,cursor)	= execute (mkUnlinkQuery i) [fromValue id] cursor
		| isJust mbErr	= (Just (DatabaseError (fromJust mbErr)), cursor)
		= unlinkRecords is id cursor
	| otherwise
		= unlinkRecords is id cursor

// --- Predicates about fields --- //

//Checks if a record field's value should be stored now, or if it's 
//values are stored in a different database record.
store :: RelMapFieldInfo -> Bool
store info =: { fld_table, fld_select, rec_table} = (isJust fld_select) && (fld_table == rec_table)

//Checks if a field which is used for an incoming pointer/relation is direct, or uses a separate link table.
isDirect :: RelMapFieldInfo -> Bool
isDirect info =: {fld_table, val_fields}
	| isEmpty val_fields					= True
	| getTable val_fields == fld_table		= True
											= False
//Checks if a field is an indirect link
isIndirect :: RelMapFieldInfo -> Bool
isIndirect info =: { fld_select, fld_match} = (isJust fld_select) && (isJust fld_match)

//Checks if a field is an outgoing pointer
isOutPointer :: RelMapFieldInfo -> Bool
isOutPointer info =: {fld_table, fld_select, rec_table, val_fields} = (fld_table == rec_table) && (isJust fld_select) && (not (isEmpty val_fields))

// --- SQL generation functions --- //

mkSelectQuery :: !RelMapFieldInfo -> String
mkSelectQuery info
	# select = if (isNothing info.fld_select) (getKey info.val_fields) (fromJust info.fld_select)
	# match  = if (isNothing info.fld_match) info.rec_key (fromJust info.fld_match) 
	= "SELECT " +++ select +++ " FROM " +++ info.fld_table +++ " WHERE " +++ match +++ " = ?"

mkInsertQuery :: ![RelMapFieldInfo] -> String
mkInsertQuery info
	# table				= (hd info).fld_table
	# (fields,values)	= unzip [(fromJust i.fld_select,"?")  \\ i <- info]
	= "INSERT INTO " +++ table +++ " (" +++ (join "," fields) +++  ") VALUES (" +++ (join "," values) +++  ")"

mkUpdateQuery :: ![RelMapFieldInfo] -> String
mkUpdateQuery infos
	= "UPDATE " +++ table +++ " SET " +++ (join "," fields) +++ "WHERE " +++ match +++ " = ?"
	where
		table	= (hd infos).fld_table
		match	= fromJust (hd infos).fld_select
		fields	= [(fromJust info.fld_select) +++ " = ? " \\ info <- infos]

mkDeleteQuery :: !RelMapFieldInfo -> String
mkDeleteQuery info = "DELETE FROM " +++ info.fld_table +++ " WHERE " +++ (fromJust info.fld_select) +++ " = ?"

mkLinkQuery :: !RelMapFieldInfo -> String
mkLinkQuery info
	= "INSERT INTO " +++ info.fld_table +++ " (" +++ (fromJust info.fld_select) +++ "," +++ (fromJust info.fld_match) +++ ") VALUES (?,?)"

mkUnlinkQuery :: !RelMapFieldInfo -> String
mkUnlinkQuery info
	= "DELETE FROM " +++ info.fld_table +++ " WHERE " +++ (fromJust info.fld_match) +++ " = ?"

mkCheckLinkQuery :: !RelMapFieldInfo -> String
mkCheckLinkQuery info
	= "SELECT 0 FROM " +++ info.fld_table +++ " WHERE ( " +++ (fromJust info.fld_select) +++ " = ? ) AND ( " +++ (fromJust info.fld_match) +++ " = ? )"

mkCleanLinkQuery :: !RelMapFieldInfo ![SQLValue] -> String
mkCleanLinkQuery info []
	= "DELETE FROM " +++ info.fld_table +++ " WHERE " +++ (fromJust info.fld_match) +++ " = ?"
mkCleanLinkQuery info values
	= "DELETE FROM " +++ info.fld_table +++ " WHERE " +++ (fromJust info.fld_match) +++ " = ? AND NOT ( " +++ (fromJust info.fld_select) +++ " IN ( " +++ fields +++ " ))"
where
	fields = join "," ["?" \\ v <- values]

mkUnlinkDirectQuery :: !RelMapFieldInfo ![SQLValue] -> SQLStatement
mkUnlinkDirectQuery info values
	= "UPDATE " +++ info.fld_table +++ " SET " +++ (fromJust info.fld_match) +++ " = NULL WHERE " +++ (getKey info.val_fields) +++ " IN (" +++ fields +++ ")"
where
	fields = join "," ["?" \\ v <- values]

// --- Functions which manipulate token lists  --- //

//Merges the result token lists of two passes
mergeTokens :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] -> [RelMapToken]
mergeTokens [] pass1 pass2 = []
mergeTokens [i:is] pass1 pass2 
	| store i	= x ++ (mergeTokens is xs pass2) with (x,xs) = (takeTokens i.val_list pass1)
	| otherwise	= x ++ (mergeTokens is pass1 xs) with (x,xs) = (takeTokens i.val_list pass2)

//Zips two token lists on a per field basis
zipTokens :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] -> [RelMapToken]
zipTokens [] tokx toky = []
zipTokens [i:is] tokx toky = x ++ y ++ (zipTokens is xs ys)
where
	(x,xs) = takeTokens i.val_list tokx
	(y,ys) = takeTokens i.val_list toky

//Takes all the tokens for one field from the list, the Bool indicates if we need to take a list or a single value
takeTokens :: Bool [RelMapToken] -> ([RelMapToken],[RelMapToken])
takeTokens False [] = ([],[])
takeTokens False tokens = ([hd tokens],tl tokens)
takeTokens True [] = ([],[])
takeTokens True [RelMapTerminator:xs] = ([RelMapTerminator],xs)
takeTokens True [x:xs] = ([x:y],ys) where (y,ys) = takeTokens True xs

//Finds the tokens from the first list that are not present in the second
removedTokens :: [RelMapToken] [RelMapToken] -> [RelMapToken]
removedTokens orig new
	= [x \\ x <- init orig | not (isMember x (init new))] ++ [RelMapTerminator]

// --- Functions to piggyback overrides at the front of the tokenlist --- //

//Create the override tokens
mkOverrides :: [RelMapFieldInfo] RelMapToken -> [RelMapToken]
mkOverrides [] id = []
mkOverrides [i:is] id
	| isNothing i.fld_select	= [RelMapOverride (i.fld_table +++ "_" +++ (fromJust i.fld_match)) (fromValue id): mkOverrides is id]
								= mkOverrides is id

//Remove the overrides from the front of the token list
takeOverrides :: [RelMapToken] -> ([RelMapToken],[RelMapToken])
takeOverrides tokens = (takeWhile isOverride tokens, dropWhile isOverride tokens)
where
	isOverride (RelMapOverride f o)	= True
	isOverride _					= False

//Apply the overrides on a list of tokens
appOverrides :: [RelMapFieldInfo] [RelMapToken] [RelMapToken] -> [RelMapToken]
appOverrides info values overrides = [appOverride i v overrides \\ (i,v) <- (zip (info,values))]
where
	appOverride i v [] = v
	appOverride i v [x:xs]
		| (isNothing i.fld_select)	= v
		| otherwise = case x of
			(RelMapOverride f o)	= if ( f == (i.fld_table +++ "_" +++ (fromJust i.fld_select))) (RelMapValue o) v
			_						= v

// --- General helper functions --- //

instance == RelMapToken
where
	(==) (RelMapValue x)		(RelMapValue y)			= x == y
	(==) (RelMapTerminator)		(RelMapTerminator)		= True
	(==) (RelMapOverride xs xv)	(RelMapOverride ys yv)	= xs == ys && xv == yv
	(==) _						_						= False

//Tests if a given record is an ID (or an object)
isID :: String -> Bool
isID s = (s % ((size s) - 2, size s)) == "ID"

//Tests if a token is a value
isValue :: RelMapToken -> Bool
isValue (RelMapValue x)	= True
isValue _				= False

//Tests if a token is a value and is zero
isZeroValue :: RelMapToken -> Bool
isZeroValue (RelMapValue (SQLVInteger x))	= x == 0
isZeroValue _								= False

//Tests if a token is null value
isNullValue :: RelMapToken -> Bool
isNullValue (RelMapValue SQLVNull)		= True
isNullValue _							= False

fromValue :: RelMapToken -> SQLValue
fromValue (RelMapValue x) = x
fromValue _				= SQLVNull

getValues :: [RelMapToken] -> [SQLValue]
getValues tokens = map fromValue (takeWhile isValue tokens)

getKey :: [RelMapFieldInfo] -> String
getKey fields				= fromJust (hd fields).fld_select

getTable :: [RelMapFieldInfo] -> String
getTable fields			= (hd fields).fld_table

fetchIDToken :: *cur -> (Maybe SQLError, RelMapToken, *cur) | SQLCursor cur
fetchIDToken cursor
	# (err, val, cursor)	= insertId cursor
	= (err, RelMapValue (SQLVInteger val), cursor)


//--- debug functions --//
instance toString RelMapToken
where
	toString (RelMapValue v)		= "RelMapValue " +++ (toString v)
	toString (RelMapTerminator)		= "RelMapTerminator"
	toString (RelMapOverride f v)	= "RelMapOverride " +++ f +++ " " +++ (toString v)

tokenString :: [RelMapToken] -> String
tokenString [] = ""
tokenString [x] = toString x
tokenString [x:xs] = toString x +++ ", " +++ tokenString xs
