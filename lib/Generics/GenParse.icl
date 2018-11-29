implementation module GenParse

import StdGeneric, StdEnv, StdMaybe

//---------------------------------------------------------------------------


:: StringInput = { si_str :: !String, si_pos :: !Int} 

mkStringInput :: String -> StringInput 
mkStringInput str = {si_str = str, si_pos = 0}

instance ParseInput StringInput where
	parseInput s=:{si_pos, si_str}
		#! size_str = size si_str
		| size_str == si_pos 
			= (Nothing, {s & si_str = si_str})
		| otherwise
			#! ch = si_str.[si_pos]
			= (Just ch, {s & si_str = si_str, si_pos = inc si_pos})

instance ParseInput File where	
	parseInput file 
		# (ok, c, file) = sfreadc file
		| ok
			= (Just c, file)
			= (Nothing, file)
						
//---------------------------------------------------------------------------

// lex tokens
:: Token 
	= TokenInt Int
	| TokenChar Char
	| TokenReal Real 
	| TokenBool Bool
	| TokenString String
	| TokenIdent String
	| TokenOpenPar
	| TokenClosePar
	| TokenOpenCurly
	| TokenCloseCurly
	| TokenOpenList
	| TokenCloseList
	| TokenComma
	| TokenEnd
	| TokenError String

instance toString Token where
	toString (TokenInt x) = toString x
	toString (TokenChar x) = toString x
	toString (TokenReal x) = toString x
	toString (TokenBool x) = toString x
	toString (TokenString x) = x
	toString (TokenIdent x) = x
	toString TokenOpenPar = "("
	toString TokenClosePar = ")"
	toString TokenOpenCurly = "{"
	toString TokenCloseCurly = "}"
	toString TokenOpenList = "["
	toString TokenCloseList = "]"	
	toString TokenComma = ","
	toString TokenEnd = "<end>"
	toString (TokenError err) = "<error: " +++ err +++ ">"

// preparsed expressions
:: Expr 
	= ExprInt Int
	| ExprChar Char
	| ExprReal Real 
	| ExprBool Bool
	| ExprString String
	| ExprIdent String
	| ExprApp {Expr} 
	| ExprTuple {Expr}
	| ExprField String Expr
	| ExprRecord (Maybe String) {Expr}
	| ExprList [Expr]
	| ExprArray [Expr]
	| ExprEnd Token
	| ExprError String

	// aux
	| ExprUnit
	| ExprAppInInfix {Expr} GenConsAssoc Int GenConsAssoc
	| ExprPair Expr Expr


instance toString Expr where
	toString (ExprInt x) = toString x
	toString (ExprChar x) = toString x
	toString (ExprBool x) = toString x
	toString (ExprReal x) = toString x
	toString (ExprString x) = x
	toString (ExprIdent x) = x
	toString (ExprApp xs) = "(" +++ tostr [x\\x<-:xs] +++ ")"
	where
		tostr [] = ""
		tostr [x] = toString x
		tostr [x:xs] = toString x +++ " " +++ tostr xs
	toString (ExprTuple xs) = "(" +++ tostr [x\\x<-:xs] +++ ")"
	where
		tostr [] = ""
		tostr [x] = toString x
		tostr [x:xs] = toString x +++ ", " +++ tostr xs
	toString (ExprRecord name xs) = "{" +++ tostr [x\\x<-:xs] +++ "}"
	where
		tostr [] = ""
		tostr [x] = toString x
		tostr [x:xs] = toString x +++ ", " +++ tostr xs
	toString (ExprField name expr) = name +++ "=" +++ toString expr

		
:: ParseState s =
	{ ps_input 	:: !s			// lex input
	, ps_char 	:: !Maybe Char	// unget char
	, ps_tokens :: ![Token]		// unget tokens
	}

lexGetChar ps=:{ps_char=Nothing, ps_input}
	# (mc, ps_input) = parseInput ps_input
	= (mc, {ps & ps_input = ps_input})
lexGetChar ps=:{ps_char} = (ps_char, {ps & ps_char = Nothing})

lexUngetChar c ps=:{ps_char=Nothing} = {ps & ps_char = Just c}
lexUngetChar c ps = abort "cannot unget\n"	

isSpecialChar	:: !Char -> Bool
isSpecialChar '~'	= True
isSpecialChar '@'	= True
isSpecialChar '#'	= True
isSpecialChar '$'	= True
isSpecialChar '%'	= True
isSpecialChar '^'	= True
isSpecialChar '?'	= True
isSpecialChar '!'	= True
isSpecialChar '+'	= True
isSpecialChar '-'	= True
isSpecialChar '*'	= True
isSpecialChar '<'	= True
isSpecialChar '>'	= True
isSpecialChar '\\'	= True
isSpecialChar '/'	= True
isSpecialChar '|'	= True
isSpecialChar '&'	= True
isSpecialChar '='	= True
isSpecialChar ':'	= True
isSpecialChar '.'	= True
isSpecialChar c		= False

//----------------------------------------------------------------------------------		
// lex input

lexUngetToken token ps=:{ps_tokens} = {ps & ps_tokens = [token:ps_tokens]}

lexGetToken ps=:{ps_tokens=[token:tokens]} = (token, {ps & ps_tokens = tokens})
lexGetToken ps=:{ps_tokens=[]}
	= lex ps
where
	lex s	
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing  -> (TokenEnd, s)
			Just '\0' -> (TokenEnd, s)
			Just '(' -> (TokenOpenPar, s)
			Just ')' -> (TokenClosePar, s)
			Just '{' -> (TokenOpenCurly, s)
			Just '}' -> (TokenCloseCurly, s)
			Just '[' -> (TokenOpenList, s)
			Just ']' -> (TokenCloseList, s)
			Just ',' -> (TokenComma, s)
			Just '\'' -> lex_char 0 [] s
			Just '"'  -> lex_string 0 [] s
			Just '_' -> lex_ident 1 ['_'] s
			Just '`' -> lex_ident 1 ['`'] s
			Just '+'
				# (mc, s) = lexGetChar s
				-> case mc of
					Nothing -> (TokenIdent "+", s)
					Just c
						| isDigit c
							-> lex_number +1 (lexUngetChar c s)
						| otherwise
							-> lex_ident 1 ['+'] (lexUngetChar c s)
			Just '-'
				# (mc, s) = lexGetChar s
				-> case mc of
					Nothing -> (TokenIdent "-", s)
					Just c
						| isDigit c
							-> lex_number -1 (lexUngetChar c s)
						| otherwise
							-> lex_funny_ident 1 ['-'] (lexUngetChar c s) // PK
	//						-> lex_ident 1 ['-'] (lexUngetChar c s)
			Just c
				| isSpace c
					-> lex s 
				| isDigit c
					-> lex_number +1 (lexUngetChar c s)
				| isAlpha c
					-> lex_ident 1 [c] s
				| isSpecialChar c
					-> lex_funny_ident 1 [c] s
				| otherwise
					-> (TokenError ("Unknown character " +++ toString c), s)

	lex_digits s 
		= lex_digits_acc 0 [] s	
	lex_digits_acc num acc s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing 
				-> (num, acc, s)
			Just c
				| isDigit c
					-> lex_digits_acc (inc num) [digitToInt c:acc] s
				| otherwise 
					-> (num, acc, lexUngetChar c s)									

	digits_to_int :: [Int] -> Int
	digits_to_int [] = 0
	digits_to_int [digit:digits] = digit + 10 * digits_to_int digits 

	digits_to_real :: [Int] -> Real 
	digits_to_real [] = 0.0
	digits_to_real [digit:digits] = toReal digit + 10.0 * digits_to_real digits

	lex_number sign s
		#! (num_digits, digits, s) = lex_digits s
		#! (mc, s) = lexGetChar s
		= case mc of		 
			Nothing -> (TokenInt (sign * digits_to_int digits), s)
			Just '.'
				-> lex_real_with_fraction (toReal sign) (digits_to_real digits) s
			Just 'E'
				#! real = toReal sign * digits_to_real digits 
				-> lex_real_with_exp real s
			Just 'e'
				#! real = toReal sign * digits_to_real digits 
				-> lex_real_with_exp real s
			Just c	
				-> (TokenInt (sign * digits_to_int digits), lexUngetChar c s)									

	lex_real_with_fraction sign real s
		#! (num_digits, digits, s) = lex_digits s
		#! fraction = digits_to_real digits  / 10.0^ toReal num_digits	
		#! real = sign * (real + fraction)	
		#! (mc, s) = lexGetChar s
		= case mc of		 
			Nothing -> (TokenReal real, s)
			Just 'E'
				-> lex_real_with_exp real s
			Just 'e'
				-> lex_real_with_exp real s
			Just c	
				-> (TokenReal real, lexUngetChar c s)									

	lex_real_with_exp real s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing -> (TokenReal real, s)
			Just '+' 
				#! (num_digits, digits, s) = lex_digits s
				-> (TokenReal (real * 10.0 ^ digits_to_real digits), s)  
			Just '-' 
				#! (num_digits, digits, s) = lex_digits s
				-> (TokenReal (real * 10.0 ^ (-1.0 * digits_to_real digits)), s)  
			Just c 
				| isDigit c
					#! (num_digits, digits, s) = lex_digits (lexUngetChar c s)
					-> (TokenReal (real * 10.0 ^ digits_to_real digits), s)  
				| otherwise	
					-> (TokenError "error in real constant", s)
						
	lex_ident num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing -> (mktoken num_chars acc_chars, s)
			Just '_' -> lex_ident (inc num_chars) ['_':acc_chars] s
			Just '`' -> lex_ident (inc num_chars) ['`':acc_chars] s
			Just c	
				| isAlphanum c
					-> lex_ident (inc num_chars) [c:acc_chars] s
				| otherwise  
					-> (mktoken num_chars acc_chars, lexUngetChar c s)									
	where
		mktoken num_chars acc_chars
			= case mk_str num_chars acc_chars of
				"True"  -> TokenBool True
				"False" -> TokenBool False
				str		-> TokenIdent str			

	lex_funny_ident num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing 	-> (TokenIdent (mk_str num_chars acc_chars), s)
			Just c
				| isSpecialChar c
					-> lex_funny_ident (inc num_chars) [c:acc_chars] s
				| otherwise		
					-> (TokenIdent (mk_str num_chars acc_chars), lexUngetChar c s)									

	lex_string num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing -> (TokenError "error in string constant", s)
			Just '"' -> (TokenString (mk_str num_chars acc_chars), s)
			Just '\\' 
				#! (mc, s) = lex_special_char s
				-> case mc of
					Nothing -> (TokenError "error in string constant", s)
					Just c -> lex_string (inc num_chars) [c:acc_chars] s
			Just c	-> lex_string (inc num_chars) [c:acc_chars] s


	lex_char num_chars acc_chars s
		# (mc, s) = lexGetChar s
		= case mc of
			Nothing -> (TokenError "error in char constant", s)
			Just '\'' 
				| num_chars == 1
					-> (TokenChar (hd acc_chars), s)
				| num_chars == 0
					-> (TokenError "char constant contains no characters ", s)
				| otherwise 	
					-> (TokenError "char constant contains more than one character", s)
			Just '\\' 
				#! (mc, s) = lex_special_char s
				-> case mc of
					Nothing -> (TokenError "error in char constant", s)
					Just c -> lex_char (inc num_chars) [c:acc_chars] s
			Just c	-> lex_char (inc num_chars) [c:acc_chars] s

	lex_special_char s 
		#! (mc, s) = lexGetChar s
		= case mc of
			Just 'n' -> (Just '\n', s)
			Just 'r' -> (Just '\r', s)
			Just 'f' -> (Just '\f', s)
			Just 'b' -> (Just '\b', s)
			Just 't' -> (Just '\t', s)
			Just '\\' -> (Just '\\', s)
			Just '\'' -> (Just '\'', s)
			Just '\"' -> (Just '\"', s)
			Just '\0' -> (Just '\0', s)
			//Just '\x' -> abort "lex: hex char not implemented\n"
			//Just '\0' -> abort "lex: oct char not implemented\n"
			_ -> (mc, s)

	mk_str num_chars acc_chars
		# str = createArray num_chars ' '
		= fill (dec num_chars) acc_chars str
	where	
		fill i [] str 		= str
		fill i [x:xs] str 	= fill (dec i) xs {str & [i] = x}
	

//----------------------------------------------------------------------------------		
// preparse input


:: ParseEnv = PETop | PETuple | PEPar | PERecord | PEList

preParse :: (ParseState s) -> (Expr, ParseState s) | ParseInput s
preParse s 
	= parse_expr PETop s
where
	parse_expr env s
		= parse_app env [] s
	
	parse_app env exprs s
		#! (token, s) = lexGetToken s
		= parse token env exprs s
	where
		parse TokenComma PETuple exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PERecord exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PEList exprs	s 	= (mkexpr exprs, lexUngetToken TokenComma s)
		parse TokenComma PETop exprs s 		= (ExprError "end of input expected instead of ,", s)
		parse TokenComma PEPar exprs s 		= (ExprError ") expected instead of ,", s)
		parse TokenComma env exprs s 		= abort "unknown env\n"

		parse TokenClosePar PETuple	exprs s	= (mkexpr exprs, lexUngetToken TokenClosePar s)
		parse TokenClosePar PERecord exprs s = (ExprError "} expected instead of )", s)
		parse TokenClosePar PEList exprs s  = (ExprError "] expected instead of )", s)
		parse TokenClosePar PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenClosePar PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenClosePar s)
		parse TokenClosePar env exprs s 	= abort "unknown env\n"

		parse TokenCloseCurly PETuple	exprs s	= (ExprError ") expected instead of }", s)
		parse TokenCloseCurly PEList	exprs s	= (ExprError "] expected instead of }", s)
		parse TokenCloseCurly PERecord exprs s = (mkexpr exprs, lexUngetToken TokenCloseCurly s)
		parse TokenCloseCurly PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenCloseCurly PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenCloseCurly s)
		parse TokenCloseCurly env exprs s 	= abort "unknown env\n"

		parse TokenCloseList PETuple exprs s	= (ExprError ") expected instead of ]", s)
		parse TokenCloseList PERecord exprs s = (ExprError "} expected instead of ]", s)
		parse TokenCloseList PEList exprs s = (mkexpr exprs, lexUngetToken TokenCloseList s)
		parse TokenCloseList PETop	exprs s	= (ExprError "end of input expected instead of )", s)
		parse TokenCloseList PEPar	exprs s = (mkexpr exprs, lexUngetToken TokenCloseList s)
		parse TokenCloseList env exprs s 	= abort "unknown env\n"

		parse TokenEnd PETuple exprs s		= (ExprError ") or, expected instead of end of input", s)
		parse TokenEnd PERecord exprs s		= (ExprError "} or, expected instead of end of input", s)
		parse TokenEnd PEList exprs s		= (ExprError "] or, expected instead of end of input", s)
		parse TokenEnd PETop exprs s		= (mkexpr exprs, lexUngetToken TokenEnd s)
		parse TokenEnd PEPar exprs s		= (ExprError ") expected instead of end of input",s)
		parse TokenEnd env exprs s 			= abort "unknown env\n"
	
		parse (TokenInt x) env exprs s 		= parse_app env [ExprInt x:exprs] s
		parse (TokenBool x) env exprs s 	= parse_app env [ExprBool x:exprs] s
		parse (TokenReal x) env exprs s 	= parse_app env [ExprReal x:exprs] s
		parse (TokenChar x) env exprs s 	= parse_app env [ExprChar x:exprs] s
		parse (TokenString x) env exprs s 	= parse_app env [ExprString x:exprs] s
		parse (TokenIdent x) env exprs s 	= parse_app env [ExprIdent x:exprs] s
		parse TokenOpenPar env exprs s 	
			# (expr, s)	= parse_par_expr s
			= case expr of
				ExprError err 	-> (ExprError err, s)
				_				->  parse_app env [expr:exprs] s
		parse TokenOpenCurly env exprs s
			# (expr, s) = parse_record_or_array s
			= case expr of
				ExprError err 	-> (ExprError err, s)
				_				->  parse_app env [expr:exprs] s
		parse TokenOpenList env exprs s	
			# (expr, s) = parse_list s
			= case expr of
				ExprError err 	-> (ExprError err, s)
				_				->  parse_app env [expr:exprs] s
		parse (TokenError err) env exprs s 
			= (ExprError ("lex error in parse_app: "  +++ err), s) 		
				
		parse token env exprs s 
			= abort ("parse app - unknown token " +++ toString token)
		
		
		mkexpr []		= ExprError "expression expected"
		mkexpr [expr] 	= expr
		mkexpr exprs 	= ExprApp {e\\e <- reverse exprs}

	parse_par_expr s
		#! (expr, s) = parse_expr PETuple s
		= case expr of
			ExprError err -> (ExprError err, s)
			_
				#! (token, s) = lexGetToken s
				-> case token of
					TokenClosePar -> (expr, s)
					TokenComma -> parse_tuple [expr] (lexUngetToken token s)
					_	-> (ExprError (", or ) expected, found " +++ toString token), s)
							 				
	parse_tuple exprs s 
		#! (token, s) = lexGetToken s
		= case token of
			TokenComma 
				#! (expr, s) = parse_expr PETuple s
				-> case expr of
					ExprError err -> (ExprError err, s)
					_	-> parse_tuple [expr:exprs] s
			TokenClosePar 
				-> (ExprTuple {e\\e<-reverse exprs}, s)
			_ 	
				-> (ExprError "parse tuple: , or ) expected", s) 		

	parse_list s
		#! (token, s) = lexGetToken s
		= case token of
			TokenCloseList 
				-> (ExprList [], s)
			_  
				#! (expr, s) = parse_expr PEList (lexUngetToken token s)
				-> case expr of
					ExprError err -> (ExprError (err +++ " ; parse list"), s)
					_ -> parse_rest [expr] s
	where
		parse_rest exprs s		
			#! (token, s) = lexGetToken s
			= case token of
				TokenComma 
					#! (expr, s) = parse_expr PEList s
					-> case expr of
						ExprError err -> (ExprError err, s)
						_	-> parse_rest [expr:exprs] s
				TokenCloseList 
					-> (ExprList (reverse exprs), s)
				_ 	
					-> (ExprError "parse list: , or ] expected", s) 		

		
	parse_record_or_array s 
		#! (token, s) = lexGetToken s
		= case token of
			TokenCloseCurly 
				-> (ExprArray [], s)
			TokenIdent name
				#! (token, s) = lexGetToken s
				-> case token of
					TokenIdent "="
						#! (expr, s) = parse_expr PERecord s
						-> parse_record Nothing [ExprField name expr] s
					TokenIdent "|"
						-> parse_record (Just ("_" +++ name)) [] (lexUngetToken TokenComma s)
					_
						#! (expr, s) = parse_expr PERecord 
							(lexUngetToken (TokenIdent name) (lexUngetToken token s))
						-> parse_array [expr] s
			_	
				#! (expr, s) = parse_expr PERecord (lexUngetToken token s)
				-> parse_array [expr] s
	where
		parse_record rec_name fields s
			#! (token, s) = lexGetToken s
			= case token of
				TokenCloseCurly 
					-> (ExprRecord rec_name {e\\e<- reverse fields}, s)
				TokenComma
					#! (token, s) = lexGetToken	s
					-> case token of
						TokenIdent field_name
							#! (token, s) = lexGetToken	s
							-> case token of
								TokenIdent "=" 
									#! (expr, s) = parse_expr PERecord s
									-> parse_record rec_name [ExprField field_name expr:fields] s
				_ -> (ExprError ("parse record failed on token " +++ toString token), s)			

		parse_array exprs s
			#! (token, s) = lexGetToken s
			= case token of
				TokenCloseCurly 
					-> (ExprArray (reverse exprs), s)
				TokenComma
					#! (expr, s) = parse_expr PERecord s
					-> parse_array [expr:exprs] s
				_ -> (ExprError ("parse array failed on token " +++ toString token), s)			


//----------------------------------------------------------------------------------		

generic gParse a :: Expr -> Maybe a

gParse{|Int|} (ExprInt x)			= Just x 
gParse{|Int|} _						= Nothing

gParse{|Char|} (ExprChar x)			= Just x 
gParse{|Char|} _					= Nothing

gParse{|Bool|} (ExprBool x)			= Just x  
gParse{|Bool|} _					= Nothing

gParse{|Real|} (ExprReal x)			= Just x  
gParse{|Real|} _					= Nothing

gParse{|String|} (ExprString x)		= Just x
gParse{|String|} _					= Nothing 

gParse{|UNIT|}	ExprUnit			= Just UNIT
gParse{|UNIT|} _					= Nothing 

gParse{|PAIR|} fx fy (ExprPair ex ey)	
	= case fx ex of
		Just x -> case fy ey of
			Just y 					-> Just (PAIR x y)
			Nothing 				-> Nothing
		Nothing 					-> Nothing
gParse{|PAIR|} fx fy _				= Nothing

gParse{|EITHER|} fl fr expr	
	= case fl expr of
		Nothing 					-> case fr expr of
			Nothing 				-> Nothing
			Just x 					-> Just (RIGHT x)
		Just x 						-> Just (LEFT x)	
		
gParse{|CONS of d|} parse_arg expr
	| d.gcd_arity == 0 	
		= parse_nullary expr
	| is_tuple d.gcd_name
		= parse_tuple expr	
	| otherwise
		= case d.gcd_prio of
			GenConsNoPrio			
				-> parse_nonfix expr
			GenConsPrio assoc prio 	
				-> parse_infix assoc prio expr 						
where
	parse_nullary (ExprIdent name)
		| name == d.gcd_name
			= mapMaybe CONS (parse_arg ExprUnit)
	parse_nullary _
		= Nothing

	parse_nonfix (ExprApp exprs)
		= parse_nonfix1 exprs
	parse_nonfix (ExprAppInInfix exprs _ _ _)
		= parse_nonfix1 exprs
	parse_nonfix _ 
		= Nothing

	parse_nonfix1 exprs
		#! size_exprs = size exprs
		| size_exprs == d.gcd_arity + 1 && is_ident d.gcd_name exprs.[0]
			#! arg_exprs = [exprs.[i] \\ i <- [1 .. size_exprs - 1]]
			= mapMaybe CONS (parse_arg (mkprod arg_exprs))
		| otherwise
			= Nothing
	
	is_ident wanted_name (ExprIdent name) = name == wanted_name
	is_ident _ _ = False		

	parse_tuple (ExprTuple exprs) 
		= mapMaybe CONS (parse_arg (mkprod [e\\e<-:exprs]))
	parse_tuple expr = Nothing
	
	parse_infix this_assoc this_prio (ExprApp exprs)
		= parse_infix1 this_assoc this_prio exprs
	parse_infix this_assoc this_prio (ExprAppInInfix exprs outer_assoc outer_prio branch)
		| this_prio > outer_prio
			= parse_infix1 this_assoc this_prio exprs
		| this_prio < outer_prio
			= Nothing
		| otherwise
			= case (this_assoc, outer_assoc, branch) of
				(GenConsAssocLeft, GenConsAssocLeft, GenConsAssocLeft)
					-> parse_infix1 this_assoc this_prio exprs
				(GenConsAssocRight, GenConsAssocRight, GenConsAssocRight)
					-> parse_infix1 this_assoc this_prio exprs
				_ -> Nothing
	parse_infix this_assoc this_prio expr
		= Nothing
		
	parse_infix1 this_assoc this_prio exprs
		#! size_exprs = size exprs
		| size_exprs < 3 = Nothing
		= case (case this_assoc of GenConsAssocLeft -> find_last; _ -> find_first) exprs of
			Nothing -> Nothing
			Just op_index
				#! left_arg  = mkarg GenConsAssocLeft {exprs.[i] \\ i <- [0 .. op_index - 1]}
				#! right_arg = mkarg GenConsAssocRight {exprs.[i] \\ i <- [op_index + 1 .. size_exprs - 1]}
				-> mapMaybe CONS (parse_arg (ExprPair left_arg right_arg))
	where
		mkarg branch exprs
			= case size exprs of
				0 -> abort "mkarg\n"
				1 -> exprs.[0]
				_ -> ExprAppInInfix exprs this_assoc this_prio branch
	
	find_last exprs 
		= find (size exprs - 2) exprs
	where
		find i exprs
			| i < 1
				= Nothing
			| otherwise	
				= case exprs.[i] of
					ExprIdent s | s == d.gcd_name 	-> Just i  
				 	_ 								-> find (dec i) exprs	
	find_first exprs
		= find 1 exprs
	where
		find i exprs
			| i >= size exprs - 1
				= Nothing
			| otherwise	
				= case exprs.[i] of
					ExprIdent s | s == d.gcd_name 	-> Just i  
				 	_ 								-> find (inc i) exprs	

	is_tuple name 
		#! size_name = size name
		= (size_name == 7 || size_name == 8)
		&& name.[0] == '_'
		&& name.[1] == 'T'
		&& name.[2] == 'u'
		&& name.[3] == 'p'
		&& name.[4] == 'l'
		&& name.[5] == 'e'
		&& isDigit name.[6]
		&& (size_name == 7 || isDigit name.[7])

gParse{|RECORD of d|} parse_arg (ExprRecord rec_name exprs)
	| check_name rec_name d.grd_name
		= mapMaybe RECORD (parse_arg (mkprod [e\\e<-:exprs]))
		= Nothing
	where
		check_name Nothing cons_name = True
		check_name (Just rec_name) cons_name = rec_name == cons_name
gParse{|RECORD of d|} parse_arg expr
	= Nothing

mkprod [] 		= abort "mkprod\n"
mkprod [expr] 	= expr
mkprod exprs 	
	# (xs, ys) = splitAt (length exprs / 2) exprs
	= ExprPair (mkprod xs) (mkprod ys)
	
gParse{|FIELD of d|} parse_arg (ExprField name value) 
	| d.gfd_name == name
		= mapMaybe FIELD (parse_arg value)
		= Nothing
gParse{|OBJECT|} parse_arg expr
	= mapMaybe OBJECT (parse_arg expr)

gParse{|[]|} parse_arg (ExprList exprs) 
	= maybeAll (map parse_arg exprs)
gParse{|[]|} parse_arg _ = Nothing

gParse{|{}|} parse_arg (ExprArray exprs)
	= mapMaybe (\xs -> {x\\x<-xs}) (maybeAll (map parse_arg exprs)) 
gParse{|{}|} parse_arg _ = Nothing
		
gParse{|{!}|} parse_arg (ExprArray exprs)
	= mapMaybe (\xs -> {x\\x<-xs}) (maybeAll (map parse_arg exprs))
gParse{|{!}|} parse_arg _ = Nothing

maybeAll [] 			= Just []
maybeAll [Nothing:_] 	= Nothing
maybeAll [Just x: mxs] 
	= case maybeAll mxs of
		Nothing -> Nothing
		Just xs -> Just [x:xs]  

//----------------------------------------------------------------------------------		

preParseInput :: s -> Expr | ParseInput s
preParseInput input 
	# (expr, s) = preParse {ps_input=input, ps_char = Nothing, ps_tokens = [] }
	= expr
	
preParseString :: String -> Expr
preParseString str = preParseInput {si_pos = 0, si_str = str}

preParseFile :: File -> Expr 
preParseFile file = preParseInput file

parseString :: String -> Maybe a | gParse{|*|} a
parseString str = gParse{|*|} (preParseString str)

parseFile :: File -> Maybe a | gParse{|*|} a
parseFile file = gParse{|*|} (preParseFile file)

//Start = preParseString "{rec_field = A (B1, B2) (C D), rec_field2 = (X,Y)}"
//Start = preParseString "123.456e1"
//Start = preParseString "([1,2,3], [4,5,6])"
//Start = preParseString "{A B D,X Y Z,I J K}"

//----------------------------------------------------------------------------------		
/*
:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: T
	= :+: infixl 2 T T
	| :-: infixl 2 T T
	| :*: infixl 3 T T
	| :->: infixr 4 T T
	| U

:: Rec = { rec_x :: T, rec_y :: (.Tree Int Real, Real) }

derive gParse (,), (,,), (,,,), Tree, T, Rec
derive bimap Maybe, ParseState, []
*/
//Start :: Maybe T
//Start = parseString "U :+: U :+: U"

//Start :: Maybe (Tree Int Int)
//Start = parseString "Bin 1 (Tip 2) (Tip 3)"

//Start :: Maybe (Tree Int Int, Int)
//Start = parseString "((Bin 1 (Tip (2)) (Tip 3), 1000))"

//Start :: Maybe Rec
//Start = parseString "{ Rec | rec_x = U :+: U :+: U, rec_y = (Bin 1.1 (Tip 2) (Tip 3), 1.09) }"

//Start :: Maybe [Tree Int Int]
//Start = parseString "[Bin 1 (Tip (2)) (Tip 3), Tip 100, Tip 200]" 

//Start = preParseString "1.23e12"

/*
Start :: *World -> (Maybe Rec, *World)
Start w 
	#! (ok, f, w) = sfopen "test.txt" FReadText w
	| not ok
		= (abort "sfopen failed", w)
		= (parseFile f, w)		
*/