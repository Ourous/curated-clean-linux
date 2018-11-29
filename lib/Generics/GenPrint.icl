implementation module GenPrint

import StdGeneric, StdEnv, StdMaybe

//-------------------------------------------------------------------------------------

:: *StringOutput = {so_str :: !*String, so_pos :: !Int}


instance PrintOutput StringOutput where
	printOutput ch s=:{so_str, so_pos} 
		#! new_str = realloc_if_needed so_pos so_str 
		= {s & so_str = {new_str & [so_pos] = ch}, so_pos = inc so_pos}
	where
		realloc_if_needed :: Int u:String -> v:String, [u <= v]
		realloc_if_needed pos str
			#! size_str = size str
			| pos == size_str
				#! new_str = createArray ((size_str + 1) * 3 /2) '\0'
				#! (new_str, str) = fill 0 size_str new_str str
				= new_str
			| otherwise	
				= str 
		fill i n new_str str 
			| i == n
				= (new_str, str)
			| otherwise	
				#! (ch, str) = str![i] 
				= fill (inc i) n {new_str & [i] = ch} str	
					
instance PrintOutput File where
	printOutput x s
		= fwritec x s


//-------------------------------------------------------------------------------------


($) infixl 9
($) x y = y o x

(@) infix 8 
(@) x y = x y

mapSt f [] st = ([], st)
mapSt f [x:xs] st
	# (y, st) = f x st
	# (ys, st) = mapSt f xs st
	= ([y:ys], st)

foldSt f [] 		= id 
foldSt f [x:xs] 	= foldSt f xs o f x

//-------------------------------------------------------------------------------------
:: PrintState s =
	{ ps_output :: !s
	, ps_context :: !Context
	}
:: Context 
	= CtxNone										// initial env
	| CtxNullary									// nullary constructor
	| CtxRecord										// record constructor	
	| CtxTuple										// tuple constructor
	| CtxNonfix										// normal nonfix constructor
	| CtxInfix 										// infix constructor
		String 										// name
		GenConsAssoc 								// constructor's associativity	
		Prio 										// constructors priority	
		GenConsAssoc								// left or right argument
:: Prio :== Int
	
instance == GenConsAssoc where
	(==) GenConsAssocNone GenConsAssocNone = True
	(==) GenConsAssocLeft GenConsAssocLeft = True
	(==) GenConsAssocRight GenConsAssocRight = True
	(==) _ _ = False

mkContextCons :: GenericConsDescriptor -> Context
mkContextCons {gcd_prio=GenConsNoPrio, gcd_name, gcd_arity}
	| gcd_arity == 0
		= CtxNullary
	| is_tuple gcd_name
		= CtxTuple
	| otherwise
		= CtxNonfix
where
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
	
mkContext {gcd_prio=GenConsPrio assoc prio, gcd_name} 
	= CtxInfix gcd_name assoc prio GenConsAssocNone

needParenthesis :: Context Context -> Bool
needParenthesis this_ctx CtxNullary		= abort "needParenthesis: outer_ctx = CtxNullary"
needParenthesis CtxNullary outer_ctx 	= False
needParenthesis CtxTuple outer_ctx 		= True		// the tuple parenthesis
needParenthesis CtxNonfix CtxNone		= False
needParenthesis CtxNonfix CtxTuple		= False
needParenthesis CtxNonfix CtxRecord		= False
needParenthesis CtxNonfix CtxNonfix		= True
needParenthesis CtxNonfix (CtxInfix _ _ _ _) = False
needParenthesis (CtxInfix _ _ _ _) CtxNone = False
needParenthesis (CtxInfix _ _ _ _) CtxTuple = False
needParenthesis (CtxInfix _ _ _ _) CtxRecord = False
needParenthesis (CtxInfix _ _ _ _) CtxNonfix = True // False // PK
needParenthesis (CtxInfix _ this_assoc this_prio _) (CtxInfix _ outer_assoc outer_prio branch) 
	= 	outer_prio > this_prio 
	||  (outer_prio == this_prio && not (this_assoc == outer_assoc && this_assoc == branch))

//derive bimap PrintState

//-------------------------------------------------------------------------------------


printChar :: Char (PrintState *s) -> (PrintState *s) | PrintOutput s 
printChar ch s=:{ps_output}
 	# ps_output = printOutput ch ps_output
 	= {s & ps_output = ps_output}

printCharLiteral '\\' 	= printChar '\\' $ printChar '\\'
printCharLiteral '\n' 	= printChar '\\' $ printChar 'n'
printCharLiteral '\t' 	= printChar '\\' $ printChar 't'
printCharLiteral '\r' 	= printChar '\\' $ printChar 'r'
printCharLiteral '\b'	= printChar '\\' $ printChar 'b'
printCharLiteral '\'' 	= printChar '\\' $ printChar '\''
printCharLiteral '\"' 	= printChar '\\' $ printChar '"'
printCharLiteral '\0' 	= printChar '\\' $ printChar '0'
printCharLiteral c 		= printChar c

printString str
	#! size_str = size str
	= do_it 0 size_str str
where
	do_it i n str
		| i == n
			= id
			= printChar str.[i]
			$ do_it (inc i) n str 

printStringLiteral str
	#! size_str = size str
	= do_it 0 size_str str
where
	do_it i n str
		| i == n
			= id
			= printCharLiteral str.[i]
			$ do_it (inc i) n str 


printList f xs ps=:{ps_context}
	= 	{ print_list f xs { ps & ps_context = CtxNone} 
		& ps_context = ps_context 
		}
where
	print_list f [] = id
	print_list f [x] = f x
	print_list f [x:xs] 
		= f x 			
		$ printString ", "
		$ print_list f xs	

//-------------------------------------------------------------------------------------
generic gPrint a :: a (PrintState *s) -> (PrintState *s) | PrintOutput s
gPrint{|Int|} x st 
	= printString (toString x) st
gPrint{|Real|} x st 
	# str = toString x
	| all isDigit [c\\c<-:str] // add .0 if needed
		= printString (str +++ ".0") st 
	| str.[0] == '.'
		= printString ("0" +++ str) st
	| otherwise 	
		= printString str st
gPrint{|Bool|} x st 
	= printString (toString x) st
gPrint{|Char|} x st 
	= printChar '\'' $ printCharLiteral x $ printChar '\'' @ st 
gPrint{|String|} x st 
	= printChar '"'
	$ printStringLiteral x 
	$ printChar '"'
	@ st
gPrint{|UNIT|} x st 
	= st
	
gPrint{|EITHER|} fl fr (LEFT x) st = fl x st
gPrint{|EITHER|} fl fr (RIGHT x) st = fr x st

gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxNone}
	= abort "gOutput{|PAIR|}: CtxNone\n" 
gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxNullary}
	= abort "gOutput{|PAIR|}: CtxNullary\n" 
gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxTuple}
	= fx x $ printString ", " $ fy y @ st
gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxRecord}
	= fx x $ printString ", " $ fy y @ st
gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxNonfix}
	= fx x $ printChar ' ' $ fy y @ st	
gPrint{|PAIR|} fx fy (PAIR x y) st=:{ps_context = CtxInfix name assoc prio branch} 
	# st = fx x {st & ps_context = CtxInfix name assoc prio GenConsAssocLeft} 
	# st = printChar ' ' $ printStringLiteral name $ printChar ' ' @ st
	# st = fy y {st & ps_context = CtxInfix name assoc prio GenConsAssocRight} 
	= {st & ps_context = CtxInfix name assoc prio branch} 

gPrint{|CONS of d|} print_arg (CONS x) st=:{ps_context}
	#! ctx = mkContextCons d
	#! st = { st & ps_context = ctx }
	| needParenthesis ctx ps_context
		= 	{ printChar '(' 
			$ print print_arg ctx 
			$ printChar ')' 
			@ st 
			& ps_context = ps_context 
			}
	| otherwise
		= { print print_arg ctx st & ps_context = ps_context }
where
	print print_arg CtxNone 			
		= abort "gOutput{|CONS|}: CtxNone\n"
	print print_arg CtxNullary  		
		= printStringLiteral d.gcd_name 
	print print_arg CtxTuple
		= print_arg x
	print print_arg CtxNonfix		
		= printStringLiteral d.gcd_name
		$ printChar ' '
		$ print_arg x 
	print print_arg (CtxInfix _ _ _ _)  		
		= print_arg x

gPrint{|RECORD of d|} print_arg (RECORD x) st=:{ps_context}
	#! st = {st & ps_context = CtxRecord}
	= { printString "{ "
		$ printStringLiteral d.grd_name 
		$ printString " | "
		$ print_arg x
		$ printString " }"
		@ st
		& ps_context = ps_context
	  }

gPrint{|FIELD of d|} f (FIELD x) st
	= printStringLiteral d.gfd_name
	$ printString " = " 
	$ f x 
	@ st
gPrint{|OBJECT|} f (OBJECT x) st
	= f x st	
	
gPrint{|[]|} f xs st
	= printChar '['
	$ printList f xs 
	$ printChar ']'
	@ st

gPrint{|{}|} f xs st
	= printChar '{'
	$ printList f [ x \\ x <-: xs] 
	$ printChar '}'
	@ st

gPrint{|{!}|} f xs st
	= printChar '{'
	$ printList f [ x \\ x <-: xs] 
	$ printChar '}'
	@ st

//derive gOutput (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
	
//-------------------------------------------------------------------------------------
(<<-) infixl 0 :: (PrintState *s) a -> *(PrintState *s) | gPrint{|*|} a & PrintOutput s
(<<-) s x = gPrint{|*|} x s

mkPrintState :: *s -> PrintState *s | PrintOutput s
mkPrintState s =
	{ ps_output = s
	, ps_context = CtxNone
	} 

mkStringPrintState :: PrintState StringOutput
mkStringPrintState = mkPrintState {so_pos = 0, so_str = createArray 16 '\0'}

openFilePrintState :: String *fs -> (Maybe (PrintState *File), *fs) | FileSystem fs
openFilePrintState name fs 
	# (ok, file, fs) = fopen name FWriteText fs
	| ok 	= (Just (mkPrintState file), fs)
			= (Nothing, fs)

printToString :: a -> String | gPrint{|*|} a
printToString x
	# string_output = (mkStringPrintState <<- x).ps_output
	= string_output.so_str % (0,string_output.so_pos-1)
