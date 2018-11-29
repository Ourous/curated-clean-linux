definition module StdGeneric

// embedding-projection
:: Bimap a b = { map_to :: .(a -> b), map_from :: .(b -> a) }
bimapId :: Bimap .a .a

// generic representation
:: UNIT = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR a b = PAIR a b

// for constructor information
:: OBJECT a =: OBJECT a		// object marking
:: CONS a = CONS a 			// constructor marking
:: RECORD a = RECORD a 		// record marking
:: FIELD a =: FIELD a 		// record field marking

:: GenericTypeDefDescriptor =
	{ gtd_name  	:: String 
	, gtd_arity 	:: Int 
	, gtd_num_conses :: Int
	, gtd_conses 	:: [GenericConsDescriptor]
	}

:: GenericConsDescriptor = 
	{ gcd_name 		:: String
	, gcd_arity  	:: Int	
	, gcd_prio 		:: GenConsPrio				// priority and associativity
	, gcd_type_def	:: GenericTypeDefDescriptor // type def of the constructor
	, gcd_type 		:: GenType					// type of the constructor
	, gcd_index 	:: Int						// index of the contructor in the type def
	}
:: GenConsPrio = GenConsNoPrio | GenConsPrio GenConsAssoc Int
:: GenConsAssoc = GenConsAssocNone | GenConsAssocLeft | GenConsAssocRight   	

:: GenericRecordDescriptor = 
	{ grd_name 		:: String
	, grd_arity  	:: Int	
	, grd_type_arity:: Int 						// arity of the type
	, grd_type 		:: GenType					// type of the constructor
	, grd_fields 	:: [String]
	}

:: GenericFieldDescriptor = 
	{ gfd_name 	:: String
	, gfd_index :: Int							// index of the field in the record
	, gfd_cons 	:: GenericRecordDescriptor 		// the record constructor
	}

:: GenType 	= GenTypeCons String
			| GenTypeVar !Int
			| GenTypeApp GenType GenType
			| GenTypeArrow GenType GenType
	
// determine the path in the generic binary-sum-tree of a constructor
:: ConsPos = ConsLeft | ConsRight
getConsPath :: !GenericConsDescriptor -> [ConsPos]

// generic bidirectional mapping
generic bimap a b :: Bimap .a .b

derive bimap c
derive bimap PAIR
derive bimap EITHER
derive bimap OBJECT
derive bimap CONS
derive bimap RECORD
derive bimap FIELD
derive bimap (->)
derive bimap Bimap

// HACK: dictionaries for all generics.
// It works since all generic classes have only one method and do not inherit 
// from other classes
:: GenericDict a = { generic_dict :: !a }
:: GenericDict0 a = { generic_dict0 :: a }
