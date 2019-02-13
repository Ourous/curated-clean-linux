/*
	module owner: Martijn Vervoort
*/
definition module type_io_common

// common between compiler and static linker
import StdEnv
import syntax
import StdOverloaded

APPEND_DEFINING_TYPE_MODULE_NAMES_TO_TYPE_NAMES yes no :== yes

/*
// Priority
PrioCode						:== toChar 0
NoPrioCode						:== toChar 1

// Assoc
LeftAssocCode					:== toChar 2
RightAssocCode					:== toChar 3
NoAssocCode						:== toChar 4
*/

// TypeRhs
AlgTypeCode						:== (toChar 5)
SynTypeCode 					:== (toChar 6)
RecordTypeCode					:== (toChar 7)
AbstractTypeCode				:== (toChar 8)
GenericDictionaryTypeCode		:== '\x25'

// Type
//TypeTACode						:== (toChar 9)		// TA
TypeTASCode						:== (toChar 10)		// TAS
TypeArrowCode 					:== (toChar 11)		// -->
TypeConsApplyCode				:== (toChar 12)		// :@:
TypeTBCode						:== (toChar 13)		// TB
TypeGTVCode						:== (toChar 14)		// GTV
TypeTVCode						:== (toChar 15)		// TV
TypeTQVCode						:== (toChar 16)		// TempTQV
TypeTECode						:== (toChar 17)		// TE
GenericFunctionTypeCode			:== '\x24'			// TGenericFunction

// Type; TB
BT_IntCode						:== (toChar 18)	
BT_CharCode						:== (toChar 19)
BT_RealCode						:== (toChar 20)
BT_BoolCode						:== (toChar 21)
BT_DynamicCode					:== (toChar 22)
BT_FileCode						:== (toChar 23)
BT_WorldCode					:== (toChar 24)
BT_StringCode					:== (toChar 25)

// ConsVariable	
ConsVariableCVCode				:== (toChar 26)	
ConsVariableTempCVCode			:== (toChar 27)
ConsVariableTempQCVCode			:== (toChar 28)

// TypeSymbIdent
TypeSymbIdentWithoutDefinition	:== (toChar 29)		// valid only for predefined in PD_PredefinedModule e.g. _String, _List
TypeSymbIdentWithDefinition		:== (toChar 30)		// for all types which have definitions in some .icl-module

// Maybe
MaybeNothingCode				:== (toChar 31)
MaybeJustCode					:== (toChar 32)

// StrictnessList
NotStrictCode					:== (toChar 33)
StrictCode						:== (toChar 34)
StrictListCode					:== (toChar 35)

// used by {compiler,dynamic rts} to make String representation of types
PredefinedModuleName			:== "_predefined"

isPredefinedModuleName name		:== name == PredefinedModuleName

UnderscoreSystemModule			:== "_system"		// implements the predefined module

LowLevelInterfaceModule			:== "StdDynamicLowLevelInterface"

FunctionTypeConstructorAsString	:== " -> "
 
// instance toString GlobalTCType

create_type_string type_ident module_name
	:== if (type_ident == FunctionTypeConstructorAsString)
			type_ident
			(type_ident +++ (APPEND_DEFINING_TYPE_MODULE_NAMES_TO_TYPE_NAMES ("'" +++ module_name ) ""))

get_type_name_and_module_name_from_type_string :: !String -> (!String,!String)

