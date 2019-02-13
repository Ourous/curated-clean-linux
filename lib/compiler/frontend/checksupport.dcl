definition module checksupport

import StdEnv
import syntax, predef, containers

CS_NotChecked 	:== -1
NotFound		:== -1

cModuleScope	:== 0
cGlobalScope	:== 1
cRankTwoScope	:== 2

cStdArrayImportMissing	:== 1
cStdEnumImportMissing	:== 2
cNeedStdDynamic 		:== 4
cNeedStdGeneric			:== 8
cNeedStdStrictLists		:== 16

::	Heaps =
	{	hp_var_heap			::!.VarHeap
	,	hp_expression_heap	::!.ExpressionHeap
	,	hp_type_heaps		::!.TypeHeaps
	,	hp_generic_heap		::!.GenericHeap
	}

::	ErrorAdmin = { ea_file :: !.File, ea_loc :: ![IdentPos], ea_ok :: !Bool }

::	CheckState = { cs_symbol_table :: !.SymbolTable, cs_predef_symbols :: !.PredefinedSymbols, cs_error :: !.ErrorAdmin,cs_x :: !CheckStateX }

::	CheckStateX = {x_needed_modules :: !BITVECT,x_main_dcl_module_n :: !Int, x_check_dynamic_types :: !Bool }

::	Group = { group_members :: ![Int] }

::	Component = { component_members :: !ComponentMembers }

::	ComponentMembers
	= ComponentMember !Int !ComponentMembers
	| GeneratedComponentMember !Int !FunctionInfoPtr !ComponentMembers
	| NoComponentMembers

instance == STE_Kind

cTypeDefs				:== 0
cConstructorDefs		:== 1
cSelectorDefs			:== 2
cClassDefs				:== 3
cMemberDefs				:== 4
cGenericDefs			:== 5
cGenericCaseDefs		:== 6
cInstanceDefs			:== 7
cFunctionDefs			:== 8
cMacroDefs				:== 9

cConversionTableSize	:== 10

class Erroradmin state
where
	pushErrorAdmin :: !IdentPos *state -> *state
	setErrorAdmin :: !IdentPos *state -> *state
	popErrorAdmin  :: *state -> *state

instance Erroradmin ErrorAdmin, CheckState

newPosition :: !Ident !Position -> IdentPos 
stringPosition :: !String !Position -> StringPos

checkError :: !a !b !*ErrorAdmin -> *ErrorAdmin | <<< a & <<< b
	special a={#Char},b={#Char}; a=Ident,b={#Char}
checkWarning :: !a !b !*ErrorAdmin -> *ErrorAdmin | <<< a & <<< b
checkErrorWithIdentPos :: !IdentPos !a !*ErrorAdmin -> .ErrorAdmin | <<< a;
checkErrorWithPosition :: !Ident !Position !a !*ErrorAdmin -> .ErrorAdmin | <<< a;
checkStringErrorWithPosition :: !{#Char} !Position !a !*ErrorAdmin -> *ErrorAdmin | <<< a
	special a={#Char};
checkWarningWithPosition :: !Ident !Position !a !*ErrorAdmin -> .ErrorAdmin | <<< a;

class envLookUp a :: !a !(Env Ident .b) -> (!Bool,.b)

instance envLookUp TypeVar, AttributeVar, ATypeVar

class toIdent a :: !a -> Ident

instance toIdent ConsDef, (TypeDef a), ClassDef, MemberDef, FunDef, SelectorDef // , ClassInstance
instance toIdent SymbIdent, TypeSymbIdent, BoundVar, TypeVar, ATypeVar, Ident

instance toInt STE_Kind
instance <<< IdentPos, StringPos

::	ExpressionInfo =
	{	ef_type_defs		:: !.{# CheckedTypeDef}
	,	ef_selector_defs	:: !.{# SelectorDef}
	,	ef_cons_defs		:: !.{# ConsDef}
	,	ef_member_defs		:: !.{# MemberDef}
	,	ef_class_defs		:: !.{# ClassDef}
	,	ef_generic_defs		:: !.{# GenericDef}
	,	ef_modules			:: !.{# DclModule}
	,	ef_macro_defs		:: !.{#.{#FunDef}}
	,	ef_is_macro_fun		:: !Bool
	}

retrieveGlobalDefinition :: !SymbolTableEntry !STE_Kind !Index -> (!Index, !Index)

addLocalFunctionDefsToSymbolTable :: !Level !Index !Index !Bool !*{#FunDef} !*SymbolTable !*ErrorAdmin -> (!*{# FunDef}, !*SymbolTable, !*ErrorAdmin)
addLocalDclMacroDefsToSymbolTable :: !Level !Int !Index !Index !*{#*{#FunDef}} !*SymbolTable !*ErrorAdmin -> (!*{#*{#FunDef}}, !*SymbolTable, !*ErrorAdmin)
addDefToSymbolTable :: !Level !Index !Ident !STE_Kind !*SymbolTable !*ErrorAdmin -> (!* SymbolTable, !*ErrorAdmin)
addDeclarationsOfDclModToSymbolTable :: Int !{!Declaration} !{!Declaration} !*{#DclModule} !*CheckState -> (!*{#DclModule},!*CheckState)
addGlobalDefinitionsToSymbolTable :: ![Declaration] !*CheckState -> .CheckState;
addSymbol :: !(Optional a) !Ident !Position !STE_Kind !STE_Kind !.Int !.Int !Int !*{#DclModule} !*CheckState -> (!Bool,!*{#DclModule},!*CheckState)
addImportedFunctionOrMacro :: !(Optional IndexRange) !Ident !Int !*{#DclModule} !*CheckState -> (!Bool,!*{#DclModule},!.CheckState)

removeImportedSymbolsFromSymbolTable :: Declaration !*SymbolTable -> .SymbolTable
removeFieldFromSelectorDefinition :: !Ident .Int .Int !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeDeclarationsFromSymbolTable :: ![Declaration] !Int !*SymbolTable -> *SymbolTable
removeLocalIdentsFromSymbolTable :: .Int !.[Ident] !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeIdentFromSymbolTable :: !.Int !Ident !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeImportsAndLocalsOfModuleFromSymbolTable :: !Declarations !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry
removeLocalFunctionsFromSymbolTable :: !Level !IndexRange !*{# FunDef} !*(Heap SymbolTableEntry) -> (!.{# FunDef}, !.Heap SymbolTableEntry)
removeLocalDclMacrosFromSymbolTable :: !Level !Index !IndexRange !*{#*{#FunDef}} !*(Heap SymbolTableEntry) -> (!.{#.{#FunDef}}, !.Heap SymbolTableEntry)

newFreeVariable :: !FreeVar ![FreeVar] ->(!Bool, ![FreeVar])

local_declaration_for_import :: !u:Declaration .Index -> v:Declaration, [u <= v]

:: BelongingSymbols
	=	BS_Constructors ![DefinedSymbol]
	|	BS_Fields !{#FieldSymbol}
	|	BS_Members !{#DefinedSymbol}
	|	BS_MembersAndMacros !{#DefinedSymbol} !{#MacroMember} !{#Int} !{!MacroMember}
	|	BS_Nothing

getBelongingSymbols :: !Declaration !v:{#DclModule} -> (!BelongingSymbols, !v:{#DclModule})
nrOfBelongingSymbols :: !BelongingSymbols -> Int

import_ident :: Ident
restoreIdentsSymbolPtrs :: ![Ident] !*SymbolTable -> *SymbolTable
