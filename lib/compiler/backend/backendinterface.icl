/*
	module owner: Ronny Wichers Schreur
*/
implementation module backendinterface

import StdEnv

import frontend
import backend
import backendpreprocess, backendsupport, backendconvert
import Version
import partition

checkVersion :: VersionsCompatability *File -> (!Bool, !*File)
checkVersion VersionsAreCompatible errorFile
	=	(True, errorFile)
checkVersion VersionObservedIsTooNew errorFile
	#	errorFile
			=	fwrites "Error: the back end library is too new\n" errorFile
	=	(False, errorFile)
checkVersion VersionObservedIsTooOld errorFile
	#	errorFile
			=	fwrites "Error: the back end library is too old\n" errorFile
	=	(False, errorFile)

backEndInterface :: !{#Char} [{#Char}] !ListTypesOption !{#Char} !PredefinedSymbols !FrontEndSyntaxTree !Int
							  !*VarHeap !*TypeVarHeap !*AttrVarHeap !*File !*File
					-> (!Bool,!*VarHeap,!*TypeVarHeap,!*AttrVarHeap,!*File,!*File)
backEndInterface outputFileName commandLineArgs listTypes typesPath predef_symbols syntaxTree=:{fe_icl,fe_components,fe_dcls} main_dcl_module_n
		var_heap type_var_heap attrHeap errorFile outFile
	# (observedCurrent, observedOldestDefinition, observedOldestImplementation)
		=	BEGetVersion
	  observedVersion =
		{	versionCurrent
				=	observedCurrent
		,	versionOldestDefinition
				=	observedOldestDefinition
		,	versionOldestImplementation
				=	observedOldestImplementation
		}
	  expectedVersion =
		{	versionCurrent
				=	kBEVersionCurrent
		,	versionOldestDefinition
				=	kBEVersionOldestDefinition
		,	versionOldestImplementation
				=	kBEVersionOldestImplementation
		}
	# (compatible, errorFile)
		=	checkVersion (versionCompare expectedVersion observedVersion) errorFile
	| not compatible
		=	(False, var_heap, type_var_heap, attrHeap, errorFile, outFile)
	# varHeap
		=	backEndPreprocess predefined_idents.[PD_DummyForStrictAliasFun] functionIndices fe_icl var_heap
		with
			functionIndices = function_indices 0 fe_components

			function_indices i components
				| i<size components
					= function_indices2 components.[i].component_members i components
					= []

			function_indices2 (ComponentMember member members) i components
				= [member : function_indices2 members i components]
			function_indices2 (GeneratedComponentMember member _ members) i components
				= [member : function_indices2 members i components]
			function_indices2 NoComponentMembers i components
				= function_indices (i+1) components
	# backEndFiles
		=	0
	# (backEnd, backEndFiles)
		=	BEInit (length commandLineArgs) backEndFiles
	# backEnd
		=	foldState BEArg commandLineArgs backEnd
	# (type_var_heap,var_heap,attrHeap,backEnd)
		=	backEndConvertModules predef_symbols syntaxTree main_dcl_module_n type_var_heap varHeap attrHeap backEnd
	# (success, backEnd)
		=	BEGenerateCode outputFileName backEnd
	# backEnd
		=	BECloseFiles backEnd
	# (attrHeap, outFile, backEnd)
		=	optionallyPrintFunctionTypes listTypes typesPath (DictionaryToClassInfo main_dcl_module_n fe_icl fe_dcls) fe_components fe_icl.icl_functions attrHeap outFile backEnd
	# backEndFiles
		=	BEFree backEnd backEndFiles
	=	(backEndFiles == 0 && success, var_heap, type_var_heap, attrHeap, errorFile, outFile)

:: DictionaryToClassInfo =
	{	dtci_iclModuleIndex :: Int
	,	dtci_iclModule :: IclModule
	,	dtci_dclModules :: {#DclModule}
	}

DictionaryToClassInfo iclModuleIndex iclModule dclModules :== 
	{	dtci_iclModuleIndex = iclModuleIndex
	,	dtci_iclModule = iclModule
	,	dtci_dclModules = dclModules
	}

optionallyPrintFunctionTypes :: ListTypesOption {#Char} DictionaryToClassInfo {!Component} {#FunDef} *AttrVarHeap *File !*BackEnd -> (*AttrVarHeap, *File, *BackEnd)
optionallyPrintFunctionTypes {lto_listTypesKind, lto_showAttributes} typesPath info components functions attrHeap outFile backEnd
	| lto_listTypesKind == ListTypesStrictExports || lto_listTypesKind == ListTypesAll
		=	printFunctionTypes (lto_listTypesKind == ListTypesAll) lto_showAttributes info components functions attrHeap outFile backEnd
		=	(attrHeap, outFile, backEnd)

printFunctionTypes :: Bool Bool DictionaryToClassInfo {!Component} {#FunDef} *AttrVarHeap *File *BackEnd -> (*AttrVarHeap, *File, *BackEnd)
printFunctionTypes all attr info components functions attrHeap file backEnd
	=	foldSt (printFunctionType all attr info) functionIndicesAndFunctions (attrHeap, file, backEnd)
	where
		functionIndicesAndFunctions
			= function_indices_and_functions 0 components

		function_indices_and_functions i components
			| i<size components
				= function_indices_and_functions2 components.[i].component_members i components
				= []

		function_indices_and_functions2 (ComponentMember member members) i components
			= [(member,functions.[member]) : function_indices_and_functions2 members i components]
		function_indices_and_functions2 (GeneratedComponentMember member _ members) i components
			= [(member,functions.[member]) : function_indices_and_functions2 members i components]
		function_indices_and_functions2 NoComponentMembers i components
			= function_indices_and_functions (i+1) components

printFunctionType :: Bool Bool DictionaryToClassInfo (Int, FunDef) (*AttrVarHeap, *File, *BackEnd) -> (*AttrVarHeap, *File, *BackEnd)
printFunctionType all attr info (functionIndex, {fun_ident,fun_type=Yes type}) (attrHeap, file, backEnd)
	| not all && functionIndex >= size info.dtci_dclModules.[info.dtci_iclModuleIndex].dcl_functions
		=	(attrHeap, file, backEnd)

//	| trace_tn (toString fun_ident) && True ---> type.st_args

	# (strictnessAdded, type, backEnd)
		=	addStrictnessFromBackEnd functionIndex fun_ident.id_name backEnd type
	| not strictnessAdded && not all
		=	(attrHeap, file, backEnd)
	// FIXME: shouldn't have to repair the invariant here
	# (type, attrHeap)
		=	collectSymbolTypeAttrVars type attrHeap
	# type
		=	dictionariesToClasses info type
	# (type, attrHeap)
		=	beautifulizeAttributes type attrHeap
	# file
		=	file <<< fun_ident <<< " :: "
				<:: ({ form_properties = (if attr cAttributed 0) bitor cAnnotated, form_attr_position = No }, type, Yes initialTypeVarBeautifulizer) <<< '\n'
	=	(attrHeap, file, backEnd)

addStrictnessFromBackEnd :: Int {#Char} *BackEnd SymbolType -> (Bool, SymbolType, *BackEnd)
addStrictnessFromBackEnd functionIndex functionName backEnd type
	# (bitSize, strictPositionsC, backEnd)
		=	BEStrictPositions functionIndex backEnd
	| bitSize == 0 // short cut
		=	(False, type, backEnd)
	# strictPositions
		=	copyInts ((bitSize+31)/32) strictPositionsC // assumes 32 bit ints
	# strictnessInfo
		=	{si_robust_encoding = False, si_positions = strictPositions, si_size = bitSize, si_name = functionName}
	  offset
	  	=	0

//	| trace_tn (toString bitSize+++" "+++toString strictPositions.[0])

	# (robust, offset)
		=	nextBit strictnessInfo offset
	  strictnessInfo
	  	=	{strictnessInfo & si_robust_encoding = robust}
	# (anyStrictnessAdded, offset)
		=	nextBit strictnessInfo offset
	# (type, offset)
		=	addStrictnessToSymbolType strictnessInfo type offset
	# type
		=	checkFinalOffset strictnessInfo offset type
	= (anyStrictnessAdded, type, backEnd)

addStrictnessToSymbolType strictPositions=:{si_size} args offset
	| offset >= si_size // short cut
		=	(args, offset)
addStrictnessToSymbolType strictPositions type=:{st_args,st_args_strictness} offset
	# (st_args, offset,args_strictness)
		=	addStrictness strictPositions st_args offset st_args_strictness 0
	=	({type & st_args = st_args,st_args_strictness=args_strictness}, offset)

:: StrictnessInfo =
	{	si_size :: !Int
	,	si_positions :: !LargeBitvect
	,	si_name :: {#Char}
	,	si_robust_encoding :: !Bool
	}

class addStrictness a :: !StrictnessInfo !a Int StrictnessList Int -> (!a, !Int,!StrictnessList)

nextBit :: StrictnessInfo Int -> (Bool, Int)
nextBit {si_size, si_positions, si_robust_encoding} offset
	| offset < si_size
		=	(bitvectSelect offset si_positions, offset+1)
	// otherwise
		| si_robust_encoding
			=	abort "backendinterface, nextBit: bit vector too small"
		// otherwise
			=	(False, offset)

checkStrictness :: StrictnessInfo Bool Int -> Int
checkStrictness info=:{si_robust_encoding} wasStrict offset
	| si_robust_encoding
		# (bit, offset)
			=	nextBit info offset
		| bit <> wasStrict
			=	abort "backendinterface, checkStrictness: wrong info for strictness annotation"
		=	offset
	// otherwise
		=	offset

checkType :: StrictnessInfo Bool Int -> Int
checkType info=:{si_robust_encoding} isTuple offset
	| si_robust_encoding
		# (bit, offset)
			=	nextBit info offset
		| bit <> isTuple
			=	abort "backendinterface, checkType: wrong type"
		=	offset
	// otherwise
		=	offset

checkFinalOffset :: StrictnessInfo Int a -> a
checkFinalOffset info=:{si_size, si_robust_encoding} offset value
	| offset < si_size || (si_robust_encoding && offset > si_size)
		=	abort "backendinterface, checkFinalOffset: wrong offset"
	// otherwise
		=	value

instance addStrictness [a] | addStrictness a where
	addStrictness strictPositions [] offset args_strictness args_strictness_index
		= ([],offset,args_strictness)
	addStrictness strictPositions [type:types] offset args_strictness args_strictness_index
		# (type,offset,args_strictness)=addStrictness strictPositions type offset args_strictness args_strictness_index
		# (types,offset,args_strictness)=addStrictness strictPositions types offset args_strictness (args_strictness_index+1)
		= ([type:types],offset,args_strictness)

instance addStrictness AType where
	addStrictness strictPositions arg=:{at_type} offset args_strictness args_strictness_index
		# (is_strict,offset,args_strictness)
			=	addStrictnessAnnotation strictPositions offset args_strictness args_strictness_index
		# (at_type, offset)
			=	addStrictnessToType strictPositions is_strict at_type offset
		=	({arg & at_type=at_type}, offset,args_strictness)

addStrictnessAnnotation info offset args_strictness args_strictness_index
	# wasStrict = arg_is_strict args_strictness_index args_strictness
	# offset
		=	checkStrictness info wasStrict offset
	# (strictAdded, offset)
		=	nextBit info offset
	| strictAdded
		| wasStrict
				=	abort "backendinterface, addStrictness: already strict"
		// otherwise
			# args_strictness = add_strictness args_strictness_index args_strictness
			=	(True, offset,args_strictness)
	// otherwise
		=	(wasStrict, offset,args_strictness)

addStrictnessToType :: StrictnessInfo Bool Type Int -> (Type, Int)
addStrictnessToType strictPositions isStrict type=:(TA ident=:{type_index={glob_object,glob_module}} args) offset
	# offset
		=	checkType strictPositions isTuple offset
	| isTuple && isStrict
		# (args, offset,args_strictness)
			=	addStrictness strictPositions args offset NotStrict 0
		| is_not_strict args_strictness
			=	(TA ident args, offset)
			=	(TAS ident args args_strictness, offset)
	// otherwise
		=	(type, offset)
	where
		isTuple
			= glob_module==cPredefinedModuleIndex && (glob_object>=PD_Arity2TupleTypeIndex && glob_object<=PD_Arity32TupleTypeIndex)
addStrictnessToType strictPositions isStrict type=:(TAS ident=:{type_index={glob_object,glob_module}} args strictness) offset
	# offset
		=	checkType strictPositions isTuple offset
	| isTuple && isStrict
		# (args, offset,strictness)
			=	addStrictness strictPositions args offset strictness 0
		=	(TAS ident args strictness, offset)
	// otherwise
		=	(type, offset)
	where
		isTuple
			= glob_module==cPredefinedModuleIndex && (glob_object>=PD_Arity2TupleTypeIndex && glob_object<=PD_Arity32TupleTypeIndex)
addStrictnessToType strictPositions _ type offset
	# offset
		=	checkType strictPositions False offset
	=	(type, offset)

collectSymbolTypeAttrVars :: SymbolType *AttrVarHeap -> (SymbolType, *AttrVarHeap)
collectSymbolTypeAttrVars type=:{st_attr_vars, st_result, st_args} attrVarHeap
	# attrVarHeap
		=	foldSt markAttrVarCollected st_attr_vars attrVarHeap
	# (st_attr_vars, attrVarHeap)
		=	collectAttrVars st_result (collectAttrVars st_args (st_attr_vars, attrVarHeap))
	=	({type & st_attr_vars = st_attr_vars}, attrVarHeap)

/* maybe should collect st_vars as well (these are not used currently) */
class collectAttrVars a :: a ([AttributeVar], *AttrVarHeap) -> ([AttributeVar], *AttrVarHeap)

instance collectAttrVars AType where
	collectAttrVars {at_attribute, at_type} collect
		=	collectAttrVars at_attribute (collectAttrVars at_type collect)

instance collectAttrVars TypeAttribute where
	collectAttrVars (TA_Var attrVar) collect
		=	collectAttrVars attrVar collect
	collectAttrVars (TA_RootVar attrVar) collect
		=	collectAttrVars attrVar collect
	collectAttrVars (TA_List _ attribute) collect
		=	collectAttrVars attribute collect
	collectAttrVars (TA_Locked attribute) collect
		=	collectAttrVars attribute collect
	collectAttrVars _ collect
		=	collect

instance collectAttrVars Type where
	collectAttrVars (TA _ types) collect
		=	collectAttrVars types collect
	collectAttrVars (TAS _ types _) collect
		=	collectAttrVars types collect
	collectAttrVars (type1 --> type2) collect
		=	collectAttrVars type1 (collectAttrVars type2 collect)
	collectAttrVars (TArrow1 type) collect
		=	collectAttrVars type collect
	collectAttrVars (_ :@: types) collect
		=	collectAttrVars types collect
	collectAttrVars (TFA _ type) collect
		=	collectAttrVars type collect
	collectAttrVars _ collect
		=	collect

instance collectAttrVars AttributeVar where
	collectAttrVars  attrVar=:{av_info_ptr} (attrVars, attrVarHeap)
		# (info, attrVarHeap)
			=	readPtr av_info_ptr attrVarHeap
		=	case info of
				AVI_Collected
					->	(attrVars, attrVarHeap)
				_
					->	([attrVar : attrVars], markAttrVarCollected attrVar attrVarHeap)

instance collectAttrVars [a] | collectAttrVars a where
	collectAttrVars l collect
		=	foldSt collectAttrVars l collect

markAttrVarCollected :: AttributeVar *AttrVarHeap -> *AttrVarHeap
markAttrVarCollected {av_info_ptr} attrVarHeap
	=	writePtr av_info_ptr AVI_Collected attrVarHeap

dictionariesToClasses :: DictionaryToClassInfo SymbolType -> SymbolType
dictionariesToClasses info type=:{st_args, st_args_strictness, st_arity, st_context=[]}
	# (reversedTypes, reversedContexts)
		=	dictionaryArgsToClasses info st_args ([], [])
	# n_contexts = length reversedContexts
	# new_st_args_strictness = remove_first_n n_contexts st_args_strictness
	=	{type & st_args = reverse reversedTypes, st_context = reverse reversedContexts,
				st_arity = st_arity - n_contexts, st_args_strictness=new_st_args_strictness}

dictionaryArgsToClasses :: DictionaryToClassInfo [AType] ([AType], [TypeContext]) -> ([AType], [TypeContext])
dictionaryArgsToClasses info args result
	=	foldSt (dictionaryArgToClass info) args result
where
	dictionaryArgToClass :: DictionaryToClassInfo AType ([AType], [TypeContext]) -> ([AType], [TypeContext])
	dictionaryArgToClass info type=:{at_type=TA typeSymbol args} (reversedTypes, reversedContexts)
		=	case typeToClass info typeSymbol of
				Yes klass
					->	(reversedTypes, [dictionary_to_context klass args : reversedContexts])
				No
					->	([type : reversedTypes], reversedContexts)
	dictionaryArgToClass info type=:{at_type=TAS typeSymbol args _} (reversedTypes, reversedContexts)
		=	case typeToClass info typeSymbol of
				Yes klass
					->	(reversedTypes, [dictionary_to_context klass args : reversedContexts])
				No
					->	([type : reversedTypes], reversedContexts)
	dictionaryArgToClass _ type (reversedTypes, reversedContexts)
		=	([type : reversedTypes], reversedContexts)
	
	dictionary_to_context klass args
		=	{tc_class = TCClass klass, tc_types = [at_type \\ {at_type} <- args], tc_var = nilPtr}

typeToClass :: DictionaryToClassInfo TypeSymbIdent -> Optional (Global DefinedSymbol)
typeToClass {dtci_iclModuleIndex, dtci_iclModule, dtci_dclModules} {type_ident, type_arity, type_index={glob_module, glob_object}}
	#! nDclTypes = size dclModule.dcl_common.com_type_defs
	| glob_module <> dtci_iclModuleIndex || glob_object < nDclTypes
		# classIndex = glob_object - (nDclTypes - nDclClasses)
		| classIndex>=0
			# class_ident = dclModule.dcl_common.com_class_defs.[classIndex].class_ident
			= Yes {glob_module=glob_module, glob_object = {ds_ident = class_ident, ds_arity = type_arity, ds_index = glob_object}}			
			= No
		# classIndex = glob_object - (nIclTypes-nIclClasses)
		| classIndex>=nDclClasses
			# class_ident = dtci_iclModule.icl_common.com_class_defs.[classIndex].class_ident
			= Yes {glob_module=glob_module, glob_object = {ds_ident = class_ident, ds_arity = type_arity, ds_index = glob_object}}			
			= No		
	where
		/*
			This how the types are organised (#classes == #dictionaries)

						  com_classes
			+--------(1)-------+--------(2)-------+
			|   dcl classes    |   icl classes    |
			+------------------+------------------+
						  nDclClasses		 nIclClasses

									com_type_defs
			+-----------+--------(1)-------+-----------+--------(2)-------+
			| dcl types | dcl dictionaries | icl types | icl dictionaries |
			+-----------+------------------+-----------+------------------+
									   nDclTypes					   nIclTypes
		*/
		dclModule
			=	dtci_dclModules.[glob_module]
		nDclClasses
			=	size dclModule.dcl_common.com_class_defs
		nIclTypes
			=	size dtci_iclModule.icl_common.com_type_defs
		nIclClasses
			=	size dtci_iclModule.icl_common.com_class_defs

/*
copyInts :: !Int !Int -> {#Int}
copyInts length cArray
    = code {
    		push_b 0
            create_array_ INT 0 1
  
            push_a  0
            ccall   BECopyInts "IIA-I"
            pop_b   1
	}
*/

copyInts :: !Int !Int -> {#Int}
copyInts length cArray
    = {BEGetIntFromArray i cArray \\ i<-[0..length-1]}