definition module predef

import syntax, hashtable

::	PredefinedSymbols	:== {# PredefinedSymbol}

::	PredefinedSymbol = {
		pds_module	:: !Index,
		pds_def		:: !Index
	}

init_identifiers :: !*SymbolTable !*World -> (!*SymbolTable,!*World)

predefined_idents :: {!Ident}

buildPredefinedSymbols :: !*HashTable -> (!.PredefinedSymbols,!*HashTable)

buildPredefinedModule :: !Bool !*PredefinedSymbols -> (!ScannedModule, !.PredefinedSymbols)

cPredefinedModuleIndex :== 1

PD_StringTypeIndex :== 0
PD_Arity2TupleTypeIndex :== 8
PD_Arity32TupleTypeIndex :== 38

/* identifiers not present the hashtable */

PD_PredefinedModule			:== 0

FirstTypePredefinedSymbolIndex:==PD_StringType; // to compute index in com_type_defs

PD_StringType				:== 1

PD_ListType :== 2
PD_StrictListType :== 3
PD_UnboxedListType :== 4
PD_TailStrictListType :== 5
PD_StrictTailStrictListType :== 6
PD_UnboxedTailStrictListType :== 7
PD_OverloadedListType :== 8

PD_Arity2TupleType			:== 9
PD_Arity32TupleType			:== 39

PD_LazyArrayType			:== 40
PD_StrictArrayType			:== 41
PD_UnboxedArrayType			:== 42

PD_UnitType :== 43

// constructors:

FirstConstructorPredefinedSymbolIndex :== PD_ConsSymbol; // to compute index in com_cons_defs

PD_ConsSymbol :== 44
PD_StrictConsSymbol :== 45
PD_UnboxedConsSymbol :== 46
PD_TailStrictConsSymbol :== 47
PD_StrictTailStrictConsSymbol :== 48
PD_UnboxedTailStrictConsSymbol :== 49
PD_OverloadedConsSymbol :== 50

PD_NilSymbol :== 51
PD_StrictNilSymbol :== 52
PD_UnboxedNilSymbol :== 53
PD_TailStrictNilSymbol :== 54
PD_StrictTailStrictNilSymbol :== 55
PD_UnboxedTailStrictNilSymbol :== 56
PD_OverloadedNilSymbol :== 57

PD_Arity2TupleSymbol		:== 58
PD_Arity32TupleSymbol		:== 88

PD_UnitConsSymbol :== 89

// end constructors

PD_TypeVar_a0				:== 90
PD_TypeVar_a31				:== 121

/* identifiers present in the hashtable */

PD_StdArray					:== 122
PD_StdEnum					:== 123
PD_StdBool					:== 124

PD_AndOp					:== 125
PD_OrOp						:== 126

/* Array functions */

PD_ArrayClass				:== 127

PD_CreateArrayFun			:== 128
PD__CreateArrayFun			:== 129
PD_ArraySelectFun			:== 130
PD_UnqArraySelectFun		:== 131
PD_ArrayUpdateFun			:== 132
PD_ArrayReplaceFun			:== 133
PD_ArraySizeFun				:== 134
PD_UnqArraySizeFun			:== 135

/* Enum/Comprehension functions */

PD_SmallerFun				:== 136
PD_LessOrEqualFun			:== 137
PD_IncFun					:== 138
PD_SubFun					:== 139
PD_From						:== 140
PD_FromThen					:== 141
PD_FromTo					:== 142
PD_FromThenTo				:== 143

/* StdMisc */
PD_StdMisc					:== 144
PD_abort					:== 145
PD_undef					:== 146

PD_Start					:== 147

PD_DummyForStrictAliasFun	:== 148

PD_StdStrictLists:==149

PD_cons:==150
PD_decons:==151

PD_cons_u:==152
PD_decons_u:==153

PD_cons_uts:==154
PD_decons_uts:==155

PD_nil:==156
PD_nil_u:==157
PD_nil_uts:==158

PD_ListClass :== 159
PD_UListClass :== 160
PD_UTSListClass :== 161

/* Dynamics */

// TC class
PD_TypeCodeMember			:== 162
PD_TypeCodeClass			:== 163
// dynamic module
PD_StdDynamic				:== 164
// dynamic type
PD_Dyn_DynamicTemp				:== 165
// type code (type)
PD_Dyn_TypeCode					:== 166
// unification (type)
PD_Dyn_UnificationEnvironment	:== 167
// type code (expressions)
PD_Dyn_TypeScheme			:== 168
PD_Dyn_TypeApp				:== 169
PD_Dyn_TypeVar				:== 170
PD_Dyn_TypeCons				:== 171
PD_Dyn_TypeUnique			:== 172
PD_Dyn__TypeFixedVar		:== 173
// unification (expressions)
PD_Dyn_initial_unification_environment	:== 174
PD_Dyn_bind_global_type_pattern_var		:== 175
PD_Dyn_unify							:== 176
PD_Dyn_normalise						:== 177

/* Generics */
PD_StdGeneric				:== 178
// Generics types
PD_TypeBimap				:== 179
PD_TypeUNIT					:== 180
PD_TypeEITHER				:== 181
PD_TypePAIR					:== 182
// for constructor info
PD_TypeCONS					:== 183
PD_TypeRECORD				:== 184
PD_TypeFIELD				:== 185
PD_TypeOBJECT				:== 186
PD_TGenericConsDescriptor	:== 187
PD_TGenericRecordDescriptor	:== 188
PD_TGenericFieldDescriptor 	:== 189
PD_TGenericTypeDefDescriptor :== 190
PD_TGenConsPrio				:== 191
PD_TGenConsAssoc			:== 192
PD_TGenType					:== 193

PD_TypeGenericDict 			:== 194
// Generics fields
PD_map_to					:== 195
PD_map_from					:== 196
// Generics expression
PD_ConsBimap				:== 197
PD_ConsUNIT					:== 198
PD_ConsLEFT					:== 199
PD_ConsRIGHT				:== 200
PD_ConsPAIR					:== 201
// for constructor info
PD_ConsCONS					:== 202
PD_ConsRECORD				:== 203
PD_ConsFIELD				:== 204
PD_ConsOBJECT				:== 205
PD_CGenericConsDescriptor 	:== 206
PD_CGenericRecordDescriptor	:== 207
PD_CGenericFieldDescriptor 	:== 208
PD_CGenericTypeDefDescriptor :== 209
PD_CGenConsNoPrio			:== 210
PD_CGenConsPrio				:== 211
PD_CGenConsAssocNone		:== 212
PD_CGenConsAssocLeft		:== 213
PD_CGenConsAssocRight		:== 214
PD_CGenTypeCons				:== 215
PD_CGenTypeVar				:== 216
PD_CGenTypeArrow			:== 217
PD_CGenTypeApp				:== 218

PD_bimapId					:== 219
PD_GenericBimap				:== 220

PD_FromS					:== 221
PD_FromTS					:== 222
PD_FromSTS					:== 223
PD_FromU					:== 224
PD_FromUTS					:== 225
PD_FromO					:== 226

PD_FromThenS				:== 227
PD_FromThenTS				:== 228
PD_FromThenSTS				:== 229
PD_FromThenU				:== 230
PD_FromThenUTS				:== 231
PD_FromThenO				:== 232

PD_FromToS					:== 233
PD_FromToTS					:== 234
PD_FromToSTS				:== 235
PD_FromToU					:== 236
PD_FromToUTS				:== 237
PD_FromToO					:== 238

PD_FromThenToS				:== 239
PD_FromThenToTS				:== 240
PD_FromThenToSTS			:== 241
PD_FromThenToU				:== 242
PD_FromThenToUTS			:== 243
PD_FromThenToO				:== 244

PD_Dyn__to_TypeCodeConstructor	:== 245
PD_TypeCodeConstructor :== 246

PD_TC_Int			:== 247
PD_TC_Char			:== 248
PD_TC_Real			:== 249
PD_TC_Bool			:== 250
PD_TC_Dynamic		:== 251
PD_TC_File			:== 252
PD_TC_World			:== 253

PD_TC__Arrow		:== 254

PD_TC__List			:== 255
PD_TC__StrictList	:== 256
PD_TC__UnboxedList	:== 257
PD_TC__TailStrictList	:== 258
PD_TC__StrictTailStrictList	:== 259
PD_TC__UnboxedTailStrictList	:== 260

PD_TC__Tuple2		:== 261
PD_TC__Tuple3		:== 262
PD_TC__Tuple4		:== 263
PD_TC__Tuple5		:== 264
PD_TC__Tuple6		:== 265
PD_TC__Tuple7		:== 266
PD_TC__Tuple8		:== 267
PD_TC__Tuple9		:== 268
PD_TC__Tuple10		:== 269
PD_TC__Tuple11		:== 270
PD_TC__Tuple12		:== 271
PD_TC__Tuple13		:== 272
PD_TC__Tuple14		:== 273
PD_TC__Tuple15		:== 274
PD_TC__Tuple16		:== 275
PD_TC__Tuple17		:== 276
PD_TC__Tuple18		:== 277
PD_TC__Tuple19		:== 278
PD_TC__Tuple20		:== 279
PD_TC__Tuple21		:== 280
PD_TC__Tuple22		:== 281
PD_TC__Tuple23		:== 282
PD_TC__Tuple24		:== 283
PD_TC__Tuple25		:== 284
PD_TC__Tuple26		:== 285
PD_TC__Tuple27		:== 286
PD_TC__Tuple28		:== 287
PD_TC__Tuple29		:== 288
PD_TC__Tuple30		:== 289
PD_TC__Tuple31		:== 290
PD_TC__Tuple32		:== 291

PD_TC__LazyArray	:== 292
PD_TC__StrictArray	:== 293
PD_TC__UnboxedArray	:== 294

PD_TC__Unit			:== 295

PD_NrOfPredefSymbols		:== 296

GetTupleConsIndex tup_arity :== PD_Arity2TupleSymbol + tup_arity - 2
GetTupleTypeIndex tup_arity :== PD_Arity2TupleType + tup_arity - 2

// changes requires recompile of {static,dynamic}-linker plus all dynamics ever made
UnderscoreSystemDynamicModule_String	:== "_SystemDynamic"	

// List-type
PD_ListType_String				:== "_List"
PD_ConsSymbol_String			:== "_Cons"
PD_NilSymbol_String				:== "_Nil"

// Array-type
PD_UnboxedArray_String			:== "_#Array"

DynamicRepresentation_String			:== "DynamicTemp" // "_DynamicTemp"		
