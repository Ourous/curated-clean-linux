definition module unitype

import StdEnv
import syntax, analunitypes

::	TypePosition :== [#Int!]

AttrUni			:== 0
AttrMulti		:== 1
AttrExi			:== 2
FirstAttrVar	:== 3

instance toInt TypeAttribute

::	CoercionTree	= CT_Node !Int !CoercionTree !CoercionTree | CT_Empty | CT_Unique | CT_NonUnique  | CT_Existential

::	Coercions		= { coer_demanded :: !.{! .CoercionTree}, coer_offered :: !.{! .CoercionTree }}

isNonUnique				:: !CoercionTree -> Bool
isUnique  				:: !CoercionTree -> Bool
isExistential  			:: !CoercionTree -> Bool

isNonUniqueAttribute	:: !Int !Coercions -> Bool
isUniqueAttribute		:: !Int !Coercions -> Bool

::	BOOLVECT :== Int

BITINDEX temp_var_id :== temp_var_id >> 5
BITNUMBER temp_var_id :== temp_var_id bitand 31
set_bit :: !Int !*{# BOOLVECT} -> .{# BOOLVECT}

determineAttributeCoercions :: !AType !AType !Bool !Bool !u:{!Type} !*Coercions !{#CommonDefs} !{#BOOLVECT} !*TypeDefInfos !*TypeHeaps
												 -> (!Optional (TypePosition, AType),!u:{!Type},!*Coercions,!*TypeDefInfos,!*TypeHeaps)

::	AttributePartition	:== {# Int}

partitionateAttributes :: !{! CoercionTree} !{! *CoercionTree} -> (!AttributePartition, !*{! CoercionTree})

newInequality :: !Int !Int !*Coercions -> *Coercions

tryToMakeNonUnique :: !Int !*Coercions -> (!Bool, !*Coercions)

tryToMakeUnique :: !Int !*Coercions -> (!Bool, !*Coercions)

uniquenessErrorVar :: !FreeVar !FunctionBody !String !*ErrorAdmin -> *ErrorAdmin

liftOfferedSubstitutions :: !AType !AType !{#CommonDefs} !{#BOOLVECT} !Int !*{!Type} !*TypeDefInfos !*TypeHeaps
																  -> (!Int,!*{!Type},!*TypeDefInfos,!*TypeHeaps)

liftSubstitution :: !*{!Type} !{#CommonDefs}!{#BOOLVECT} !Int !*TypeHeaps !*TypeDefInfos -> (*{!Type}, !Int, !*TypeHeaps, !*TypeDefInfos)

liftRemainingSubstitutions :: !*{!Type} !{#CommonDefs }!{#BOOLVECT} !Int !*TypeHeaps !*TypeDefInfos -> (*{!Type}, !Int, !*TypeHeaps, !*TypeDefInfos)

expand_and_coerce_type :: !{#CommonDefs} !{#Int} !AType !*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos)
											 -> (!AType,!*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos))

checkExistentionalAttributeVars :: [TempAttrId] !AttributePartition !*{! CoercionTree} -> (!Bool,!*{! CoercionTree})

copyCoercions :: *Coercions -> (*Coercions, *Coercions)
