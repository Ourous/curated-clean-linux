definition module frontend

from scanner import ::SearchPaths
from general import ::Optional (Yes, No)
import checksupport, overloading
from trans import ::FusionOptions(..)
from partition import ::Component(..),::ComponentMembers

:: FrontEndOptions
	=	{	feo_up_to_phase			:: !FrontEndPhase
		,	feo_fusion	 			:: !FusionOptions
		,	feo_generics 			:: !Bool
		}

:: FrontEndSyntaxTree
	=	{	fe_icl					:: !IclModule
		,	fe_dcls					:: !{#DclModule}
		,	fe_components			:: !{!Component}
		,	fe_arrayInstances		:: !ArrayAndListInstances
		}

:: FrontEndPhase
	=	FrontEndPhaseCheck
	|	FrontEndPhaseTypeCheck
	|	FrontEndPhaseConvertDynamics
	|	FrontEndPhaseTransformGroups
	|	FrontEndPhaseConvertModules
	|	FrontEndPhaseAll

:: ListTypesKind = ListTypesNone | ListTypesInferred | ListTypesStrictExports | ListTypesAll
:: ListTypesOption =
	{	lto_showAttributes :: Bool
	,	lto_listTypesKind :: ListTypesKind
	}
instance == ListTypesKind

defaultFrontEndOptions :: FrontEndOptions // used by sparkle, because FrontEndOptions in trunk and itask branch differ

frontEndInterface :: !(Optional (*File,{#Char},{#Char})) !FrontEndOptions !Ident !SearchPaths !{#DclModule} !*{#*{#FunDef}} !(Optional Bool) !*PredefinedSymbols !*HashTable (ModTimeFunction *Files) !*Files !*File !*File !*File !(Optional *File) !*Heaps
	-> ( !Optional *FrontEndSyntaxTree,!*{#*{#FunDef}},!{#DclModule},!Int,!*PredefinedSymbols, !*HashTable, !*Files, !*File, !*File, !*File, !Optional *File, !*Heaps) 
