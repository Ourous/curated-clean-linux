definition module comparedefimp

import syntax, checksupport

compare_members_of_exported_classes :: !(Optional {#{#Int}}) !Int !Int !*(CommonDefsR b) !*{#DclModule} !*CheckState -> (!*(CommonDefsR b),!*{#DclModule},!*CheckState)

// compare definition and implementation module

compareDefImp :: !Int !DclModule !(Optional {#Index}) !CopiedDefinitions !Int !*IclModule !*{#*{#FunDef}} !*Heaps !*ErrorAdmin 
																		  -> (!.IclModule,!.{#.{#FunDef}},!.Heaps,!.ErrorAdmin)

compare_specified_and_derived_instance_types :: !SymbolType !SymbolType !*TypeHeaps -> (!ComparisionErrorCode, !*TypeHeaps)

:: ComparisionErrorCode :== Int
// arg n not ok: n
CEC_ResultNotOK :== 0
CEC_Ok :== -1
CEC_NrArgsNotOk :== -2
CEC_StrictnessOfArgsNotOk :== -3
CEC_ContextNotOK :== -4
CEC_AttrEnvNotOK :== -5
CEC_OkWithFirstMoreStrictness :== -6 // only for compare_specified_and_derived_instance_types
