definition module analunitypes

import StdEnv
import syntax, checksupport

typeProperties :: !Index  !Index ![SignClassification] ![PropClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!TypeSymbProperties, !*TypeVarHeap, !*TypeDefInfos)

signClassification :: !Index  !Index  ![SignClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!SignClassification, !*TypeVarHeap, !*TypeDefInfos)

propClassification :: !Index !Index ![PropClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!PropClassification, !*TypeVarHeap, !*TypeDefInfos)
