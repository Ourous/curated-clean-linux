implementation module iTasks.UI.Layout.Debug

import iTasks
import StdDebug
import StdMisc

from Data.Map import toList, toAscList, foldrWithKey
from Text import class Text(join), instance Text String

import Text.GenPrint
derive gPrint JSONNode, UIType

traceLayout :: String -> Layout
traceLayout m =
	{ apply  = \ui->trace_n (m +++ "- (apply)  : \n" +++ printUI 0 ui) (NoChange, LSNone)
	, adjust = \x ->trace_n (m +++ "- (adjust) : \n") x
	, restore= \c ->trace_n (m +++ "- (restore): \n") NoChange
	}
where
	printUI ident (UI type attr children)
		=   toString (take (ident*4) (repeat ' '))
		+++ printToString type +++ " {" +++ join "," (map printAttr (toList attr)) +++ "} :\n"
		+++ foldr (+++) "" (map (printUI (ident+1)) children)

	printAttr (t, json) = toString t +++ ": " +++ printToString json
