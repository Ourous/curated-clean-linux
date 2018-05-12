definition module Sapl.SaplStruct

import StdString, StdOverloaded
import Sapl.SaplTokenizer
import Data.Maybe

/**
* Possible function types and language constructs.
*/
:: FuncType = FTRecord SaplVar [SaplTypedVar]
			| FTADT SaplVar [SaplConstructor]
			| FTCAF SaplTypedVar SaplTerm
			| FTMacro SaplTypedVar SaplTerm [SaplTypedVar]
			| FTFunc SaplTypedVar SaplTerm [SaplTypedVar]

:: SaplConstructor 	= SaplConstructor SaplVar Int [SaplTypedVar]

:: SaplTerm = SLit Literal
			| SVar SaplVar
			| SApplication SaplTerm [SaplTerm]
			| SCase SaplTerm [(SaplPattern, SaplTerm)]
			| SLet SaplTerm [SaplLetDef]
			| SSelect SaplTerm SaplType Int
			| SUpdate SaplTerm SaplType [(Int, SaplTerm)]			
			| SAbortBody			

:: SaplLetDef = SaplLetDef SaplTypedVar SaplTerm

:: SaplName :== String

:: SaplVar = NormalVar SaplName Int 
		   | StrictVar SaplName Int
           | GlobalVar SaplName

:: SaplTypedVar = TypedVar SaplVar SaplType

:: SaplPattern = PCons String [SaplVar]
			   | PLit Literal
			   | PDefault

:: SaplType = Type String | NoType

instance toString SaplVar

// I don't provide instances of (==) and (<) here because multiple good way can be imagined...
eqVarByName :: !SaplVar !SaplVar -> Bool
ltVarByName :: !SaplVar !SaplVar -> Bool
eqVarByNameLevel :: !SaplVar !SaplVar -> Bool
ltVarByNameLevel :: !SaplVar !SaplVar -> Bool

removeTypeInfo :: !SaplTypedVar -> SaplVar

class eqStrictVar v :: !String !v -> Bool
class isStrictVar v :: !v -> Bool
class toNormalVar v :: !v -> v
class toStrictVar v :: !v -> v
class unpackVar v :: !v -> String

instance eqStrictVar SaplVar
instance eqStrictVar SaplTypedVar
instance isStrictVar SaplVar
instance isStrictVar SaplTypedVar
instance toNormalVar SaplVar
instance toNormalVar SaplTypedVar
instance toStrictVar SaplVar
instance toStrictVar SaplTypedVar
instance unpackVar SaplVar
instance unpackVar SaplTypedVar

unpackBindVar  :: !SaplLetDef -> SaplTypedVar
unpackBindExpr :: !SaplLetDef -> SaplTerm
unpackConsName :: !SaplPattern -> Maybe String
toStrictBind   :: !SaplLetDef -> SaplLetDef

isConsPattern    :: !SaplPattern -> Bool
isDefaultPattern :: !SaplPattern -> Bool

