definition module Sapl.SaplTokenizer

import StdString, Text.Unicode

/**
* Possible token types of a SAPL program
*/
:: Token = TIdentifier String 
	     | TStrictIdentifier String
	     | TComment String 
	     | TInlineAnnotation	     
	     | TAssignmentOp
	     | TCaseAssignmentOp
	     | TMacroAssignmentOp
	     | TCAFAssignmentOp	     
     	 | TLambda
     	 | TComma
     	 | TColon
     	 | TVerticalBar     	 
		 | TOpenParenthesis
 		 | TCloseParenthesis
 		 | TOpenBracket
 		 | TCloseBracket
 		 | TOpenSquareBracket
 		 | TCloseSquareBracket
 		 | TTypeDef
		 | TLit Literal
		 | TCaseKeyword
		 | TSelectKeyword
		 | TUpdateKeyword		 
		 | TLetKeyword
		 | TInKeyword
		 | TEndOfLine

instance toString Literal	
instance toString Token

/**
* Token wrapped around position information (line and column numbers)
*/
:: PosToken = PosToken Int Int Token

// String and Char constants may contain Clean escape sequences. If the target
// language uses different escaping technique the code generator must replace the
// escape sequences
:: Literal = LString UString 
		   | LChar UString 
		   | LInt Int 
		   | LReal Real 
		   | LBool Bool

/**
* Low level function to read a token from a given position of the input string.
* 
* @param start position (from zero)
* @param input string
* @return start position of the token (can differ from the argument)
* @return start position of next token
* @return the read token
*/
read_token :: !Int !String -> (!Int, !Int, !Token)

/**
* Convert a given SAPL string to a list of tokens.
*/
tokens              :: !String -> [Token]		// used by linker
tokensWithPositions :: !String -> [PosToken]	// used by parser

	