definition module Sapl.SaplParser

import Sapl.SaplTokenizer, Sapl.SaplStruct
import Data.Error
from Data.Map import :: Map

// Cannot be abstract because code generator uses it
:: ParserState = { ps_level 		:: Int
				 , ps_constructors  :: Map String ConstructorDef
				 , ps_functions		:: Map String [SaplTypedVar]
				 , ps_CAFs			:: Map String ()
				 , ps_genFuns		:: [FuncType] 			// generated functions during parsing				 
		      	 }

:: ConstructorDef = { index		:: !Int
					, nr_cons   :: !Int
					, nr_args	:: !Int			// for efficiency
					, args		:: [SaplTypedVar]
					}

:: ErrorMsg :== String

/**
* Convert a list of position wrapped tokens into a list of AST per function.
* On error an error message is provided. It is assumed that the tokens encode
* function definitions.
*
* @param a token stream
* @return an [AST] or an error message
*/			
parse :: [PosToken] -> MaybeError ErrorMsg ([FuncType], ParserState) 

/**
* Convert a list of position wrapped tokens into an AST.
* On error an error message is provided. It is assumed that the tokens encode
* a simple expression.
*
* @param a token stream
* @return an AST, ParserState pair or an error message
*/
parseExpr :: [PosToken] -> MaybeError ErrorMsg (SaplTerm, ParserState) 

/**
* Merge a ParserState record into another, by adding the elements of the Map fields
* to the Map fields of the other structure.
*
* @param first parser state
* @param second parser state (supposed to be bigger)
* @return merged parser state
*/
mergeParserStates :: ParserState (Maybe ParserState) -> ParserState
