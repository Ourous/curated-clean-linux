definition module Sapl.Target.Flavour

import Sapl.SaplParser, Text.StringAppender
from Data.Map import :: Map (..)
from Data.Set import :: Set

:: Arity :== Int

/**
* Describes a function which generates code from an AST using a specific strategy.
* (normal, force, ...)
*/
:: TermCoderFun :== SaplTerm StringAppender -> StringAppender

/**
* Describes a function which generates inline code using the elements of its
* third argument as the arguments of the inline function. The two TermCoderFun are the normal one (first)
* and a the one which forces the evaluation (brings it to HNF) of the given term.
*/
:: InlineCoderFun :== TermCoderFun TermCoderFun [SaplTerm] StringAppender -> StringAppender

:: InlineFunDef = 
		{ fun		 :: !InlineCoderFun
		, arity		 :: !Arity
		, strictness :: !{#Char}	// '1','0'		
		, data_cons  :: !Bool
		}

:: Flavour = { fun_prefix       :: String

			 // Optional flags to tune the compiler
			 , options			:: Set String

			 /**
			 * Returns a map of function names which are built-in. For every such function
			 * a implementation specific name and its arity is provided.
			 */
		     , builtInFunctions :: Map String (String, Arity)
		     
			 /**
			 * Returns a map of function names which are built-in and can be inlined. For every such function
			 * a generator function, the arity of the built-in functions and strictness information is provided.
			 */		     		     
		     , inlineFunctions  :: Map String InlineFunDef
			 }

//Serialized representation
:: FlavourRep = { fun_prefix  :: String
				, options	  :: [String]
		        , bifs        :: [BIFRep]
			    }
	
:: BIFRep = { sapl_fun   :: String
			// number of arguments
            , arity      :: Int
            // custom data constructor? (always inlined)
            , data_cons	 :: Maybe Bool 
            // JavaScript function name, if the expression cannot be inloined
            , ext_fun    :: Maybe String
            // JavaScript expression if inlinement is possible:
            // the expression is at strict position and saturated
            , inline_exp  :: Maybe String
            }
         
toFlavour :: !String -> Maybe Flavour

fromFlavourRep :: !FlavourRep -> Flavour

// Check if a given flag is set or not in the flavour file
isSet :: !Flavour !String -> Bool


