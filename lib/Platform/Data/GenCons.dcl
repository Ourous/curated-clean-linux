definition module Data.GenCons

import StdGeneric
from Data.Maybe import :: Maybe

class gCons a | conses{|*|}, consName{|*|}, consIndex{|*|}, consNum{|*|} a

/**
 * Give an instance for a type that matches the given constructor name
 *
 * @param constructor name
 * @result maybe a value for that type
 */
consByName :: String -> Maybe a | conses{|*|}, consName{|*|} a

/**
 * Gives the name for the constructor
 *
 * @param Value for a type
 * @result Constructor name
 */
generic consName a :: a -> String
derive consName CONS of {gcd_name},UNIT,PAIR,EITHER,OBJECT,FIELD,RECORD,Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]

/**
 * Gives the index of the constructor. Note that this is always 0 if the type is not an ADT
 *
 * @param Value for a type
 * @result Index
 */
generic consIndex a :: a -> Int
derive consIndex CONS of {gcd_index},UNIT,PAIR,EITHER,OBJECT,FIELD,RECORD,Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]

/**
 * Gives a list of values for every constructor, for non-ADTs it is a singleton list with one item. For ADTs it is a list with the length of the number of constructors
 *
 * @result list of values with one value for each constructor
 */
generic conses a :: [a]
derive conses CONS,UNIT,PAIR,EITHER,OBJECT,FIELD,RECORD,Int,Bool,Char,Real,String,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!],(->)

/**
 * Gives the number of constructors
 *
 * @param value of the type (unused, just needed for type checking)
 * @result number of constructors
 */
generic consNum a :: a -> Int
derive consNum CONS,UNIT,PAIR,EITHER,OBJECT of {gtd_num_conses},FIELD,RECORD,Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]
