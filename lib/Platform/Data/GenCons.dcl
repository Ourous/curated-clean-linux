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
consName{|CONS of {gcd_name}|} f x = gcd_name
consName{|UNIT|} _ = "UNIT"
consName{|PAIR|} f g (PAIR x y) = f x
consName{|EITHER|} f g (LEFT x) = f x
consName{|EITHER|} f g (RIGHT y) = g y
consName{|OBJECT|} f (OBJECT x) = f x
consName{|RECORD|} f (RECORD x) = f x
consName{|FIELD|} f (FIELD x) = f x
derive consName Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]

/**
 * Gives the index of the constructor. Note that this is always 0 if the type is not an ADT
 *
 * @param Value for a type
 * @result Index
 */
generic consIndex a :: a -> Int
consIndex{|CONS of {gcd_index}|} f x = gcd_index
consIndex{|UNIT|} _ = 0
consIndex{|PAIR|} f g (PAIR x y) = f x
consIndex{|EITHER|} f g (LEFT x) = f x
consIndex{|EITHER|} f g (RIGHT y) = g y
consIndex{|OBJECT|} f (OBJECT x) = f x
consIndex{|RECORD|} f (RECORD x) = f x
consIndex{|FIELD|} f (FIELD x) = f x
derive consIndex Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]

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
consNum{|CONS|} _ _ = consNumError
consNum{|UNIT|} _ = consNumError
consNum{|PAIR|} _ _ _ = consNumError
consNum{|EITHER|} _ _ _ = consNumError
consNum{|OBJECT of {gtd_num_conses}|} f _ = gtd_num_conses
consNum{|RECORD|} _ _ = 1
consNum{|FIELD|} _ _ = consNumError

consNumError :: a

derive consNum Int,Bool,Char,Real,String,File,(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),{},{!},[],[! ],[ !],[!!]
