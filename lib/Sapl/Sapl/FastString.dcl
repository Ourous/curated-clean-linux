definition module Sapl.FastString

/**
* Skip characters indicated by the predicate
*
* @param The string that is being searched.
* @param Start position.
* @param Predicate function.
*/
skipChars :: !String !Int (Char -> Bool) -> Int

/**
* Predicate which tests if a string starts with another substring
*
* @param The substring.
* @param The string that is being searched.
*/
startsWith :: !String !String -> Bool

/**
* Predicate which tests if a string ends with another substring
*
* @param The substring.
* @param The string that is being searched.
*/
endsWith :: !String !String -> Bool

/**
* Find the first occurence of a character in another string
* starting from a given character position
*
* @param The string that is being searched.
* @param The start position.
* @param The character.
*/
charIndex :: !String !Int !Char -> (!Bool,!Int)

/**
* Find the first occurence of a character in another string
* starting from a given character position BACKWARDS.
*
* @param The string that is being searched.
* @param The start position.
* @param The character.
*/
charIndexBackwards :: !String !Int !Char -> (!Bool,!Int)

/**
* Predicate which tests if a substring matches another string
* at a given position
*
* @param The substring that is being matched.
* @param The string that is being searched.
* @param The start position.
*/
matchAt :: !String !String !Int -> Bool

/**
* Count the number of characters in a sequence from the end of the string
* E.g.: counCharBackwards '.' ".a.a..." gives 3
*
* @param The char that is being searched.
* @param The string that is being searched.
*/
countCharBackwards :: !Char !String -> Int
