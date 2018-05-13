definition module Text

/**
 * This module defines the basic operations on pieces of text
 * It also gives an implementation for the String and [Char] types which
 * are assumed to have only ASCII characters.
 */

from StdOverloaded import class +, class toString

/**
 * This class defines the basic operations for manipulating pieces of text.
 */
class Text s 
	where
	
	/**
	 * Calculates the number of logical characters in a piece of text.
	 * When a multibyte encoding is used, this is less then the size in bytes.
	 */
	textSize			:: !s -> Int
	
	/**
	 * Concatenates a list of strings.
	 */
	concat				:: ![s] -> s
	
	/**
	 * Splits a string into a list of strings using a separator string.
	 *
	 * @param The separator string.
	 * @param The string that is to be splitted.
	 */
	split				:: !s !s -> [s]
	
	/**
	 * Joins a list of strings using a separator string.
	 *
	 * @param The separator string.
	 * @param The string that is to be joined.
	 */
	join				:: !s ![s] -> s
	
	/**
	 * Find the first occurence of a substring in another string.
	 *
	 * @param The search string.
	 * @param The string that is being searched.
	 * @result The index of the first occurrence, or -1 if it does not occur
	 */
	indexOf 			:: !s !s -> Int
	
	/**
 	 * Find the last occurence of a substring in another string.
	 *
	 * @param The search string.
	 * @param The string that is being searched.
	 * @result The index of the last occurrence, or -1 if it does not occur
	 */
	lastIndexOf 		:: !s !s -> Int
	
	/**
	 * Find the first occurence of a substring in another string after a given offset.
	 *
	 * @param The offset.
	 * @param The search string.
	 * @param The string that is being searched.
	 * @result The index of the first occurrence after the offset amount of characters, or -1 if it does not occur
	 */
	indexOfAfter		:: !Int !s !s -> Int
	
	/**
	 * Predicate which tests if a string starts with another substring
	 *
	 * @param The substring.
	 * @param The string that is being searched.
	 */
	startsWith			:: !s !s -> Bool
	
	/**
	 * Predicate which tests if a string ends with another substring
	 *
	 * @param The substring.
	 * @param The string that is being searched.
	 */
	endsWith			:: !s !s -> Bool
	
	/**
	 * Take a substring from a string
	 *
	 * @param The logical start index.
	 * @param The logical length of the substring.
	 * @param The string from which the substring is taken.
	 */
	subString			:: !Int !Int !s -> s 
	
	/**
	 * Replaces all occurences of a substring with another in a string
	 *
	 * @param The substring.
	 * @param The replacement.
	 * @param The string that is being searched.
	 */
	replaceSubString	:: !s !s !s -> s
	
	/**
	 * Removes whitespace from the beginning and end of a string.
	 */
	trim				:: !s -> s
	
	/**
	 * Removes whitespace from the beginning of a string.
	 */
	ltrim				:: !s -> s
	
	/**
	 * Removes whitespace from the end of a string.
	 */
	rtrim				:: !s -> s
	
	/**
	 * Pads a string to a fixed length by adding characters to the beginning of a string.
	 */
	lpad				:: !s !Int !Char -> s
	
	/**
	 * Pads a string to a fixed length by adding characters to the end of a string.
	 */
	rpad				:: !s !Int !Char -> s
	
	/**
	 * Converts all characters in a string to lower case.
	 */
	toLowerCase			:: !s -> s
	
	/**
	 * Converts all characters in a string to upper case.
	 */
	toUpperCase			:: !s -> s
	
	/**
	 * Convert the first character in a string to upper case.
	 */
	upperCaseFirst		:: !s -> s
	
	/**
	 * Drop given number of chars from the beginning of the string
	 */
	dropChars			:: !Int !s -> s

instance Text String
instance Text [Char]
instance + String

/**
 * Concatenate two things.
 * This is a simple wrapper around the {{`+++`}} instance for `String` which
 * first applies {{`toString`}} to both its parameters.
 */
(<+) infixr 5 :: a b -> String | toString a & toString b
