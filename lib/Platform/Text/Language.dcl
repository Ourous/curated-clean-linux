definition module Text.Language

/**
 * A natural language.
 */
:: Language = English

/**
 * Check whether a character is a vowel.
 */
isVowel :: Language Char -> Bool

/**
 * Check whether a character is a consonant.
 */
isConsonant :: Language Char -> Bool

/**
 * Pluralise a noun.
 */
pluralise :: Language String -> String

/**
 * Construct a string like "3 items" based on a number and a noun.
 */
pluralisen :: Language Int String -> String
