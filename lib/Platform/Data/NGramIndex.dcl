definition module Data.NGramIndex

/**
 * *n*-grams are sequences of *n* characters. The *n*-gram index maps *n*-grams
 * to sets of values. This is for instance useful in search engines.
 */

from StdClass import class Eq, class ==, class Ord, class <
from Data.Map import :: Map
from Data.Maybe import :: Maybe

/**
 * The *n*-gram index maps character *n*-grams to sets of values.
 */
:: NGramIndex v =
	{ n   :: !Int            //* The parameter *n* for the size of the grams
	, ci  :: !Bool           //* Whether matching is case-insensitive
	, idx :: !Map String [v] //* The values
	}

/**
 * Create a new {{`NGramIndex`}}.
 * @param The parameter *n*
 * @param Whether the index should be case insensitive
 */
newNGramIndex :: !Int !Bool -> NGramIndex v

/**
 * Get the size (the number of grams, not the number of values) of an
 * {{`NGramIndex`}}.
 */
ngramSize :: !(NGramIndex v) -> Int

/**
 * Add a certain value with a certain key to an index.
 * @param The key. For all *n*-grams of the key, the value will be added to the
 *   index
 * @param The value
 */
index :: !String !v !(NGramIndex v) -> NGramIndex v | Eq v

/**
 * Search for a key in the index.
 * @param The key. For all *n*-grams of the key, the values will be returned.
 *   If the key has less than *n* characters, it is used as gram itself
 * @result For each matching value, a tuple of the value and the number of
 *   matching *n*-grams is returned
 * 
 */
search :: !String !(NGramIndex v) -> [(v,Int)] | Eq, Ord v

/**
 * Get the *n*-grams of a string.
 * @param Whether this should be done case insensitively
 * @param The parameter *n*
 * @param The string
 */
ngrams :: !Bool !Int !String -> [String]
