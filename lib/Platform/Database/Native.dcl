definition module Database.Native

/**
 * This module provides types and functions to build a database on the native
 * Clean heap. It can be tedious to add new entries, but access is fast. and
 * only little memory is used.
 */

from StdOverloaded import class ==, class <

from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

/**
 * A database. Entries can be soft deleted. Entries are indexed with integers
 * which can be difficult to work with but at least provide constant time
 * access.
 * Annotations are not designed to be persistent. If you need to add persistent
 * data to the database use a map over values.
 *
 * @var The type of values stored.
 * @var The key type of annotations.
 * @var The type of annotations.
 */
:: *NativeDB v a

:: Index =: Index Int

:: Entry v a =
	{ value       :: !v
	, included    :: !Bool
	, annotations :: ![a]
	}

instance == Index
instance < Index

/**
 * Two search modes are available.
 */
:: SearchMode
	= Intersect   //* Only consider included entries (i.e., an AND with previous searches)
	| AddExcluded //* Re-include matching entries but don't remove non-matching entries (i.e., an OR with previous searches)

/**
 * Create a new database from a list of entries.
 */
newDB :: ![v] -> *NativeDB v a

/**
 * Save the database to a file.
 */
saveDB :: !*(NativeDB v a) !*File -> *(!*NativeDB v a, !*File) | JSONEncode{|*|} v

/**
 * Open a database from a file.
 */
openDB :: !*File -> *(!Maybe (*NativeDB v a), !*File) | JSONDecode{|*|} v

/**
 * Reset all entries to included.
 */
resetDB :: !*(NativeDB v a) -> *NativeDB v a

/**
 * Return all entries (whether they have been excluded or not).
 */
allEntries :: !*(NativeDB v a) -> *(![v], !*NativeDB v a)

/**
 * Get all entries that are still included, and their annotations.
 */
getEntries :: !*(NativeDB v a) -> *(![(v, [a])], !*NativeDB v a)

/**
 * Like {{`getEntries`}}, but also returns the indices of the entries.
 */
getEntriesWithIndices :: !*(NativeDB v a) -> *(![(Index, v, [a])], !*NativeDB v a)

/**
 * An in-place map over all entries (also the excluded ones).
 */
mapInPlace :: !(Int v -> v) !*(NativeDB v a) -> *(NativeDB v a)

/**
 * Linear search for entries. The search function returns whether the entry
 * should be included and which annotations should be added (if any). Excluded
 * entries are ignored.
 */
search :: !SearchMode !(v -> (Bool, [a])) !*(NativeDB v a) -> *NativeDB v a

/**
 * Like {{`search`}}, but search for one particular index. The {{`SearchMode`}}
 * is assumed to be {{`AddExcluded`}}.
 */
searchIndex :: !Index ![a] !*(NativeDB v a) -> *NativeDB v a

/**
 * Exclude an index from the result set.
 */
unsearchIndex :: !Index !*(NativeDB v a) -> *NativeDB v a

/**
 * Like {{`search`}}, but search for specific indices.
 */
searchIndices :: !SearchMode ![(!Index, ![a])] !*(NativeDB v a) -> *NativeDB v a

/**
 * Exclude a list of indices.
 */
unsearchIndices :: ![Index] !*(NativeDB v a) -> *NativeDB v a

/**
 * Like {{`searchIndices`}}, but also check on some property.
 * This search always uses the {{`AddExcluded`}} {{`SearchMode`}}.
 */
searchWithIndices :: !(v -> (Bool, ![a])) ![Index] !*(NativeDB v a) -> *NativeDB v a

/**
 * Get an entry and its annotations.
 * Also see {{`getIndices`}}.
 */
getIndex :: !Index !*(NativeDB v a) -> *(!Entry v a, !*(NativeDB v a))

/**
 * Like {{`getIndex`}}, but for a list of indices.
 */
getIndices :: ![Index] !*(NativeDB v a) -> *(![Entry v a], !*(NativeDB v a))
