definition module Data.OrdList

/**	
 * This module extends StdOrdList with extra functions.
 */

import StdOrdList

/**
 * removeMembersSortedList `xs ys` removes all occurrences of elements in `ys`
 * from `xs`. The function assumes that both `xs` and `ys` are sorted and
 * contain no duplicate elements.
 */
removeMembersSortedList :: ![a] ![a] -> [a] | Eq, Ord a

/**
 * removeDupSortedList `xs` removes all duplicate elements from `xs`, assuming
 * `xs` is sorted.
 */
removeDupSortedList :: ![a] -> [a] | Eq a
