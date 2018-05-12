definition module Data.OrdList

/**	This module extends StdOrdList with extra functions.
*/

import StdOrdList

/** removeMembersSortedList @xs @ys = @zs:
       removes all occurrences of elements in @ys from @xs, resulting in @zs.
       The function assumes that both @xs and @ys are sorted and contain no duplicate elements.
*/
removeMembersSortedList :: ![a] ![a] -> [a] | Eq, Ord a

/** removeDupSortedList @xs = @zs:
       removes all duplicate elements from @xs, resulting in @zs.
       The function assumes that @xs is sorted.
*/
removeDupSortedList :: ![a] -> [a] | Eq a
