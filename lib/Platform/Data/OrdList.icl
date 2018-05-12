implementation module Data.OrdList

import StdList, StdOrdList

/** removeMembersSortedList @xs @ys = @zs:
       removes all occurrences of elements in @ys from @xs, resulting in @zs.
       The function assumes that both @xs and @ys are sorted and contain no duplicate elements.
*/
removeMembersSortedList :: ![a] ![a] -> [a] | Eq, Ord a
removeMembersSortedList [] ys
	= []
removeMembersSortedList xs []
	= xs
removeMembersSortedList [x:xs] [y:ys]
| x  < y    = [x : removeMembersSortedList xs [y:ys]]
| x == y    = removeMembersSortedList xs ys
| otherwise = removeMembersSortedList [x:xs] ys

/** removeDupSortedList @xs = @zs:
       removes all duplicate elements from @xs, resulting in @zs.
       The function assumes that @xs is sorted.
*/
removeDupSortedList :: ![a] -> [a] | Eq a
removeDupSortedList []
	= []
removeDupSortedList [x:xs]
	= [x : removeDupSortedList (dropWhile ((==) x) xs)]
