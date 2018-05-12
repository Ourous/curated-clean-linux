definition module Data.IntSet.Base

:: IntSet = Bin !Prefix !Mask !IntSet !IntSet
// Invariant: Nil is never found as a child of Bin.
// Invariant: The Mask is a power of 2.  It is the largest bit position at which
//            two elements of the set differ.
// Invariant: Prefix is the common high-order bits that all elements share to
//            the left of the Mask bit.
// Invariant: In Bin prefix mask left right, left consists of the elements that
//            don't have the mask bit set; right is all the elements that do.
            | Tip !Prefix !BitMap
// Invariant: The Prefix is zero for all but the last 5 (on 32 bit arches) or 6
//            bits (on 64 bit arches). The values of the map represented by a tip
//            are the prefix plus the indices of the set bits in the bit map.
            | Nil

// A number stored in a set is stored as
// * Prefix (all but last 5-6 bits) and
// * BitMap (last 5-6 bits stored as a bitmask)
//   Last 5-6 bits are called a Suffix.

:: Prefix :== Int
:: Mask   :== Int
:: BitMap :== Int
:: Key    :== Int

member :: !Key IntSet -> Bool

fromList :: [Key] -> IntSet
