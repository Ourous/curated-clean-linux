definition module Data.Encoding.RunLength

from StdOverloaded import class ==

encodeInt :: ![Int] -> [Int]
decodeInt :: ![Int] -> [Int]
encode :: ![a] -> [(Int, a)] | == a
decode :: ![(Int, a)] -> [a] | == a
