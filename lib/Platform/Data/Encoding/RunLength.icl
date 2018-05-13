implementation module Data.Encoding.RunLength

import StdOverloaded, StdList

encodeInt :: ![Int] -> [Int]
encodeInt xs = reverse (rleInt` xs [])
  where
  rleInt` :: ![Int] ![Int] -> [Int]
  rleInt` []     acc = acc
  rleInt` [x:xs] []  = rleInt` xs [x, 1]
  rleInt` [x:xs] [y : n : ys]
    | x == y    = rleInt` xs [y : n + 1 : ys]
    | otherwise = rleInt` xs [x : 1 : y : n : ys]

decodeInt :: ![Int] -> [Int]
decodeInt xs = reverse (rldInt` xs [])
  where
  rldInt` :: ![Int] ![Int] -> [Int]
  rldInt` []           acc = acc
  rldInt` [0 : x : xs] acc = rldInt` xs acc
  rldInt` [n : x : xs] acc = rldInt` [n - 1 : x : xs] [x : acc]

encode :: ![a] -> [(Int, a)] | == a
encode xs = reverse (rle` xs [])
  where
  rle` :: ![a] ![(!Int, !a)] -> [(Int, a)] | == a
  rle` []     acc = acc
  rle` [x:xs] [] = rle` xs [(1, x)]
  rle` [x:xs] [t=:(n, y) : ys]
    | x == y    = rle` xs [(n + 1, y) : ys]
    | otherwise = rle` xs [(1, x) : t : ys]

decode :: ![(!Int, !a)] -> [a] | == a
decode xs = reverse (rld` xs [])
  where
  rld` :: ![(!Int, !a)] ![a] -> [a] | == a
  rld` []            acc = acc
  rld` [(0, x) : xs] acc = rld` xs acc
  rld` [(n, x) : xs] acc = rld` [(n - 1, x) : xs] [x : acc]

