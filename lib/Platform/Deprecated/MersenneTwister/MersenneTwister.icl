implementation module MersenneTwister

import qualified Math.Random

genRandReal :: !Int -> [Real]
genRandReal n = 'Math.Random'.genRandReal n

genRandInt :: !Int -> [Int]
genRandInt n = 'Math.Random'.genRandInt n
