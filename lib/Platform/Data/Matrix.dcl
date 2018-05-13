definition module Data.Matrix

from StdEnv import class *, class +, class zero

:: Matrix a :== {{a}}
:: Vector a :== {a}

instance * (Matrix a) | *, +, zero a

dotVec :: !(Vector a) (Vector a) -> a | *, +, zero a

mulVecMatrix :: !(Vector a) !(Matrix a) -> Matrix a | *, +, zero a

mulMatrixVec :: !(Matrix a) !(Vector a) -> Matrix a | *, +, zero a

row :: !Int !(Matrix a) -> Vector a

col :: !Int !(Matrix a) -> Vector a

cols :: !(Matrix a) -> Int

rows :: !(Matrix a) -> Int
