implementation module Data.Graph.Inductive.Internal.Queue

import Data.List
import StdBool, StdFunc, StdList

:: Queue a = MkQueue [a] [a]

mkQueue :: Queue a
mkQueue = MkQueue [] []

queuePut :: a (Queue a) -> Queue a
queuePut item (MkQueue ins outs) = MkQueue [item:ins] outs

queuePutList :: [a] (Queue a) -> Queue a
queuePutList xs q = foldl (flip queuePut) q xs

queueGet :: (Queue a) -> (a, Queue a)
queueGet (MkQueue ins [item:rest]) = (item, MkQueue ins rest)
queueGet (MkQueue ins []) = queueGet (MkQueue [] (reverse ins))

queueEmpty :: (Queue a) -> Bool
queueEmpty (MkQueue ins outs) = isEmpty ins && isEmpty outs
