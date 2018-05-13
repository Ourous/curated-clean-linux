definition module Data.Graph.Inductive.Internal.Queue

:: Queue a = MkQueue [a] [a]

mkQueue :: Queue a

queuePut :: a (Queue a) -> Queue a

queuePutList :: [a] (Queue a) -> Queue a

queueGet :: (Queue a) -> (a, Queue a)

queueEmpty :: (Queue a) -> Bool
