definition module Data.Bifunctor

class Bifunctor p
where
	bifmap :: (a -> b) (c -> d) (p a c) -> p b d
	first :: (a -> b) (p a c) -> p b c
	second :: (b -> c) (p a b) -> p a c
