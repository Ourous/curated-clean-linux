definition module Data.Bifunctor

class Bifunctor p
where
	bifmap :: (a -> b) (c -> d) (p a c) -> p b d
	bifmap l r a = first l (second r a)
	first :: (a -> b) (p a c) -> p b c
	first l a = bifmap l (\x->x) a
	second :: (b -> c) (p a b) -> p a c
	second r a = bifmap (\x->x) r a
