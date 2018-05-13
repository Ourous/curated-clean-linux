definition module Data.Tuple

from Data.Functor import class Functor
from Data.Monoid import class Semigroup, class Monoid
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Data.Bifunctor import class Bifunctor

tuple  :: .a .b -> .(.a,.b)
tuple3 :: .a .b .c -> .(.a,.b,.c)

appFst :: .(.a -> .c) !(.a,.b) -> (.c,.b)
appSnd :: .(.b -> .c) !(.a,.b) -> (.a,.c)

appFst3 :: .(.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appSnd3 :: .(.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appThd3 :: .(.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)

swap :: !.(.a, .b) -> .(.b, .a)

instance Functor ((,) a)

instance Semigroup (a, b) | Semigroup a & Semigroup b
instance Semigroup (a, b, c) | Semigroup a & Semigroup b & Semigroup c
instance Semigroup (a, b, c, d) | Semigroup a & Semigroup b & Semigroup c & Semigroup d
instance Semigroup (a, b, c, d, e) | Semigroup a & Semigroup b & Semigroup c & Semigroup d & Semigroup e

instance Monoid (a, b) | Monoid a & Monoid b
instance Monoid (a, b, c) | Monoid a & Monoid b & Monoid c
instance Monoid (a, b, c, d) | Monoid a & Monoid b & Monoid c & Monoid d
instance Monoid (a, b, c, d, e) | Monoid a & Monoid b & Monoid c & Monoid d & Monoid e

instance Foldable ((,) a)
instance Traversable ((,) a)

instance Bifunctor (,)
instance Bifunctor ((,,) x)
instance Bifunctor ((,,,) x y)
instance Bifunctor ((,,,,) x y z)
