definition module Data.Maybe.Ord

/**
 * This contains a possible `<` and with this an `Ord` instance of the `Maybe` type.
 * As there is no natural order and therefore an arbitrary choice is made,
 * the instances is provided in a separate module. This makes is easier to use another ordering.
 */

from StdOverloaded import class <
from Data.Maybe    import :: Maybe

instance < (Maybe a) | < a
