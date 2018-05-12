definition module Gast.CommandLine

/**
 * A wrapper around a set of Gast properties that handles options as defined in
 * {{`Testing.Options`}}.
 */

from Gast.Testable import class Testable, :: Testoption, :: PrintOption

class getOptions a :: a -> [Testoption]
class getPrintOptions a :: a -> [PrintOption]

instance getOptions a
instance getPrintOptions a

instance Testable (o1, o2, a) | Testable a
instance getOptions ([Testoption], a, b)
instance getPrintOptions (a, [PrintOption], b)

/**
 * Wrap a TestableWithOptions in an existential type to easily build
 * quasi-heterogeneous lists of properties.
 */
:: ExposedProperty = E.p: EP p & Testable, getOptions p

instance Testable ExposedProperty
instance getOptions ExposedProperty

/**
 * Expose a set of Gast properties as a CLI application.
 *
 * @param The print options
 * @param The default options for all tests.
 * @param The tests to expose.
 */
exposeProperties :: ![PrintOption] ![Testoption] ![a] !*World -> *World | Testable, getOptions a
