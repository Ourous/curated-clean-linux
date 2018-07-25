definition module Clean.PrettyPrint.Common

from syntax import :: Ident, :: Import

from Clean.PrettyPrint.Util import class print

instance print Ident, Import
