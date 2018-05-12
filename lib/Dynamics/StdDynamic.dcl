definition module StdDynamic

from StdClass import
	class toString, class ==

from _SystemDynamic import

	:: TypeCode (..),
	instance toString (TypeCode),
	instance == TypeCode,

	:: TypeCodeConstructor,
	instance == TypeCodeConstructor,
	instance toString TypeCodeConstructor,

	typeCodeOfDynamic