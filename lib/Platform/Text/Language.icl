implementation module Text.Language

import StdArray
import StdBool
import StdChar
from StdFunc import id
import StdList
import StdString

import Text

isVowel :: !Language !Char -> Bool
isVowel English c = isMember c ['aeiou']

isConsonant :: !Language !Char -> Bool
isConsonant l c = isAlpha c && not (isVowel l c)

pluralise :: !Language !String -> String
pluralise English s
	| last == 'y' && (isConsonant English forelast || forelast == 'u')
		= s % (0, size s-2) +++ "ies"
		= s +++ "s"
	where (last, forelast) = (s.[size s-1], s.[size s-2])

pluralisen :: !Language !Int !String -> String
pluralisen English n s = n <+ " " <+ if (n == 1) id (pluralise English) s
