definition module Text.Unicode.UChar

import StdClass

:: UChar (:== Int)

instance ==	UChar
instance < UChar

instance fromInt UChar
instance fromChar UChar

// UNSAFE if isAscii gives false!
instance toChar UChar
instance toInt UChar

// | Selects alphabetic Unicode characters (lower-case, upper-case and
// title-case letters, plus letters of caseless scripts and modifiers letters).
// This function is equivalent to 'Data.Char.isAlpha'.
isLetter 		:: !UChar -> Bool
// | Selects Unicode mark characters, e.g. accents and the like, which
// combine with preceding letters.
isMark 			:: !UChar -> Bool
// | Selects Unicode numeric characters, including digits from various
// scripts, Roman numerals, etc.
isNumber 		:: !UChar -> Bool
// | Selects Unicode punctuation characters, including various kinds
// of connectors, brackets and quotes.
isPunctuation 	:: !UChar -> Bool
// | Selects Unicode symbol characters, including mathematical and
// currency symbols.
isSymbol 		:: !UChar -> Bool
// | Selects Unicode space and separator characters.
isSeparator 	:: !UChar -> Bool

// | Selects alphabetic Unicode characters (lower-case, upper-case and
// title-case letters, plus letters of caseless scripts and modifiers letters).
// This function is equivalent to 'Data.Char.isLetter'.
isAlpha 		:: !UChar -> Bool
// | Selects alphabetic or numeric digit Unicode characters.
isAlphaNum  	:: !UChar -> Bool
// | Selects the first 128 characters of the Unicode character set,
// corresponding to the ASCII character set.
isAscii 		:: !UChar -> Bool
// | Selects the first 256 characters of the Unicode character set,
// corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1 		:: !UChar -> Bool
// | Selects ASCII lower-case letters,
// i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower 	:: !UChar -> Bool
// | Selects ASCII upper-case letters,
// i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper 	:: !UChar -> Bool
// | Returns 'True' for any Unicode space character, and the control
// characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
isSpace 		:: !UChar -> Bool
// | Selects control characters, which are the non-printing characters of
// the Latin-1 subset of Unicode.
isControl 		:: !UChar -> Bool                 
// | Selects printable Unicode characters
// (letters, numbers, marks, punctuation, symbols and spaces).
isPrint 		:: !UChar -> Bool
// | Selects upper-case or title-case alphabetic Unicode characters (letters).
// Title case is used by a small number of letter ligatures like the
// single-character form of /Lj/.
isUpper 		:: !UChar -> Bool
// | Selects lower-case alphabetic Unicode characters (letters).
isLower 		:: !UChar -> Bool
// | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit 		:: !UChar -> Bool
// | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit 		:: !UChar -> Bool
// | Selects ASCII hexadecimal digits,
// i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit 		:: !UChar -> Bool
// | Convert a letter to the corresponding lower-case letter, if any.
// Any other character is returned unchanged.
toLower 		:: !UChar -> UChar
// | Convert a letter to the corresponding upper-case letter, if any.
// Any other character is returned unchanged.
toUpper 		:: !UChar -> UChar
// | Convert a letter to the corresponding title-case or upper-case
// letter, if any.  (Title case differs from upper case only for a small
// number of ligature letters.)
// Any other character is returned unchanged.
toTitle 		:: !UChar -> UChar

