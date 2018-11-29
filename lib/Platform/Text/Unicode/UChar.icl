implementation module Text.Unicode.UChar

import StdEnum, StdClass, StdBool, StdFunc, StdMisc

import code from "bsearch."
import code from "WCsubst."

:: UChar :== Int

:: GeneralCategory
        = UppercaseLetter       // ^ Lu: Letter, Uppercase
        | LowercaseLetter       // ^ Ll: Letter, Lowercase
        | TitlecaseLetter       // ^ Lt: Letter, Titlecase
        | ModifierLetter        // ^ Lm: Letter, Modifier
        | OtherLetter           // ^ Lo: Letter, Other
        | NonSpacingMark        // ^ Mn: Mark, Non-Spacing
        | SpacingCombiningMark  // ^ Mc: Mark, Spacing Combining
        | EnclosingMark         // ^ Me: Mark, Enclosing
        | DecimalNumber         // ^ Nd: Number, Decimal
        | LetterNumber          // ^ Nl: Number, Letter
        | OtherNumber           // ^ No: Number, Other
        | ConnectorPunctuation  // ^ Pc: Punctuation, Connector
        | DashPunctuation       // ^ Pd: Punctuation, Dash
        | OpenPunctuation       // ^ Ps: Punctuation, Open
        | ClosePunctuation      // ^ Pe: Punctuation, Close
        | InitialQuote          // ^ Pi: Punctuation, Initial quote
        | FinalQuote            // ^ Pf: Punctuation, Final quote
        | OtherPunctuation      // ^ Po: Punctuation, Other
        | MathSymbol            // ^ Sm: Symbol, Math
        | CurrencySymbol        // ^ Sc: Symbol, Currency
        | ModifierSymbol        // ^ Sk: Symbol, Modifier
        | OtherSymbol           // ^ So: Symbol, Other
        | Space                 // ^ Zs: Separator, Space
        | LineSeparator         // ^ Zl: Separator, Line
        | ParagraphSeparator    // ^ Zp: Separator, Paragraph
        | Control               // ^ Cc: Other, Control
        | Format                // ^ Cf: Other, Format
        | Surrogate             // ^ Cs: Other, Surrogate
        | PrivateUse            // ^ Co: Other, Private Use
        | NotAssigned           // ^ Cn: Other, Not Assigned

wgencat :: !Int -> Int
wgencat ch = code inline {
	ccall u_gencat "I:I"
}

iswalpha :: !Int -> Int
iswalpha ch = code inline {
	ccall u_iswalpha "I:I"
}

iswalnum :: !Int -> Int
iswalnum ch = code inline {
	ccall u_iswalnum "I:I"
}

iswcntrl :: !Int -> Int
iswcntrl ch = code inline {
	ccall u_iswcntrl "I:I"
}

iswspace :: !Int -> Int
iswspace ch = code inline {
	ccall u_iswspace "I:I"
}

iswprint :: !Int -> Int
iswprint ch = code inline {
	ccall u_iswprint "I:I"
}

iswlower :: !Int -> Int
iswlower ch = code inline {
	ccall u_iswlower "I:I"
}

iswupper :: !Int -> Int
iswupper ch = code inline {
	ccall u_iswupper "I:I"
}

towlower :: !Int -> Int
towlower ch = code inline {
	ccall u_towlower "I:I"
}

towupper :: !Int -> Int
towupper ch = code inline {
	ccall u_towupper "I:I"
}

towtitle :: !Int -> Int
towtitle ch = code inline {
	ccall u_towtitle "I:I"
}

instance fromInt GeneralCategory
where
	fromInt 0  = UppercaseLetter       // ^ Lu: Letter, Uppercase
	fromInt 1  = LowercaseLetter       // ^ Ll: Letter, Lowercase
	fromInt 2  = TitlecaseLetter       // ^ Lt: Letter, Titlecase
	fromInt 3  = ModifierLetter        // ^ Lm: Letter, Modifier
	fromInt 4  = OtherLetter           // ^ Lo: Letter, Other
	fromInt 5  = NonSpacingMark        // ^ Mn: Mark, Non-Spacing
	fromInt 6  = SpacingCombiningMark  // ^ Mc: Mark, Spacing Combining
	fromInt 7  = EnclosingMark         // ^ Me: Mark, Enclosing
	fromInt 8  = DecimalNumber         // ^ Nd: Number, Decimal
	fromInt 9  = LetterNumber          // ^ Nl: Number, Letter
	fromInt 10 = OtherNumber           // ^ No: Number, Other
	fromInt 11 = ConnectorPunctuation  // ^ Pc: Punctuation, Connector
	fromInt 12 = DashPunctuation       // ^ Pd: Punctuation, Dash
	fromInt 13 = OpenPunctuation       // ^ Ps: Punctuation, Open
	fromInt 14 = ClosePunctuation      // ^ Pe: Punctuation, Close
	fromInt 15 = InitialQuote          // ^ Pi: Punctuation, Initial quote
	fromInt 16 = FinalQuote            // ^ Pf: Punctuation, Final quote
	fromInt 17 = OtherPunctuation      // ^ Po: Punctuation, Other
	fromInt 18 = MathSymbol            // ^ Sm: Symbol, Math
	fromInt 19 = CurrencySymbol        // ^ Sc: Symbol, Currency
	fromInt 20 = ModifierSymbol        // ^ Sk: Symbol, Modifier
	fromInt 21 = OtherSymbol           // ^ So: Symbol, Other
	fromInt 22 = Space                 // ^ Zs: Separator, Space
	fromInt 23 = LineSeparator         // ^ Zl: Separator, Line
	fromInt 24 = ParagraphSeparator    // ^ Zp: Separator, Paragraph
	fromInt 25 = Control               // ^ Cc: Other, Control
	fromInt 26 = Format                // ^ Cf: Other, Format
	fromInt 27 = Surrogate             // ^ Cs: Other, Surrogate
	fromInt 28 = PrivateUse            // ^ Co: Other, Private Use
	fromInt 29 = NotAssigned           // ^ Cn: Other, Not Assigned
	fromInt _  = abort "error in fromInt_GeneralCategory\n"

generalCategory c = fromInt (wgencat (toInt c))

// | Selects alphabetic Unicode characters (lower-case, upper-case and
// title-case letters, plus letters of caseless scripts and modifiers letters).
// This function is equivalent to 'Data.Char.isAlpha'.
isLetter :: !UChar -> Bool
isLetter c = case generalCategory c of
        UppercaseLetter         -> True
        LowercaseLetter         -> True
        TitlecaseLetter         -> True
        ModifierLetter          -> True
        OtherLetter             -> True
        _                       -> False

// | Selects Unicode mark characters, e.g. accents and the like, which
// combine with preceding letters.
isMark :: !UChar -> Bool
isMark c = case generalCategory c of
        NonSpacingMark          -> True
        SpacingCombiningMark    -> True
        EnclosingMark           -> True
        _                       -> False

// | Selects Unicode numeric characters, including digits from various
// scripts, Roman numerals, etc.
isNumber :: !UChar -> Bool
isNumber c = case generalCategory c of
        DecimalNumber           -> True
        LetterNumber            -> True
        OtherNumber             -> True
        _                       -> False

// | Selects Unicode punctuation characters, including various kinds
// of connectors, brackets and quotes.
isPunctuation :: !UChar -> Bool
isPunctuation c = case generalCategory c of
        ConnectorPunctuation    -> True
        DashPunctuation         -> True
        OpenPunctuation         -> True
        ClosePunctuation        -> True
        InitialQuote            -> True
        FinalQuote              -> True
        OtherPunctuation        -> True
        _                       -> False

// | Selects Unicode symbol characters, including mathematical and
// currency symbols.
isSymbol :: !UChar -> Bool
isSymbol c = case generalCategory c of
        MathSymbol              -> True
        CurrencySymbol          -> True
        ModifierSymbol          -> True
        OtherSymbol             -> True
        _                       -> False

// | Selects Unicode space and separator characters.
isSeparator :: !UChar -> Bool
isSeparator c = case generalCategory c of
        Space                   -> True
        LineSeparator           -> True
        ParagraphSeparator      -> True
        _                       -> False 

// | Selects alphabetic Unicode characters (lower-case, upper-case and
// title-case letters, plus letters of caseless scripts and modifiers letters).
// This function is equivalent to 'Data.Char.isLetter'.
isAlpha :: !UChar -> Bool
isAlpha c = not (iswalpha c == 0)

// | Selects alphabetic or numeric digit Unicode characters.
//
// Note that numeric digits outside the ASCII range are selected by this
// function but not by 'isDigit'.  Such digits may be part of identifiers
// but are not used by the printer and reader to represent numbers.
isAlphaNum  :: !UChar -> Bool
isAlphaNum c = not (iswalnum c == 0)

// | Selects the first 128 characters of the Unicode character set,
// corresponding to the ASCII character set.
isAscii :: !UChar -> Bool
isAscii c = c < 0x80

// | Selects the first 256 characters of the Unicode character set,
// corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1 :: !UChar -> Bool
isLatin1 c = c <= 0xff

// | Selects ASCII lower-case letters,
// i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower :: !UChar -> Bool
isAsciiLower c =  c >= fromChar 'a' && c <= fromChar 'z'

// | Selects ASCII upper-case letters,
// i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper :: !UChar -> Bool
isAsciiUpper c = c >= fromChar 'A' && c <= fromChar 'Z'

// | Returns 'True' for any Unicode space character, and the control
// characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
// isSpace includes non-breaking space
// Done with explicit equalities both for efficiency, and to avoid a tiresome
// recursion with GHC.List elem
isSpace :: !UChar -> Bool
isSpace c
		  = c == fromChar ' '     ||
            c == fromChar '\t'    ||
            c == fromChar '\n'    ||
            c == fromChar '\r'    ||
            c == fromChar '\f'    ||
            c == fromChar '\v'    ||
            c == 0xa0  			  ||
            not (iswspace c == 0)

// | Selects control characters, which are the non-printing characters of
// the Latin-1 subset of Unicode.
isControl :: !UChar -> Bool                 
isControl c = not (iswcntrl c == 0)

// | Selects printable Unicode characters
// (letters, numbers, marks, punctuation, symbols and spaces).
isPrint :: !UChar -> Bool
isPrint c = not (iswprint c == 0)

// | Selects upper-case or title-case alphabetic Unicode characters (letters).
// Title case is used by a small number of letter ligatures like the
// single-character form of /Lj/.
isUpper :: !UChar -> Bool
isUpper c = not (iswupper c == 0)

// | Selects lower-case alphabetic Unicode characters (letters).
isLower :: !UChar -> Bool
isLower c = not (iswlower c == 0)

// | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit :: !UChar -> Bool
isDigit c = c >= fromChar '0' && c <= fromChar '9'

// | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit :: !UChar -> Bool
isOctDigit c =  c >= fromChar '0' && c <= fromChar '7'

// | Selects ASCII hexadecimal digits,
// i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit :: !UChar -> Bool
isHexDigit c =  
		isDigit c || c >= fromChar 'A' && c <= fromChar 'F' ||
                     c >= fromChar 'a' && c <= fromChar 'f'

// | Convert a letter to the corresponding lower-case letter, if any.
// Any other character is returned unchanged.
toLower :: !UChar -> UChar
toLower c = towlower c

// | Convert a letter to the corresponding upper-case letter, if any.
// Any other character is returned unchanged.
toUpper :: !UChar -> UChar
toUpper c = towupper c

// | Convert a letter to the corresponding title-case or upper-case
// letter, if any.  (Title case differs from upper case only for a small
// number of ligature letters.)
// Any other character is returned unchanged.
toTitle :: !UChar -> UChar
toTitle c =  towtitle c

instance ==	UChar
where
	(==) a b = code inline {
			eqI
	}

instance < UChar // TODO: correct?
where
	(<) x y = code inline { 
			ltI
	} 

instance fromInt UChar
where 
	fromInt i = i

instance fromChar UChar
where
	fromChar c = code inline {
			CtoI
	}
	
instance toChar UChar
where
	toChar i = code inline {
			ItoC
	}

instance toInt UChar
where
	toInt i = i
	
