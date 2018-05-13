definition module Text.Parsers.Simple.Chars

from Text.Parsers.Simple.Core import :: Parser

pUpper    :: Parser Char Char
pLower    :: Parser Char Char
pAlpha    :: Parser Char Char
pAlphanum :: Parser Char Char
pDigit    :: Parser Char Char
pOctDigit :: Parser Char Char
pHexDigit :: Parser Char Char
pSpace    :: Parser Char Char
pControl  :: Parser Char Char
pPrint    :: Parser Char Char
pAscii    :: Parser Char Char

pPOpen      :: Parser Char Char
pPClose     :: Parser Char Char
pBOpen      :: Parser Char Char
pBClose     :: Parser Char Char
pCOpen      :: Parser Char Char
pCClose     :: Parser Char Char
pComma      :: Parser Char Char
pSemiCol    :: Parser Char Char
pLt         :: Parser Char Char
pGt         :: Parser Char Char
pEq         :: Parser Char Char
pAmp        :: Parser Char Char
pPipe       :: Parser Char Char
pDash       :: Parser Char Char
pUnderscore :: Parser Char Char
