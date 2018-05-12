implementation module Text.Parsers.Simple.Chars

import Text.Parsers.Simple.Core
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Func
import Data.List
from StdFunc import o, const

pUpper :: Parser Char Char
pUpper = pSatisfy isUpper

pLower :: Parser Char Char
pLower = pSatisfy isLower

pAlpha :: Parser Char Char
pAlpha = pSatisfy isAlpha

pAlphanum :: Parser Char Char
pAlphanum = pSatisfy isAlphanum

pDigit :: Parser Char Char
pDigit = pSatisfy isDigit

pOctDigit :: Parser Char Char
pOctDigit = pSatisfy isOctDigit

pHexDigit :: Parser Char Char
pHexDigit = pSatisfy isHexDigit

pSpace :: Parser Char Char
pSpace = pSatisfy isSpace

pControl :: Parser Char Char
pControl = pSatisfy isControl

pPrint :: Parser Char Char
pPrint = pSatisfy isPrint

pAscii :: Parser Char Char
pAscii = pSatisfy isAscii

pPOpen :: Parser Char Char
pPOpen = pToken '('

pPClose :: Parser Char Char
pPClose = pToken ')'

pBOpen :: Parser Char Char
pBOpen = pToken '['

pBClose :: Parser Char Char
pBClose = pToken ']'

pCOpen :: Parser Char Char
pCOpen = pToken '{'

pCClose :: Parser Char Char
pCClose = pToken '}'

pComma :: Parser Char Char
pComma = pToken ','

pSemiCol :: Parser Char Char
pSemiCol = pToken ';'

pLt :: Parser Char Char
pLt = pToken '<'

pGt :: Parser Char Char
pGt = pToken '>'

pEq :: Parser Char Char
pEq = pToken '='

pAmp :: Parser Char Char
pAmp = pToken '&'

pPipe :: Parser Char Char
pPipe = pToken '|'

pDash :: Parser Char Char
pDash = pToken '-'

pUnderscore :: Parser Char Char
pUnderscore = pToken '_'
