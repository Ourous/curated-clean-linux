definition module Text.PPrint

from Data.Maybe import :: Maybe

/*
 * PPrint
 * Pretty print module based on Philip Wadlers "prettier printer"
 *       "A prettier printer"
 *       Draft paper, April 1997, revised March 1998.
 *       http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps
 *
 * Haskell implementation by Daan Leijen
 *
 * Copyright (c) 2000, Daan Leijen, http://www.cs.uu.nl/~daan
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * This software is provided by the copyright holders and contributors "as is" and
 * any express or implied warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose are
 * disclaimed. in no event shall the copyright holder be liable for any
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and
 * on any theory of liability, whether in contract, strict liability, or tort
 * (including negligence or otherwise) arising in any way out of the use of this
 * software, even if advised of the possibility of such damage.
 */

class Pretty a where
  pretty        :: !a -> Doc

instance Pretty [a] | Pretty a

instance Pretty Doc

instance Pretty Bool

instance Pretty Char

instance Pretty Int

instance Pretty Real

instance Pretty (a,b) | Pretty a & Pretty b

instance Pretty (a,b,c) | Pretty a & Pretty b & Pretty c

instance Pretty (Maybe a) | Pretty a

list :: ([Doc] -> Doc)
tupled :: ([Doc] -> Doc)
semiBraces :: ([Doc] -> Doc)
encloseSep :: Doc Doc Doc ![Doc] -> Doc
punctuate :: Doc ![Doc] -> [Doc]

sep :: ([Doc] -> Doc)
fillSep :: ([Doc] -> Doc)
hsep :: ([Doc] -> Doc)
vsep :: ([Doc] -> Doc)
cat :: ([Doc] -> Doc)
fillCat :: ([Doc] -> Doc)
hcat :: ([Doc] -> Doc)
vcat :: ([Doc] -> Doc)
fold :: (Doc Doc ->Doc) ![Doc] -> Doc
(<->) infixr 6 :: Doc Doc -> Doc
(<+>) infixr 6 :: Doc Doc -> Doc
(</>) infixr 5 :: Doc Doc -> Doc
(<//>) infixr 5 :: Doc Doc -> Doc
(<$>) infixr 5 :: Doc Doc -> Doc
(<$$>) infixr 5 :: Doc Doc -> Doc
softline :: Doc
softbreak :: Doc
squotes :: (Doc -> Doc)
dquotes :: (Doc -> Doc)
braces :: (Doc -> Doc)
parens :: (Doc -> Doc)
angles :: (Doc -> Doc)
brackets :: (Doc -> Doc)
enclose :: Doc Doc Doc -> Doc
lparen :: Doc
rparen :: Doc
langle :: Doc
rangle :: Doc
lbrace :: Doc
rbrace :: Doc
lbracket :: Doc
rbracket :: Doc
squote :: Doc
dquote :: Doc
semi :: Doc
colon :: Doc
comma :: Doc
space :: Doc
dot :: Doc
backslash :: Doc
equals :: Doc

/* -----------------------------------------------------------
 * Combinators for prelude types
 * ----------------------------------------------------------- */

string :: !String -> Doc
bool :: !Bool -> Doc
int :: !Int -> Doc
real :: !Real -> Doc

/* -----------------------------------------------------------
 * semi primitive: fill and fillBreak
 * ----------------------------------------------------------- */
fillBreak :: Int Doc -> Doc
fill :: Int Doc -> Doc
width :: Doc (Int -> Doc) -> Doc

/* -----------------------------------------------------------
 * semi primitive: Alignment and indentation
 * ----------------------------------------------------------- */
indent :: Int Doc -> Doc
hang :: Int Doc -> Doc
align :: Doc -> Doc

/* -----------------------------------------------------------
 * Primitives
 * ----------------------------------------------------------- */
:: Doc
::  SimpleDoc

empty :: Doc
char :: !Char -> Doc
text :: !String -> Doc
line :: Doc
linebreak :: Doc
beside :: Doc Doc -> Doc
nest :: !Int Doc -> Doc
column :: (Int -> Doc) -> Doc
nesting :: (Int -> Doc) -> Doc

/* -----------------------------------------------------------
 * Renderers
 * ----------------------------------------------------------- */

renderPretty :: Real Int !Doc -> SimpleDoc
renderCompact :: !Doc -> SimpleDoc
display :: !SimpleDoc -> String
