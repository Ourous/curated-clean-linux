implementation module Text.PPrint

import StdEnv
import qualified Data.Foldable
import Data.List
import Data.Maybe

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

/* -----------------------------------------------------------
 * list, tupled and semiBraces pretty print a list of
 * documents either horizontally or vertically aligned.
 * ----------------------------------------------------------- */

list :: ([Doc] -> Doc)
list            = encloseSep lbracket rbracket comma

tupled :: ([Doc] -> Doc)
tupled          = encloseSep lparen   rparen  comma

semiBraces :: ([Doc] -> Doc)
semiBraces      = encloseSep lbrace   rbrace  semi

encloseSep :: Doc Doc Doc ![Doc] -> Doc
encloseSep left right sep ds
    = case ds of
        []  -> left <-> right
        [d] -> left <-> d <-> right
        _   -> align (cat (zipWith (<->) [left : repeat sep] ds) <-> right)

zipWith :: (a b -> c) [a] [b] -> [c]
zipWith f xs ys = map (\(a,b) = f a b) (zip2 xs ys)

/* -----------------------------------------------------------
 * punctuate p [d1,d2,...,dn] => [d1 <-> p,d2 <-> p, ... ,dn]
 * ----------------------------------------------------------- */
punctuate :: Doc ![Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p [d:ds]  = [(d <-> p) : punctuate p ds]


/* -----------------------------------------------------------
 * high-level combinators
 * ----------------------------------------------------------- */

sep :: ([Doc] -> Doc)
sep             = group o vsep

fillSep :: ([Doc] -> Doc)
fillSep         = fold (</>)

hsep :: ([Doc] -> Doc)
hsep            = fold (<+>)

vsep :: ([Doc] -> Doc)
vsep            = fold (<$>)

cat :: ([Doc] -> Doc)
cat             = group o vcat

fillCat :: ([Doc] -> Doc)
fillCat         = fold (<//>)

hcat :: ([Doc] -> Doc)
hcat            = fold (<->)

vcat :: ([Doc] -> Doc)
vcat            = fold (<$$>)

fold :: (Doc Doc -> Doc) ![Doc] -> Doc
fold f []       = empty
fold f ds       = 'Data.Foldable'.foldr1 f ds

(<->) infixr 6 :: Doc Doc -> Doc
(<->) x y         = beside x y

(<+>) infixr 6 :: Doc Doc -> Doc
(<+>) x y         = x <-> space <-> y

(</>) infixr 5 :: Doc Doc -> Doc
(</>) x y         = x <-> softline <-> y

(<//>) infixr 5 :: Doc Doc -> Doc
(<//>) x y        = x <-> softbreak <-> y

(<$>) infixr 5 :: Doc Doc -> Doc
(<$>) x y         = x <-> line <-> y

(<$$>) infixr 5 :: Doc Doc -> Doc
(<$$>) x y        = x <-> linebreak <-> y

softline :: Doc
softline        = group line

softbreak :: Doc
softbreak       = group linebreak

squotes :: (Doc -> Doc)
squotes         = enclose squote squote

dquotes :: (Doc -> Doc)
dquotes         = enclose dquote dquote

braces :: (Doc -> Doc)
braces          = enclose lbrace rbrace

parens :: (Doc -> Doc)
parens          = enclose lparen rparen

angles :: (Doc -> Doc)
angles          = enclose langle rangle

brackets :: (Doc -> Doc)
brackets        = enclose lbracket rbracket

enclose :: Doc Doc Doc -> Doc
enclose l r x   = l <-> x <-> r

lparen :: Doc
lparen          = char '('

rparen :: Doc
rparen          = char ')'

langle :: Doc
langle          = char '<'

rangle :: Doc
rangle          = char '>'

lbrace :: Doc
lbrace          = char '{'

rbrace :: Doc
rbrace          = char '}'

lbracket :: Doc
lbracket        = char '['

rbracket :: Doc
rbracket        = char ']'

squote :: Doc
squote          = char '\''

dquote :: Doc
dquote          = char '"'

semi :: Doc
semi            = char ';'

colon :: Doc
colon           = char ':'

comma :: Doc
comma           = char ','

space :: Doc
space           = char ' '

dot :: Doc
dot             = char '.'

backslash :: Doc
backslash       = char '\\'

equals :: Doc
equals          = char '='

/* -----------------------------------------------------------
 * Combinators for predefined types
 * ----------------------------------------------------------- */

//string is like "text" but replaces '\n' by "line"
string :: !String -> Doc
string s		= str s 0 0 (size s) empty
where
	str :: String Int Int Int Doc -> Doc
	str s fr to len acc | to == len      = acc <-> text (s % (fr, to - 1))
	str s fr to len acc | s.[to] == '\n' = str s (to + 1) (to + 1) len (acc <-> text (s % (fr, to - 1)) <-> line)
						| otherwise      = str s fr (to + 1) len acc

bool :: !Bool -> Doc
bool b          = text (toString b)

int :: !Int -> Doc
int i           = text (toString i)

real :: !Real -> Doc
real r         = text (toString r)

/* -----------------------------------------------------------
 * overloading "pretty"
 * ----------------------------------------------------------- */
class Pretty a where
  pretty        :: !a -> Doc

instance Pretty [a] | Pretty a where
  pretty :: ![a] -> Doc | Pretty a
  pretty        xs = list (map pretty xs)

instance Pretty Doc where
  pretty :: !Doc -> Doc
  pretty        doc = doc

instance Pretty Bool where
  pretty :: !Bool -> Doc
  pretty b      = bool b

instance Pretty Char where
  pretty :: !Char -> Doc
  pretty c      = char c

instance Pretty Int where
  pretty :: !Int -> Doc
  pretty i      = int i

instance Pretty Real where
  pretty :: !Real -> Doc
  pretty r      = real r

instance Pretty (a,b) | Pretty a & Pretty b where
  pretty :: !(a,b) -> Doc | Pretty a & Pretty b
  pretty (x,y)  = tupled [pretty x, pretty y]

instance Pretty (a,b,c) | Pretty a & Pretty b & Pretty c where
  pretty :: !(a,b,c) -> Doc | Pretty a & Pretty b & Pretty c
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty (Maybe a) | Pretty a where
  pretty :: !(Maybe a) -> Doc | Pretty a
  pretty Nothing        = empty
  pretty (Just x)       = pretty x

/* -----------------------------------------------------------
 * semi primitive: fill and fillBreak
 * ----------------------------------------------------------- */
fillBreak :: Int Doc -> Doc
fillBreak f x   = width x (\w ->
                  if (w > f) (nest f linebreak)
                             (text (spaceString (f - w))))

fill :: Int Doc -> Doc
fill f d        = width d (\w ->
                  if (w >= f) empty
                              (text (spaceString (f - w))))

width :: Doc (Int -> Doc) -> Doc
width d f       = column (\k1 -> d <-> column (\k2 -> f (k2 - k1)))

/* -----------------------------------------------------------
 * semi primitive: Alignment and indentation
 * ----------------------------------------------------------- */
indent :: Int Doc -> Doc
indent i d      = hang i (text (spaceString i) <-> d)

hang :: Int Doc -> Doc
hang i d        = align (nest i d)

align :: Doc -> Doc
align d         = column (\k =
                  nesting (\i = nest (k - i) d))   //nesting might be negative :-)

/* -----------------------------------------------------------
 * Primitives
 * ----------------------------------------------------------- */
:: Doc          = Empty
                | Char Char              // invariant: char is not '\n'
                | Text !Int String       // invariant: text doesn't contain '\n'
                | Line !Bool             // True <=> when undone by group, do not insert a space
                | Cat Doc Doc
                | Nest !Int Doc
                | Union Doc Doc          // invariant: first lines of first doc longer than the first lines of the second doc
                | Column  (Int -> Doc)
                | Nesting (Int -> Doc)

::  SimpleDoc   = SEmpty
                | SChar Char SimpleDoc
                | SText !Int String SimpleDoc
                | SLine !Int SimpleDoc

empty :: Doc
empty           = Empty


char :: !Char -> Doc
char '\n'       = line
char c          = Char c

text :: !String -> Doc
text ""         = Empty
text s          = Text (size s) s

line :: Doc
line            = Line False

linebreak :: Doc
linebreak       = Line True

beside :: Doc Doc -> Doc
beside x y      = Cat x y

nest :: !Int Doc -> Doc
nest i x        = Nest i x

column :: (Int -> Doc) -> Doc
column f        = Column f

nesting :: (Int -> Doc) -> Doc
nesting f       = Nesting f

group :: Doc -> Doc
group x         = Union (flattenDoc x) x

flattenDoc :: Doc -> Doc
flattenDoc (Cat x y)       = Cat (flattenDoc x) (flattenDoc y)
flattenDoc (Nest i x)      = Nest i (flattenDoc x)
flattenDoc (Line break)    = if break Empty (Text 1 " ")
flattenDoc (Union x y)     = flattenDoc x
flattenDoc (Column f)      = Column (flattenDoc o f)
flattenDoc (Nesting f)     = Nesting (flattenDoc o f)
flattenDoc other           = other                     //Empty,Char,Text

/* -----------------------------------------------------------
 * Renderers
 * ----------------------------------------------------------- */

/* -----------------------------------------------------------
 * renderPretty: the default pretty printing algorithm
 * ----------------------------------------------------------- */

//list of indentation/document pairs; saves an indirection over [(Int,Doc)]
:: Docs   = Nil
          | Cons !Int Doc Docs

renderPretty :: Real Int !Doc -> SimpleDoc
renderPretty rfrac w x
    = best 0 0 (Cons 0 x Nil)
    where
      // r :: the ribbon width in characters
      r  = max 0 (min w (toInt (toReal w * rfrac)))

      /* -----------------------------------------------------------
       * best :: n = indentation of current line
       *         k = current column
       *        (ie. (k >= n) && (k - n == count of inserted characters)
       * ----------------------------------------------------------- */
      best n k Nil      = SEmpty
      best n k (Cons i d ds)
        = case d of
            Empty       -> best n k ds
            Char c      -> let k` = k+1 in (SChar c (best n k` ds))
            Text l s    -> let k` = k+l in (SText l s (best n k` ds))
            Line _      -> SLine i (best i i ds)
            Cat x y     -> best n k (Cons i x (Cons i y ds))
            Nest j x    -> let i` = i+j in (best n k (Cons i` x ds))
            Union x y   -> nicest n k (best n k (Cons i x ds))
                                      (best n k (Cons i y ds))

            Column f    -> best n k (Cons i (f k) ds)
            Nesting f   -> best n k (Cons i (f i) ds)

      /* -----------------------------------------------------------
       * nicest :: r = ribbon width, w = page width,
       *           n = indentation of current line, k = current column
       *           x and y, the (simple) documents to chose from.
       * precondition: first lines of x are longer than the first lines of y.
       * ----------------------------------------------------------- */
      nicest n k x y    | fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)

fits :: Int SimpleDoc -> Bool
fits w x        | w < 0         = False
fits w SEmpty                   = True
fits w (SChar c x)              = fits (w - 1) x
fits w (SText l s x)            = fits (w - l) x
fits w (SLine i x)              = True


/* -----------------------------------------------------------
 * renderCompact: renders documents without indentation
 *  fast and fewer characters output, good for machines
 * ----------------------------------------------------------- */

renderCompact :: !Doc -> SimpleDoc
renderCompact x
    = scan 0 [x]
    where
      scan k []     = SEmpty
      scan k [d:ds] = case d of
                        Empty       -> scan k ds
                        Char c      -> let k` = k+1 in (SChar c (scan k` ds))
                        Text l s    -> let k` = k+l in (SText l s (scan k` ds))
                        Line _      -> SLine 0 (scan 0 ds)
                        Cat x y     -> scan k [x:y:ds]
                        Nest j x    -> scan k [x:ds]
                        Union x y   -> scan k [y:ds]
                        Column f    -> scan k [f k:ds]
                        Nesting f   -> scan k [f 0:ds]

/* -----------------------------------------------------------
 * Displayers: display
 * ----------------------------------------------------------- */

display :: !SimpleDoc -> String
display sdoc = display` sdoc (createArray (displaySize sdoc) '\0') 0
where
	display` 	SEmpty			dst	offset	= dst
	display`	(SChar c x)		dst	offset	= display` x {dst & [offset] = c } (offset + 1)
	display`	(SText l s x)	dst	offset	= display` x (copyChars offset 0 (size s) s dst) (offset + size s)
	display`	(SLine i x)		dst	offset	= display` x (copyChars offset 0 (i+1) {spaceString (i+1) & [0] = '\n'} dst) (offset + i + 1)

	copyChars offset i num src dst
	| i == num		= dst
	| otherwise		= copyChars offset (inc i) num src {dst & [offset + i] = src.[i]}

	displaySize SEmpty             = 0
	displaySize (SChar c x)        = 1 + displaySize x
	displaySize (SText l s x)      = size s + displaySize x
	displaySize (SLine i x)        = 1 + i + displaySize x

spaceString :: Int -> *{#Char}
spaceString i = createArray i ' '
