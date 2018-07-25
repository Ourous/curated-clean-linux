implementation module Clean.Types.Parse

from StdFunc import o
import StdList
import StdString
import StdTuple

import Clean.Types
import Clean.Types.Util
import Data.Either
import Data.GenEq
import Data.Maybe
from Data.Func import $
from Data.List import instance Functor [], instance Applicative [], instance Alternative []
from Text import class Text(concat), instance Text String
import Data.Functor
import Control.Applicative
import Control.Monad
from Text.Parsers.Simple.Core import :: Parser, :: Error,
	instance Functor (Parser t), instance Applicative (Parser t),
	instance Alternative (Parser t), instance Monad (Parser t),
	parse, pToken, pSepBy, pList, pSatisfy

(|<<) infixl 1 :: !(m a) !(m b) -> m a | Monad m
(|<<) ma mb = ma >>= \a -> mb >>= \_ -> pure a

derive gEq Token
instance == Token where == a b = a === b

:: Token
	= TIdent String            // UpperCaseId or FunnyId
	| TVar String              // LowerCaseId

	| TArrow                   // ->
	| TComma                   // ,
	| TStar                    // *
	| TAnonymous               // .
	| TUnboxed                 // #
	| TStrict                  // !
	| TColon                   // :
	| TUniversalQuantifier     // A.
	| TPipe                    // |
	| TAmpersand               // &
	| TLtEq                    // <=

	| TParenOpen | TParenClose // ( )
	| TBrackOpen | TBrackClose // [ ]
	| TBraceOpen | TBraceClose // { }

isTIdent (TIdent _) = True; isTIdent _ = False
isTVar   (TVar   _) = True; isTVar   _ = False

tokenize :: ([Char] -> Maybe [Token])
tokenize = fmap reverse o tkz []
where
	tkz :: ![Token] ![Char] -> Maybe [Token]
	tkz tks [] = Just tks
	tkz tks ['-':'>':cs] = tkz [TArrow:tks]               cs
	tkz tks [',':cs]     = tkz [TComma:tks]               cs
	tkz tks ['*':cs]     = tkz [TStar:tks]                cs
	tkz tks ['.':cs]     = tkz [TAnonymous:tks]           cs
	tkz tks ['#':cs]     = tkz [TUnboxed:tks]             cs
	tkz tks ['!':cs]     = tkz [TStrict:tks]              cs
	tkz tks ['(':cs]     = tkz [TParenOpen:tks]           cs
	tkz tks [')':cs]     = tkz [TParenClose:tks]          cs
	tkz tks ['[':cs]     = tkz [TBrackOpen:tks]           cs
	tkz tks [']':cs]     = tkz [TBrackClose:tks]          cs
	tkz tks ['{':cs]     = tkz [TBraceOpen:tks]           cs
	tkz tks ['}':cs]     = tkz [TBraceClose:tks]          cs
	tkz tks ['A':'.':cs] = tkz [TUniversalQuantifier:tks] cs
	tkz tks [':':cs]     = tkz [TColon:tks]               cs
	tkz tks ['|':cs]     = tkz [TPipe:tks]                cs
	tkz tks ['&':cs]     = tkz [TAmpersand:tks]           cs
	tkz tks ['<':'=':cs] = tkz [TLtEq:tks]                cs
	tkz tks [c:cs]
	| isSpace c = tkz tks cs
	| isUpper c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isIdentChar cs
	| isFunny c = tkz [TIdent $ toString [c:id]:tks] cs`
		with (id, cs`) = span isFunny cs
	| isLower c = tkz [TVar $ toString [c:var]:tks] cs`
		with (var, cs`) = span isIdentChar cs
	tkz _ _ = Nothing

	isIdentChar :: !Char -> Bool
	isIdentChar c = any (\f->f c) [isLower, isUpper, isDigit, (==)'_', (==) '`']

	isFunny :: !Char -> Bool
	isFunny c = isMember c ['~@#$%^?!+-*<>\\/|&=:']

type :: Parser Token Type
type = liftM3 Func (some argtype) (pToken TArrow >>| type) optContext
	<|> liftM2 Cons cons (some argtype)
	<|> liftM2 Type ident (many argtype)
	<|> liftM3 Forall
		(pToken TUniversalQuantifier >>| some (Var <$> var) |<< pToken TColon)
		type
		optContext
	<|> argtype
where
	argtype :: Parser Token Type
	argtype = (pList [pToken TParenOpen, pToken TParenClose] $> Type "_Unit" [])
		<|> parenthised type
		<|> liftM (\t -> Type t []) ident
		<|> liftM Uniq uniq
		<|> liftM (\t -> Type "_#Array" [t]) (braced  (pToken TUnboxed >>| type))
		<|> liftM (\t -> Type "_!Array" [t]) (braced  (pToken TStrict  >>| type))
		<|> liftM (\t -> Type "_Array"  [t]) (braced  type)
		<|> liftM (\t -> Type "_#List!" [t]) (bracked (pToken TUnboxed >>| type |<< pToken TStrict))
		<|> liftM (\t -> Type "_!List!" [t]) (bracked (pToken TStrict  >>| type |<< pToken TStrict))
		<|> liftM (\t -> Type "_#List"  [t]) (bracked (pToken TUnboxed >>| type))
		<|> liftM (\t -> Type "_!List"  [t]) (bracked (pToken TStrict  >>| type))
		<|> liftM (\t -> Type "_List!"  [t]) (bracked (type |<< pToken TStrict))
		<|> liftM (\t -> Type "_List"   [t]) (bracked type)
		<|> liftM (\ts -> Type ("_Tuple" +++ toString (length ts)) ts)
			(parenthised (pSepBy type (pToken TComma)))
		<|> (pToken TStrict >>| argtype)           // ! ignored for now
		<|> (pToken TAnonymous >>| argtype)        // . ignored for now
		<|> (unqvar >>| pToken TColon >>| argtype) // u: & friends ignored for now
		<|> liftM Var var

	ident :: Parser Token String
	ident = (\(TIdent id)->id) <$> pSatisfy isTIdent

	var :: Parser Token TypeVar
	var = (\(TVar var)->var) <$> pSatisfy isTVar
	cons = var
	unqvar = var

	uniq :: Parser Token Type
	uniq = pToken TStar >>| argtype

	optContext :: Parser Token TypeContext
	optContext = liftM2 (++) (context <|> pure []) (uniquenessEqualities <|> pure [])

	context :: Parser Token TypeContext
	context = pToken TPipe >>| flatten <$> pSepBy context` (pToken TAmpersand)
	where
		context` :: Parser Token TypeContext
		context` = pSepBy classOrGeneric (pToken TComma) >>= \restrictions ->
			some argtype >>= \ts ->
			mapM (flip ($) ts) restrictions

		classOrGeneric :: Parser Token ([Type] -> Parser Token TypeRestriction)
		classOrGeneric = className >>= \name ->
			optional (braced $ piped skipKind) >>= \kind ->
			case kind of
				Nothing -> pure $ pure o Instance name
				Just _  -> pure $ deriv name
		where
			deriv :: !String ![Type] -> Parser Token TypeRestriction
			deriv d [t] = pure $ Derivation d t
			deriv _ _   = empty

		className :: Parser Token String
		className = ident <|> var

		skipKind :: Parser Token [Token]
		skipKind = some $ pSatisfy \t -> case t of
			TStar       -> True
			TArrow      -> True
			TParenOpen  -> True
			TParenClose -> True
			_           -> False

	uniquenessEqualities :: Parser Token TypeContext
	uniquenessEqualities = pToken TComma >>| bracked (pSepBy inequality (pToken TComma)) $> []
	where
		inequality = unqvar >>| pToken TLtEq >>| unqvar

	parenthised p = pToken TParenOpen >>| p |<< pToken TParenClose
	braced p = pToken TBraceOpen >>| p |<< pToken TBraceClose
	bracked p = pToken TBrackOpen >>| p |<< pToken TBrackClose
	piped p = pToken TPipe >>| p |<< pToken TPipe

parseType :: ![Char] -> Maybe Type
parseType cs
# mbTokens = tokenize cs
| isNothing mbTokens = Nothing
= case parse type (fromJust mbTokens) of
	Right t -> Just t
	_       -> Nothing
