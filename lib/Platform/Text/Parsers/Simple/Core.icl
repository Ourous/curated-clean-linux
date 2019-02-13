implementation module Text.Parsers.Simple.Core

import StdEnv

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Func
import Data.Functor
import Data.List

:: PCont t a :== [t] -> ([(a, [t])], [Error])
:: Parser t a = Parser (PCont t a)

instance Functor (Parser t) where
  fmap f p = pMap f p

instance pure (Parser t)
where
	pure x    = pYield x

instance <*> (Parser t)
where
	(<*>) l r = ap l r

instance *> (Parser t)
instance <* (Parser t)

instance Alternative (Parser t) where
  empty     = pFail
  (<|>) l r = pChoice l r

instance Monad (Parser t) where
  bind l r = pBind l r

instance MonadPlus (Parser t) where
  mzero = pFail
  mplus l r = l <|> r

parse :: !(Parser t a) [t] -> Either [Error] a
parse p ts
  = case runParser p ts of
      (rs, []) -> case [r \\ (r, []) <- rs] of
                    [r : _] -> Right r
                    _       -> Left ["No parse results with a fully consumed input."]
      (_, es)  -> Left es

runParser :: !(Parser t a) [t] -> ([(a, [t])], [Error])
runParser (Parser f) xs = f xs

pFail :: Parser t a
pFail = Parser (\_ -> ([], []))

pError :: Error -> Parser t a
pError err = Parser (\_ -> ([], [err]))

(@!) infixr 4 :: (Parser t a) Error -> Parser t a
(@!) p err = p <<|> pError err

pYield :: a -> Parser t a
pYield x = Parser (\input -> ([(x, input)], []))

pSatisfy :: (t -> Bool) -> Parser t t
pSatisfy pred = Parser pSatisfy`
  where
  pSatisfy` [token : input]
    | pred token = ([(token, input)], [])
  pSatisfy` _ = ([], [])

pPeek :: Parser t [t]
pPeek = Parser \input -> ([(input, input)], [])

pMap :: (a -> b) (Parser t a) -> Parser t b
pMap f p = pure f <*> p

pChoice :: (Parser t a) (Parser t a) -> Parser t a
pChoice pl pr = Parser choice`
  where
  choice` input
    = case (runParser pl input, runParser pr input) of
        ((rs1, es1), (rs2, es2)) -> (rs1 ++ rs2, es1 ++ es2)

pAp :: (Parser t (a -> b)) (Parser t a) -> Parser t b
pAp pf pa
  =         pf
  >>= \f -> pa
  >>= \x -> pure (f x)

pBind :: (Parser t a) (a -> Parser t b) -> Parser t b
pBind pl f = Parser (bind pl)
  where
  bind (Parser p) input
    # (rs1, es1) = p input
    # (rs`, es`) = unzip [  ((res2, input2), es1 ++ es2)
                         \\ (res1, input1) <- rs1
                         ,  let (rs2, es2) = runParser (f res1) input1
                         ,  (res2, input2) <- rs2
                         ]
    = (rs`, flatten es`)

(<<|>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a
(<<|>) pl pr = Parser pbl
  where
  pbl input = case runParser pl input of
                ([], _) -> runParser pr input
                res     -> res

(<|>>) infixr 4 :: (Parser t a) (Parser t a) -> Parser t a
(<|>>) pl pr = pr <<|> pl

(<:>) infixr 6 :: !(Parser s r) (Parser s [r]) -> Parser s [r]
(<:>) p1 p2 = (\r rs -> [r : rs]) <$> p1 <*> p2

pMany :: (Parser s r) -> Parser s [r]
pMany p = pSome p <<|> pure []

pSome :: !(Parser s r) -> Parser s [r]
pSome p = p <:> pMany p

pOptional :: !(Parser s r) (r -> Parser s r) -> Parser s r
pOptional p1 p2 = p1 >>= \r -> p2 r <<|> pure r

pOneOf :: [t] -> Parser t t | == t
pOneOf ts = pSatisfy (\x -> elem x ts)

pChainl :: !(Parser t a) (Parser t (a a -> a)) a -> Parser t a
pChainl p op a = pChainl1 p op <|> pure a

pChainl1 :: !(Parser t a) (Parser t (a a -> a)) -> Parser t a
pChainl1 p op = p >>= \a -> rest a
  where
  rest a = (op >>= \f -> p >>= \b -> rest (f a b))
       <|> pure a

pToken :: t -> Parser t t | == t
pToken c = pSatisfy (\x -> c == x)

pSepBy :: !(Parser t a) (Parser t s) -> Parser t [a]
pSepBy pa psep = pSepBy1 pa psep <|> pure []

pSepBy1 :: !(Parser t a) (Parser t s) -> Parser t [a]
pSepBy1 pa psep = pa <:> pMany (psep >>| pa)

pList :: ![Parser t a] -> Parser t [a]
pList xs = sequence xs
