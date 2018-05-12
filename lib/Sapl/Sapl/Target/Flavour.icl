implementation module Sapl.Target.Flavour

import StdList, StdFunc, StdArray
import Data.Maybe, Data.Functor, Text.GenJSON, Text.StringAppender
import Sapl.SaplParser

from Data.Set import qualified fromList, member
from Data.Set import :: Set
import qualified Data.Map as DM
           
derive JSONEncode FlavourRep, BIFRep
derive JSONDecode FlavourRep, BIFRep

:: X = StringPart !String | Binding !Int | ForceBinding !Int

extractBindings str = extractBindings` (fromString str) []
where
	extractBindings` [] [] = []
	extractBindings` [] ss = [StringPart (revstr ss)]
	extractBindings` [':','!',x,':':xs] ss 
		| isDigit x = case ss of
						[] = [ForceBinding (digitToInt x):extractBindings` xs []]
						ss = [StringPart (revstr ss),ForceBinding (digitToInt x):extractBindings` xs []]
	extractBindings` [':',x,':':xs] ss 
		| isDigit x = case ss of
						[] = [Binding (digitToInt x):extractBindings` xs []]
						ss = [StringPart (revstr ss),Binding (digitToInt x):extractBindings` xs []]
	extractBindings` [x:xs] ss = extractBindings` xs [x:ss]
	
	revstr ss = toString (reverse ss)

toFlavour :: !String -> Maybe Flavour
toFlavour str = fmap fromFlavourRep (fromJSON (fromString str))

fromFlavourRep :: !FlavourRep -> Flavour
fromFlavourRep rep 
		= { fun_prefix = rep.FlavourRep.fun_prefix
		  , options = 'Data.Set'.fromList rep.FlavourRep.options
		  , builtInFunctions = builtInFunctions
		  , inlineFunctions = inlineFunctions}  
where
		bifs = filter (\f -> isJust f.ext_fun) rep.bifs
		bifList = map (\f -> (f.sapl_fun, (fromJust f.ext_fun, f.BIFRep.arity))) bifs
		builtInFunctions = 'DM'.fromList bifList

		ifs = filter (\f -> isJust f.inline_exp) rep.bifs
		ifList = map toInlineFunDef ifs
		inlineFunctions = 'DM'.fromList ifList
			
		toInlineFunDef f 
				=	(f.sapl_fun,  { InlineFunDef 
								  |	fun			= toInlineFun
								  , arity 		= f.BIFRep.arity
								  , strictness  = collectStrictnessInfo (createArray f.BIFRep.arity '0') bindings
								  ,	data_cons 	= maybe False id f.BIFRep.data_cons })	
		where
			toInlineFun = inst
			where
				inst eval feval args a = foldl app a bindings
				where
					app a (StringPart str) = a <++ str
					app a (Binding x) = eval (args!!(x-1)) a
					app a (ForceBinding x) = feval (args!!(x-1)) a			 
					  
			template = fromJust f.inline_exp							  
			bindings = extractBindings template				  
			
			collectStrictnessInfo arr [] = arr
			collectStrictnessInfo arr [ForceBinding i:bs] 
				# arr = if (i<=size arr) (update arr (i-1) '1') arr
				= collectStrictnessInfo arr bs
			collectStrictnessInfo arr [_:bs] = collectStrictnessInfo arr bs

isSet :: !Flavour !String -> Bool
isSet f opt = 'Data.Set'.member opt f.Flavour.options



			
