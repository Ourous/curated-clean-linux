implementation module iTasks.WF.Combinators.Common
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators
*/
import StdBool, StdList,StdOrdList, StdTuple, StdGeneric, StdMisc, StdInt, StdClass, StdString
import Text, System.Time, Data.Maybe, Data.Tuple, Data.List, Data.Either, Data.Functor, Data.GenEq, Text.GenJSON
import iTasks.Internal.Util
from StdFunc			import id, const, o
from iTasks.SDS.Sources.Core import randomInt
from iTasks.SDS.Sources.System import currentDateTime, topLevelTasks
import iTasks.SDS.Combinators.Common
from iTasks.Internal.TaskState		import :: TaskTree(..), :: DeferredJSON
from iTasks.Internal.TaskEval         import :: TaskTime
import qualified Data.Map as DM
from iTasks.Extensions.DateTime import waitForTimer
from iTasks.UI.Definition import :: UIType(UILoader)

import iTasks.WF.Tasks.Core
import iTasks.WF.Tasks.SDS
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.SDS
import iTasks.WF.Combinators.Core, iTasks.WF.Combinators.Tune, iTasks.WF.Combinators.Overloaded
import iTasks.UI.Editor
import iTasks.UI.Editor.Controls
import iTasks.UI.Prompt
import iTasks.UI.Tune
import iTasks.UI.Layout
import iTasks.UI.Layout.Common, iTasks.UI.Layout.Default

(>>*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task b | iTask a & iTask b
(>>*) task steps = step task (const Nothing) steps

tbind :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
tbind taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf), OnValue (ifStable taskbf)]

(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>!) taska taskbf = step taska (const Nothing) [OnAction ActionContinue (hasValue taskbf)]

(>>-) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>-) taska taskbf = step taska (const Nothing) [OnValue (ifStable taskbf)]

(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>~) taska taskbf = step taska (const Nothing) [OnValue (hasValue taskbf)]

(>>^) infixl 1 :: !(Task a) (Task b) -> Task a | iTask a & iTask b
(>>^) taska taskb = taska >>= \x -> taskb >>| return x

(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b
(@?) task f = transform f task

(@) infixl 1 :: !(Task a) !(a -> b) -> Task b
(@) task f = transform (fmap f) task

(@!) infixl 1 :: !(Task a) !b -> Task b
(@!) task b = transform (fmap (const b)) task

try :: !(Task a) (e -> Task a) -> Task a | iTask a & iTask, toString e
try task handler = step task id [OnValue (ifStable return), OnException handler]

catchAll :: !(Task a) (String -> Task a) -> Task a | iTask a
catchAll task handler = step task id [OnValue (ifStable return), OnAllExceptions handler]

(>^*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b
(>^*) task steps = sideStep task steps

sideStep :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b
sideStep ta conts = parallel [(Embedded,const ta)] (map pcont conts) @ (map snd) @? firstRes
where
    pcont (OnValue tfun)         = OnValue (vfun tfun)
    pcont (OnAction action tfun) = OnAction action (vfun tfun)
    pcont (OnException tfun)     = OnException (efun tfun)
    pcont (OnAllExceptions tfun) = OnAllExceptions (efun tfun)

    vfun tfun (Value [(t,v):_] _) = fmap (\t -> (Embedded,removeWhenStable t)) (tfun v)
    efun tfun e = (\t -> (Embedded,removeWhenStable t)) (tfun e)

    firstRes (Value [v:_] _) = v

removeWhenStable :: (Task a) -> ParallelTask b | iTask a & iTask b
removeWhenStable t
    = \l -> t >>* [OnValue (ifStable (const (get (taskListSelfId l) >>- \id -> removeTask id l @? const NoValue)))]

//Helper functions for projections
projectJust :: (Maybe a) r -> Maybe (Maybe a)
projectJust mba _ = Just mba

justdo :: !(Task (Maybe a)) -> Task a | iTask a
justdo task
= task >>= \r -> case r of
	Just x	= return x
	Nothing	= throw ("The task returned nothing.")

sequence :: !String ![Task a]  -> Task [a] | iTask a
sequence _ tasks = seqTasks tasks
where
	seqTasks []		= return []
	seqTasks [t:ts]	= t >>- \a -> seqTasks ts >>- \as -> return [a:as]

foreverStIf :: (a -> Bool) a !(a -> Task a) -> Task a | iTask a
foreverStIf pred st t = parallel [(Embedded, par st Nothing)] []
	>>- \tv->case tv of
		[(_, Value i True)] = treturn i
		_ = throw "Corrupt parallel in foreverStIf"
where
	par st (Just tid) tlist = removeTask tid tlist >>- \_->par st Nothing tlist
	par st Nothing tlist
		| not (pred st) = treturn st
		= get (sdsFocus {gDefault{|*|} & onlySelf=True} tlist)
			>>- \(_, [{TaskListItem|taskId}])->t st
			>>- \st`->appendTask Embedded (par st` (Just taskId)) tlist @? const NoValue

(-||-) infixr 3 :: !(Task a) !(Task a) -> (Task a) | iTask a
(-||-) taska taskb = anyTask [taska,taskb]

(||-) infixr 3 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(||-) taska taskb
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res	(Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(-||) infixl 3 :: !(Task a) !(Task b) -> Task a | iTask a & iTask b
(-||) taska taskb
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res	(Value [(_,Value (Left a) s),_] _)	= Value a s
	res _									= NoValue
	
(-&&-) infixr 4 :: !(Task a) !(Task b) -> (Task (a,b)) | iTask a & iTask b
(-&&-) taska taskb
	= parallel
		[(Embedded, \_ -> taska @ Left),(Embedded, \_ -> taskb @ Right)] [] @? res
where
	res (Value [(_,Value (Left a) sa),(_,Value (Right b) sb)] _)	= Value (a,b) (sa && sb)
	res _														    = NoValue

feedForward :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
feedForward taska taskbf = parallel
	[(Embedded, \s -> taska @ Left)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (sdsFocus (Left 0) (taskListItemValue s)))) @ Right)
	] [] @? res
where
	prj (Value (Left a) _)  = Just a
	prj _					= Nothing
	
	res (Value [_,(_,Value (Right b) s)] _)	= Value b s
	res _									= NoValue
	
(>&>) infixl 1  :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task b | iTask a & iTask b
(>&>) taska taskbf = feedForward taska taskbf
				
feedSideways :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | iTask a & iTask b
feedSideways taska taskbf = parallel
    [(Embedded, \s -> taska)
	,(Embedded, \s -> taskbf (mapRead prj (toReadOnly (sdsFocus (Left 0) (taskListItemValue s)))) @? const NoValue)
    ] [] @? res
where
	prj (Value a _)	= Just a
	prj _			= Nothing

    res (Value [(_,v):_] _) = v
    res _                   = NoValue

(>&^) infixl 1 :: (Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task a | iTask a & iTask b
(>&^) taska taskbf = feedSideways taska taskbf

anyTask :: ![Task a] -> Task a | iTask a
anyTask tasks
	= parallel [(Embedded,const t) \\ t <- tasks] [] @? res
where
	res (Value l _) = let sl = sortBy (\a b -> fst a > fst b) l in
                        hd ([v \\ (_,v=:(Value _ True)) <- sl] ++ [v \\ (_,v=:(Value _ False)) <- sl] ++ [NoValue])
	res _			= NoValue

allTasks :: ![Task a] -> Task [a] | iTask a
allTasks tasks
	= parallel
		[(Embedded,const t) \\ t <- tasks] [] @? res
where
	res (Value l _)	= Value [v \\ (_,Value v _) <- l] (foldl allStable True l)

    allStable cur (_,Value _ s) = cur && s
    allStable cur _             = False
				
eitherTask :: !(Task a) !(Task b) -> Task (Either a b) | iTask a & iTask b
eitherTask taska taskb = (taska @ Left) -||- (taskb @ Right)

randomChoice :: ![a] -> Task a | iTask a
randomChoice [] = throw "Cannot make a choice from an empty list"
randomChoice list = get randomInt >>= \i -> return (list !! ((abs i) rem (length list)))

//We throw an exception when the share changes to make sure that the right hand side of
//the -||- combinator is not evaluated anymore (because it was created from the 'old' share value)
whileUnchanged :: !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask b
whileUnchanged share task
	= 	( (get share >>- \val ->
            try (
					((watch share >>* [OnValue (ifValue ((=!=) val) (\_ -> throw ShareChanged))])
					  -||- (task val @ Just)
					 ) <<@ ApplyLayout (sequenceLayouts [removeSubUIs (SelectByPath [0]), unwrapUI])) 
                (\ShareChanged -> (return Nothing) )
          ) <! isJust
        )
	@?	onlyJust

:: ShareChanged = ShareChanged
derive class iTask ShareChanged
instance toString ShareChanged where toString ShareChanged = "Share changed exception"

onlyJust (Value (Just x) s) = Value x s
onlyJust _                  = NoValue

whileUnchangedWith :: !(r r -> Bool) !(ReadWriteShared r w) (r -> Task b) -> Task b | iTask r & iTask w & iTask b
whileUnchangedWith eq share task
	= 	((get share >>= \val -> (wait () (eq val) share <<@ NoUserInterface @ const Nothing) -||- (task val @ Just)) <! isJust)
	@?	onlyJust

withSelection :: (Task c) (a -> Task b) (ReadOnlyShared (Maybe a)) -> Task b | iTask a & iTask b & iTask c
withSelection def tfun s = whileUnchanged s (maybe (def @? const NoValue) tfun)

appendTopLevelTask :: !TaskAttributes !Bool !(Task a) -> Task TaskId | iTask a
appendTopLevelTask attr evalDirect task = appendTask (Detached attr evalDirect) (\_ -> task <<@ ApplyLayout defaultSessionLayout @! ()) topLevelTasks

compute :: !String a -> Task a | iTask a
compute s a = enterInformation s [EnterUsing id ed] >>~ \_->return a
where
	ed :: Editor Bool
	ed = fieldComponent UILoader

valToMaybe :: (TaskValue a) -> Maybe a
valToMaybe (Value v _)  = Just v
valToMaybe NoValue		= Nothing

always :: b (TaskValue a) -> Maybe b
always taskb val = Just taskb

never :: b (TaskValue a) -> Maybe b
never taskb val	= Nothing

ifValue :: (a -> Bool) (a -> b) (TaskValue a) -> Maybe b
ifValue pred ataskb (Value a _) 
    | pred a 	= Just (ataskb a)
    | otherwise = Nothing
ifValue _ _ _ = Nothing

hasValue	:: (a -> b) (TaskValue a) -> Maybe b
hasValue ataskb (Value a _) = Just (ataskb a)
hasValue _ _ = Nothing

ifCond :: Bool b (TaskValue a) -> Maybe b
ifCond True taskb _ = Just taskb
ifCond False taskb _ = Nothing

ifStable :: (a -> b) (TaskValue a) -> Maybe b
ifStable ataskb (Value a True) = Just (ataskb a)
ifStable _ _ 				   = Nothing

ifUnstable :: (a -> b) (TaskValue a) -> Maybe b
ifUnstable ataskb (Value a False) = Just (ataskb a)
ifUnstable _ _ 				   = Nothing

withoutValue :: (Maybe b) (TaskValue a) -> Maybe b
withoutValue b NoValue = b
withoutValue _ _       = Nothing

withValue :: (a -> Maybe b) (TaskValue a) -> Maybe b
withValue a2mb (Value tv _) = a2mb tv
withValue _    _            = Nothing

withStable :: (a -> Maybe b) (TaskValue a) -> Maybe b
withStable a2mb (Value tv True) = a2mb tv
withStable _    _               = Nothing

withUnstable :: (a -> Maybe b) (TaskValue a) -> Maybe b
withUnstable a2mb (Value tv False) = a2mb tv
withUnstable _    _                = Nothing

tvHd :: (TaskValue [a]) -> TaskValue a
tvHd (Value [x] s) = Value x s
tvHd _             = NoValue

tvFst :: (TaskValue (a,b)) -> TaskValue a
tvFst tv = fmap fst tv

tvSnd :: (TaskValue (a,b)) -> TaskValue b
tvSnd tv = fmap snd tv

tvFromMaybe :: (TaskValue (Maybe a)) -> TaskValue a
tvFromMaybe (Value (Just a) s) = Value a s
tvFromMaybe _                  = NoValue

tvToMaybe :: (TaskValue a) -> TaskValue (Maybe a)
tvToMaybe (Value a s) = Value (Just a) s
tvToMaybe NoValue     = Value Nothing False
