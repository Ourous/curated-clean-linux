implementation module iTasks.Extensions.Collection

import iTasks
/*
* General purpose management task for a collection of data
*/
manageCollection :: !String (c -> i) (Shared sds [c]) -> Task (Maybe i) | iTask c & iTask i & RWShared sds
manageCollection itemname identify collection
	= manageCollectionWith selectItem (\s1 s2 i -> (Title ("Details of " +++ itemname)) @>> viewItem s1 s2 i)
		[OnAction ActionNew (always ((Title ("Add " +++ itemname)) @>> addItem collection identify))
		,OnAction ActionEdit (hasValue (\v -> Title ("Edit " +++ itemname) @>> (editItem collection (itemShare identify) identify v)))
		,OnAction ActionDelete (hasValue (\v -> Title ("Delete " +++ itemname) @>> Hint ("Are you sure you want to delete the following " +++ itemname +++ "?") @>> (deleteItem collection (itemShare identify) identify v)))
		]
		identify
		(itemShare identify)
		collection
/*
* Customizable management task for a collection of data
*/
manageCollectionWith ::
	((Shared sdsc [c]) (c -> i) -> Task i)											//Make selection
	((Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (Maybe i) -> Task a)		//Use selection
	[TaskCont i (Task (Maybe i))]												//Actions
	(c -> i)																	//Identification function
	((Shared sdsc [c]) i -> Shared sdss (Maybe c))						    //Item share function
	(Shared sdsc [c])																//Shared collection
	-> Task (Maybe i) | iTask c & iTask i & iTask a & RWShared sdsc & RWShared sdss
manageCollectionWith makeSelection useSelection selectionActions identify itemShare collection
	=	feedForward (makeSelection collection identify)
		( \mbSel -> 
			forever (
				whileUnchanged mbSel (\sel -> useSelection collection itemShare sel @ const sel @? onlyJust >>* selectionActions)
			)
		)
where
	onlyJust (Value (Just x) s)	= Value x s
	onlyJust _					= NoValue

itemShare :: (c -> i) (Shared sds [c]) i -> Shared SDSLens (Maybe c) | iTask i & iTask c & RWShared sds
itemShare identify collection i = mapReadWrite (toItem,fromItem) Nothing collection
where
	toItem l	= case [c \\ c <- l | identify c === i] of
		[c]		= Just c
		_		= Nothing
	
	fromItem Nothing l 		= Just l
	fromItem (Just c`) l	= Just [if (identify c === i) c` c \\ c <- l]

selectItem :: (Shared sds [c]) (c -> i) -> Task i | iTask c & iTask i & RWShared sds
selectItem collection identify
	=	enterChoiceWithSharedAs [ChooseFromGrid id] collection identify

viewItem :: (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (Maybe i) -> Task (Maybe i) | iTask c & iTask i & RWShared sdsc & RWShared sdss
viewItem collection itemShare Nothing	= viewInformation [] "Make a selection first..." @ const Nothing
viewItem collection itemShare (Just i)	= viewSharedInformation [] (itemShare collection i) @ const (Just i)

addItem :: (Shared sds [c]) (c -> i) -> Task (Maybe i) | iTask i & iTask c & RWShared sds
addItem collection identify
	=	enterInformation []
	>>*	[OnAction ActionCancel (always (return Nothing))
		,OnAction ActionOk (hasValue (\item -> upd (\l -> l ++ [item]) collection >-| return (Just (identify item))))
		]

editItem :: (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (c -> i) i -> Task (Maybe i) | iTask c & iTask i & RWShared sdsc & RWShared sdss
editItem collection itemShare identify i
	=	get (itemShare collection i)
	>>- \mbItem -> case mbItem of
			Nothing		= (return Nothing)
			(Just item)	=	updateInformation [] item
						>>*	[OnAction ActionCancel (always (return Nothing))
							,OnAction ActionOk (hasValue (\item` -> 
													upd (\l -> [if (identify c === i) item` c \\ c <- l ]) collection
													>-| return (Just i)
												 ))
							]

deleteItem :: (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (c -> i) i -> Task (Maybe i) | iTask c & iTask i & RWShared sdsc & RWShared sdss
deleteItem collection itemShare identify i
	=	viewSharedInformation [] (itemShare collection i)
	>>*	[OnAction ActionNo (always (return Nothing))
		,OnAction ActionYes (always (upd (\l -> [c \\ c <- l | identify c =!= i]) collection >-| return Nothing))
		]


