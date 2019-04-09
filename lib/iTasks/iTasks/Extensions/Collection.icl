implementation module iTasks.Extensions.Collection

import iTasks
/*
* General purpose management task for a collection of data
*/
manageCollection :: !String (c -> i) (Shared sds [c]) -> Task (Maybe i) | iTask c & iTask i & RWShared sds
manageCollection itemname identify collection
	= manageCollectionWith (selectItem ()) (viewItem (Title ("Details of " +++ itemname)))
		[OnAction ActionNew (always (addItem (Title ("Add " +++ itemname)) collection identify))
		,OnAction ActionEdit (hasValue (editItem ("Edit " +++ itemname) collection (itemShare identify) identify))
		,OnAction ActionDelete (hasValue (deleteItem ("Delete " +++ itemname,"Are you sure you want to delete the following " +++ itemname +++ "?") collection (itemShare identify) identify))
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

selectItem :: !d (Shared sds [c]) (c -> i) -> Task i | toPrompt d & iTask c & iTask i & RWShared sds
selectItem desc collection identify
	=	enterChoiceWithSharedAs desc [ChooseFromGrid id] collection identify

viewItem :: !d (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (Maybe i) -> Task (Maybe i) | toPrompt d & iTask c & iTask i & RWShared sdsc & RWShared sdss
viewItem desc collection itemShare Nothing	= viewInformation desc [] "Make a selection first..." @ const Nothing
viewItem desc collection itemShare (Just i)	= viewSharedInformation desc [] (itemShare collection i) @ const (Just i)

addItem :: !d (Shared sds [c]) (c -> i) -> Task (Maybe i) | toPrompt d & iTask i & iTask c & RWShared sds
addItem desc collection identify
	=	enterInformation desc []
	>>*	[OnAction ActionCancel (always (return Nothing))
		,OnAction ActionOk (hasValue (\item -> upd (\l -> l ++ [item]) collection >>| return (Just (identify item))))
		]

editItem :: !d (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (c -> i) i -> Task (Maybe i) | toPrompt d & iTask c & iTask i & RWShared sdsc & RWShared sdss
editItem desc collection itemShare identify i
	=	get (itemShare collection i)
	>>= \mbItem -> case mbItem of
			Nothing		= (return Nothing)
			(Just item)	=	updateInformation desc [] item
						>>*	[OnAction ActionCancel (always (return Nothing))
							,OnAction ActionOk (hasValue (\item` -> 
													upd (\l -> [if (identify c === i) item` c \\ c <- l ]) collection
													>>| return (Just i)
												 ))
							]

deleteItem :: !d (Shared sdsc [c]) ((Shared sdsc [c]) i -> Shared sdss (Maybe c)) (c -> i) i -> Task (Maybe i) | toPrompt d & iTask c & iTask i & RWShared sdsc & RWShared sdss
deleteItem desc collection itemShare identify i
	=	viewSharedInformation desc [] (itemShare collection i)
	>>*	[OnAction ActionNo (always (return Nothing))
		,OnAction ActionYes (always (upd (\l -> [c \\ c <- l | identify c =!= i]) collection >>| return Nothing))
		]


