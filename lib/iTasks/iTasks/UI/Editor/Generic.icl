implementation module iTasks.UI.Editor.Generic

import StdMisc
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Editor.Controls
import iTasks.UI.Editor.Modifiers
import iTasks.UI.Editor.Common

import qualified Data.Map as DM
import StdArray, StdBool, StdFunc, StdList, Data.Maybe, StdString
import Text.GenJSON
import Text.Language
import System.Time
import Data.GenEq, Data.Func, Control.GenBimap, Data.Functor, Data.Tuple

generic gEditor a | gText a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor, MaybeError

gEditor{|UNIT|} = emptyEditorWithDefaultInEnterMode_ (\_ _ -> [JSONNull]) (\_ [JSONNull: json] -> (Just UNIT, json)) UNIT

gEditor{|RECORD of {grd_arity}|} {Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst=:{taskId, optional} = case mode of
		Enter
			| optional //Just show the ui to enable the remaining fields
				# (enableUI,enableSt) = genEnableUI taskId dp False
				= (Ok (uic UIRecord [enableUI], (False, optional), [enableSt]), vst)
			| otherwise
				= case exGenUI (pairPath grd_arity dp) Enter {VSt|vst & optional = False} of
					(Ok viz, vst)
						# (ui, childSts) = fromPairUI UIRecord grd_arity viz
						= (Ok (ui, (False, optional), childSts), vst)
					(Error e,vst) = (Error e,vst)
		Update (RECORD x)
			= case exGenUI (pairPath grd_arity dp) (Update x) {VSt|vst & optional = False} of
				(Ok viz,vst)
					# (UI type attr items, childSts) = fromPairUI UIRecord grd_arity viz
					//When optional we add a checkbox to clear the record
					| optional
						# (enableUI, enableSt) = genEnableUI taskId dp True
						= (Ok (UI type attr [enableUI:items], (False, optional), [enableSt: childSts]), vst)
					| otherwise
						= (Ok (UI type attr items, (False, optional), childSts),vst)
				(Error e,vst) = (Error e,vst)
		View  (RECORD x)
			= case exGenUI (pairPath grd_arity dp) (View x) {VSt|vst & optional = False} of
				(Ok viz,vst)
					# (ui, childSts) = fromPairUI UIRecord grd_arity viz
					= (Ok (ui, (True, optional), childSts),vst)
				(Error e,vst) = (Error e,vst)

	genEnableUI taskId dp enabled = (uia UICheckbox (editAttrs taskId (editorId dp) (Just (JSONBool enabled))),newLeafState)

	//Enabling an optional record
	onEdit dp ([],JSONArray [_, JSONBool True]) (viewMode, optional) [enableSt:childSts] vst
		| not optional
			= (Error "Enabling non-optional record",vst)
		//Create and add the fields
		= case exGenUI (pairPath grd_arity dp) Enter {vst & optional = False} of
			(Ok viz,vst)
				# (UI type attr items, childSts) = fromPairUI UIRecord grd_arity viz
				# change = ChangeUI [] [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# enableSt = LeafState {touched=True,state=JSONBool True}
				= (Ok (change, (viewMode, optional), [enableSt:childSts]), vst)
			(Error e,vst) = (Error e, vst)

	//Disabling an optional record
	onEdit dp ([],JSONArray [_, JSONBool False]) (viewMode, optional) [enableSt:childSts] vst
		| not optional
			= (Error "Disabling non-optional record",vst)
		//Remove all fields except the enable/disable checkbox
		# change = ChangeUI [] (repeatn grd_arity (1,RemoveChild))
		# enableSt = LeafState {touched=True,state=JSONBool False}
		= (Ok (change, (viewMode, optional), [enableSt:childSts]), vst)
	onEdit _ ([],e) _ _ vst
		= (Error $ "Unknown edit event for record" +++ toString e,vst)
	
	onEdit dp ([d:ds],e) (viewMode, optional) childSts vst
		| d >= grd_arity
			= (Error "Edit aimed at non-existent record field",vst)
		//When optional we need to adjust for the added checkbox, so we need to offset the record field index with one
		//In the generated UI and state (but not in the paths when targeting the edit!!).
		# idx = if optional (d + 1) d
		= case exOnEdit (pairPath grd_arity dp) (pairSelectPath d grd_arity ++ ds,e) (childSts !! idx) {VSt|vst & optional = False} of
			(Ok (change, childSt),vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(idx,ChangeChild change)]
				//Update the state
				# childSts = updateAt idx childSt childSts
				= (Ok (change, (viewMode, optional), childSts), vst)
			(Error e,vst) = (Error e, vst)
				
	onEdit _ val viewModeAndOpt childSts vst = (Ok (NoChange, viewModeAndOpt, childSts),vst)

	onRefresh dp (RECORD new) (viewMode, optional) childSts vst=:{VSt|taskId}
		| optional && not viewMode
			//Account for the extra state of the enable/disable checkbox
			# enableSt = hd childSts
			= case exOnRefresh (pairPath grd_arity dp) new (toPairState $ CompoundState JSONNull (tl childSts)) {VSt|vst & optional = False} of
				(Ok (change, childSt),vst)
					# change = fromPairChange 0 grd_arity change
					# fields = fromPairState grd_arity childSt
					//Adjust the change
					# change = case change of 
						NoChange = NoChange
						ChangeUI attrChanges itemChanges = ChangeUI attrChanges ([(i + 1,c) \\ (i,c) <- itemChanges])
						(ReplaceUI ui=:(UI type attr items))
							# (enableUI,_) = genEnableUI taskId dp True
							= ReplaceUI (UI type attr [enableUI:items])
					= (Ok (change, (viewMode, optional), [enableSt:fields]), vst)
				(Error e,vst)
					= (Error e, vst)
		| otherwise
			= case exOnRefresh (pairPath grd_arity dp) new (toPairState $ CompoundState JSONNull childSts) {VSt|vst & optional = False} of
				(Ok (change, childSt),vst)
					= (Ok (fromPairChange 0 grd_arity change, (viewMode, optional), fromPairState grd_arity childSt), vst)
				(Error e,vst)
					= (Error e, vst)

	valueFromState (_, False) childrenSts      = valueFromState` childrenSts
	valueFromState (_, True)  [_: childrenSts] = valueFromState` childrenSts
	valueFromState _          _                = Nothing

	valueFromState` childrenSts = mapMaybe RECORD $ exValueFromState $ toPairState $ CompoundState JSONNull childrenSts

gEditor{|FIELD of {gfd_name}|} {Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst = case exGenUI dp (mapEditMode (\(FIELD x) -> x) mode) vst of //Just add the field name as a label
		(Ok (UI type attr items, childSt),vst) = (Ok (UI type ('DM'.union attr (labelAttr gfd_name)) items, childSt),vst)
		(Error e,vst)                          = (Error e,vst)

	onEdit dp (tp,e) childSt vst         = exOnEdit dp (tp,e) childSt vst
	onRefresh dp (FIELD new) childSt vst = exOnRefresh dp new childSt vst
	valueFromState state                 = mapMaybe (\x -> FIELD x) $ exValueFromState state

/*
* For ADT's we need to deal with two cases:
* - There is only one constructor
* - There are multiple constructors
*/
gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ce=:{Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
	//Newtypes or ADTs just use the child editor
	| gtd_num_conses < 2
		= bijectEditorValue (\(OBJECT i)->i) (\i->OBJECT i) ce
	= withEditModeAttr $ compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	gcd_names   = [gcd_name  \\ {GenericConsDescriptor | gcd_name}  <- gtd_conses]  // preselect cons names   to circumvent cyclic dependencies
	gcd_arities = [gcd_arity \\ {GenericConsDescriptor | gcd_arity} <- gtd_conses]  // preselect cons arities to circumvent cyclic dependencies
	
	genUI dp mode vst=:{VSt|taskId,selectedConsIndex} = case mode of
		Enter
			//Only generate a UI to select the constructor
			# (consChooseUI, consChooseSt) = genConsChooseUI taskId dp gcd_names Nothing
			= (Ok (UI UIVarCons 'DM'.newMap [consChooseUI], False, [consChooseSt]),{vst & selectedConsIndex = selectedConsIndex})
		Update (OBJECT x)
			//Generate the ui for the current value
			= case exGenUI dp (Update x) vst of
				(Ok (consUI=:(UI UICons attr items), childSt),vst)
					//Add the UI to select the constructor and change the type to UIVarCons
					# (consChooseUI, consChooseSt) = genConsChooseUI taskId dp gcd_names (Just vst.selectedConsIndex)
					= (Ok (UI UIVarCons attr [consChooseUI:items], False, [consChooseSt, childSt])
							,{vst & selectedConsIndex = selectedConsIndex})
				(Error e,vst) = (Error e, vst)
		View (OBJECT x)
			= case exGenUI dp (View x) vst of
				(Ok (consUI=:(UI UICons attr items), childSt),vst)
					# (consViewUI,consViewSt) = genConsViewUI gcd_names vst.selectedConsIndex
					= (Ok (UI UIVarCons attr [consViewUI:items], True, [consViewSt, childSt])
						,{vst & selectedConsIndex = selectedConsIndex})
				(Error e,vst) = (Error e,vst)

	genConsChooseUI taskId dp gcd_names mbSelectedCons = (consChooseUI,consChooseSt)
	where
		consOptions = [JSONObject [("id",JSONInt i),("text",JSONString gcd_name)] \\ gcd_name <- gcd_names & i <- [0..]]
		consChooseUI = uia UIDropdown (choiceAttrs taskId (editorId dp) (maybe [] (\x -> [x]) mbSelectedCons) consOptions)
		consChooseSt = LeafState {touched=False,state=maybe JSONNull (\x -> JSONInt x) mbSelectedCons}

	genConsViewUI gcd_names selectedCons
		= (uia UITextView (valueAttr (JSONString (gcd_names !! selectedCons))), newLeafState)

	//Update is a constructor switch
	onEdit dp ([],JSONArray [JSONInt consIdx]) viewMode [LeafState {LeafState|touched,state}: _] vst=:{pathInEditMode}
		| consIdx < 0 || consIdx >= gtd_num_conses
			= (Error "Constructor selection out of bounds",vst)
		//Create a UI for the new constructor
		# (ui, vst) = exGenUI dp Enter {vst & pathInEditMode = consCreatePath consIdx gtd_num_conses}
		# vst = {vst & pathInEditMode = pathInEditMode}
		= case ui of
			Ok (UI UICons attr items, childSt)
				//Construct a UI change that does the following: 
				//1: If necessary remove the fields of the previously selected constructor
				# removals = case state of
					(JSONInt prevConsIdx) = repeatn (gcd_arities !! prevConsIdx) (1,RemoveChild)
					_                 = []
				//2: Inserts the fields of the newly created ui
				# inserts = [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# change = ChangeUI [] (removals ++ inserts)
				//Create a new state for the constructor selection
				# consChooseSt = LeafState {touched=True,state=JSONInt consIdx}
				= (Ok (change, viewMode, [consChooseSt, childSt]), vst)
			Error e = (Error e, vst)

	//Other events targeted directly at the ADT 
	onEdit dp ([],e) viewMode [LeafState {LeafState|touched,state}, _] vst
		| e =: JSONNull || e =: (JSONArray []) // A null or an empty array are accepted as a reset events
			//If necessary remove the fields of the previously selected constructor
			# change = case state of
				JSONInt prevConsIdx = ChangeUI [] (repeatn (gcd_arities !! prevConsIdx) (1,RemoveChild))
				_                   = NoChange
			# consChooseSt = LeafState {touched = True, state = JSONNull}
			= (Ok (change, viewMode, [consChooseSt]), vst)
		| otherwise
			= (Error ("Unknown constructor select event: '" +++ toString e +++ "'"),vst)

	//Update is targeted somewhere inside this value
	onEdit dp (tp,e) viewMode [consChooseSt, childSt] vst = case exOnEdit dp (tp,e) childSt vst of
		(Ok (change, childSt),vst)
			# change = case change of
				(ChangeUI attrChanges itemChanges) = ChangeUI attrChanges [(i + 1,c) \\ (i,c) <- itemChanges]
				_                                  = NoChange
			= (Ok (change, viewMode, [consChooseSt, childSt]),vst)
		(Error e,vst) = (Error e, vst)

	onRefresh dp (OBJECT new) viewMode [consChooseSt, childSt] vst=:{VSt|taskId,selectedConsIndex=curSelectedConsIndex}
		//Adjust for the added constructor view/choose UI
		= case exOnRefresh dp new childSt {vst & selectedConsIndex = 0} of
			(Ok (change, childSt),vst=:{VSt|selectedConsIndex})
				//If the cons was changed we need to update the selector
				# consIndex = ~selectedConsIndex - 1
				# consChange = if (selectedConsIndex < 0)
					[(0, ChangeChild (ChangeUI [SetAttribute "value" (JSONArray [JSONInt consIndex,JSONBool True])] []))]
					[]
				//Adjust the changes
				# change = case change of
					NoChange						     = if (consChange =: []) NoChange (ChangeUI [] consChange)
					(ChangeUI attrChanges itemChanges)   = ChangeUI attrChanges (consChange ++ [(i + 1,c) \\ (i,c) <- itemChanges])
					(ReplaceUI ui=:(UI type attr items))
						//Add the constructor selection/view ui
						# (consUI,_) = if viewMode (genConsViewUI gcd_names consIndex)
						                           (genConsChooseUI taskId dp gcd_names (Just consIndex))
						= ReplaceUI (UI type attr [consUI:items])
				= (Ok (change, viewMode, [consChooseSt, childSt]), {vst & selectedConsIndex = curSelectedConsIndex})
			(Error e,vst) = (Error e,vst)

	valueFromState _ [_, childSt] = mapMaybe (\x -> OBJECT x) $ exValueFromState childSt
	valueFromState _ _            = Nothing

gEditor{|EITHER|} ex _ _ _ ey _ _ _
	// the additional boolean state represents the LEFT/RIGHT choice (LEFT = False, RIGHT = True)
	= compoundEditorToEditor
		{CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	genUI dp mode vst=:{pathInEditMode} = case (mode, pathInEditMode) of
		(Enter, [nextChoiceIsRight: restOfPath])
			# vst = {vst & pathInEditMode = restOfPath}
			| nextChoiceIsRight = attachInfoToChildResult True  viewMode $ ey.Editor.genUI dp Enter vst
			| otherwise         = attachInfoToChildResult False viewMode $ ex.Editor.genUI dp Enter vst
		(Update (LEFT x),  _) = attachInfoToChildResult False viewMode $ ex.Editor.genUI dp (Update x) vst
		(Update (RIGHT y), _) = attachInfoToChildResult True  viewMode $ ey.Editor.genUI dp (Update y) vst
		(View   (LEFT x),  _) = attachInfoToChildResult False viewMode $ ex.Editor.genUI dp (View x)   vst
		(View   (RIGHT y), _) = attachInfoToChildResult True  viewMode $ ey.Editor.genUI dp (View y)   vst
	where
		viewMode = mode =: View _

	//Special case to create a LEFT, after a constructor switch
	onEdit dp ([-1],e)    (_, viewMode) [childSt] vst = (Ok (NoChange, (False, viewMode), [childSt]),vst)
	onEdit dp ([-1:ds],e) (_, viewMode) [childSt] vst = attachInfoToChildResult False viewMode $ ex.Editor.onEdit dp (ds,e) childSt vst
	//Special cases to create a RIGHT, after a constructor switch
	onEdit dp ([-2],e)    (_, viewMode) [childSt] vst = (Ok (NoChange, (True, viewMode), [childSt]),vst)
	onEdit dp ([-2:ds],e) (_, viewMode) [childSt] vst = attachInfoToChildResult True viewMode $ ey.Editor.onEdit dp (ds,e) childSt vst
	//Just pass the edit event through 
	onEdit dp (tp,e) (isRight, viewMode) [childSt] vst =
		attachInfoToChildResult isRight viewMode $ childOnEdit dp (tp,e) childSt vst
	where
		childOnEdit = if isRight ey.Editor.onEdit ex.Editor.onEdit
	onEdit _ _ _ _ vst = (Error "Corrupt edit state in generic EITHER editor", vst)

	//Same choice is for previous value is made
	onRefresh dp (LEFT new) (False, viewMode) [childSt] vst =
		attachInfoToChildResult False viewMode $ ex.Editor.onRefresh dp new childSt vst
	onRefresh dp (RIGHT new) (True, viewMode) [childSt] vst =
		attachInfoToChildResult True  viewMode $ ey.Editor.onRefresh dp new childSt vst

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	onRefresh dp (RIGHT new) (False, viewMode) _ vst = case ey.Editor.genUI dp (if viewMode View Update $ new) vst of
		(Ok (ui,childSt),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui, (True, viewMode), [childSt]),{vst & selectedConsIndex = -1 - selectedConsIndex})
		(Error e,vst=:{selectedConsIndex}) = (Error e,{vst & selectedConsIndex = -1 - selectedConsIndex})

	onRefresh dp (LEFT new) (True, viewMode) _ vst = case ex.Editor.genUI dp (if viewMode View Update $ new) vst of
		(Ok (ui,childSt),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui, (False, viewMode), [childSt]),{vst & selectedConsIndex = -1 - selectedConsIndex})
		(Error e,vst=:{selectedConsIndex}) = (Error e,{vst & selectedConsIndex = -1 - selectedConsIndex})
	onRefresh _ _ _ _ vst = (Error "Corrupt edit state in generic EITHER editor", vst)

	valueFromState (False, _) [childSt] = LEFT  <$> ex.Editor.valueFromState childSt
	valueFromState (True,  _) [childSt] = RIGHT <$> ey.Editor.valueFromState childSt
	valueFromState _          _         = Nothing

    attachInfoToChildResult :: !Bool !Bool !(!MaybeErrorString (!ui, !EditState), !*VSt)
	                        -> (!MaybeErrorString (!ui, !(!Bool, !Bool), ![EditState]), !*VSt)
	attachInfoToChildResult isRight viewMode (res, vst) =
		((\(ui, childSt) -> (ui, (isRight, viewMode), [childSt])) <$> res, vst)

gEditor{|CONS of {gcd_index,gcd_arity}|} {Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
	= compoundEditorToEditor {CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState}
where
	genUI dp mode vst = case exGenUI (pairPath gcd_arity dp) (mapEditMode (\(CONS x) -> x) mode) vst of
		(Ok viz,vst)
			# (ui, childSts) = fromPairUI UICons gcd_arity viz
			= (Ok (ui, (), childSts), {VSt| vst & selectedConsIndex = gcd_index})
		(Error e,vst) = (Error e,{VSt| vst & selectedConsIndex = gcd_index})

	onEdit dp ([d:ds],e) _ childSts vst
		| d >= gcd_arity
			= (Error "Edit aimed at non-existent constructor field",vst)
		//Update the targeted field in the constructor
		= case exOnEdit (pairPath gcd_arity dp) (pairSelectPath d gcd_arity ++ ds,e) (childSts !! d) vst of
			(Ok (change, st),vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(d,ChangeChild change)]
				//Update the state
				= (Ok (change, (), updateAt d st childSts),vst)
			(Error e,vst) = (Error e,vst)

	onRefresh dp (CONS new) _ childSts vst
		//Refresh 
		= case exOnRefresh (pairPath gcd_arity dp) new (toPairState $ CompoundState JSONNull childSts) vst of
			(Ok (change, childSts),vst)
				= (Ok (fromPairChange 0 gcd_arity change, (), fromPairState gcd_arity childSts), vst)
			(Error e,vst)
				= (Error e, vst)

	valueFromState _ childSts = mapMaybe CONS $ exValueFromState $ toPairState $ CompoundState JSONNull childSts

gEditor{|PAIR|} {Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
				{Editor|genUI=eyGenUI,onEdit=eyOnEdit,onRefresh=eyOnRefresh,valueFromState=eyValueFromState} _ _ _
	= {Editor|genUI=genUI,onRefresh=onRefresh,onEdit=onEdit,valueFromState=valueFromState}
where
	genUI dp mode vst
		# (dpx, dpy)  = pairPathSplit dp
		# (vizx, vst) = exGenUI dpx (mapEditMode (\(PAIR x _) -> x) mode) vst
		| vizx =: (Error _) = (vizx,vst)
		# (vizy, vst) = eyGenUI dpy (mapEditMode (\(PAIR _ y) -> y) mode) vst
		| vizy =: (Error _) = (vizy,vst)
		# ((vizx, stx), (vizy, sty)) = (fromOk vizx, fromOk vizy)
		= (Ok (uic UIPair [vizx, vizy],CompoundState JSONNull [stx, sty]),vst)

	onEdit dp ([0:ds],e) stX ust
		# (dpx,_)   = pairPathSplit dp
		= exOnEdit dpx (ds,e) stX ust
	onEdit dp ([1:ds],e) stY ust
		# (_,dpy) = pairPathSplit dp
		= eyOnEdit dpy (ds,e) stY ust
	onEdit _ val st ust = (Ok (NoChange, st), ust)

	onRefresh dp (PAIR newx newy) (CompoundState _ [stX, stY]) vst
		# (dpx,dpy)		= pairPathSplit dp
		# (changex,vst) 	= exOnRefresh dpx newx stX vst
		| changex=: (Error _) = (changex,vst)
		# (changey,vst) 	= eyOnRefresh dpy newy stY vst
		| changey =: (Error _) = (changey,vst)
		# ((changex,stX),(changey,stY)) = (fromOk changex,fromOk changey)
		= (Ok (ChangeUI [] [(0,ChangeChild changex),(1,ChangeChild changey)],CompoundState JSONNull [stX, stY]), vst)

	onRefresh _ _ _ vst
		= (Error "Corrupt editor state in generic PAIR editor", vst)

	valueFromState (CompoundState _ [stX, stY]) = case (exValueFromState stX, eyValueFromState stY) of
        (Just x, Just y) = Just $ PAIR x y
        _                = Nothing
    valueFromState _     = Nothing

//The maybe editor makes its content optional
gEditor{|Maybe|} {Editor|genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh,valueFromState=exValueFromState} _ _ _
	= compoundEditorToEditor {CompoundEditor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	// Viewing Nothing is always the empty UI
	genUI _ (View Nothing) vst = (Ok (ui UIEmpty, (False, True), [newLeafState]), vst)
	genUI dp mode vst=:{VSt|optional} = case exGenUI dp childMode {VSt|vst & optional = True} of
		(Ok (UI type attr items, childSt), vst) = (Ok (UI type ('DM'.union (optionalAttr True) attr) items, (containsJustVal, mode =: View _), [childSt]), {VSt|vst & optional = optional})
		(Error e, vst) = (Error e, {VSt|vst & optional = optional})
	where
		childMode = case mode of
			Enter           = Enter
			Update Nothing  = Enter
			Update (Just x) = Update x
			View (Just x)   = View x

		containsJustVal = case mode of
			Enter      = False
			Update val = isJust val
			View val   = isJust val

	onEdit dp (tp,e) (_, viewMode) [childSt] vst=:{VSt|optional}
		= case exOnEdit dp (tp,e) childSt {VSt|vst & optional = True} of
			(Ok (change, childSt),vst)
				= (Ok (change, (True, viewMode), [childSt]), {VSt|vst & optional = optional})
			(Error e,vst) = (Error e,{VSt|vst & optional = optional})
	onEdit _ _ _ _ vst = (Error "Corrupt edit state in Maybe editor", vst)

	onRefresh _ Nothing (isJust, viewMode) [childSt] vst
		//Change to empty ui
		| isJust    = (Ok (ReplaceUI $ ui UIEmpty, (False, viewMode), [newLeafState]), vst)
		//No change
		| otherwise = (Ok (NoChange, (isJust, viewMode), [childSt]), vst)
	onRefresh dp (Just new) (isJust, viewMode) [childSt] vst=:{VSt|optional}
		//Update argument UI
		| isJust
			# (change, vst) = exOnRefresh dp new childSt {VSt|vst & optional = True}
			= ((\(ui, childSt) -> (ui, (True, viewMode), [childSt])) <$> change, {vst & optional = optional})
		//Generate a UI and replace
		| otherwise
			# (ui, vst) = exGenUI dp (if viewMode View Update $ new) {VSt|vst & optional = True}
			# ui = ( \(UI type attr items, childSt) ->
			          ( ReplaceUI $ UI type ('DM'.union (optionalAttr True) attr) items
			          , (True, viewMode)
			          , [childSt]
			          )
			       ) <$> ui
			= (ui, {vst & optional = optional})
	onRefresh _ _ _ _ vst = (Error "Corrupt editor state in generic Maybe editor", vst)

	valueFromState (True, _) [st] = Just $ exValueFromState st
	valueFromState _          _   = Just Nothing

//Encode the full range of fields in the datapath, such that it can be decomposed in PAIRs by the pairSplit
pairPath 0 dp = dp
pairPath 1 dp = dp ++ [0]
pairPath n dp = [0, n - 1: dp]

pairPathSplit [begin,end:dp]
	| range == 2	= (dp ++ [begin],dp ++ [end])
	| range == 3	= (dp ++ [begin],[begin + 1,end:dp])
					= ([begin,middle - 1:dp],[middle,end:dp])
where
	range = end - begin + 1
	middle = begin + range / 2

//Make a special datapath that encodes the desired nesting of EITHER's to create a constructor
consCreatePath :: !Int !Int -> [Bool]
consCreatePath i n
 	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [ False: consCreatePath i (n/2) ]
	| otherwise  = [ True:  consCreatePath (i - (n/2)) (n - (n/2)) ]

//Create a path that encodes a sequence of choices (0 for the first, 1 for the second) between the elements of nested PAIR's
pairSelectPath :: Int Int -> DataPath
pairSelectPath i n
	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [0: pairSelectPath i (n /2)]
	| otherwise  = [1: pairSelectPath (i - (n/2)) (n - (n/2))]

//When UIs, or UI differences are aggregated in PAIR's they form a binary tree 

//Recreate the binary-tree representation for a pair
toPairState :: !EditState -> EditState
toPairState m=:(CompoundState _ [])         = m
toPairState m=:(CompoundState _ [m1])       = m1
toPairState m=:(CompoundState _ [m1,m2])    = m
toPairState m=:(CompoundState _ [m1,m2,m3]) = CompoundState JSONNull [m1, CompoundState JSONNull [m2,m3]]
toPairState m=:(CompoundState _ fields)     = CompoundState JSONNull [m1,m2]
where
	half = length fields / 2
	m1 = toPairState (CompoundState JSONNull (take half fields))
	m2 = toPairState (CompoundState JSONNull (drop half fields))

//Desconstruct the binary-tree representation of a pair
fromPairState :: !Int !EditState -> [EditState]
fromPairState 0 m                                                   = [m]
fromPairState 1 m                                                   = [m]
fromPairState 2 m=:(CompoundState _ sts)                            = sts
fromPairState 3 m=:(CompoundState _ [m1, CompoundState _ [m2, m3]]) = [m1,m2,m3]
fromPairState n m=:(CompoundState _ [m1, m2])                       = f1 ++ f2
where
	half = n / 2
	f1 = fromPairState half m1
	f2 = fromPairState (n - half) m2

//These functions flatten this tree back to a single CompoundEditor or ChangeUI definition
fromPairUI :: !UIType !Int !(!UI, !EditState) -> (!UI, ![EditState])
fromPairUI type arity (ui, st) | arity < 2 = (UI type 'DM'.newMap [ui], [st])
fromPairUI type 2 (UI UIPair _ [ul,ur], CompoundState _ [ml,mr])
	= (UI type 'DM'.newMap [ul,ur],[ml,mr])
fromPairUI type 3 (UI UIPair _ [ul,UI UIPair _ [um,ur]], CompoundState _ [ml,CompoundState _ [mm,mr]])
	= (UI type 'DM'.newMap [ul,um,ur], [ml,mm,mr])
fromPairUI type n (UI UIPair _ [ul,ur], CompoundState _ [ml,mr])
	= (UI type 'DM'.newMap (uls ++ urs), (mls ++ mrs))
where
	half = n / 2
	(UI _ _ uls, mls) = fromPairUI type half (ul,ml)
	(UI _ _ urs, mrs) = fromPairUI type (n - half) (ur,mr)

fromPairChange :: Int Int UIChange -> UIChange
//No pairs are introduced for 0 or 1 fields
fromPairChange s arity change | arity < 2 = ChangeUI [] [(s,ChangeChild change)]
//For two and three fields, set the correct child index values 
fromPairChange s 2 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)])
	= ChangeUI [] [(s,ChangeChild l),(s+1,ChangeChild r)]
fromPairChange s 3 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild (ChangeUI _ [(_,ChangeChild m),(_,ChangeChild r)]))])
	= ChangeUI [] [(s,ChangeChild l), (s+1,ChangeChild m), (s+2,ChangeChild r)]
fromPairChange s n (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)])
	= ChangeUI [] (ll ++ rl)
where
	half = n / 2
	(ChangeUI _ ll) = fromPairChange s half l
	(ChangeUI _ rl) = fromPairChange (s + half) (n - half) r

gEditor{|Int|}    = selectByMode 
						(bijectEditorValue toString toInt textView)
						(withDynamicHintAttributes "whole number" (withEditModeAttr integerField))
						(withDynamicHintAttributes "whole number" (withEditModeAttr integerField))
gEditor{|Real|}   = selectByMode
						(bijectEditorValue toString toReal textView)
						(withDynamicHintAttributes "decimal number" (withEditModeAttr decimalField ))
						(withDynamicHintAttributes "decimal number" (withEditModeAttr decimalField ))
gEditor{|Char|}   = bijectEditorValue toString (\c -> c.[0]) (selectByMode
							textView
							(withDynamicHintAttributes "single character" (withEditModeAttr textField <<@ boundedlengthAttr 1 1))
							(withDynamicHintAttributes "single character" (withEditModeAttr textField <<@ boundedlengthAttr 1 1)))
						
gEditor{|String|} = selectByMode
						textView
						(withDynamicHintAttributes "single line of text" (withEditModeAttr textField <<@ minlengthAttr 1))
						(withDynamicHintAttributes "single line of text" (withEditModeAttr textField <<@ minlengthAttr 1))
gEditor{|Bool|}   = selectByMode (checkBox <<@ enabledAttr False) checkBox checkBox

gEditor{|[]|} ex _ tjx _ = listEditor_ tjx (Just (const Nothing)) True True (Just (\l -> pluralisen English (length l) "item")) ex

gEditor{|()|} = emptyEditorWithDefaultInEnterMode ()
gEditor{|(->)|} _ _ tjx fjx _ _ tjy fjy =
	emptyEditorWithErrorInEnterMode_  (JSONEncode{|* -> * -> *|} tjx tjy)
	                                  (JSONDecode{|* -> * -> *|} fjx fjy)
	                                  "A function cannot be entered."
gEditor{|Dynamic|} = emptyEditorWithErrorInEnterMode "A dynamic value cannot be entered."
gEditor{|HtmlTag|} = htmlView

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), (,,,,,), Timestamp, Map
