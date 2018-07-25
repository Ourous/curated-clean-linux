implementation module iTasks.UI.Editor.Generic

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
import Data.GenEq

generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a
derive bimap Editor,(,),(,,),(,,,), MaybeError

gEditor{|UNIT|} = emptyEditor

gEditor{|RECORD of {grd_arity}|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ _ _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (RECORD x) vst=:{VSt|taskId,mode,optional}
		= case mode of
			Enter 
				| optional //Just show the ui to enable the remaining fields
					# (enableUI,enableMask) = genEnableUI taskId dp False	
					= (Ok (uic UIRecord [enableUI], CompoundMask [enableMask]), vst)
				| otherwise
					= case exGenUI (pairPath grd_arity dp) x {VSt|vst & optional = False} of
						(Ok viz, vst) = (Ok (fromPairUI UIRecord grd_arity viz),vst)
						(Error e,vst) = (Error e,vst)
			Update
				= case exGenUI (pairPath grd_arity dp) x {VSt|vst & optional = False} of
					(Ok viz,vst) 
						# (UI type attr items, CompoundMask masks) = fromPairUI UIRecord grd_arity viz 
						//When optional we add a checkbox to clear the record
						| optional
							# (enableUI,enableMask) = genEnableUI taskId dp True
							= (Ok (UI type attr [enableUI:items],CompoundMask [enableMask:masks]), vst)
						| otherwise 
							= (Ok (UI type attr items,CompoundMask masks),vst)
					(Error e,vst) = (Error e,vst)
			View 
				= case exGenUI (pairPath grd_arity dp) x {VSt|vst & optional = False} of
					(Ok viz,vst)  = (Ok (fromPairUI UIRecord grd_arity viz),vst)
					(Error e,vst) = (Error e,vst)

	genEnableUI taskId dp enabled = (uia UICheckbox (editAttrs taskId (editorId dp) (Just (JSONBool enabled))),newFieldMask)

	onEdit dp ([],JSONBool True) (RECORD val) (CompoundMask [enableMask:masks]) vst=:{VSt|mode,optional} //Enabling an optional record
		| not optional
			= (Error "Enabling non-optional record",RECORD val,vst)
		//Create and add the fields
		= case exGenUI (pairPath grd_arity dp) val {vst & mode = Enter, optional = False} of
			(Ok viz,vst)
				# (UI type attr items, CompoundMask masks) = fromPairUI UIRecord grd_arity viz 
				# change = ChangeUI [] [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# enableMask = FieldMask {touched=True,valid=True,state=JSONBool True}
				= (Ok (change,CompoundMask [enableMask:masks]), RECORD val, {vst & mode = mode})
			(Error e,vst) = (Error e, RECORD val, {vst & mode = mode})

	onEdit dp ([],JSONBool False) (RECORD val) (CompoundMask [enableMask:masks]) vst=:{VSt|optional} //Disabling an optional record
		| not optional
			= (Error "Disabling non-optional record",RECORD val,vst)
		//Remove all fields except the enable/disable checkbox
		# change = ChangeUI [] (repeatn grd_arity (1,RemoveChild))
		# enableMask = FieldMask {touched=True,valid=True,state=JSONBool False}
		= (Ok (change,CompoundMask [enableMask:masks]), RECORD val, vst)
	onEdit dp ([],_) (RECORD val) mask vst
		= (Error "Unknown edit event for record",RECORD val,vst)
	
	onEdit dp ([d:ds],e) (RECORD val) (CompoundMask masks) vst=:{VSt|optional}
		| d >= grd_arity
			= (Error "Edit aimed at non-existent record field",RECORD val,vst)
		//When optional we need to adjust for the added checkbox, so we need to offset the record field index with one
		//In the generated UI and mask (but not in the paths when targeting the edit!!).
		# idx = if optional (d + 1) d
		= case exOnEdit (pairPath grd_arity dp) (pairSelectPath d grd_arity ++ ds,e) val (masks !! idx) {VSt|vst & optional = False} of
			(Ok (change,mask),val,vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(idx,ChangeChild change)]
				//Update the mask
				# mask = CompoundMask (updateAt idx mask masks)
				= (Ok (change,mask),RECORD val,vst)
			(Error e,val ,vst) = (Error e, RECORD val, vst)
				
	onEdit _ _ val mask vst = (Ok (NoChange,mask),val,vst)

	onRefresh dp (RECORD new) (RECORD old) mask=:(CompoundMask fields) vst=:{VSt|taskId,optional,mode}
		| optional && not (mode =: View)
			//Account for the extra mask of the enable/disable checkbox
			# enableMask = hd fields
			= case exOnRefresh (pairPath grd_arity dp) new old (toPairMask (CompoundMask (tl fields))) {VSt|vst & optional = False} of
				(Ok (change,mask),val,vst)
					# change = fromPairChange 0 grd_arity change
					# (CompoundMask fields) = fromPairMask grd_arity mask
					//Adjust the change
					# change = case change of 
						NoChange = NoChange
						ChangeUI attrChanges itemChanges = ChangeUI attrChanges ([(i + 1,c) \\ (i,c) <- itemChanges])
						(ReplaceUI ui=:(UI type attr items))
							# (enableUI,_) = genEnableUI taskId dp True
							= ReplaceUI (UI type attr [enableUI:items])
					= (Ok (change,CompoundMask [enableMask:fields]), RECORD val, vst)
				(Error e,val,vst)
					= (Error e, RECORD val, vst)
		| otherwise
			= case exOnRefresh (pairPath grd_arity dp) new old (toPairMask mask) {VSt|vst & optional = False} of
				(Ok (change,mask),val,vst)
					= (Ok (fromPairChange 0 grd_arity change, fromPairMask grd_arity mask), RECORD val, vst)
				(Error e,val,vst)
					= (Error e, RECORD val, vst)
	onRefresh dp (RECORD new) (RECORD old) mask vst=:{VSt|taskId,optional,mode}
		= (Error "Corrupt mask in generic RECORD editor",RECORD old, vst)

gEditor{|FIELD of {gfd_name}|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ _ _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (FIELD x) vst = case exGenUI dp x vst of //Just add the field name as a label
		(Ok (UI type attr items, mask),vst) = (Ok (UI type ('DM'.union attr (labelAttr gfd_name)) items, mask),vst) 
		(Error e,vst)                       = (Error e,vst)

	onEdit dp (tp,e) (FIELD field) mask vst
		# (mbmask,field,vst) = exOnEdit dp (tp,e) field mask vst
		= (mbmask,FIELD field,vst)

	onRefresh dp (FIELD new) (FIELD old) mask vst
		# (change,val,vst) = exOnRefresh dp new old mask vst
		= (change,FIELD val,vst)

/*
* For ADT's we need to deal with two cases:
* - There is only one constructor
* - There are multiple constructors
*/
gEditor{|OBJECT of {gtd_num_conses,gtd_conses}|} ce=:{genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ _ _ _
	//This is a newtype
	| gtd_num_conses == 0
		= bijectEditorValue (\(OBJECT i)->i) (\i->OBJECT i) ce
	//This is not a newtype
	= withEditModeAttr {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	gcd_names   = [gcd_name  \\ {GenericConsDescriptor | gcd_name}  <- gtd_conses]  // preselect cons names   to circumvent cyclic dependencies
	gcd_arities = [gcd_arity \\ {GenericConsDescriptor | gcd_arity} <- gtd_conses]  // preselect cons arities to circumvent cyclic dependencies
	
	genUI dp (OBJECT x) vst=:{VSt|taskId,mode,optional,selectedConsIndex}
		= case mode of
			Enter
				//If there is only one constructor, we generate the UI for that constructor
				| gtd_num_conses == 1
					= case exGenUI dp x vst of
						(Ok (consUI,consMask), vst) //The CONS will already create a ui of type UICons
							= (Ok (consUI,consMask), {vst & selectedConsIndex = selectedConsIndex})
						(Error e, vst)
							= (Error e, {vst & selectedConsIndex = selectedConsIndex})
				//If there are multiple constructors, we only generate a UI to select the constructor
				| otherwise
					# (consChooseUI,consChooseMask) = genConsChooseUI taskId dp optional gcd_names Nothing
					= (Ok (UI UIVarCons 'DM'.newMap [consChooseUI],CompoundMask [consChooseMask]),{vst & selectedConsIndex = selectedConsIndex})
			Update
				//Generate the ui for the current value
				= case exGenUI dp x vst of
					(Ok (consUI=:(UI UICons attr items), consMask=:(CompoundMask fields)),vst)
						//If there is only constructor, we are done
						| gtd_num_conses == 1
							= (Ok (consUI, consMask),{vst & selectedConsIndex = selectedConsIndex})
						//If there are multiple constructors, we add the UI to select the constructor and change the type to UIVarCons	
						| otherwise
							# (consChooseUI,consChooseMask) = genConsChooseUI taskId dp optional gcd_names (Just vst.selectedConsIndex)
							= (Ok (UI UIVarCons attr [consChooseUI:items],CompoundMask [consChooseMask:fields])
								,{vst & selectedConsIndex = selectedConsIndex})
					(Error e,vst) = (Error e,vst)
			View
				//If there is only one constructor we don't need to show the constructor name
				= case exGenUI dp x vst of
					(Ok (consUI=:(UI UICons attr items), consMask=:(CompoundMask fields)),vst)
						| gtd_num_conses == 1
							= (Ok (consUI, consMask),{vst & selectedConsIndex = selectedConsIndex})
						| otherwise
							# (consViewUI,consViewMask) = genConsViewUI gcd_names vst.selectedConsIndex
							= (Ok (UI UIVarCons attr [consViewUI:items],CompoundMask [consViewMask:fields])
								,{vst & selectedConsIndex = selectedConsIndex})
					(Error e,vst) = (Error e,vst)

	genConsChooseUI taskId dp optional gcd_names mbSelectedCons = (consChooseUI,consChooseMask)
	where
		consOptions = [JSONObject [("id",JSONInt i),("text",JSONString gcd_name)] \\ gcd_name <- gcd_names & i <- [0..]]
		consChooseUI = uia UIDropdown (choiceAttrs taskId (editorId dp) (maybe [] (\x -> [x]) mbSelectedCons) consOptions)
		consChooseMask = FieldMask {touched=False,valid=optional || isJust mbSelectedCons,state=maybe JSONNull (\x -> JSONInt x) mbSelectedCons}

	genConsViewUI gcd_names selectedCons
		= (uia UITextView (valueAttr (JSONString (gcd_names !! selectedCons))), newFieldMask)

	//Update is a constructor switch
	onEdit dp ([],JSONArray [JSONInt consIdx]) (OBJECT val) (CompoundMask [FieldMask {FieldMask|touched,valid,state}:masks]) vst=:{VSt|mode} 
		| consIdx < 0 || consIdx >= gtd_num_conses
			= (Error "Constructor selection out of bounds",OBJECT val,vst)
		//Create a default value for the selected constructor
		//This is a rather ugly trick: We create a special target path that consists only of negative values that is
		//decoded by the the onEdit instance of EITHER to create a value that consists of the correct nesting of LEFT's and RIGHT's
    	# (_,val,vst)	= exOnEdit dp (consCreatePath consIdx gtd_num_conses,JSONNull) val newCompoundMask vst 
		//Create a UI for the new constructor 
		= case exGenUI dp val {vst & mode = Enter} of
			(Ok (UI UICons attr items, CompoundMask masks),vst)
				//Construct a UI change that does the following: 
				//1: If necessary remove the fields of the previously selected constructor
				# removals = case state of
					(JSONInt prevConsIdx) = repeatn (gcd_arities !! prevConsIdx) (1,RemoveChild)
					_                 = []
				//2: Inserts the fields of the newly created ui
				# inserts = [(i,InsertChild ui) \\ ui <- items & i <- [1..]]
				# change = ChangeUI [] (removals ++ inserts)
				//Create a new mask for the constructor selection
				# consChooseMask = FieldMask {touched=True,valid=True,state=JSONInt consIdx}
				= (Ok (change,CompoundMask [consChooseMask:masks]), OBJECT val, {vst & mode = mode})
			(Error e,vst) = (Error e, OBJECT val, {vst & mode = mode})

	//Other events targeted directly at the ADT 
	onEdit dp ([],e) (OBJECT val) (CompoundMask [consChooseMask=:(FieldMask {FieldMask|touched,valid,state}):masks]) vst=:{VSt|optional}
		| e =: JSONNull || e =: (JSONArray []) // A null or an empty array are accepted as a reset events
			//If necessary remove the fields of the previously selected constructor
			# change = case state of
				(JSONInt prevConsIdx) = ChangeUI [] (repeatn (gcd_arities !! prevConsIdx) (1,RemoveChild))
				_                     = NoChange		
			# consChooseMask = FieldMask {touched=True,valid=optional,state=JSONNull}
			= (Ok (change,CompoundMask [consChooseMask:masks]),OBJECT val, vst)	
		| otherwise
			= (Error ("Unknown constructor select event: '" +++ toString e +++ "'"),OBJECT val,vst)

	//Update is targeted somewhere inside this value
	onEdit dp (tp,e) (OBJECT val) mask=:(CompoundMask fields) vst 
		| gtd_num_conses == 1
			//Just call onEdit for the inner value
			= case exOnEdit dp (tp,e) val mask vst of
				(Ok (change,mask),val,vst) = (Ok (change,mask),OBJECT val,vst)
				(Error e,val,vst) = (Error e, OBJECT val, vst)
		| otherwise
			//Adjust for the added constructor switch UI
			# consChooseMask = hd fields
			= case exOnEdit dp (tp,e) val (CompoundMask (tl fields)) vst of
				(Ok (change,CompoundMask fields),val,vst)
					# change = case change of
						(ChangeUI attrChanges itemChanges) = ChangeUI attrChanges [(i + 1,c) \\ (i,c) <- itemChanges]
						_                                  = NoChange
					= (Ok (change,CompoundMask [consChooseMask:fields]),OBJECT val,vst)
				(Error e,val,vst) = (Error e, OBJECT val, vst)

	onRefresh dp (OBJECT new) (OBJECT old) mask=:(CompoundMask fields) vst=:{VSt|mode,taskId,optional,selectedConsIndex=curSelectedConsIndex}
		| gtd_num_conses == 1
			# (change,val,vst) = exOnRefresh dp new old mask vst
			= (change,OBJECT val,vst)
		| otherwise
			//Adjust for the added constructor view/choose UI
			# consChooseMask = hd fields
			//Don't recursively refresh if no constructor has been chosen
			| (not mode =: View) && consChooseMask =: (FieldMask {FieldMask|state=JSONNull})
				= (Ok (NoChange,mask),OBJECT old,vst)
			= case exOnRefresh dp new old (CompoundMask (tl fields)) {vst & selectedConsIndex = 0} of
				(Ok (change,CompoundMask fields),val,vst=:{VSt|selectedConsIndex}) 
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
							# (consUI,_) = if (mode =: View) 
												(genConsViewUI gcd_names consIndex)
												(genConsChooseUI taskId dp optional gcd_names (Just consIndex))
							= ReplaceUI (UI type attr [consUI:items])
					| otherwise
						= (Ok (change, CompoundMask [consChooseMask:fields]), OBJECT val,{vst & selectedConsIndex = curSelectedConsIndex})

				(Ok (change,mask),val,vst=:{VSt|selectedConsIndex}) 
					= (Error "Corrupt mask in generic OBJECT editor",OBJECT old, vst)
				(Error e,val,vst) = (Error e,OBJECT val,vst)
	onRefresh dp (OBJECT new) (OBJECT old) mask vst
		= (Error "Corrupt mask in generic OBJECT editor",OBJECT old, vst)

gEditor{|EITHER|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ dx _ _ 
                  {genUI=eyGenUI,onEdit=eyOnEdit,onRefresh=eyOnRefresh} _ dy _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (LEFT  x) vst = exGenUI dp x vst
	genUI dp (RIGHT y) vst = eyGenUI dp y vst	

	//Special case to create a LEFT, after a constructor switch
	onEdit dp ([-1],e) _ mask vst = (Ok (NoChange,mask),LEFT dx,vst)
	onEdit dp ([-1:ds],e) _ mask vst
		# (mask,x,vst) = exOnEdit dp (ds,e) dx mask vst
		= (mask,LEFT x,vst)
	//Special cases to create a RIGHT, after a constructor switch
	onEdit dp ([-2],e) _ mask vst = (Ok (NoChange,mask),RIGHT dy,vst)
	onEdit dp ([-2:ds],e) _ mask vst 
		# (mask,y,vst) = eyOnEdit dp (ds,e) dy mask vst
		= (mask,RIGHT y,vst)
	//Just pass the edit event through 
	onEdit dp (tp,e) (LEFT x) mask vst
		# (mask,x,vst) = exOnEdit dp (tp,e) x mask vst
		= (mask,LEFT x,vst)
	onEdit dp (tp,e) (RIGHT y) mask vst
		# (mask,y,vst) = eyOnEdit dp (tp,e) y mask vst
		= (mask,RIGHT y,vst)

	onRefresh dp (LEFT new) (LEFT old) mask vst 
		# (change,val,vst) = exOnRefresh dp new old mask vst
		= (change,LEFT val,vst)
	onRefresh dp (RIGHT new) (RIGHT old) mask vst
		# (change,val,vst) = eyOnRefresh dp new old mask vst
		= (change,RIGHT val,vst)

	//A different constructor is selected -> generate a new UI
	//We use a negative selConsIndex to encode that the constructor was changed
	onRefresh dp (RIGHT new) (LEFT old) mask vst 
		= case eyGenUI dp new vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui,mask),RIGHT new,{vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,LEFT old,{vst & selectedConsIndex = -1 - selectedConsIndex})

	onRefresh dp (LEFT new) (RIGHT old) mask vst 
		= case exGenUI dp new vst of
			(Ok (ui,mask),vst=:{selectedConsIndex}) = (Ok (ReplaceUI ui,mask),LEFT new,{vst & selectedConsIndex = -1 - selectedConsIndex})
			(Error e,vst=:{selectedConsIndex}) = (Error e,RIGHT old,{vst & selectedConsIndex = -1 - selectedConsIndex})

gEditor{|CONS of {gcd_index,gcd_arity}|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ _ _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (CONS x) vst = case exGenUI (pairPath gcd_arity dp) x vst of
		(Ok viz,vst)  = (Ok (fromPairUI UICons gcd_arity viz), {VSt| vst & selectedConsIndex = gcd_index})
		(Error e,vst) = (Error e,{VSt| vst & selectedConsIndex = gcd_index})

	onEdit dp ([d:ds],e) (CONS val) (CompoundMask masks) vst
		| d >= gcd_arity
			= (Error "Edit aimed at non-existent constructor field",CONS val,vst)
		//Update the targeted field in the constructor
		= case exOnEdit (pairPath gcd_arity dp) (pairSelectPath d gcd_arity ++ ds,e) val (masks !! d) vst of	
			(Ok (change,mask),val,vst)
				//Extend the change
				# change = case change of NoChange = NoChange; _ = ChangeUI [] [(d,ChangeChild change)]
				//Update the mask
				# mask = CompoundMask (updateAt d mask masks)
				= (Ok (change,mask),CONS val,vst)
			(Error e,val,vst) = (Error e,CONS val,vst)

	onRefresh dp (CONS new) (CONS old) mask vst 
		//Refresh 
		= case exOnRefresh (pairPath gcd_arity dp) new old (toPairMask mask) vst of
			(Ok (change,mask),val,vst)
				= (Ok (fromPairChange 0 gcd_arity change, fromPairMask gcd_arity mask), CONS val, vst)
			(Error e,val,vst)
				= (Error e, CONS val, vst)

gEditor{|PAIR|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ _ _ _
				{genUI=eyGenUI,onEdit=eyOnEdit,onRefresh=eyOnRefresh} _ _ _ _
	= {Editor|genUI=genUI,onRefresh=onRefresh,onEdit=onEdit}
where
	genUI dp (PAIR x y) vst
		# (dpx,dpy)		= pairPathSplit dp
		# (vizx, vst)	= exGenUI dpx x vst
		| vizx =: (Error _) = (vizx,vst)
		# (vizy, vst)	= eyGenUI dpy y vst
		| vizy =: (Error _) = (vizy,vst)
		# ((vizx,maskx),(vizy,masky)) = (fromOk vizx,fromOk vizy)
		= (Ok (uic UIPair [vizx,vizy],CompoundMask [maskx,masky]),vst)

	onEdit dp ([0:ds],e) (PAIR x y) xmask ust
		# (dpx,_)		= pairPathSplit dp
		# (xmask,x,ust) = exOnEdit dpx (ds,e) x xmask ust
		= (xmask,PAIR x y,ust)
	onEdit dp ([1:ds],e) (PAIR x y) ymask ust
		# (_,dpy)		= pairPathSplit dp
		# (ymask,y,ust) = eyOnEdit dpy (ds,e) y ymask ust
		= (ymask,PAIR x y,ust)
	onEdit _ _ val mask ust = (Ok (NoChange,mask),val,ust)

	onRefresh dp (PAIR newx newy) (PAIR oldx oldy) (CompoundMask [maskx,masky]) vst
		# (dpx,dpy)		= pairPathSplit dp
		# (changex,newx,vst) 	= exOnRefresh dpx newx oldx maskx vst
		| changex=: (Error _) = (changex,PAIR oldx oldy,vst)
		# (changey,newy,vst) 	= eyOnRefresh dpy newy oldy masky vst
		| changey =: (Error _) = (changey,PAIR oldx oldy,vst)
		# ((changex,maskx),(changey,masky)) = (fromOk changex,fromOk changey)
		= (Ok (ChangeUI [] [(0,ChangeChild changex),(1,ChangeChild changey)],CompoundMask [maskx,masky]),PAIR newx newy, vst)

	onRefresh dp (PAIR newx newy) (PAIR oldx oldy) mask vst
		= (Error "Corrupt mask in generic PAIR editor",PAIR oldx oldy, vst)

//The maybe editor makes it content optional
gEditor{|Maybe|} {genUI=exGenUI,onEdit=exOnEdit,onRefresh=exOnRefresh} _ dx _ _
	= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode,optional}
		| mode =: View && val =: Nothing // Viewing Nothing is always the empty UI
			= (Ok (ui UIEmpty,newFieldMask),vst)
		= case exGenUI dp (fromMaybe dx val) {VSt|vst & optional = True} of
			(Ok (UI type attr items, mask),vst) = (Ok (UI type ('DM'.union (optionalAttr True) attr) items,mask), {VSt|vst & optional = optional})
			(Error e,vst) = (Error e, {VSt|vst & optional = optional})

	onEdit dp (tp,e) val mask vst=:{VSt|optional}
		= case exOnEdit dp (tp,e) (fromMaybe dx val) mask {VSt|vst & optional = True} of
			(Ok (change, mask),val,vst)
				| isEmpty tp && (e === JSONNull || e === JSONBool False)
					= (Ok (change, mask),Nothing, {VSt|vst & optional = optional}) //The event was a direct reset (switch to nothing)
				| otherwise
					= (Ok (change, mask),Just val, {VSt|vst & optional = optional}) //The event edited the value in the maybe
			(Error e,val,vst) = (Error e,Nothing,{VSt|vst & optional = optional})

	onRefresh dp Nothing Nothing mask vst = (Ok (NoChange,mask),Nothing,vst)
	onRefresh dp (Just new) Nothing mask vst=:{VSt|optional}
		//Generate a UI and replace
		= case exGenUI dp new {VSt|vst & optional = True} of
			(Ok (UI type attr items, mask),vst) 
				= (Ok (ReplaceUI (UI type ('DM'.union (optionalAttr True) attr) items),mask), Just new, {VSt|vst & optional = optional})
			(Error e,vst) = (Error e, Just new, {VSt|vst & optional = optional})
	onRefresh dp Nothing (Just old) mask vst=:{VSt|optional}
		//Change to empty ui
		= (Ok (ReplaceUI (ui UIEmpty),newFieldMask),Nothing,{VSt|vst & optional = optional})
	onRefresh dp (Just new) (Just old) mask vst=:{VSt|optional}
		# (change,val,vst) = exOnRefresh dp new old mask {VSt|vst & optional = True}
		= (change,Just val,{VSt|vst & optional = optional})

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
consCreatePath i n
 	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [ -1: consCreatePath i (n/2) ]
	| otherwise  = [ -2: consCreatePath (i - (n/2)) (n - (n/2)) ]

//Create a path that encodes a sequence of choices (0 for the first, 1 for the second) between the elements of nested PAIR's
pairSelectPath i n
	| i >= n     = []
	| n == 1     = []
	| i < (n /2) = [0: pairSelectPath i (n /2)]
	| otherwise  = [1: pairSelectPath (i - (n/2)) (n - (n/2))]

//When UIs, or UI differences are aggregated in PAIR's they form a binary tree 

//Recreate the binary-tree representation for a pair
toPairMask m=:(CompoundMask []) = m
toPairMask m=:(CompoundMask [m1]) = m1
toPairMask m=:(CompoundMask [m1,m2]) = m
toPairMask m=:(CompoundMask [m1,m2,m3]) = CompoundMask [m1, CompoundMask [m2,m3]]
toPairMask m=:(CompoundMask fields) = CompoundMask [m1,m2]
where
	half = length fields / 2
	m1 = toPairMask (CompoundMask (take half fields))
	m2 = toPairMask (CompoundMask (drop half fields))

//Desconstruct the binary-tree representation of a pair
fromPairMask 0 m                                           = m 
fromPairMask 1 m                                           = CompoundMask [m]
fromPairMask 2 m=:(CompoundMask [m1,m2])                   = m
fromPairMask 3 m=:(CompoundMask [m1,CompoundMask [m2,m3]]) = CompoundMask [m1,m2,m3]
fromPairMask n m=:(CompoundMask [m1,m2])                   = CompoundMask (f1 ++ f2) 
where
	half = n / 2
	(CompoundMask f1) = fromPairMask half m1
	(CompoundMask f2) = fromPairMask (n - half) m2

//These functions flatten this tree back to a single CompoundEditor or ChangeUI definition
fromPairUI type 0 (ui,mask) = (UI type 'DM'.newMap [],CompoundMask [])
fromPairUI type 1 (ui,mask) = (UI type 'DM'.newMap [ui],CompoundMask [mask])
fromPairUI type 2 (UI UIPair _ [ul,ur], CompoundMask [ml,mr])
	= (UI type 'DM'.newMap [ul,ur],CompoundMask [ml,mr])
fromPairUI type 3 (UI UIPair _ [ul,UI UIPair _ [um,ur]], CompoundMask [ml,CompoundMask [mm,mr]])
	= (UI type 'DM'.newMap [ul,um,ur],CompoundMask [ml,mm,mr])
fromPairUI type n (UI UIPair _ [ul,ur], CompoundMask [ml,mr])
	= (UI type 'DM'.newMap (uls ++ urs), CompoundMask (mls ++ mrs))
where
	half = n / 2
	(UI _ _ uls,CompoundMask mls) = fromPairUI type half (ul,ml)
	(UI _ _ urs,CompoundMask mrs) = fromPairUI type (n - half) (ur,mr)

//No pairs are introduced for 0 or 1 fields
fromPairChange s 0 change = change
fromPairChange s 1 change = ChangeUI [] [(s,ChangeChild change)]
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
/*
//No pairs are introduced for 0 or 1 fields
fromPairDiff s 0 (change,mask) = (change,mask)
fromPairDiff s 1 (change,mask) = (change,mask)
//For two and three fields, set the correct child index values 
fromPairDiff s 2 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)],CompoundMask [ml,mr]) 
	= (ChangeUI [] [(s,ChangeChild l),(s+1,ChangeChild r)],CompoundMask [ml,mr])
fromPairDiff s 3 (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild (ChangeUI _ [(_,ChangeChild m),(_,ChangeChild r)]))],CompoundMask [ml,CompoundMask [mm,mr]])
	= (ChangeUI [] [(s,ChangeChild l), (s+1,ChangeChild m), (s+2,ChangeChild r)],CompoundMask [ml,mm,mr])
//For more fields we aggregate both sides
fromPairDiff s n (ChangeUI _ [(_,ChangeChild l),(_,ChangeChild r)],CompoundMask [ml,mr]) 
	# (ChangeUI _ l,CompoundMask ml) = fromPairDiff s half (l,ml)
	# (ChangeUI _ r,CompoundMask mr) = fromPairDiff (s + half) (n - half) (r,mr)
	= (ChangeUI [] (l ++ r),CompoundMask (ml ++ mr))
where
	half = n / 2
*/

gEditor{|Int|}    = selectByMode 
						(bijectEditorValue toString toInt textView)
						(withDynamicHintAttributes "whole number" (withEditModeAttr integerField ))
						(withDynamicHintAttributes "whole number" (withEditModeAttr integerField ))
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
gEditor{|Bool|}   = selectByMode (checkBox <<@ enabledAttr False) (withEditMode Update checkBox) checkBox

gEditor{|[]|} ex _ dx tjx _ = listEditor_ tjx dx (Just (const Nothing)) True True (Just (\l -> pluralisen English (length l) "item")) ex

gEditor{|()|} = emptyEditor
gEditor{|(->)|} _ _ _ _ _ _ _ _ _ _ = emptyEditor
gEditor{|Dynamic|} = emptyEditor
gEditor{|HtmlTag|} = htmlView

derive gEditor JSONNode, Either, MaybeError, (,), (,,), (,,,), (,,,,), (,,,,,), Timestamp, Map

