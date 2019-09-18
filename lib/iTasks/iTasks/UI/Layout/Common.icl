implementation module iTasks.UI.Layout.Common

import StdEnv

import iTasks.UI.Layout, iTasks.UI.Layout.Default
import iTasks.UI.Definition
import iTasks.UI.Tune
import iTasks.WF.Combinators.Overloaded
import iTasks.WF.Tasks.Interaction
import Data.List, Text.GenJSON, Data.Maybe, StdString, Data.GenEq
import Data.Monoid
import qualified Data.Map as DM
import Data.Map.GenJSON
from Data.Func import $
from StdListExtensions import foldlSt
from iTasks.Internal.TaskEval import :: TaskEvalOpts(..), :: TaskTime
import qualified Data.Foldable
import qualified Text as T
from Text import class Text, instance Text String

addCSSClass :: String -> LayoutRule
addCSSClass className = modifyUIAttributes (SelectKeys ["class"]) add
where
	add attr = 'DM'.put "class" (maybe
		(JSONArray [JSONString className])
		(\v->case v of
			(JSONArray classNames) = JSONArray (classNames ++ [JSONString className])
			(JSONString s) = JSONArray [JSONString s, JSONString className]
		) ('DM'.get "class" attr)) attr

removeCSSClass :: String -> LayoutRule
removeCSSClass className = modifyUIAttributes (SelectKeys ["class"]) remove
where
	remove attr = case 'DM'.get "class" attr of
		(Just (JSONArray items)) = 'DM'.put "class" (JSONArray [item \\ item=:(JSONString name) <- items | name <> className]) attr
		_ = attr

arrangeWithTabs :: Bool -> LayoutRule
arrangeWithTabs closeable = layoutSubUIs
	(SelectAND (SelectByPath []) (SelectOR (SelectByClass "parallel") (SelectByClass "parallel-actions")))
	(sequenceLayouts
		[setUIType UITabSet
		,layoutSubUIs SelectChildren scrollContent
		:if closeable [moveCloseToTab] []
		])
where
	moveCloseToTab = layoutSubUIs //Only on children directly containing a close action
		(SelectAND
			SelectChildren
			(SelectByContains
				(SelectAND
					(SelectByDepth 2)
					selectCloseButton
				)
			)
		)
		reallyMoveCloseToTab

	selectCloseButton = SelectAND
		(SelectByType UIAction)
		(SelectByAttribute "actionId" ((==) (JSONString "Close")))

	reallyMoveCloseToTab = sequenceLayouts
		[moveSubUIs (SelectAND SelectChildren selectCloseButton) [] 0
		,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes SelectAll
			(\ui->case 'DM'.get "taskId" ui of
				Nothing = ui
				Just tid = 'DM'.put "closeTaskId" tid ui))
		,copySubUIAttributes (SelectKeys ["closeTaskId"]) [0] []
		,removeSubUIs (SelectByPath [0])
		]

arrangeWithHeader :: !Int -> LayoutRule
arrangeWithHeader index = setAside "itasks-headerbar" index TopSide False

arrangeWithSideBar :: !Int !UISide !Bool -> LayoutRule
arrangeWithSideBar index side resize = setAside ("itasks-sidebar" +++ suffix side) index side resize
where
	suffix TopSide = "-top"
	suffix BottomSide = "-bottom"
	suffix LeftSide = "-left"
	suffix RightSide = "-right"

setAside className index side resize = sequenceLayouts
	[wrapUI UIPanel //Push the current container down a level
	,copySubUIAttributes SelectAll [0] [] //Keep the attributes from the original UI
	,addCSSClass className
	,moveSubUIs (SelectByPath [0,index]) [] sidePanelIndex
	,layoutSubUIs (SelectByPath [sidePanelIndex]) (sequenceLayouts
		(if resize
		[wrapUI UIPanel
		,addCSSClass "aside"
		,setUIAttributes (resizableAttr (resizers side))
		]
		[addCSSClass "aside"]
		)
	)
	]
where
	sidePanelIndex = if (side === TopSide || side === LeftSide) 0 1

	resizers TopSide = [BottomSide]
	resizers BottomSide = [TopSide]
	resizers LeftSide = [RightSide]
	resizers RightSide = [LeftSide]

arrangeAsMenu :: [[Int]] -> LayoutRule
arrangeAsMenu seps = sequenceLayouts
	// Wrap in panel
	[ wrapUI UIPanel
	// Add a buttonbar to hold the menu
	, insertChildUI 0 (ui UIToolBar)
	// Move the actions with a matching id to the menubar
	, moveSubUIs (SelectAND
			(SelectByDepth 2)
			(SelectAND
				(SelectByType UIAction)
				(SelectByAttribute "actionId" (\s->case s of
						(JSONString s) = s.[0] == '/'
						_ = False)
				)
			)
		) [0] 0
	// Transform the menubar in an actual menu
	//, layoutSubUIs (SelectByPath [0]) makeMenu//(sequenceLayouts makeMenu actionToButton)
	]
/*
where
	makeMenu :: Layout
	makeMenu =	
		{apply=apply
		,adjust=  \t->case t of
			(NoChange, s) = (NoChange, s)
			(ReplaceUI ui, _) = apply ui
			(change, LSType ui) = (change, LSType (applyUIChange change ui))
		,restore= \(LSType ui) = ReplaceUI ui
		}

	apply ui=:(UI t attr cs)
		# (actions, others) = partition (\s->s=:(UI UIAction _ _)) cs
		= (ReplaceUI (UI t attr (mkmenu actions ++ others)), LSType ui)
	
	adjust (NoChange,s)   = (NoChange,s)
	adjust (ReplaceUI ui,_) = apply ui
	adjust (change, LSType ui) = (change, LSType (applyUIChange change ui))
	
	restore (LSType ui) = ReplaceUI ui 
	
	mkmenu :: ([UI] -> [UI])
	mkmenu  = seq (map separators seps) o flip (foldlSt (uncurry ins)) [] o map (\t->(exPath t, t))

	separators :: [Int] [UI] -> [UI]
	separators [] uis  = uis
	separators d [ui=:(UI UIMenuSep _ _):uis] = [ui:separators d uis]
	separators [0] uis = [ui UIMenuSep:uis]
	separators [0:ds] [UI t attr cs:uis] = [UI t attr (separators ds cs):uis]
	separators [d:ds] [ui:uis] = [ui:separators [d-1:ds] uis]
	separators _ [] = []
	
	exPath :: UI -> [String]
	exPath ui=:(UI _ attr _) = case 'DM'.get "actionId" attr of
		Just (JSONString p) = 'T'.split "/" $ 'T'.subString 1 (size p) p
		_ = []
	
	ins :: [String] UI [UI] -> [UI]
	//Leaf path, thus we insert a button
	ins [p] ui=:(UI _ attr cs) []
		# attr = 'DM'.unions
			[ attr
			, textAttr p
			, valueAttr $ maybe (JSONString "") id $ 'DM'.get "actionId" attr]
		= [UI UIButton attr cs]
	//Fork but we haven't found a matching node
	ins [p:ps] ui []
		= [UI UIMenu (textAttr p) $ ins ps ui []]
	//Fork and there is already a menu tree, so we look for the matching node
	ins [p:ps] ui [(UI t attr cs):us]
		// If the label on the menu node matches we can add it there
		| maybe False ((==) $ JSONString p) $ 'DM'.get "text" attr
			= [UI t attr (ins ps ui cs):us]
		// Otherwise we create a new menu node
		= [(UI t attr cs):ins [p:ps] ui us]
*/

arrangeSplit :: !UIDirection !Bool -> LayoutRule
arrangeSplit direction resize 
	= sequenceLayouts
		[layoutSubUIs (SelectByPath []) (if (direction === Horizontal) arrangeHorizontal arrangeVertical)
		,layoutSubUIs SelectChildren (setUIAttributes (sizeAttr FlexSize FlexSize))
		]

arrangeVertical :: LayoutRule
arrangeVertical = addCSSClass "itasks-vertical"

arrangeHorizontal :: LayoutRule
arrangeHorizontal = addCSSClass "itasks-horizontal"

frameCompact :: LayoutRule
frameCompact = sequenceLayouts
	[addCSSClass "itasks-frame-compact-inner"
	,wrapUI UIContainer
	,addCSSClass "itasks-frame-compact-outer"
	]

scrollContent :: LayoutRule
scrollContent = addCSSClass "itasks-scroll-content"

toWindow :: UIWindowType UIVAlign UIHAlign -> LayoutRule
toWindow windowType vpos hpos = sequenceLayouts
	[wrapUI UIWindow
	//Move title and class attributes to window
	,copySubUIAttributes (SelectKeys ["title"]) [0] []
	,layoutSubUIs (SelectByPath [0]) (delUIAttributes (SelectKeys ["title"]))
	//Set window specific attributes
	,setUIAttributes ('DM'.unions [windowTypeAttr windowType,vposAttr vpos, hposAttr hpos])
	]

insertToolBar :: [String] -> LayoutRule
insertToolBar actions = sequenceLayouts
	[insertChildUI 0 (ui UIToolBar)
	,moveSubUIs ('Data.Foldable'.foldl1 SelectOR [SelectByAttribute "actionId" ((==) (JSONString action))\\ action <- actions]) [0] 0
	,layoutSubUIs (SelectByPath [0]) (layoutSubUIs (SelectByType UIAction) actionToButton)
	]

toEmpty :: LayoutRule
toEmpty = setUIType UIEmpty

toContainer :: LayoutRule
toContainer = setUIType UIContainer 

toPanel :: Bool -> LayoutRule
toPanel fs = sequenceLayouts
	[setUIType UIPanel
	:if fs [setUIAttributes ('DM'.put "fullscreenable" (JSONBool True) 'DM'.newMap)] [] 
	]

actionToButton :: LayoutRule
actionToButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) (\attr -> maybe 'DM'.newMap
		(\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a,icon a])
		('DM'.get "actionId" attr))
	]
where
	//Set default icons
	icon "Ok" = iconClsAttr "icon-ok"
	icon "Cancel" = iconClsAttr "icon-cancel"
	icon "Yes" = iconClsAttr "icon-yes"
	icon "No" = iconClsAttr "icon-no"
	icon "Next" = iconClsAttr "icon-next"
	icon "Previous" = iconClsAttr "icon-previous"
	icon "Finish" = iconClsAttr "icon-finish"
	icon "Continue" = iconClsAttr "icon-next"
	icon "/File/Open" = iconClsAttr "icon-open"
	icon "/File/Save" = iconClsAttr "icon-save"
	icon "/File/Save as" = iconClsAttr "icon-save"
	icon "/File/Quit" = iconClsAttr "icon-quit"
	icon "/Help/Help" = iconClsAttr "icon-help"
	icon "/Help/About" = iconClsAttr "icon-about"
	icon "/Edit/Find" = iconClsAttr "icon-find"
	icon "New" = iconClsAttr "icon-new"
	icon "Edit" = iconClsAttr "icon-edit"
	icon "Delete" = iconClsAttr "icon-delete"
	icon "Refresh" = iconClsAttr "icon-refresh"
	icon "Close" = iconClsAttr "icon-close"
	icon _ = 'DM'.newMap

setActionIcon :: (Map String String) -> LayoutRule
setActionIcon icons = sequenceLayouts
	// Buttons and actions
	[layoutSubUIs (SelectOR (SelectByType UIAction) (SelectByType UIButton))
		$ ic "actionId"
	,layoutSubUIs (SelectByType UIMenu)
		$ ic "text"
	]
where
	ic field = modifyUIAttributes (SelectKeys [field]) $ \attr->fromMaybe attr
		$ 'DM'.get field attr
		  >>= \(JSONString f) -> 'DM'.get f icons
		  >>= \icon ->           return ('DM'.union (iconClsAttr ("icon-" +++ icon)) attr)

instance tune ArrangeWithTabs Task
where tune (ArrangeWithTabs b) t = tune (ApplyLayout (arrangeWithTabs b)) t

instance tune ArrangeWithSideBar Task 
where
    tune (ArrangeWithSideBar index side resize) t = tune (ApplyLayout (arrangeWithSideBar index side resize)) t

instance tune ArrangeWithHeader Task
where
    tune (ArrangeWithHeader index) t = tune (ApplyLayout (arrangeWithHeader index)) t

instance tune ArrangeAsMenu Task
where
	tune (ArrangeAsMenu i) t = tune (ApplyLayout (arrangeAsMenu i)) t

instance tune ArrangeSplit Task
where
    tune (ArrangeSplit direction resize) t = tune (ApplyLayout (arrangeSplit direction resize)) t

instance tune ArrangeVertical Task
where
    tune ArrangeVertical t = tune (ApplyLayout arrangeVertical)  t

instance tune ArrangeHorizontal Task
where
    tune ArrangeHorizontal t = tune (ApplyLayout arrangeHorizontal) t

instance tune ScrollContent Task
where
    tune ScrollContent t = tune (ApplyLayout scrollContent) t

instance tune AddCSSClass Task
where
	tune (AddCSSClass s) t = tune (ApplyLayout (addCSSClass s)) t

instance tune CSSStyle Task
where
	tune (CSSStyle s) t = tune ("style",JSONString s) t

instance tune ToWindow Task
where
	tune (ToWindow windowType vpos hpos) t = tune (ApplyLayout (toWindow windowType vpos hpos)) t

instance tune InPanel Task
where
	tune (InPanel fullscreenable) t =  tune (ApplyLayout (toPanel fullscreenable)) t

instance tune InContainer Task
where
	tune InContainer t = tune (ApplyLayout toContainer) t

instance tune NoUserInterface Task
where
    tune NoUserInterface task = Task (eval task)
    where
	    eval (Task task) event repOpts iworld = case task event repOpts iworld of
			(ValueResult taskvalue evalinfo _ newtask, iworld)
				# change = case event of 
					ResetEvent = ReplaceUI (ui UIEmpty)
					_          = NoChange
				= (ValueResult taskvalue evalinfo change (Task (eval newtask)), iworld)
			other = other

toFormItem :: LayoutRule
toFormItem = layoutSubUIs (SelectAND (SelectByPath []) (SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE) (SelectByHasAttribute HINT_ATTRIBUTE)))
	(sequenceLayouts
		//Create the 'row' that holds the form item
		[wrapUI UIContainer
		,addCSSClass "itasks-form-item"
		//If there is a label attribute, create a label 
		,optAddLabel
		//If there is hint attribute, create an extra icon 
		,optAddIcon
		,removeLabelAttribute
		]
	)
where
	optAddLabel = layoutSubUIs (SelectByContains (SelectAND (SelectByPath [0]) (SelectByHasAttribute LABEL_ATTRIBUTE))) addLabel
	addLabel = sequenceLayouts
		[insertChildUI 0 (uia UILabel (widthAttr WrapSize))
		,sequenceLayouts
			[copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
			,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes (SelectKeys ["label","optional","mode"]) createLabelText)
			]
		]
	where
		createLabelText attr = textAttr text
		where	
			text = formatDefaultLabel label +++ (if (enterOrUpdate && not optional) "*" "") +++ ":"
			formatted = formatDefaultLabel label
			enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attr) 
			optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr) 
			label = maybe "-" (\(JSONString s) -> s) ('DM'.get "label" attr)

	optAddIcon = layoutSubUIs (SelectByContains (SelectAND SelectChildren (SelectByHasAttribute HINT_ATTRIBUTE)))
					(sequenceLayouts
						[layoutSubUIs (SelectAND (SelectByPath []) (SelectByNumChildren 2)) (addIcon 2) //A label was added
						,layoutSubUIs (SelectAND (SelectByPath []) (SelectByNumChildren 1)) (addIcon 1) //No label was added
						]
					)

	addIcon iconIndex = sequenceLayouts
		[insertChildUI iconIndex (ui UIIcon)
		,copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [iconIndex - 1] [iconIndex]
		,layoutSubUIs (SelectByPath [iconIndex]) (modifyUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) createIconAttr)
		]
	where
		createIconAttr attr = 'DM'.unions [iconClsAttr iconCls, tooltipAttr tooltip]
		where 
			iconCls = maybe "icon-info" (\(JSONString t) -> "icon-" +++ t) ('DM'.get HINT_TYPE_ATTRIBUTE attr)
			tooltip = maybe "-" (\(JSONString s) -> s) ('DM'.get HINT_ATTRIBUTE attr)

	formatDefaultLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
	where
		[lname:lnames]		= fromString label
		addspace []			= []
		addspace [c:cs]
			| c == '_'			= [' ':addspace cs]
			| isUpper c			= [' ',toLower c:addspace cs]
			| otherwise			= [c:addspace cs]

	removeLabelAttribute = layoutSubUIs (SelectAND SelectChildren (SelectByHasAttribute "label"))
	                                    (delUIAttributes (SelectKeys ["label"]))

