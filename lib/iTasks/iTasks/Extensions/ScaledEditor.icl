implementation module iTasks.Extensions.ScaledEditor

import StdFunctions, StdString, StdList, StdBool, StdArray
import Text.HTML, Text.GenJSON, Data.Error, Data.Func
import iTasks.UI.Definition
import iTasks.UI.Editor
import iTasks.UI.Editor.Modifiers
import iTasks.UI.JavaScript
import iTasks.Internal.Serialization
import qualified Data.Map as DM

//Basic idea:
//- Give the inner editor an exact size in pixels
//- Wrap the editor in a container
//- Add an onResize handler on the container that measures both the outer
//  and inner element and sets a CSS transform on the inner element.

scaledEditor :: Int Int (Editor a) -> Editor a
scaledEditor width height editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	fixedEditor = (sizeAttr (ExactSize width) (ExactSize height)) @>> editor

	genUI attr datapath mode vst = case fixedEditor.Editor.genUI attr datapath (mapEditMode id mode) vst of
		(Ok (editorUI,editorState),vst) 
 			# (initUIString, vst) = serializeForClient (wrapInitUIFunction initUI) vst 
			= (Ok (wrapUI initUIString editorUI,editorState),vst)
		(Error e,vst) = (Error e,vst)

	onEdit datapath event state vst = case fixedEditor.Editor.onEdit datapath event state vst of
		(Ok (change,state),vst) 
 			# (initUIString, vst) = serializeForClient (wrapInitUIFunction initUI) vst 
			= (Ok (wrapChange initUIString change,state),vst)
		(Error e,vst) = (Error e,vst)

	onRefresh datapath value state vst = case fixedEditor.Editor.onRefresh datapath value state vst of
		(Ok (change,state),vst)
 			# (initUIString, vst) = serializeForClient (wrapInitUIFunction initUI) vst 
			= (Ok (wrapChange initUIString change,state),vst)
		(Error e,vst) = (Error e,vst)

	valueFromState = fixedEditor.Editor.valueFromState

	wrapUI initUI ui = uiac UIContainer ('DM'.fromList [("initUI",JSONString initUI)]) [ui]

	wrapChange initUI NoChange = NoChange
	wrapChange initUI (ReplaceUI ui) = ReplaceUI (wrapUI initUI ui)
	wrapChange initUI change = ChangeUI [] [(0,ChangeChild change)]
	
	//Add the onResize event handler on the wrapping container to scale the inner element
	initUI me world 
		# (jsOnResize,world) = jsWrapFun (onResize me) me world
		# world = (me .# "onResize" .= jsOnResize) world
		= world

	onResize me args world
		//Select the inner editor's dom element
		# (children,world) = (me .# "domEl.children") .? world
		# (innerEl,world) = (children .# 0) .? world
		//Measure the inner size of the container element
		# (domElClientHeight,world) = (me .# "domEl.clientHeight") .? world
		# (domElClientWidth,world) = (me .# "domEl.clientWidth") .? world
		//Measure the outer size of the editor's element
		# (innerElOffsetHeight,world) = (innerEl .# "offsetHeight") .? world
		# (innerElOffsetWidth,world) = (innerEl .# "offsetWidth") .? world
		//Determine the scale factor
		# scaleHeight = toReal (fromMaybe 1 (jsValToInt domElClientHeight)) / toReal (fromMaybe 1 (jsValToInt innerElOffsetHeight))
		# scaleWidth = toReal (fromMaybe 1 (jsValToInt domElClientWidth)) / toReal (fromMaybe 1 (jsValToInt innerElOffsetWidth))
		# scale = min scaleHeight scaleWidth
		# world = (innerEl .# "style.transformOrigin" .= "top left") world
		# world = (innerEl .# "style.transform" .= ("scale(" +++ toString scale +++ ")")) world
		= world 
