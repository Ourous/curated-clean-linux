implementation module iTasks.Extensions.Dashboard
import iTasks
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.JavaScript
import qualified Data.Map as DM, Data.Error
import Text.HTML, StdMisc, StdArray, Data.Func

derive JSONEncode ControlLight
derive JSONDecode ControlLight
derive gEq ControlLight
derive gDefault ControlLight
derive gText ControlLight

gEditor{|ControlLight|} = controlLightEditlet

//SVG Based fake control light
controlLightEditlet :: Editor ControlLight
controlLightEditlet = leafEditorToEditor
	{LeafEditor
      |genUI  = withClientSideInit initUI genUI
      ,onEdit = \_ (_,()) m vst -> (Ok (NoChange,m),vst)
      ,onRefresh = \_ val st vst -> (Ok (if (valueFromState st === Just val) NoChange (ChangeUI [SetAttribute "value" (JSONString (color val))] []),st),vst)
	  ,valueFromState = valueFromState
      }
where
	genUI attr dp mode world
		# val = fromMaybe LightOff (editModeValue mode)
		# attr = 'DM'.unions [sizeAttr (ExactSize 20) (ExactSize 20),valueAttr (JSONString (toString (svgLight (color val)))), attr]
		= (Ok (uia UIHtmlView attr,val), world)

    initUI me world 
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) me world
		# world = (me .# "onAttributeChange" .= jsOnAttributeChange) world
		= world

	valueFromState s = Just s

	onAttributeChange me {[0]=name,[1]=value} world = case jsValToString name of
		Just "diff"
			# (color, world) = jsValToList` value id world
			# (svgEl,world)    = me .# "domEl" .# "children" .# 0 .? world
			# (lightEl,world)  = svgEl .# "children" .# 1 .? world
			# (_,world)        = (lightEl .# "setAttribute" .$ ("fill",color)) world //Just update the color
			= world
		_
			= jsTrace "Unknown attribute change" world

    color LightOnGreen  = "green"
    color LightOnRed    = "red"
    color LightOnOrange = "orange"
    color _             = "#333"

    svgLight val = SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          [defs,light val,glass,flare]

    defs  = DefsElt [] [] [glassgr,flaregr]
    where
    	glassgr = RadialGradientElt [IdAttr "glass-gradient"] []
				   [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "100%",StopColorAttr "white",StopOpacityAttr "0"]]
    	flaregr = LinearGradientElt [IdAttr "flare-gradient"] [X1Attr ("0",PX),X2Attr ("0",PX),Y1Attr ("0",PX),Y2Attr ("1",PX)] 
                   [StopElt [] [OffsetAttr "0%",StopColorAttr "white"],StopElt [] [OffsetAttr "90%",StopColorAttr "white",StopOpacityAttr "0"]]

    light val = CircleElt [] [CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX),FillAttr (PaintColor (SVGColorText val) Nothing)]
    glass = CircleElt [StyleAttr "stroke: #000;stroke-width: 8px"] [FillAttr (PaintFuncIRI (IRI ("#glass-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("50",PX),RAttr ("45",PX)]
    flare = EllipseElt [] [FillAttr (PaintFuncIRI (IRI ("#flare-gradient")) Nothing),CxAttr ("50",PX),CyAttr ("45",PX),RxAttr ("35",PX),RyAttr ("30",PX)]
