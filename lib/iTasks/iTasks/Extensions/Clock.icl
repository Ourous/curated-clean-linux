implementation module iTasks.Extensions.Clock
/**
* This module provides a type for visualizing time as an analog clock
*/
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor
import iTasks.UI.JS.Interface
import iTasks.Extensions.DateTime
import qualified Data.Map as DM, Data.Tuple, Data.Error
import Text.HTML, Data.Func

derive JSONEncode AnalogClock
derive JSONDecode AnalogClock
derive gEq AnalogClock
derive gDefault AnalogClock
derive gText AnalogClock

gEditor{|AnalogClock|} = analogClockEditor

//SVG Based analog clock editlet
analogClockEditor :: Editor AnalogClock
analogClockEditor = leafEditorToEditor
	{LeafEditor
	|genUI          = withClientSideInit initUI genUI
	,onEdit         = onEdit
	,onRefresh      = onRefresh
	,valueFromState = valueFromState
	}
where
	genUI dp mode world
		# time=:(AnalogClock {Time|hour,min,sec}) = fromMaybe (AnalogClock {Time|hour=0,min=0,sec=0}) $ editModeValue mode
		# attr = 'DM'.unions [sizeAttr (ExactSize 100) (ExactSize 100),valueAttr (JSONString (toString (svgClock hour min sec)))]
		= (Ok (uia UIHtmlView attr,time), world)
	where
		svgClock hour min sec 
			= SvgTag [StyleAttr "flex: 1; align-self: stretch;"] [ViewBoxAttr "0" "0" "100" "100"]
                          (face ++
                          [hand 45 (degrees 0 sec) "#000"
                          ,hand 50 (degrees 1 min) "#666"
                          ,hand 40 (degrees 2 hour) "#999"])

    face = [RectElt [WidthAttr "100px",HeightAttr "100px",StyleAttr "fill:#ccc;stroke: #000;stroke-width: 3px"] [XAttr ("0",PX),YAttr ("0",PX)]
           :[RectElt [WidthAttr "10px",HeightAttr "2px",StyleAttr "fill: #ddd;"]
                     [XAttr ("90",PX),YAttr ("50",PX),TransformAttr [RotateTransform (toString (30*i)) (Just ("50","50"))]] \\ i <- [0..11]
            ]]

    hand len angle color
        = RectElt [WidthAttr (toString len +++"px"),HeightAttr "2px",StyleAttr ("fill: "+++color)]
                  [XAttr ("50",PX),YAttr ("50",PX),TransformAttr [RotateTransform (toString (angle - 90)) (Just ("50","50"))]]

	initUI me world
		//Register listener for ui diffs from the server
		# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
		# world = (me .# "onAttributeChange" .= jsOnAttributeChange) world
		= world

	onAttributeChange me args world
		| jsArgToString (args !! 0) == "diff"
			# (changes, world) = fromJSArray (toJSVal (args !! 1)) id world
			# world = foldl (updateHand me) world changes
			= (jsNull,world)
		| otherwise
			= (jsNull,jsTrace "Unknown attribute change" world)

    updateHand me world change
		# (which,world) = appFst jsValToInt (.? (change .# 0) world)
		# (value,world) = appFst jsValToInt (.? (change .# 1) world)
		# (svgEl,world)  = .? (me .# "domEl" .# "children" .# 0) world
		# (handEl,world) = .? (svgEl .# "children" .# (13 + which)) world //The first 13 svg elements are the clock face and markers
        # (_,world)      = (handEl .# "setAttribute" .$ [toJSArg "transform",toJSArg ("rotate("+++toString (degrees which value - 90)+++" 50 50)")]) world
        = world

	degrees 0 v = 6 * v
	degrees 1 v = 6 * v
	degrees 2 v = 30 * v

	onRefresh _ new=:(AnalogClock t2) old=:(AnalogClock t1) vst = case ((if (t1.Time.sec == t2.Time.sec) [] [(0,t2.Time.sec)])
						 ++ (if (t1.Time.min == t2.Time.min) [] [(1,t2.Time.min)])
						 ++ (if (t1.Time.hour == t2.Time.hour) [] [(2,t2.Time.hour)])
						 ) of [] = (Ok (NoChange,old),vst) ; delta = (Ok (ChangeUI [SetAttribute "diff" (toJSON delta)] [],new),vst)

	onEdit dp ([],()) s vst = (Ok (NoChange,s),vst)
	valueFromState s = Just s
